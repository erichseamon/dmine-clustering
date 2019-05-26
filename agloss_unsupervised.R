#agloss_supervised.R
#creates a map of an unsupervised model of damage cause by county,
#for a select year, commodity, and unit.
#
#unit examples:
#
#Loss
#Lossperacre
#Lossperclaim
#
#Example:
#
#damagetype options: waterscarcity OR waterexcess
#agloss_unsupervised(2014, WHEAT, Lossperacres, waterscarcity)

agloss_unsupervised <- function(years, crops, units, damagetype) {
  
  #Water Scarcity and excess damage cause lists
  waterscarcity <- c("State", "County", "Year", "Commodity", "Drought","Failure Irrig Supply","Fire", "Heat", "Hot Wind")
  waterexcess <- c("State", "County", "Year", "Commodity", "Excess Moisture/Precip/Rain", "Flood","Storm Surge")
  
  #years <- 2011
  #crops <- "WHEAT"
  #units <- "Lossperacre"
  
  library(cluster)
  library(factoextra)
  library(maptools)
  library(leaflet)
  library(raster)
  library(RColorBrewer)
  library(ff)
  
  #access data from nextcloud
  #damage <- read.csv("https://nextcloud.sesync.org/index.php/s/W5kztdb5ZkxRptT/download")
  
  #access data from local UI server
  #damage <- read.csv("/dmine/code/git/dmine-clustering/damage.csv")
  #damage <- damage[,3:13]
  gc() 
  
  #access data from google drive
  id <- "1RxLEi0DiwMZFIw5CSBaEFjlLG67bKyLV" # google file ID
  damage <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), header = TRUE)
  damage <- damage[,2:12]
  
  #access data from local sesync server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/")
  #damage <- read.csv("RMA_damage_combined.csv", strip.white=TRUE)
  
  #used for data from cloud
  #colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  #damage$State <- state.name[match(damage$State,state.abb)]
  
  colnames(damage) <- c("Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  damage$State <- state.name[match(damage$State,state.abb)]
  
  
  #RMA DAMAGE FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  
  #damage <- subset(damage, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
  #colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
  #damage <- aggregate(damage$Loss, by=list(damage$Commodity, damage$Damagecause, damage$County, damage$State, damage$Year), FUN = "sum")
  #colnames(damage) <- c("Commodity", "Damagecause", "County", "State", "Year", "Loss")
  
  
  library(reshape2) ; damage_loss <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c(units), sum)
  #library(reshape2) ; damage_lossperacre <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c(unit), sum)
  #library(reshape2) ; damage_lossperclaim <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c(unit), sum)
  
  damage_loss <- subset(damage_loss, Commodity == crops)
  damage_loss <- subset(damage_loss, Year == years)
  
  #select only damage causes that are associates with Water Scarcity
  damage_loss <- subset(damage_loss, select= damagetype)
  damage_loss <- transform(damage_loss, ID=paste(State, County, sep="-"))
  rownames(damage_loss) <- damage_loss$ID
  

  damage_loss <- damage_loss[,5:length(damagetype)]
  
  #remove damage causes that have NO values for ANY counties
  #damage_loss <- damage_loss[, colSums(damage_loss != 0) > 0]
  
  
  #library("factoextra")
  #fviz_nbclust(damage_loss, kmeans, method = "gap_stat")  +
  #geom_vline(xintercept = 3, linetype = 2)
  
  
  #library(fpc)
  #pamk.best <- pamk(damage_loss)
  #cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
  #plot(pam(damage_loss, pamk.best$nc))
  
  
  
  
  #oldw <- getOption("warn")
  #options(warn = -1)
  
  #gap_stat <- clusGap(damage_loss, FUN = kmeans, nstart = 25,
  #                K.max = 10, B = 10)
  
  #options(warn = oldw)
  
  

  #identify the optimum number of clusters using NbClust
  
  library("NbClust")
  res<-NbClust(damage_loss[,2:(length(eval(damagetype)-4))], diss=NULL, distance = "euclidean", min.nc=2, max.nc=20, 
               method = "kmeans", index = "all") 
  res$All.index
  res$Best.nc
  res$All.CriticalValues
  res$Best.partition
  
  maxclusters <- length(unique(res$Best.partition))
  
  
  
 #fbviz_cluster elbow plot 
#  opt_cluster <- fviz_nbclust(damage_loss, kmeans, method = "wss")  +
#    geom_vline(xintercept = 3, linetype = 2) + labs(title= paste(crops, " ", years, " ", units, sep=""))
  #km.res <- kmeans(damage_loss, which.max(opt_cluster$data[1:10,2]), nstart = 25)
  
  
  km.res <- kmeans(damage_loss, maxclusters, nstart = 25)

  # Visualize clusters
  library("factoextra")
  clusterplot <- fviz_cluster(km.res, data = damage_loss, ellipse.type = "norm", main = paste(crops, " ", years, " ", units, sep=""))+
    theme_minimal()
  
  clusterplot
  
  
  # PAM
  
  #library("cluster")
  #pam.res <- pam(damage_loss, 9)
  
  # Visualize PAM
  #fviz_cluster(pam.res, ellipse.type = "norm")
  
  
  #---mapping clusters 
  
  setwd("/dmine/data/counties/")
  
  counties <- readShapePoly('UScounties_conus.shp',
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  
  #--damage_loss
  
  damage_loss_km <- data.frame(km.res$cluster)
  damage_loss_km$State <- rownames(damage_loss_km)
  
  cluster <- data.frame(damage_loss_km$km.res.cluster)
  colnames(cluster) <- c("cluster")
  damage_loss_km <- data.frame(do.call('rbind', strsplit(as.character(damage_loss_km$State),'-',fixed=TRUE)))
  damage_loss_km <- cbind(cluster$cluster, damage_loss_km)
  colnames(damage_loss_km) <- c("Cluster", "STATE_NAME", "NAME")
  
  m <- merge(counties, damage_loss_km, by=c("STATE_NAME", "NAME"))
  
  pal <- colorNumeric(brewer.pal(11, "Spectral"), na.color = "#ffffff",
                      domain = eval(parse(text=paste("m$", "Cluster", sep=""))))
  
  
  exte <- as.vector(extent(counties))
  
  label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Cluster", sep=""))), 0))
  markers <- data.frame(label)
  labs <- as.list(eval(parse(text=paste("m$", "Cluster", sep=""))))
  
  
  map <- leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", "Cluster", sep="")))), popup = markers$label,  weight = 1) %>%
    addLegend(pal = pal, values = ~eval(parse(text=paste("m$", "Cluster", sep=""))), bins = 3, opacity = 0.5, title = paste(years, " ", crops, " ", units, sep=""),
              position = "bottomright")
  
  #return(list(opt_cluster, fviz_cluster(km.res, data = damage_loss, ellipse.type = "norm", main = paste("Corn ", years, " Loss", sep=""))+
      #          theme_minimal(), map))
  return(list(clusterplot, map))
  
}
