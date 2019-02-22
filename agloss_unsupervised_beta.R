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
#agloss_unsupervised(2014, WHEAT, Loss)

agloss_unsupervised <- function(years, crops, units) {
  
  
  library("cluster")
  library("factoextra")
  library(maptools)
  
  #RMA by damage cause, includes claim counts
  damage <- read.csv("https://nextcloud.sesync.org/index.php/s/W5kztdb5ZkxRptT/download")
  
  #damage load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/")
  #damage <- read.csv("RMA_damage_combined.csv", strip.white=TRUE)
  
  
  colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
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
  #damage_loss <- subset(damage_loss, State != "Minnesota")
  
  damage_loss <- transform(damage_loss, ID=paste(State, County, sep="-"))
  rownames(damage_loss) <- damage_loss$ID
  
  damage_loss <- damage_loss[,5:38]
  
  damage_loss <- damage_loss[, colSums(damage_loss != 0) > 0]
  
  damage_loss <- damage_loss[ -c(7, 15, 19) ]
  
  
  library("factoextra")
  #fviz_nbclust(damage_loss, kmeans, method = "gap_stat")  +
  #  geom_vline(xintercept = 3, linetype = 2)
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  gap_stat <- clusGap(damage_loss, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 10)
  
  options(warn = oldw)
  
  
  km.res <- kmeans(damage_loss, which.max(gap_stat$Tab[2:10,3]), nstart = 25)
  
  #fviz
  
  # Visualize
  library("factoextra")
  #fviz_cluster(km.res, data = damage_loss, ellipse.type = "norm")+
  #  theme_minimal()
  
  
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
  
  
  leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", "Cluster", sep="")))), popup = markers$label,  weight = 1) %>%
    addLegend(pal = pal, values = ~eval(parse(text=paste("m$", "Cluster", sep=""))), opacity = 0.5, title = NULL,
              position = "bottomright")
  
  
  
  
  
  
}