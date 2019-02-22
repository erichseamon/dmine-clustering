agloss_unsupervised_clusterplot <- function(years, crops, units) {
  
  
  library("cluster")
  library("factoextra")
  
  gc()
  
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
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  gap_stat <- clusGap(damage_loss, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 10)
  
  options(warn = oldw)
  
  
  #km.res <- kmeans(damage_loss, which.max(gap_stat$Tab[1:10,3]), nstart = 25)
  
  cluster <- fviz_nbclust(damage_loss, kmeans, method = "wss")  +
    geom_vline(xintercept = 3, linetype = 2)
  
  
  km.res <- kmeans(damage_loss, which.max(cluster$data[1:10,2]), nstart = 25)
  
  
  
  #fviz
  
  # Visualize
  library("factoextra")
  fviz_cluster(km.res, data = damage_loss, ellipse.type = "norm", main = paste("Corn ", years, " Loss", sep=""))+
    theme_minimal()
  
  
}

fviz_nbclust(damage_loss, kmeans, method = "silhouette")  +
geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(damage_loss, hcut, method = "gap_stat")  +
  geom_vline(xintercept = 3, linetype = 2)

#method options: wss, silhouette, and gap stat


