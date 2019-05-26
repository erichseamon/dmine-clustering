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
  
}