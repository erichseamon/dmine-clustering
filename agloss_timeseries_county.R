agloss_timeseries_county <- function(damagecause, crops, unit) {
  
  
#  [1] Area Plan Crops Only              Cold Wet Weather                 
#  [3] Cold Winter                       Cyclone                          
#  [5] Decline in Price                  Drought                          
#  [7] Earthquake                        Excess Moisture/Precip/Rain      
#  [9] Failure Irrig Equip               Failure Irrig Supply             
#  [11] Fire                              Flood                            
#  [13] Freeze                            Frost                            
#  [15] Hail                              Heat                             
#  [17] Hot Wind                          Hurricane/Tropical Depression    
#  [19] Insects                           Mycotoxin (Aflatoxin)            
#  [21] Other Causes                      Other (Snow-Lightning-Etc.)      
#  [23] Plant Disease                     Tornado                          
#  [25] Volcanic Eruption                 Wildlife                         
#  [27] Wind/Excess Wind                  Falling Numbers                  
#  [29] Inability to Prepare Land for Irr Federal or State Ordered Destruct
  
  library("cluster")
  library("factoextra")
  
  gc()
  
  #RMA by damage cause, includes claim counts
  #damage <- read.csv("https://nextcloud.sesync.org/index.php/s/W5kztdb5ZkxRptT/download")
  damage <- read.csv("/dmine/code/git/dmine-clustering/damage.csv")
  damage <- damage[,2:13]
  #damage load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/")
  #damage <- read.csv("RMA_damage_combined.csv", strip.white=TRUE)
  
  
  colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  damage$State <- state.name[match(damage$State,state.abb)]
  
  damage_loss <- subset(damage, Commodity == crops)
  damage_loss <- subset(damage_loss, Damagecause == damagecause)
  
  damage_loss <- subset(damage_loss, State == "Washington" | State == "Idaho" | State == "Oregon")
  
  
  damage_loss_county <- subset(damage_loss, County == "Clearwater" | County == "Kootenai" | County == "Benewah" 
                               | County == "Latah" | County == "Nez Perce" | County == "Lewis" 
                               | County == "Idaho" | County == "Wasco" | County == "Sherman" 
                               | County == "Gilliam" | County == "Morrow" | County == "Umatilla" 
                               | County == "Union" | County == "Wallowa" | County == "Douglas" 
                               | County == "Walla Walla" | County == "Benton" | County == "Columbia" 
                               | County == "Asotin" | County == "Garfield" | County == "Franklin" 
                               | County == "Grant" | County =="Whitman" | County == "Spokane" 
                               | County == "Lincoln" | County == "Adams" )
  
  
  damage_loss_county$Statecode <- as.numeric(gsub("Idaho", 1, gsub("Washington", 2, gsub("Oregon", 3, damage_loss_county$State))))
  
  ID_all <- expand.grid(Year = seq(2001, 2015, 1), County = c("Clearwater","Kootenai","Benewah" 
                                                     ,"Latah","Nez Perce","Lewis"
                                                     ,"Idaho"),
              State = c("Idaho"))
  
  OR_all <- expand.grid(Year = seq(2001, 2015, 1), County = c("Wasco","Sherman" 
                                                    ,"Gilliam","Morrow","Umatilla" 
                                                    ,"Union","Wallowa"),
              State = c("Oregon"))
  
  WA_all <- expand.grid(Year = seq(2001, 2015, 1), County = c("Douglas" 
                                                    ,"Walla Walla","Benton","Columbia" 
                                                    ,"Asotin","Garfield","Franklin" 
                                                    ,"Grant","Whitman","Spokane" 
                                                    ,"Lincoln","Adams"),
              State = c("Washington"))
  
  IPNW_all <- rbind(ID_all, OR_all, WA_all)
  
  damage_loss_county_joined <- plyr::join(IPNW_all, damage_loss_county, type = "left", by = c("Year", "State", "County"))
  damage_loss_county_joined$Loss[is.na(damage_loss_county_joined$Loss)] <- 0
  damage_loss_county_joined$Lossperacre[is.na(damage_loss_county_joined$Lossperacre)] <- 0
  damage_loss_county_joined$Acres[is.na(damage_loss_county_joined$Acres)] <- 0
  damage_loss_county_joined$Lossperclaim[is.na(damage_loss_county_joined$Lossperclaim)] <- 0
  damage_loss_county_joined$Count[is.na(damage_loss_county_joined$Count)] <- 0
  damage_loss_county_joined$Acresperclaim[is.na(damage_loss_county_joined$Acresperclaim)] <- 0
  damage_loss_county_joined$Statecode <- as.numeric(gsub("Idaho", 1, gsub("Washington", 2, gsub("Oregon", 3, damage_loss_county_joined$State))))
  
  
  damage_loss_county_joined$Commodity = crops
  damage_loss_county_joined$Damagecause = damagecause
  
  
  agg <- aggregate(eval(parse(text=paste("damage_loss_county_joined$", unit, sep=""))), by = list(damage_loss_county_joined$State, damage_loss_county_joined$County), FUN = sum)
  agg2 <- subset(agg, x != 0)
  colnames(agg2) <- c("State", "County", "x")
  agg3 <- agg2[,1:2]
  
  damage_loss_county2 <- merge(agg3, damage_loss_county_joined, by = c("State", "County"))
  damage_loss_county2$County <- factor(damage_loss_county2$County)
  
  
  par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
  interaction.plot(x.factor     = damage_loss_county2$Year,
                   trace.factor = damage_loss_county2$County,
                   response     = eval(parse(text=paste("damage_loss_county2$", unit, sep=""))),
                   fun = sum,
                   las = 2,
                   type="b",
                   #col = damage_loss_county$Statecode,
                   col=c("black","red","green","blue"),  ### Colors for levels of trace var.
                   pch=c(61,59,57,55,53,51,49,47,45,43,41,39,37,35,25,23,21, 19, 17, 15, 13, 11, 9, 7, 5, 3),             ### Symbols for levels of trace var.
                   fixed=TRUE,                    ### Order by factor order in data
                   leg.bty = "n",
                   ylab="Loss ($)",
                   xlab="years",
                   main=paste("Interaction Plot - Loss ($) vs. year by County for \n IPNW, 2001-2015\n", crops, " ", damagecause, " ", unit, sep=""), las = 2)
  
}