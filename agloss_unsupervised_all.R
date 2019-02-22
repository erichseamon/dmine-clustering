
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


library(reshape2) ; damage_loss <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Loss"), sum)
library(reshape2) ; damage_lossperacre <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Lossperacre"), sum)
library(reshape2) ; damage_lossperclaim <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Lossperclaim"), sum)

damage_loss <- subset(damage_loss, Commodity == "CORN")
damage_loss <- subset(damage_loss, Year == 2010)
#damage_loss <- subset(damage_loss, State != "Minnesota")

damage_loss <- transform(damage_loss, ID=paste(State, County, sep="-"))
rownames(damage_loss) <- damage_loss$ID

damage_loss <- damage_loss[,5:38]

damage_loss <- damage_loss[, colSums(damage_loss != 0) > 0]

damage_loss <- damage_loss[ -c(7, 15, 19) ]


#---

damage_lossperacre <- subset(damage_lossperacre, Commodity == "WHEAT")
damage_lossperacre <- subset(damage_lossperacre, Year == 2014)

damage_lossperacre <- transform(damage_lossperacre, ID=paste(State, County, sep="-"))
rownames(damage_lossperacre) <- damage_lossperacre$ID

damage_lossperacre <- damage_lossperacre[,5:38]

damage_lossperacre <- damage_lossperacre[, colSums(damage_lossperacre != 0) > 0]

damage_lossperacre <- damage_loss[ -c(7, 15, 19) ]

#---

#---

damage_lossperclaim <- subset(damage_lossperclaim, Commodity == "WHEAT")
damage_lossperclaim <- subset(damage_lossperclaim, Year == 2014)

damage_lossperclaim <- transform(damage_lossperclaim, ID=paste(State, County, sep="-"))
rownames(damage_lossperclaim) <- damage_lossperclaim$ID

damage_lossperclaim <- damage_lossperclaim[,5:38]

damage_lossperclaim <- damage_lossperclaim[, colSums(damage_lossperclaim != 0) > 0]

damage_lossperclaim <- damage_lossperclaim[ -c(7, 15, 19) ]



#res.dist <- get_dist(damage_loss, stand = TRUE, method = "pearson")

#fviz_dist(res.dist, 
#          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#how many clusters?

library("factoextra")
fviz_nbclust(damage_lossperacre, kmeans, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)


km.res_lossperacre <- kmeans(damage_lossperacre, 9, nstart = 25)

#fviz

# Visualize
library("factoextra")
fviz_cluster(km.res_lossperacre, data = damage_lossperacre, ellipse.type = "norm")+
  theme_minimal()

d.agr <- daisy(damage_lossperacre, metric = "gower", stand = TRUE)
plot(pam(d.agr, 3))
d.agr
d.agr2 <- as.matrix(d.agr) # via as.matrix.dist(.)


plot(agnes(d.agr), ask = TRUE)


#--damage_lossperclaim


#res.dist <- get_dist(damage_loss, stand = TRUE, method = "pearson")

#fviz_dist(res.dist, 
#          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#how many clusters?

library("factoextra")
fviz_nbclust(damage_lossperclaim, kmeans, method = "gap_stat")  +
  geom_vline(xintercept = 3, linetype = 2)

km.res <- kmeans(damage_lossperclaim, 3, nstart = 25)

#fviz

# Visualize
library("factoextra")
fviz_cluster(km.res, data = damage_lossperclaim, ellipse.type = "norm")+
  theme_minimal()

d.agr <- daisy(damage_lossperclaim, metric = "gower", stand = TRUE)
plot(pam(d.agr, 3))
d.agr
d.agr2 <- as.matrix(d.agr) # via as.matrix.dist(.)


plot(agnes(d.agr), ask = TRUE)


#------damage_loss

damage_loss_scaled <- scale(damage_loss)

library("factoextra")
fviz_nbclust(damage_loss, kmeans, method = "gap_stat")  +
  geom_vline(xintercept = 3, linetype = 2)

km.res <- kmeans(damage_loss, 9, nstart = 25)

#fviz

# Visualize
library("factoextra")
fviz_cluster(km.res, data = damage_loss, ellipse.type = "norm")+
  theme_minimal()


# PAM

library("cluster")
pam.res <- pam(damage_loss, 9)

# Visualize PAM
fviz_cluster(pam.res, ellipse.type = "norm")

library(mclust)
fit <- Mclust(damage_loss)
plot(fit) # plot results 
summary(fit) # display the best model

hclust(damage_loss, method = "complete", members = NULL)

d.agr <- daisy(damage_loss, metric = "gower", stand = TRUE)
plot(pam(d.agr, 3))
d.agr
d.agr2 <- as.matrix(d.agr) # via as.matrix.dist(.)


plot(agnes(d.agr), ask = TRUE)


#---mapping clusters 

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--damage_loss

damage_loss_pam <- data.frame(pam.res$clustering)
damage_loss_pam$State <- rownames(damage_loss_pam)

cluster <- data.frame(damage_loss_pam$pam.res.clustering)
colnames(cluster) <- c("cluster")
damage_loss_pam <- data.frame(do.call('rbind', strsplit(as.character(damage_loss_pam$State),'-',fixed=TRUE)))
damage_loss_pam <- cbind(cluster$cluster, damage_loss_pam)
colnames(damage_loss_pam) <- c("Cluster", "STATE_NAME", "NAME")

m <- merge(counties, damage_loss_pam, by=c("STATE_NAME", "NAME"))

pal <- colorNumeric(brewer.pal(11, "Spectral"), na.color = "#ffffff",
                    domain = eval(parse(text=paste("m$", "Cluster", sep=""))))


exte <- as.vector(extent(counties))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Cluster", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Cluster", sep=""))))


leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", "Cluster", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal, values = ~eval(parse(text=paste("m$", "Cluster", sep=""))), opacity = 0.5, title = NULL,
            position = "bottomright")


#--damage_lossperacre

damage_lossperacre_pam <- data.frame(km.res_lossperacre$cluster)
damage_lossperacre_pam$State <- rownames(damage_lossperacre_pam)

cluster <- data.frame(damage_lossperacre_pam$km.res_lossperacre.cluster)
colnames(cluster) <- c("cluster")
damage_lossperacre_pam <- data.frame(do.call('rbind', strsplit(as.character(damage_lossperacre_pam$State),'-',fixed=TRUE)))
damage_lossperacre_pam <- cbind(cluster$cluster, damage_lossperacre_pam)
colnames(damage_lossperacre_pam) <- c("Cluster", "STATE_NAME", "NAME")

m <- merge(counties, damage_lossperacre_pam, by=c("STATE_NAME", "NAME"))

pal <- colorNumeric(brewer.pal(11, "Spectral"), na.color = "#ffffff",
                    domain = eval(parse(text=paste("m$", "Cluster", sep=""))))


exte <- as.vector(extent(counties))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Cluster", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Cluster", sep=""))))


leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(eval(parse(text=paste("m$", "Cluster", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal, values = ~eval(parse(text=paste("m$", "Cluster", sep=""))), opacity = 0.5, title = NULL,
            position = "bottomright")


