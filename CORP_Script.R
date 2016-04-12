# CORP paper: 
# "Comparing automated methods for identifying areas of 
# critical heat demand in urban space"
# Hannes Seller, HafenCity Universität Hamburg
# April 11,2016

## Packages ####
library(dplyr)
library(foreign)
source("~/R/CORP/Funktionen_GEWISS.R")

## Loading data ####
setwd("~/R/CORP/shapes")

data1 <- read.dbf("~/R/CORP/shapes/Geom.dbf")
data2 <- read.dbf("~/R/CORP/shapes/Boundaries.dbf")
data3 <- read.dbf("~/R/CORP/shapes/Building.dbf")
data4 <- read.dbf("~/R/CORP/shapes/Years.dbf") 

data <- merge(data1, data2, by = "UUID")
data <- merge(data, data3, by = "UUID")
data <- merge(data, data4, by = "UUID")
rm(data1,data2,data3,data4)

write.csv(data,"~/R/CORP/csv/Hamburg.csv")

## Data preparation ####
# sorting out non-residential and fallacious entries

temp <- data                                                 # 371 583 objects # all buildings
temp <- temp[which(temp$GFK < 2000 & temp$GFK != 1313 ),]    # 219 521 objects # residential buildings
temp <- temp[which(temp$AdO != 0),]                          # 219 307 objects # overground res. buildings

write.csv(temp,"~/R/CORP/csv/Hamburg_residential.csv")
rm(temp)
# changed in csv: "ß" -> "ss", "ä" -> "ae", "ö" -> "oe", "ü" -> "ue" 

setwd("~/R/CORP/csv")

data <- read.csv("~/R/CORP/csv/Hamburg_residential.csv", sep=",", header=T)
data_copy <- data

## IWU typology ####
setwd("~/R/CORP/csv")

data <- read.csv("Hamburg_residential.csv", sep=",", header=T)


typ1 <- rep(NA, nrow(data))
typ1[which(data$AdO >0 & !is.na(data$Klasse) == T)]  <-"EFH"
typ1[which(data$AdO >2 & !is.na(data$Klasse) == T)]  <-"RH"
typ1[which(data$AdO >4 & !is.na(data$Klasse) == T)] <- "MFH"
typ1[which(data$AdO >6 & !is.na(data$Klasse) == T)] <- "GMH"

typ2 <- rep(NA, nrow(data))
typ2[which(data$Klasse == "vor 1859")]  <- "A"
typ2[which(data$Klasse == "1860-1918")] <- "B"
typ2[which(data$Klasse == "1919-1948")] <- "C"
typ2[which(data$Klasse == "1949-1957")] <- "D"
typ2[which(data$Klasse == "1958-1968")] <- "E"
typ2[which(data$Klasse == "1969-1978")] <- "F"
typ2[which(data$Klasse == "1979-1983")] <- "G"
typ2[which(data$Klasse == "1984-1994")] <- "H"
typ2[which(data$Klasse == "1995-2001")] <- "E"
typ2[which(data$Klasse == "2002-2009")] <- "J"
typ2[which(data$Klasse == "2010-2015")] <- "K"

#Typ = Typ1_Typ2
typ <- rep(NA, nrow(data))

for (i in 1:length(typ1))
{
  if(!is.na(typ1[i]) == T)
    typ[i] = paste(typ1[i], typ2[i], sep= "_")
  else
    typ[i] = ""
}

data$Typ <- typ
rm(typ, typ1, typ2, i)


iwu <- as.data.frame(table(data$Typ))
write.csv(iwu, "iwu_overview1.csv")


typ <- data$Typ
typ <- gsub("_K", "_J", typ)
typ <- gsub("GMH_A", "MFH_A", typ)
typ <- gsub("GMH_G", "MFH_G", typ)
typ <- gsub("GMH_H", "MFH_H", typ)
typ <- gsub("GMH_I", "MFH_I", typ)
typ <- gsub("GMH_J", "MFH_J", typ)
typ <- gsub("RH_A", "MFH_A", typ)


iwu <- as.data.frame(table(typ))
write.csv(iwu, "iwu_overview2.csv")

data$Typ <- typ
rm(typ,iwu)

write.csv(data,"~/R/CORP/csv/Hamburg_residential.csv")

## IWU specific heat demand ####
iwu <- read.csv("iwu.csv", sep=",", header=T)
iwu <- iwu[which(iwu$Zustand == "Ist"),]
data <- merge(data, iwu, by = "Typ"); rm(iwu)
data <- data[,c(3:26, 1, 28)]

data <- mutate(data,
               Demand = Geom_Area * AdO * 0.6 * NettoHeizwärme)

write.csv(data,"~/R/CORP/csv/Hamburg_residential.csv")

## Area selection ####

dataBBZ <- group_by(data, BBZ)
dataBBZ <- summarize(dataBBZ,
                  BBZ_stock = length(BBZ),
                  BBZ_demand = sum(Demand),
                  BBZ_density = sum(Demand)/max(BBZ_Area))

quantile(table(dataBBZ$BBZ), seq(0,1,0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 0    0    2    4    6    9   12   15   20   29  232 

dataSG <- group_by(data, SG)
dataSG <- summarize(dataSG,
                  SG_stock = length(SG),
                  SG_demand = sum(Demand),
                  SG_density = sum(Demand)/max(SG_Area)) %>%
                    arrange(desc(SG_stock, SG_density, SG_demand))

quantile(table(dataSG$SG), seq(0,1,0.1))
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# 1.0  21.0  39.0  57.0  74.0  91.0 110.0 139.0 188.0 258.4 959.0 

Bergedorf <- data[which(data$SG == 76010),]
Harburg <- data[which(data$SG == 93001),]

write.csv(Harburg,"~/R/CORP/csv/Harburg.csv")
write.csv(Bergedorf,"~/R/CORP/csv/Bergedorf.csv")

## Tesseleration ####

BergedorfRaster100 <- raster(578300, 579800, 5925500, 5927000, 100)
BergedorfRaster150 <- raster(578300, 579800, 5925500, 5927000, 150)
BergedorfRaster200 <- raster(578300, 579800, 5925500, 5927000, 200)

HarburgRaster100   <- raster(565000, 566500, 5920300, 5921400, 100)
HarburgRaster150   <- raster(565000, 566500, 5920300, 5921400, 150)  
HarburgRaster200   <- raster(565000, 566500, 5920300, 5921400, 200)

## Apply raster ####
## B 100 ####
B100 <- Bergedorf

for (i in 1:nrow(B100))
{
  temp <- B100[i,]
  temp2 <- BergedorfRaster100$Nr[which(temp$X >= BergedorfRaster100$X1 &
                                         temp$X <= BergedorfRaster100$X2 &
                                         temp$Y >= BergedorfRaster100$Y1 &
                                         temp$Y <= BergedorfRaster100$Y2)
                                 ]
  if(length(temp2)>0)
  {
    B100$Raster[i] <- temp2
  }
  else 
  {
    B100$Raster[i] <- ""
  }
}


for (i in 1:nrow(B100))
{
  B100$HeatArea[i] <-  B100$AdO[i] * B100$Geom_Area[i] * 0.6
} 

B100 <- group_by(B100, Raster) %>%
  mutate(DemandTotal = sum(Demand)/1000,
         DensityMWHpHA = sum(Demand)/1000/1)


write.csv(B100, "Bergedorf100.csv")

## B 150 ####
B150 <- Bergedorf

for (i in 1:nrow(B150))
{
  temp <- B150[i,]
  temp2 <- BergedorfRaster150$Nr[which(temp$X >= BergedorfRaster150$X1 &
                                               temp$X <= BergedorfRaster150$X2 &
                                               temp$Y >= BergedorfRaster150$Y1 &
                                               temp$Y <= BergedorfRaster150$Y2)
                                       ]
  if(length(temp2)>0)
  {
    B150$Raster[i] <- temp2
  }
  else 
  {
    B150$Raster[i] <- ""
  }
}

for (i in 1:nrow(B150))
{
  B150$HeatArea[i] <-  B150$AdO[i] * B150$Geom_Area[i] * 0.6
} 

B150 <- group_by(B150, Raster) %>%
  mutate(DemandTotal = sum(Demand),
         DensityMWHpHA = sum(Demand)/1000/2.25)

write.csv(B150, "Bergedorf150.csv")

### B 200 ####
B200 <- Bergedorf

for (i in 1:nrow(B200))
{
  temp <- B200[i,]
  temp2 <- BergedorfRaster200$Nr[which(temp$X >= BergedorfRaster200$X1 &
                                             temp$X <= BergedorfRaster200$X2 &
                                             temp$Y >= BergedorfRaster200$Y1 &
                                             temp$Y <= BergedorfRaster200$Y2)
                                ] 
  if(length(temp2)>0)
  {
    B200$Raster[i] <- temp2
  }
  else 
  {
    B200$Raster[i] <- ""
  }
}

for (i in 1:nrow(B200))
{
  B200$HeatArea[i] <-  B200$AdO[i] * B200$Geom_Area[i] * 0.6
} 

B200 <- group_by(B200, Raster) %>%
  mutate(DemandTotal = sum(Demand),
         DensityMWHpHA = sum(Demand)/1000/4)

write.csv(B200, "Bergedorf200.csv")

### H 100 ####
H100 <- Harburg

for (i in 1:nrow(H100))
{
  temp <- H100[i,]
  temp2 <- HarburgRaster100$Nr[which(temp$X >= HarburgRaster100$X1 &
                                       temp$X <= HarburgRaster100$X2 &
                                       temp$Y >= HarburgRaster100$Y1 &
                                       temp$Y <= HarburgRaster100$Y2)
                               ] 
  if(length(temp2)>0)
  {
    H100$Raster[i] <- temp2
  }
  else 
  {
    H100$Raster[i] <- ""
  }
}

for (i in 1:nrow(H100))
{
  H100$HeatArea[i] <-  H100$AdO[i] * H100$Geom_Area[i] * 0.6
} 

H100 <- group_by(H100, Raster) %>%
  mutate(DemandTotal = sum(Demand),
         DensityMWHpHA = sum(Demand)/1000/1)


write.csv(H100, "Harburg100.csv")

### H 150 ####
H150 <- Harburg

for (i in 1:nrow(H150))
{
  temp <- H150[i,]
  temp2 <- HarburgRaster150$Nr[which(temp$X >= HarburgRaster150$X1 &
                                       temp$X <= HarburgRaster150$X2 &
                                       temp$Y >= HarburgRaster150$Y1 &
                                       temp$Y <= HarburgRaster150$Y2)
                               ] 
  if(length(temp2)>0)
  {
    H150$Raster[i] <- temp2
  }
  else 
  {
    H150$Raster[i] <- ""
  }
}

for (i in 1:nrow(H150))
{
  H150$HeatArea[i] <-  H150$AdO[i] * H150$Geom_Area[i] * 0.6
} 

H150 <- group_by(H150, Raster) %>%
  mutate(DemandTotal = sum(Demand),
         DensityMWHpHA = sum(Demand)/1000/2.25)

write.csv(H150, "Harburg150.csv")

### H 200 ####
H200 <- Harburg

for (i in 1:nrow(H200))
{
  temp <- H200[i,]
  temp2 <- HarburgRaster200$Nr[which(temp$X >= HarburgRaster200$X1 &
                                         temp$X <= HarburgRaster200$X2 &
                                         temp$Y >= HarburgRaster200$Y1 &
                                         temp$Y <= HarburgRaster200$Y2)
                                 ] 
  if(length(temp2)>0)
  {
    H200$Raster[i] <- temp2
  }
  else 
  {
    H200$Raster[i] <- ""
  }
}

for (i in 1:nrow(H200))
{
  H200$HeatArea[i] <-  H200$AdO[i] * H200$Geom_Area[i] * 0.6
} 

H200 <- group_by(H200, Raster) %>%
  mutate(DemandTotal = sum(Demand),
         DensityMWHpHA = sum(Demand)/1000/4)

write.csv(H200, "Harburg200.csv")

## raster plots hist ####
dev.off()

hist(round(H100$DensityMWHpHA,2), breaks=9,
     main="Urban heat density: Harburg 100 m raster",
     xlab="Urban heat density: MWh/(ha*a)")

hist(round(H150$DensityMWHpHA,2), breaks=9,
     main="Urban heat density: Harburg 150 m raster",
     xlab="Urban heat density: MWh/(ha*a)")

hist(round(H200$DensityMWHpHA,2), breaks=9,
     main="Urban heat density: Harburg 200 m raster",
     xlab="Urban heat density: MWh/(ha*a)")


hist(round(B100$DensityMWHpHA,2), breaks=9,
     main="Urban heat density: Bergedorf 100 m raster",
     xlab="Urban heat density: MWh/(ha*a)")

hist(round(B150$DensityMWHpHA,2), breaks=9,
     main="Urban heat density: Bergedorf 150 m raster",
     xlab="Urban heat density: MWh/(ha*a)")

hist(round(B200$DensityMWHpHA,2), breaks=9,
     main="Urban heat density: Bergedorf 200 m raster",
     xlab="Urban heat density: MWh/(ha*a)")



length(unique(B100$Density)) # 106
median(unique(B100$Density)) # 53.574

length(unique(B150$Density)) # 55
median(unique(B150$Density)) # 45.75

length(unique(B200$Density)) # 36
median(unique(B200$Density)) # 47.846

length(unique(H100$Density)) # 78
median(unique(H100$Density)) # 111.454

length(unique(H150$Density)) # 43
median(unique(H150$Density)) # 78.880

length(unique(H200$Density)) # 23
median(unique(H200$Density)) # 103.038

quantile(table(H100$Raster), seq(0,1,0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 1.0  1.0  2.0  4.0  5.0  6.0 10.0 12.0 16.6 21.3 25.0

quantile(table(H150$Raster), seq(0,1,0.1))
#  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 1.0  2.0  3.4  5.0  7.0 14.0 17.2 21.4 26.6 41.0 53.0 
quantile(table(H200$Raster), seq(0,1,0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 1.0  5.4  9.4 15.6 22.4 30.0 36.0 41.2 47.0 59.4 80.0 


# Apply aggregation Bergedorf ####
Harburg <- read.csv("~/R/CORP/csv/Harburg.csv")
HarburgCluster <- cluster(Harburg)

write.csv(HarburgCluster, "~/R/CORP/csv/HarburgCluster.csv")


# Apply aggregation Bergedorf ####
Bergedorf <- read.csv("~/R/CORP/csv/Bergedorf.csv")
BergedorfCluster <- cluster(Bergedorf)

write.csv(BergedorfCluster, "~/R/CORP/csv/BergedorfCluster.csv")



test <- cluster(Bergedorf)
