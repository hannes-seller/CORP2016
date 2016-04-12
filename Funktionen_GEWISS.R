# GEWISS R functions
# Hannes Seller, HafenCity Universität Hamburg
# April 11,2016

## baujahre(x) ####

baujahre <- function(x)
{
  if (require(stringr)==F) {install.packages("stringr")}
  library(stringr)
  
  if (require(dplyr)==F) {install.packages("dplyr")}
  library(dplyr)

  b <- as.character(x$BJA)
  b <- gsub(")", "", b)
  b <- gsub("\\(\\d\\:", "", b)
  b <- gsub("^0", "", b)
  
  
  temp <-str_split_fixed(b, ",", 4)
  valid = T
  latest = ""
  temp <- as.data.frame(cbind(as.character(x$ID), temp, latest, valid), stringsAsFactors=FALSE)
  names(temp) <- c("ID","BJ1","BJ2","BJ3","BJ4", "Letztes_BJ", "Valid")
  
  #marking invalid dates
  temp$Valid[which(as.numeric(as.character(temp$BJ1)) > 2015)] <- FALSE
  temp$Valid[which(as.numeric(as.character(temp$BJ2)) > 2015)] <- FALSE
  temp$Valid[which(as.numeric(as.character(temp$BJ3)) > 2015)] <- FALSE
  temp$Valid[which(as.numeric(as.character(temp$BJ4)) > 2015)] <- FALSE

  y1 <- as.numeric(as.character(temp$BJ1))
  y2 <- as.numeric(as.character(temp$BJ2))
  y3 <- as.numeric(as.character(temp$BJ3))
  y4 <- as.numeric(as.character(temp$BJ4))
  
  temp$Valid[which(y1 > y2 | y1 > y3 | y1 > y4)] <- F #164 cases
  temp$Valid[which(y2 > y3 | y2 > y4)] <- F # 3 cases
  temp$Valid[which(y2 > y3 | y2 > y4)] <- F # 3 cases
  
  #assign latest Baujahr
  temp$Letztes_BJ <- temp$BJ1
  temp$Letztes_BJ[which(temp$BJ2 > temp$BJ1)] <- temp$BJ2[which(temp$BJ2 > temp$BJ1)]
  temp$Letztes_BJ[which(temp$BJ3 > temp$BJ2)] <- temp$BJ3[which(temp$BJ3 > temp$BJ2)]
  temp$Letztes_BJ[which(temp$BJ4 > temp$BJ3)] <- temp$BJ4[which(temp$BJ4 > temp$BJ3)]
  
  #all Baujahr entries on invalid sets are regarded as missing
  temp$BJ1[which(temp$Valid == F)] <- ""
  temp$BJ2[which(temp$Valid == F)] <- ""
  temp$BJ3[which(temp$Valid == F)] <- ""
  temp$BJ4[which(temp$Valid == F)] <- ""
  temp$Letztes_BJ[which(temp$Valid == F)] <- ""
  
  
  #Baualtersklassen IWU-de
  Baualtersklassen <- as.numeric(as.character(temp$BJ1))
  b <- as.numeric(as.character(temp$BJ1))
  Baualtersklassen[which(b <= 1859)] <- "vor 1859"
  Baualtersklassen[which(b >= 1860 & b <=1918)] <- "1860-1918"
  Baualtersklassen[which(b >= 1919 & b <=1948)] <- "1919-1948"
  Baualtersklassen[which(b >= 1949 & b <=1957)] <- "1949-1957"
  Baualtersklassen[which(b >= 1958 & b <=1968)] <- "1958-1968"
  Baualtersklassen[which(b >= 1969 & b <=1978)] <- "1969-1978"
  Baualtersklassen[which(b >= 1979 & b <=1983)] <- "1979-1983"
  Baualtersklassen[which(b >= 1984 & b <=1994)] <- "1984-1994"
  Baualtersklassen[which(b >= 1995 & b <=2001)] <- "1995-2001"
  Baualtersklassen[which(b >= 2002 & b <=2009)] <- "2002-2009"
  Baualtersklassen[which(b >= 2010 & b <=2015)] <- "2010-2015"
  
  temp <- cbind(temp, Baualtersklassen)
  
  Baujahre <<- temp
  
}

## raster(x1,x2,y1,y2,raster) ####

raster <- function(x1, x2, y1, y2, raster)
{
  xtimes <- ceiling((x2-x1)/raster)
  ytimes <- ceiling((x2-x1)/raster)
   
  X <- x1
  for (i in 1:xtimes)
  {
    X[i+1] <- X[i] + raster
  }

  Y <- y1
  for (i in 1:xtimes)
  {
    Y[i+1] <- Y[i] + raster
  }  

  
  XY <- cbind(rep(X[1:xtimes], xtimes),
              rep(X[2:(xtimes+1)], xtimes)
  )
  
  temp<-NULL
  for (i in 1:ytimes)
  {
    temp <- append(temp, rep(Y[i], ytimes))
  }
  
  XY <- as.data.frame(cbind(1:length(temp), XY, temp, temp+raster))
  names(XY) <- c("Nr", "X1", "X2", "Y1", "Y2")
  XY

}

## clusterArea(data, area) by Area [area in ha] ####
clusterArea <- function(data, area)
{
  data <- mutate(data,
                 Demand = Demand / 1000,  #MWh/a
                 ClusterNr = 0,
                 ClusterDemand = 0,
                 ClusterDensity = 0,
                 ClusterArea = 0,
                 Distance = 0)
  
  temp2 <- NULL
  n <- 1
  Density <- 0
  
  while(nrow(data) > 0)
  {
    data <- arrange(data, -Demand) # by Demand (descending)
    # take out entry with highest demand
    temp <- data[1,]; data <- data[-1,]
    Area <- 0 #reset Area
    
    while(Area < area & nrow(data) > 0)
    {
      # Entfernungen berechnen
      data <- mutate(data,
                     Distance = ((X-min(temp$X))^2 + (Y-min(temp$Y))^2)^(1/2)
      )
      data <- arrange(data, Distance)
      
      temp <- rbind(temp, data[1,]); data <-  data[-1,]
      
      Area <- (max(temp$X)-min(temp$X)) * (max(temp$Y)-min(temp$Y))/10000
      if(Area != 0)
      {
        Density <- sum(temp$Demand) / Area
      }
      else {Density <- 0}
    }
    temp$ClusterDemand <- sum(temp$Demand)
    temp$ClusterDensity <- Density
    temp$ClusterArea <- Area
    temp$ClusterNr <- n
    
    temp2 <- rbind(temp2, temp)
    n <- n + 1
  }
  temp2 <- temp2[,(1:ncol(temp2)-1)]
  temp2
}

## clusterDemand(data, demand) by Demand [demand in MWh/a] ####
clusterDemand <- function(data, demand)
{
  data <- mutate(data,
                 Demand = Demand / 1000,  #MWh/a
                 ClusterNr = 0,
                 ClusterDemand = 0,
                 ClusterDensity = 0,
                 ClusterArea = 0,
                 Distance = 0)
  
  temp2 <- NULL
  n <- 1
  Density <- 0
  
  while(nrow(data) > 0)
  {
    data <- arrange(data, -Demand) # by Demand (descending)
    # take out entry with highest demand
    temp <- data[1,]; data <- data[-1,]
    Area <- 0 #reset Area

    
    while(sum(temp$Demand) < demand & nrow(data) > 0)
    {
      # Entfernungen berechnen
      data <- mutate(data,
                     Distance = ((X-min(temp$X))^2 + (Y-min(temp$Y))^2)^(1/2)
      )
      data <- arrange(data, Distance)
      
      temp <- rbind(temp, data[1,]); data <-  data[-1,]
      
      Area <- (max(temp$X)-min(temp$X)) * (max(temp$Y)-min(temp$Y))/10000
      if(Area != 0)
      {
        Density <- sum(temp$Demand) / Area
      }
      else {Density <- 0}
    }
    temp$ClusterDemand <- sum(temp$Demand)
    temp$ClusterDensity <- Density
    temp$ClusterArea <- Area
    temp$ClusterNr <- n
    
    temp2 <- rbind(temp2, temp)
    n <- n + 1
  }
  temp2 <- temp2[,(1:ncol(temp2)-1)]
  temp2
}