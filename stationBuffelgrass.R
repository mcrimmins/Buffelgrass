# Buffelgrass greenup modeling prototype
# adapted from rainlogProto.R
# 4/29/21

#library(plyr)
library(RCurl)
library(jsonlite)
#library(spacetime)
#library(ggplot2)
#library(ggmap)
#library(scales)
library(tidyverse)
#library(SPEI)
library(htmlwidgets)
library(raster)

# functions
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

# set date ranges
dateRangeEnd<-"2018-08-01"
dateRangeStart<-paste0(as.Date(dateRangeEnd)-24)
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# 90-day SPI
#dateRangeStart=format(Sys.Date()-92, "%Y-%m-%d")
#dateRangeEnd=format(Sys.Date()-2, "%Y-%m-%d")
#allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# 180-day SPI
#dateRangeStart=format(Sys.Date()-182, "%Y-%m-%d")
#dateRangeEnd=format(Sys.Date()-2, "%Y-%m-%d")
#allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)


##### DOWNLOAD DATA - Rainlog, RCC-ACIS, FCDs from monsoon API
# specify center and radius for search area
# AZ extent
#xmin       : -114.8154 
#xmax       : -109.0449 
#ymin       : 31.32917 
#ymax       : 37.00459 

###### GET ACIS DATA
# ACIS query
# RCC ACIS bbox - Bounding box specified in decimal degrees (W,S,E,N) (e.g. [-90, 40, -88, 41])
#ACISbbox<-paste0(min(TucsonMap$data$lon),',',min(TucsonMap$data$lat),',',max(TucsonMap$data$lon),',',max(TucsonMap$data$lat))
ACISbbox<-paste0(-114.8154,',',31.32917,',',-109.0449,',',37.00459)

jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","elems":"pcpn","meta":"name,ll"}') # or uid
out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

    # format into dataframe
    ll<-data.frame(matrix(unlist(out$data$meta$ll), nrow=length(out$data$meta$ll), byrow=T))
    meta<-out$data$meta
    # get summary formatted
    summary<- data.frame(matrix(unlist(out$data$data), nrow=nrow(out$data), byrow=T))
    colnames(summary)<-allDates
    # convert obs to numeric
    summary[1:ncol(summary)] <- sapply(summary[1:ncol(summary)],as.character)
    summary[1:ncol(summary)] <- sapply(summary[1:ncol(summary)],as.numeric)
    # add metadata
    summary<-cbind(ll,meta$name,rowSums(summary, na.rm = TRUE), apply(summary, 1, function (x) sum(is.na(x))),
                   apply(summary, 1, function (x) sum(is.na(x))/ncol(summary)), apply(summary, 1, function (x) max(x, na.rm = TRUE)))
    # colnames
    colnames(summary)<-c("lon","lat","gaugeID","sumPrecip","daysMiss","percMiss", "maxPrecip")
    sumACIS<-summary
    
    # melt data
    #meltData<-reshape::melt(summary, id=1:3)
    #meltData$value<-as.numeric(as.character(meltData$value))
    # change to common dataframe format
    #colnames(meltData)<-c("position.lng","position.lat","gaugeId","readingDate","rainAmount")
    #meltData$readingDate<-as.Date(meltData$readingDate)-1 # adjusting to match Rainlog

##### GET PRISM from ACIS
    # ACIS query
    jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"pcpn","meta":"ll,elev","output":"json"}') # or uid
    #jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid
    
    out<-postForm("http://data.rcc-acis.org/GridData", 
                  .opts = list(postfields = jsonQuery, 
                               httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
    out<-fromJSON(out)
    
    # convert to list of matrices, flipud with PRISM
    matrixList <- vector("list",length(out$data))
    for(i in 1:length(out$data)){
      matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev) 
    }
    
    # read into raster stack
    rasterList<-lapply(matrixList, raster)
    gridStack<-stack(rasterList)
    gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
    gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
    names(gridStack)<-allDates
    gridStack[gridStack < 0] <- NA
    sumStack <- calc(gridStack, sum)
    # buffelgrass map
    buffelStack <- reclassify(sumStack, c(-Inf,1,1, 1,1.7,2, 1.7,Inf,3))
      buffelStack[is.na(buffelStack)] <- 0
      buffelStack[buffelStack <= 0] <- NA
    crs(buffelStack) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

###### GET RAINLOG DATA
# while statement to loop through pages
limit<-1000
i<-0
done<-0

# using geographic center of US 39.828165, -98.579480
while (done==0) {
  #jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0}}')
  jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region": {"type": "Rectangle","westLng": -114.8154,"eastLng": -109.0449,"northLat": 31.32917,"southLat": 37.00459}}')
  
  out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      dataStack<-out
    }else{
      dataStack<-rbind(dataStack, out)
    }
    if (out$readingDate[length(out$readingDate)]==dateRangeEnd){
      done<-1
      break
    }else{}
    
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# get gauges
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  #jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region": {"type": "Rectangle","westLng": -114.8154,"eastLng": -109.0449,"northLat": 31.32917,"southLat": 37.00459},"pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE & is.data.frame(out)==TRUE){
    if (i==0){
      gaugeStack<-jsonlite::flatten(out, recursive=FALSE)
    }else{
      gaugeStack<-rbind(gaugeStack, jsonlite::flatten(out))
    }
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# # reverse geocode
# library(revgeo)
# test<-gaugeStack[1:20,]
# location<-revgeo(test$position.lng, test$position.lat, output = 'frame')

# join data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeId")
#mergedData <- merge(dataStack,gaugeStack,by="gaugeRevisionId", all.x = TRUE)

# fix dates - raw table has observation date, rain most likely fell on previous day (Rainlog site subtracts one day)
# don't subtract if you want it to match the PRISM reporting
# mergedData$readingDate<-as.Date(mergedData$readingDate)-1
##### END DOWNLOAD DATA

##### SUMMARIZE DATA
# develop summed period dataframe
sumPeriod<- mergedData %>%
  group_by(gaugeId) %>% #gaugeId.x 
  summarize(countObs = n(),
            gaugeID= first(gaugeId), # gaugeRevisionId
            lat = min(position.lat),
            lon = min(position.lng),
            maxPrecip = max(rainAmount, na.rm = TRUE),
            snowAccum = sum(snowAccumulation, na.rm = TRUE),
            sumPrecip = sum(rainAmount, na.rm = TRUE)
  )

# delete duplicates in lat/lon locations?

# calculate percent missing
sumPeriod$percMiss<-(1-(sumPeriod$countObs/length(allDates)))
sumPeriod$daysMiss<-(length(allDates)-sumPeriod$countObs)
##### END SUMMARIZE DATA

# subset less than 10% missing
#sumPeriod<-subset(sumPeriod, percMiss>=0 & percMiss<=0.1)
#sumACIS<-subset(sumACIS, percMiss>=0 & percMiss<=0.1)

# two days or less
sumPeriod<-subset(sumPeriod, daysMiss>=0 & daysMiss<=2)
sumACIS<-subset(sumACIS, daysMiss>=0 & daysMiss<=2)

# categorical var
sumPeriod$predict<-ifelse(sumPeriod$sumPrecip>=1, ifelse(sumPeriod$sumPrecip>=1.7, "Yes","Maybe"),"No")
sumACIS$predict<-ifelse(sumACIS$sumPrecip>=1, ifelse(sumACIS$sumPrecip>=1.7, "Yes","Maybe"),"No")

# create map
library(leaflet)

# pal <- colorNumeric(
#   palette = colorRampPalette(c('lightblue','green','red'))(length(summary$sumPrecip)), 
#   domain = c(0,max(summary$sumPrecip)))

pal <- colorFactor(
  palette = c('palegreen','burlywood','darkgreen'),
  domain = sumPeriod$predict
)

pal1<-colorFactor(
  palette = c('burlywood','palegreen','darkgreen'),na.color = "transparent",
  domain = values(buffelStack)
)
  
# Rainlog Labs
labs1 <- lapply(seq(nrow(sumPeriod)), function(i) {
  paste0( '<p> <b> Observed Precip (in): ', round(sumPeriod[i, "sumPrecip"],2), '</b></p>', 
          '<p> Max 1-day Precip: ', round(sumPeriod[i, "maxPrecip"],2), '</p>',
          '<p> Days missing: ', sumPeriod[i,"daysMiss"], '</p>',
          '<p> Buffelgrass?: ', sumPeriod[i,"predict"], '</p>') 
})
# ACIS Labs
labs2 <- lapply(seq(nrow(sumACIS)), function(i) {
  paste0( '<p> <b> Observed Precip (in): ', round(sumACIS[i, "sumPrecip"],2), '</b></p>', 
          '<p> Max 1-day Precip: ', round(sumACIS[i, "maxPrecip"],2), '</p>',
          '<p> Days missing: ', sumACIS[i,"daysMiss"], '</p>',
          '<p> Buffelgrass?: ', sumACIS[i,"predict"], '</p>') 
})

# map title
mapTitle<-paste0("Buffelgrass Outlook: ", format(as.Date(dateRangeEnd, format = "%Y-%m-%d"), "%b-%d-%y"))


leafMap<-leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  #setView(-111.839389, 33.178586, zoom = 8) %>%
  addCircleMarkers(sumACIS$lon, sumACIS$lat,
                   radius=5,
                   color="grey66",
                   label=lapply(labs2, htmltools::HTML),
                   fillColor =  pal(sumACIS$predict),
                   fillOpacity=1,
                   group="NOAA/COCORAHS") %>%
  addCircleMarkers(sumPeriod$lon, sumPeriod$lat,
                   radius=5,
                   color="grey66",
                   label=lapply(labs1, htmltools::HTML),
                   fillColor =  pal(sumPeriod$predict),
                   fillOpacity=1,
                   group="RAINLOG")%>%
  addRasterImage(buffelStack, colors = pal1,
                 opacity = 0.8,
                 group="PRISM",project=FALSE)%>%
  addLegend("bottomright", pal = pal, 
            title = mapTitle, values = c("Yes","No","Maybe"),
            opacity = 1) %>%
  addLayersControl(
    overlayGroups = c("NOAA/COCORAHS", "RAINLOG", "PRISM"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("PRISM")

# save map
saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/Buffelgrass/maps/Buffelgrass_outlook_Aug2018.html"), selfcontained = FALSE)
##### END LEAFLET MAP

  








