###
### Police Data Initiative
### Exploration of city alternatives
### 
### Claire Kelling
###
### Last Updated: 1/15/19
###

library(ggplot2)

# Load data for other cities
#    Source: https://www.policedatainitiative.org/datasets/calls-for-service/
balt_dat <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Baltimore_911_Police_Calls_for_Service.csv")
cinci_dat <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Cincinnati_PDI__Police_Data_Initiative__Police_Calls_for_Service__CAD_.csv")

# Number of calls for service:
nrow(balt_dat) #about 4 million calls for service
nrow(cinci_dat) #about 2.5 million calls for service

# Convert data/time to see how many years of data we have
balt_dat$callDateTime <- as.character(balt_dat$callDateTime)
balt_dat$callDateTime[1]
balt_dat$datetime <- as.POSIXct(balt_dat$callDateTime,format="%m/%d/%Y %H:%M:%S %p",tz=Sys.timezone())
range(balt_dat$datetime, na.rm = T)
#"2015-01-01 01:00:00 EST" "2019-01-14 12:59:00 EST"
hist(balt_dat$datetime, breaks = "months")

cinci_dat$DISPATCH_TIME_PRIMARY_UNIT <- as.character(cinci_dat$DISPATCH_TIME_PRIMARY_UNIT)
cinci_dat$DISPATCH_TIME_PRIMARY_UNIT[1]
cinci_dat$datetime <- as.POSIXct(cinci_dat$DISPATCH_TIME_PRIMARY_UNIT,format="%m/%d/%Y %H:%M:%S %p",tz=Sys.timezone())
range(cinci_dat$datetime, na.rm = T)
#"2014-10-01 01:00:18 EDT" "2019-01-14 12:58:59 EST"
hist(cinci_dat$datetime, breaks = "months")

# Plot over space
# Load maps for plotting, created in 00_get_maps.R
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/Baltimore_Map.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/Cincinnati_Map.Rdata")

#need to get lat and long for Baltimore
balt_dat$location <- as.character(balt_dat$location)
balt_dat$coordinates <- rep(NA, nrow(balt_dat))
balt_dat$Latitude <- rep(NA, nrow(balt_dat))
balt_dat$Longitude <- rep(NA, nrow(balt_dat))
for(i in 1:nrow(balt_dat)){
  print(i)
  if(grepl("[\\(]", balt_dat$location[i]) == T){
    balt_dat$coordinates[i] <- gsub("[\\(\\)]", "", regmatches(balt_dat$location[i], gregexpr("\\(.*?\\)", balt_dat$location[i]))[[1]])
    
    #after comma
    balt_dat$Longitude[i] <- as.numeric(sub('.*,\\s*','', balt_dat$coordinates[i]))
    
    #before comma
    balt_dat$Latitude[i] <- as.numeric(sub('\\s*,.*','', balt_dat$coordinates[i]))
  }
  
}

#I will only use those which already have a lat/long
balt_dat <- balt_dat[which(grepl("[\\(]", balt_dat$location) == T), ]
balt_dat$coordinates <- gsub("[\\(\\)]", "", regmatches(balt_dat$location, gregexpr("\\(.*?\\)", balt_dat$location)))
#after comma
balt_dat$Longitude <- as.numeric(sub('.*,\\s*','', balt_dat$coordinates))
#before comma
balt_dat$Latitude <- as.numeric(sub('\\s*,.*','', balt_dat$coordinates))


save(balt_dat, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/balt_dat.Rdata")

#Subset to high priority calls
balt_dat$priority <- as.character(balt_dat$priority)
balt_high <- balt_dat[which(balt_dat$priority == "High"),]
cinci_high <- cinci_dat[which(cinci_dat$PRIORITY < 5),]

BaltimoreMap  + geom_point(aes(x = Longitude, y = Latitude), size = 1,
                         data = balt_high, col = "blue", alpha =0.01) + coord_equal() +
  ggtitle("Point Data, Baltimore High Priority Calls")+
  theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))

# Plot for Cincinnati Data
CincinnatiMap  + geom_point(aes(x = LONGITUDE_X, y = LATITUDE_X), size = 1,
                           data = cinci_high, col = "blue", alpha =0.009) + coord_equal() +
  ggtitle("Point Data, Cincinnati High Priority Calls")#+
  #theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
