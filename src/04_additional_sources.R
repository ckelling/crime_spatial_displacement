###
### Exploration of Additional Data Sources
### 
### Claire Kelling
###
### Last Updated: 1/15/19
###

library(ggplot2)

# Load additional data sources for Arlington


###
### Real Estate Sales
###
#    Source: https://data.arlingtonva.us/dataviews/225313/real-estate-sale-history/
arl_est <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Arlington_real_estate.csv")

#This dataset is not geocoded, but it does include addresses, so it could be geocoded.
# It includes the sale amount and the sale date
# Only includes 75,000 rows
arl_est$saleDate <- as.character(arl_est$saleDate)
arl_est$saleDate[1]


#before comma
arl_est$date <- sub('\\s*T.*','', arl_est$saleDate)
arl_est$date <- as.POSIXct(arl_est$date)

range(arl_est$date) #"2007-10-29 EDT" "2019-01-09 EST"
hist(arl_est$date, breaks = "months", main = "Histogram of Dates of Sales", xlab = "Sale Date")

#what about price it has sold for
hist(arl_est$saleAmt)
range(arl_est$saleAmt) #has one property that sold for 460 million
hist(arl_est$saleAmt[which(arl_est$saleAmt < 800000000)])
hist(log(arl_est$saleAmt), main = "Histogram of log sale amount", xlab = "log sale amount")

#Plot of sales over time with amount
ggplot() + geom_point(data = arl_est, aes(x = date, y = saleAmt), col = "navyblue")+
  ggtitle("Sale Price Over Time")



###
### Food Inspection History
###
arl_food <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Arlington_food_inspection.csv")
arl_food_s <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Arlington_food_service.csv")

#these are also not geotagged but have lat/long if we wanted to geotag

#Take out entries that have no date
arl_food$inspectionDate <- as.character(arl_food$inspectionDate)

arl_food$date <- sub('\\s*T.*','', arl_food$inspectionDate)
arl_food <- arl_food[-which(arl_food$date == ""),]
arl_food$date <- as.POSIXct(arl_food$date)

range(arl_food$date) #2011 through 2015
hist(arl_food$date, breaks = "months", main = "Histogram of Dates of Food Inspections", xlab = "Inspection Date")

###
### Arlington Picnic Shelter Reservations
###
arl_picnic <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Arlington_park.csv")

#these are also not geotagged but have lat/long if we wanted to geotag

#Take out entries that have no date
arl_picnic$ReservationBeginDate <- as.character(arl_picnic$ReservationBeginDate)

arl_picnic$date <- as.POSIXct(arl_picnic$ReservationBeginDate)
range(arl_picnic$date) #2016 through 2018
hist(arl_picnic$date, breaks = "months", main = "Histogram of Dates of Picnic Reservations", xlab = "Reservation Date")


###
### Arlington Park Reservations
###
arl_park <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Arlington_Park Facility Reservations.csv")

#these are also not geotagged but have lat/long if we wanted to geotag

#the end of the file did not load properly
arl_park <- arl_park[-c(212090:nrow(arl_park)),]

#Take out entries that have no date
arl_park$ReservationBeginDate <- as.character(arl_park$ReservationBeginDate)

#Invalid dates
arl_park <- arl_park[-which(nchar(arl_park$ReservationBeginDate) != 10), ]

#need to take out characters
arl_park$date <- as.POSIXct(arl_park$ReservationBeginDate, format="%Y-%m-%d")
range(arl_park$date, na.rm = T) #2016 through 2018

hist(arl_park$date, breaks = "months", main = "Histogram of Dates of Park Reservations", xlab = "Reservation Date")
