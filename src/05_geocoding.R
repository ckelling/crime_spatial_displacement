###
### Geocoding Real Estate Data
###
### Claire Kelling
### Last Modified: 1/26/19
###


library(googleway)
library(tidyverse)
library(httr)
library(ggmap)
library(devtools)
#install_github("trinker/mapit") #trinker
library(mapit)
library(RDSTK)

mygooglekey <- "" #put key here

register_google(key = mygooglekey)
ggmap_credentials()

# Load data on real estate sales
arl_est <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/Arlington_real_estate.csv")arl_est2 <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/full_real_est_dat.csv")
arl_est2 <- read.csv(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/original/full_real_est_dat.csv")
re_addresses <- arl_est$propertyStreetNbrNameText
re_addresses <- as.character(re_addresses)

#last character
last <- substr(re_addresses,nchar(re_addresses)-1, nchar(re_addresses))
num <- grepl("\\d", last)

#no apt number
re_addresses_no <- re_addresses[which(!num)]

#has apt number
re_addresses_apt <- re_addresses[which(num)]
re_addresses_apt <- as.data.frame(re_addresses_apt)
re_addresses_apt <- re_addresses_apt %>% 
  mutate(re_addresses_apt = sub("\\s+[^ ]+$", "", re_addresses_apt))

#new list of addresses
full_addresses <- c(re_addresses_no, re_addresses_apt$re_addresses_apt)

#add city name and state
full_addresses <- paste(full_addresses, "Arlington, VA", sep = " ")
View(head(full_addresses))

###
### Second set of addresses
###
arl_est$saleDate <- as.character(arl_est$saleDate)
arl_est$date <- sub('\\s*T.*','', arl_est$saleDate)
arl_est$date <- as.POSIXct(arl_est$date)

arl_est2$saleDate <- as.character(arl_est2$saleDate)
arl_est2$date <- sub('\\s*T.*','', arl_est2$saleDate)
arl_est2$date <- as.POSIXct(arl_est2$date)

range(arl_est$date)
range(arl_est2$date)

arl_est2 <- arl_est2[which((arl_est2$date < range(arl_est$date)[1] & 
                             arl_est2$date > as.POSIXct("2004-01-01")) |
                             arl_est2$date > range(arl_est$date)[2]),]

re_addresses2 <- arl_est2$propertyStreetNbrNameText
re_addresses2 <- as.character(re_addresses2)

#last character
last <- substr(re_addresses2,nchar(re_addresses2)-1, nchar(re_addresses2))
num <- grepl("\\d", last)

#no apt number
re_addresses2_no <- re_addresses2[which(!num)]

#has apt number
re_addresses2_apt <- re_addresses2[which(num)]
re_addresses2_apt <- as.data.frame(re_addresses2_apt)
re_addresses2_apt <- re_addresses2_apt %>% 
  mutate(re_addresses2_apt = sub("\\s+[^ ]+$", "", re_addresses2_apt))

#new list of addresses
full_addresses2 <- c(re_addresses2_no, re_addresses2_apt$re_addresses2_apt)

#add city name and state
full_addresses2 <- paste(full_addresses2, "Arlington, VA", sep = " ")
View(head(full_addresses2))

###
### Full Geocode using RDSTK package
###
### Run up to line 46 at the beginning
#geocode_dat <- NULL
#error_vec <- NULL
for(i in 1:length(full_addresses)){ #don't include 15176, done at 15226
  #i <- 1
  new_row <- try(street2coordinates(full_addresses[i])[,c("latitude", "longitude")])
  error_test <- substr(new_row[1], 1, 5)
  if(error_test == "Error"){
    error_vec <- rbind(error_vec, i)
  }else{
    new_row <- cbind(full_addresses[i], new_row)
    geocode_dat <- rbind(geocode_dat, new_row)
    print(paste("done with", i, "out of", length(full_addresses), sep = " "))
  }
  if(i %% 1000==0) {
    # Print on the screen some message
    print("saving file *****************")
    save(geocode_dat, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/comp_re.Rdata")
    save(error_vec, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/error_vec.Rdata")
  }
}

arl_est_reorder <- rbind(arl_est[which(!num),], arl_est[which(num),])
arl_est_worked <- arl_est_reorder[-error_vec,]
geocode_dat <- cbind(geocode_dat[,1:3], arl_est_worked)

#save(geocode_dat, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/comp_re.Rdata")
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/comp_re.Rdata")
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/error_vec.Rdata")

###
### Second set of addresses with updated dataset
###
#geocode_dat2 <- NULL
#error_vec2 <- NULL
for(i in 1:length(full_addresses2)){ #don't include 15176, done at 15226
  #i <- 1
  new_row <- try(street2coordinates(full_addresses2[i])[,c("latitude", "longitude")])
  error_test <- substr(new_row[1], 1, 5)
  if(error_test == "Error"){
    error_vec2 <- rbind(error_vec2, i)
  }else{
    new_row <- cbind(full_addresses2[i], new_row)
    geocode_dat2 <- rbind(geocode_dat2, new_row)
    print(paste("done with", i, "out of", length(full_addresses2), sep = " "))
  }
  if(i %% 1000==0) {
    # Print on the screen some message
    print("saving file *****************")
    save(geocode_dat2, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/comp_re2.Rdata")
    save(error_vec2, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/error_vec2.Rdata")
  }
}

arl_est_reorder <- rbind(arl_est2[which(!num),], arl_est2[which(num),])
arl_est_worked <- arl_est_reorder[-error_vec2,]
geocode_dat2 <- cbind(geocode_dat2[,1:3], arl_est_worked)

full_geocode <- rbind(geocode_dat, geocode_dat2)

###
### For reference, here are a few other ways to geocode addresses:
###

##
## Texas A&M Method
##

#Use geocode function: https://github.com/trinker/mapit/blob/master/R/geo_code.R
street_addresses <- c(re_addresses_no, re_addresses_apt$re_addresses_apt)
texas_api <- ""
geo_dat_third <- mapit::geo_code(street_addresses[1:2500], city = "Arlington", state = "VA", zip = NULL, api.key = texas_api)
colnames(geo_dat_third) <- c("lat", "lon") 

##
## Google Method
## 
# New terms by Google:
#https://developers.google.com/maps/documentation/geocoding/usage-and-billing
# see usage:
#https://console.developers.google.com/apis/dashboard?organizationId=7482988463&project=my-project-1522327296754&duration=PT6H
# example since new policy:
#https://lucidmanager.org/geocoding-with-ggmap/
geo_dat <- full_addresses[1:2000] %>% geocode()
