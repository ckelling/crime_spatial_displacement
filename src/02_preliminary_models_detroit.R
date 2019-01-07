### 
### Preliminary Model of Crime- Detroit, MI
### Claire Kelling
### 
### Last Updated: 1/7/19
### 



#	2 page summary of basic models fit and the results of careful EDA/basic modeling
#	Simple Poisson process with smooth surface
#	Smooth surface over time
#	How to choose to aggregate to what time? 
#   How does this impact the definition of movement of crime?
#   Point process model with regression
# Lagged regression- has to have some time incorporated
# Lambda(t) has to be impacted by gentrification at the last time point


# Load the data
#Libraries
library(sp)
library(gstat)
library(fields)
library(classInt)
library(maps)
library(acs)
library(tigris)
library(spdep)
library(ggplot2)
library(dplyr)
library(ade4) 
library(ggmap)
library(rgdal)

# Load Data
#    Need to refer back to original crime data to access response time
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/gentrification_displacement_project/data/working/det_bg.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/gentrification_displacement_project/data/working/detroit_data.Rdata")

detroit_data$Longitude1 <- detroit_data$Longitude
detroit_data$Latitude1 <- detroit_data$Latitude

#I will convert the files to spatial points and polygons with the same projection
coordinates(detroit_data) <- ~Longitude1+Latitude1
#proj4string(detroit_data) <- proj4string(det_bg)
proj4string(detroit_data) <- CRS("+proj=longlat")
det_bg <- spTransform(det_bg, CRS("+proj=longlat"))

#Make a map of Detroit using Google's API
#     Load detroit Map
#     Created in 311/with_key_point_process_modeling.Rmd
load(file = "C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/Detroit_Map.Rdata")
sp_f <- fortify(det_bg)
det_bg$id <- row.names(det_bg)
det_bg_plot <- DetroitMap + geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                              fill = NA, col = "black") +
  ggtitle("Block Groups in Detroit")

#map of Detroit with block groups
det_bg_plot

#Google Map of Detroit
DetroitMap

#make a grid for wayne county
grid <- makegrid(det_bg, cellsize = 0.1)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(det_bg)))

#remove variables that I am not interested in
length(which(is.na(detroit_data$`Total Response Time`)))
detroit_data <- detroit_data[-which(is.na(detroit_data$`Total Response Time`)),]

#want to keep only variables that I am interested in using as response, covariates, and location
detroit_data@data <- detroit_data@data[,c(1,5,13,17,23,24)]
#take only the complete cases
keep <- which(complete.cases(detroit_data@data))
detroit_data <- detroit_data[keep,]

ploteqc <- function(spobj, z, breaks, ...){
  pal <- tim.colors(length(breaks)-1)
  fb <- classIntervals(z, n = length(pal), 
                       style = "fixed", fixedBreaks = breaks)
  col <- findColours(fb, pal)
  plot(spobj, col = col, ...)
  image.plot(legend.only = TRUE, zlim = range(breaks), col = pal)
}

## Plotting
#deleting the cases with negative time........ (no concerns on the large positive end)
detroit_data <- detroit_data[-which(detroit_data$`Total Response Time`<0),]
#removing one outlier that is not in detroit
detroit_data <- detroit_data[-which(detroit_data$Longitude < -83.5),]

#I'm going to randomly select n points to plot
plot_ind <- runif(n=3000, min=0, max=479100)
plot_dat <- detroit_data[plot_ind,]

range(plot_dat$`Total Response Time`)
breaks <- 0:range(plot_dat$`Total Response Time`)[2]
xlim <- c(as.numeric(min(det_bg@data$INTPTLON)), as.numeric(max(det_bg@data$INTPTLON))) 
xlim <- c(xlim[2], xlim[1])
ylim <- c(as.numeric(min(det_bg@data$INTPTLAT)), as.numeric(max(det_bg@data$INTPTLAT))) 
ploteqc(plot_dat, plot_dat$`Total Response Time`, breaks, pch = 19, xlim=xlim, ylim = ylim)
plot(det_bg, add = TRUE)
title(main = "Response Times, Wayne County")

## See the bdss_igert_project/src/00_first_try_point_process_model.R for further modeling of Response Times

#Now I will do some preliminary Poisson Process Model
#First, I will convert the time variable to something we can work with in R
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/gentrification_displacement_project/data/working/detroit_data.Rdata")
#as.POSIXct(detroit_data$`Call Time`[1],format="%m/%d/%Y %H:%M:%S %p",tz=Sys.timezone())
detroit_data$datetime <- as.POSIXct(detroit_data$`Call Time`,format="%m/%d/%Y %H:%M:%S %p",tz=Sys.timezone())

hist(detroit_data$datetime, breaks = "months", main = "Histogram of Detroit Date/Time", xlab = "Date/Time")

#Subset to Priority 1 Crime Types
length(unique(detroit_data$Category)) #211
nrow(detroit_data) #566,553
nrow(detroit_data[which(detroit_data$Priority == 1),]) #70,151
high_prio <- detroit_data[which(detroit_data$Priority == 1),]
length(unique(high_prio$Category)) #139, still many call codes

#see distribution over time of high priority calls
hist(high_prio$datetime, breaks = "months", main = "Histogram of Detroit Date/Time", xlab = "Date/Time")
#almost identical to the full dataset

#See distribution of crimes over space
#  Load the city boundaries file
det_city <- readOGR(dsn="C:/Users/ckell/Desktop/Google Drive/Box Sync/SODA 502 project - 311/City of Detroit Boundary Shapefile", layer="det_city")
sp_f <- fortify(det_city)
city_bound <- DetroitMap + geom_polygon(data = sp_f, aes(long, lat, group = group),colour="red", fill = NA)

point_proc <- city_bound  + geom_point(aes(x = Longitude, y = Latitude), size = 1, 
                                       data = high_prio, col = "blue", alpha =0.01) + coord_equal() +
  ggtitle("Point Data, Detroit High Priority Calls")+
  theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))

point_proc # save at 3000x1500
