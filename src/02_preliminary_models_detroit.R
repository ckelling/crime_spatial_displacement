###
### Preliminary Model of Crime- Detroit, MI
### Claire Kelling
###
### Last Updated: 1/10/19
###

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
library(spatstat)
library(stpp)
library(rgeos)
#BiocManager::install("EBImage", version = "3.8")
library(GiNA)

#####
## Load Data and Initial Setup
#####
#    Need to refer back to original crime data to access response time
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/det_bg.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/detroit_data.Rdata")

# Adding to make into Spatial Points
detroit_data$Longitude1 <- detroit_data$Longitude
detroit_data$Latitude1 <- detroit_data$Latitude

# Convert the files to spatial points and polygons with the same projection
coordinates(detroit_data) <- ~Longitude1+Latitude1
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

#####
## Preliminary Response Time Modeling
#####
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

#function for plotting response time by colors
ploteqc <- function(spobj, z, breaks, ...){
  pal <- tim.colors(length(breaks)-1)
  fb <- classIntervals(z, n = length(pal),
                       style = "fixed", fixedBreaks = breaks)
  col <- findColours(fb, pal)
  plot(spobj, col = col, ...)
  image.plot(legend.only = TRUE, zlim = range(breaks), col = pal)
}

summ <- detroit_data %>% count(`Call Description`)


## Plotting
# Deleting the cases with negative time........ (no concerns on the large positive end)
detroit_data <- detroit_data[-which(detroit_data$`Total Response Time`<0),]
# Removing one outlier that is not in detroit
detroit_data <- detroit_data[-which(detroit_data$Longitude < -83.5),]

# Randomly select n points to plot
plot_ind <- runif(n=3000, min=0, max=479100)
plot_dat <- detroit_data[plot_ind,]

# Plot the Response Times for Wayne County
range(plot_dat$`Total Response Time`)
breaks <- 0:range(plot_dat$`Total Response Time`)[2]
xlim <- c(as.numeric(min(det_bg@data$INTPTLON)), as.numeric(max(det_bg@data$INTPTLON)))
xlim <- c(xlim[2], xlim[1])
ylim <- c(as.numeric(min(det_bg@data$INTPTLAT)), as.numeric(max(det_bg@data$INTPTLAT)))
ploteqc(plot_dat, plot_dat$`Total Response Time`, breaks, pch = 19, xlim=xlim, ylim = ylim)
plot(det_bg, add = TRUE)
title(main = "Response Times, Wayne County")

### See the bdss_igert_project/src/00_first_try_point_process_model.R for further modeling of Response Times

#####
## Preliminary Poisson Process Model
#####
#First, I will convert the time variable to something we can work with in R
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/crime_spatial_displacement/data/working/detroit_data.Rdata")
detroit_data$datetime <- as.POSIXct(detroit_data$`Call Time`,format="%m/%d/%Y %H:%M:%S %p",tz=Sys.timezone())

# See the distribution of the time
hist(detroit_data$datetime, breaks = "months", main = "Histogram of Detroit Date/Time", xlab = "Date/Time")
range(detroit_data$datetime, na.rm = T)

# Subset to Priority 1 Crime Types
length(unique(detroit_data$Category)) #211
nrow(detroit_data) #566,553
nrow(detroit_data[which(detroit_data$Priority == 1),]) #70,151
high_prio <- detroit_data[which(detroit_data$Priority == 1),]
length(unique(high_prio$Category)) #139, still many call codes

# See distribution over time of high priority calls
hist(high_prio$datetime, breaks = "months", main = "Histogram of Detroit Date/Time", xlab = "Date/Time")
# (almost identical to the full dataset)

# See distribution of crimes over space
#  Load the city boundaries file
det_city <- readOGR(dsn="C:/Users/ckell/Desktop/Google Drive/Box Sync/SODA 502 project - 311/City of Detroit Boundary Shapefile", layer="det_city")
det_city <- spTransform(det_city, CRS("+proj=longlat"))
sp_f <- fortify(det_city)
city_bound <- DetroitMap + geom_polygon(data = sp_f, aes(long, lat, group = group),colour="red", fill = NA)

point_proc <- city_bound  + geom_point(aes(x = Longitude, y = Latitude), size = 1,
                                       data = high_prio, col = "blue", alpha =0.01) + coord_equal() +
  ggtitle("Point Data, Detroit High Priority Calls")+
  theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))

point_proc # save at 3000x1500

#####
## Subset Crime data to final dataset (shots fired and priority 1 calls)
#####
final_crime <- detroit_data[which(detroit_data$`Call Description` == "SHOTS FIRED IP"),]
hist(final_crime$Priority)
hist(detroit_data$Priority)

#There are 8,907 data points in this dataset
final_crime <- final_crime[which(final_crime$Priority == 1),] #still 7,915 crimes in this dataset

hist(final_crime$datetime, breaks = "months", main = "Histogram of Shots Fired Crimes", xlab = "Date/Time")

#city_bound or ggplot() here
ggplot()  + geom_point(aes(x = Longitude, y = Latitude), size = 1,
                       data = final_crime, alpha =0.05, col = "navyblue") + coord_equal() +
  ggtitle("Point Data, Detroit 'Shots Fired' Calls")+
  theme(text = element_text(size=20))+theme(axis.text.x=element_text(size=10))

#create plot of "Officer Initiated" calls - compare with full dataset to see if the distribution is similar
ggplot()  + geom_point(aes(x = Longitude, y = Latitude, col = `Officer Initiated`), size = 1,
                       data = final_crime) + coord_equal() +
  ggtitle("Point Data, Detroit 'Shots Fired' Calls")+
  theme(text = element_text(size=20))+theme(axis.text.x=element_text(size=10))

#####
## Aggregate by block group for plotting
#####
city_bg <- det_bg[det_city,]
plot(city_bg)
plot(city_bg, col = "red", add = TRUE)

plot_dat <- final_crime
plot_dat$Longitude1 <- plot_dat$Longitude
plot_dat$Latitude1 <- plot_dat$Latitude

# Convert the files to spatial points and polygons with the same projection
coordinates(plot_dat) <- ~Longitude1+Latitude1
proj4string(plot_dat) <- proj4string(city_bg)

o = over(plot_dat, city_bg)
agg_dat <- plyr::count(o, c('GEOID'))
agg_dat$GEOID <- as.factor(agg_dat$GEOID)

sp_f <- fortify(city_bg)
city_bg$id <- row.names(city_bg)
city_bg@data <- left_join(city_bg@data, agg_dat, by = (GEOID = "GEOID"))
sp_f <- left_join(sp_f, city_bg@data[,c(13,14)])


# Make a color or grayscale plot to illustrate this
obs_by_dist <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = freq)) + coord_equal() +
  labs(fill = "No. of \nCrimes")+ geom_polygon(data=sp_f,aes(long,lat, group = group),
                                               fill = NA, col = "black") +
  ggtitle("Number of Shots Fired Calls per Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")

obs_by_dist

#####
## Fit a Point Process Model - Inhomogenous Models with varying intensity functions
#####

# Transform our data into ppm object
det_owin <- as.owin(det_city)
xyzt <- as.matrix(high_prio[,c("Longitude", "Latitude")])

crime_ppp <- as.ppp(xyzt, det_owin) #382 outside of the specified window
plot(crime_ppp)

#Fit preliminary kernel density estimate to data
plot(density.ppp(crime_ppp), main = "Kernel Density Estimate")

#####
## Incorporate temporal element
#####

# Create the dataset
xyzt <- final_crime[,c("Longitude", "Latitude",
                       "datetime")]
range(final_crime$datetime) #this data occurs over the span of 1 year (almost exactly)
colnames(xyzt) <- c("x", "y", "t")
xyzt$t <- as.numeric(as.Date(xyzt$t))

#converting to weeks
xyzt$week <- (xyzt$t - 17064)/7
xyzt <- xyzt[,-c(3)] #take out t so that week is the time variable
xyzt$week <- round(xyzt$week)
xyzt <- as.data.frame(xyzt)
colnames(xyzt) <- c('x', 'y', 't')
keep <- xyzt

xyzt <- as.3dpoints(xyzt)

#Need to create the boundary of Detroit city with points
det_bound <- fortify(det_city)
det_bound <- det_bound[,c("long", "lat")]
plot(xyzt, s.region = det_bound)
plot(xyzt, s.region = det_bound, pch = 20, mark = TRUE)
animation(xyzt, runtime = 10, cex = 0.5, s.region = det_bound)
stan(xyzt, bgpoly = det_bound, bgframe = FALSE)


#I will create a kernel density estimate for the each month
# over the course of our data and plot them
full_ppp <- keep[,c("x", "y")]

#####
## Plot basic density estimates over time
#####
crime_ppp1 <- as.ppp(full_ppp[which(keep$t<13),], det_owin)
crime_ppp2 <- as.ppp(full_ppp[which(keep$t>13 & keep$t<26),], det_owin)
crime_ppp3 <- as.ppp(full_ppp[which(keep$t>26 & keep$t<39),], det_owin)
crime_ppp4 <- as.ppp(full_ppp[which(keep$t>39),], det_owin)


par(mfrow = c(2,2))
plot(density.ppp(crime_ppp1), main = "Kernel Density Estimate, Q1")
plot(density.ppp(crime_ppp2), main = "Kernel Density Estimate, Q2")
plot(density.ppp(crime_ppp3), main = "Kernel Density Estimate, Q3")
plot(density.ppp(crime_ppp4), main = "Kernel Density Estimate, Q4")

#####
## Estimate and Plot Intensity Estimate, both static and over months
#####
#vignette: https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/inst/doc/stpp.pdf?revision=61&root=stpp&pathrev=61
#page 12
det_bound <- as.matrix(det_bound)
keep <- as.matrix(keep)
h <- mse2d(as.points(keep[, 1:2]), det_bound, nsmse = 30, range = 3000)
h <- h$h[which.min(h$mse)]

#make h = 0.01 smaller for more precise estimation
Ls <- kernel2d(as.points(keep[, 1:2]), det_bound, 0.009, nx = 100, ny = 100)
Lt <- dim(keep)[1] * density(keep[, 3], n = 54)$y
Lst <- array(0, dim = c(100, 100, 54))
for(k in 1:54) Lst[,,k] <- Ls$z * Lt[k] / dim(keep)[1]

#this can be used to generate a  realization from this point pattern
ipp2 <- rpp(lambda = Lst, s.region = det_bound, t.region = c(1, 54),
            discrete.time = TRUE)

par(mfrow = c(1,2))
image(Ls$x, Ls$y, Ls$z, col=brewer.pal(11,"RdBu"))
plot(1:54, Lt, col="white", xlab = "week", ylab = "temporal trend")
lines(Lt)
polygon(det_bound)
animation(ipp2$xyt, add = TRUE, cex = 0.5, runtime = 15)



#creating estimates of the spatial intensity over time
#https://cran.r-project.org/web/packages/splancs/splancs.pdf
b3d <- kernel3d(keep[,1:2], xyzt$t, seq(-83.2877, -82.9107, 0.008), seq(42.25540, 42.45682, 0.008),
                seq(0,53,4), 0.03, 0.05)
#brks <- quantile(b3d$v, seq(0,1,0.05))
for(i in 1:14){
  #i <- 1
  new_df <- b3d$v[,,i]
  new_df[new_df == 0] <- NA
  b3d$v[,,i] <- new_df
}

oldpar <- par(mfrow=c(3,5))
for (i in 1:14) image(seq(-83.2877, -82.9107, 0.008), seq(42.25540, 42.45682, 0.008), b3d$v[,,i],
                      asp=1, xlab="", ylab="", main=i,  col=brewer.pal(11,"RdBu"))
par(oldpar)

