# crime_spatial_displacement
Through this project, we analyze the spatial displacement of crime through urban sociological processes, such as gentrification.

These are the purposes of each file:
* src/ : This folder contains all source code for this project.
  * 01_points_of_interest.R: This file collects preliminary data for points of interest in Detroit through Yelp and Google Places. Specifically, we are starting our analysis by collecting information about restaurants in Detroit. We also attempt to collect data on reviews in order to establish when the restaurant established a presence in the city. However, Google and Yelp have some striking limitations in their API in meeting this end. Notably, through the Google Places API, we can only has 60 restaurants in an area and 5 reviews per restaurant. For Yelp, we can only collect 50 restaurants in an area and 3 reviews per restaurant. Therefore, we will need to seek other data in order to establish the presence of points of interest in a city. 
  * 02_preliminary_models_detroit.R: Through this code, we create an initial point process model of crime data in Detroit. We focus on high priority calls and start with a simple Poisson Process. 

