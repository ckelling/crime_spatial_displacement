###
### Points of Interest Gather
###
### Claire Kelling
### Last Modified: 1/7/19
###


library(googleway)
library(tidyverse)
library(httr)

mygooglekey <- "" #put key here

res <- google_places(search_string = "restaurants in Detroit, Michigan", key = mygooglekey)
#res_goog[["error_message"]]
res_next <- google_places(search_string = "restaurants in Detroit, Michigan",
                          page_token = res$next_page_token,
                          key = mygooglekey)
res_last <- google_places(search_string = "restaurants in Detroit, Michigan",
                          page_token = res_next$next_page_token,
                          key = mygooglekey)

all_results <- rbind(res_goog$results, res_next$results, res_last$results)

detailed_df <- NULL

for(i in 1:nrow(all_results)){
  #i <- 1
  
  print(paste(i, "out of", nrow(all_results), sep = " "))
  
  res_goog_details <- google_place_details(place_id = all_results$place_id[i],key = mygooglekey)
  rating_details <- res_goog_details$result$reviews %>% select(rating, text, time)
  rating_details$date <- as.POSIXct(rating_details$time, origin = "1970/01/01")
  #time the time that the review was submitted, measured in the number of seconds since since midnight, January 1, 1970 UTC.
  
  rating_details$name <- all_results$name[i]
  rating_details$placeid <- all_results$place_id[i]
  rating_details$address <- all_results$formatted_address[i]
  rating_details$lat <- all_results$geometry$location$lat[i]
  rating_details$long <- all_results$geometry$location$lng[i]
  
  detailed_df <- rbind(detailed_df, rating_details)
}

nrow(res_goog$results) #number of restaurants found

nrow(detailed_df) #number of reviews
range(detailed_df$date) #"2016-08-03 18:53:07 EDT" "2019-01-02 02:59:19 EST"


# 
# Using Yelp instead of Google Maps:
# 
yelp_clientid <- "" #put client id here
yelp_key <- "" #put key here
token <- yelp_key


yelp <- "https://api.yelp.com"
term <- "restaurants"
location <- "detroit"
limit <- 12 #maximum number is 50
radius <- 1000

url <- modify_url(yelp, path = c("v3", "businesses", "search"), 
                  query = list(term = term, location=location, limit = limit,radius=radius))

res_yelp <- GET(url, add_headers('Authorization' = paste("bearer", token)))
#http_status(res_yelp)
ct <- content(res_yelp)


detailed_yelp <- NULL

for(i in 1:length(ct$businesses)){
  
  print(paste(i, "out of", length(ct$businesses), sep = " "))
  #i <- 5
  
  address <- ct$businesses[[i]]$location$address1
  if(is.null(address)){
    address <- NA
  }
  res_info <- data_frame(ct$businesses[[i]]$name, 
                         ct$businesses[[i]]$id,
                         address, 
                         ct$businesses[[i]]$coordinates$latitude, 
                         ct$businesses[[i]]$coordinates$longitude)
  colnames(res_info) <- c("name", "id", "address", "latitude", "longitude")
  
  # Get reviews for the first location returned
  url_reviews <- modify_url(yelp, path = c("v3", "businesses", ct$businesses[[i]]$id,"reviews"))
  res_yelp_reviews <- GET(url_reviews, add_headers('Authorization' = paste("bearer", token)))
  #http_status(res_yelp_reviews)
  ct_rev <- content(res_yelp_reviews)
  
  new_df <- NULL
  for(j in 1:length(ct_rev$reviews)){
    #j <- 1
    text <- ct_rev$reviews[[j]]$text
    rating <- ct_rev$reviews[[j]]$rating
    time <- ct_rev$reviews[[j]]$time_created
    new_row <- cbind(res_info, text, rating, time)
    new_df <- rbind(new_df, new_row)
  }
  
  detailed_yelp <- rbind(detailed_yelp, new_df)
}


