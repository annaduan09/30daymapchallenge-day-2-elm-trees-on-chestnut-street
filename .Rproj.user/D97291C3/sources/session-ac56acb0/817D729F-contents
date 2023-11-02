#### Setup ####
# Packages
library(tidyverse)
library(tigris)
library(yelpr)
library(sf)
library(mapview)
library(httr)
library(purrr)
library(waffle)

#### Rockville boundaries via tigris package ####
options(tigris_use_cache = TRUE) # set cache = TRUE to save time for future calls

zip_list <- c("20847", "20848", "20849", "20850", "20851", 
              "20852", "20853", "20854", "20857") # list of zip codes to include in API query

zcta <- zctas(year = 2021) %>% # get all ZIPs in US (we can't filter by state unfortunately)
  rename(zip_code = GEOID20) %>% 
  st_transform("EPSG:26980") # re-project

rock_zctas <- zcta %>%
  filter(substr(zip_code, start = 1, stop = 5) %in% zip_list) %>% # filter to those in rockville area
  erase_water(area_threshold = 0.9)

rock_bound <- st_union(rock_zctas) # rockville boundary

bg_zctas <- zcta %>%
  st_crop(xmin= 1226134, ymin= 815032.6, xmax= 1252863, ymax= 850447.1) %>%
 erase_water(area_threshold = 0.9)

roads <- primary_roads() %>%
  st_transform("EPSG:26980") %>%
  st_crop(xmin= 1226134, ymin= 815032.6, xmax= 1252863, ymax= 850447.1) %>%
  erase_water()



#### Yelp api call function ####
url <- "https://api.yelp.com/v3/businesses/search"

get_yelp = function(category, zip_code, offset_num) { # args are category of business, zipcode, and number to offset by

  queryString = list(
    location = zip_code, # argument to be filled
    term = category, # argument to be filled
    sort_by = "distance", # sort by dist
    limit = 50, # 50 is the max for yelp fusion api, any higher and it won't work
    offset = offset_num # argument to be filled
  )
  
  # use "GET" verb to request information from url
  response <- VERB("GET", url, query = queryString, 
                   add_headers('Authorization' = 'Bearer RF7JfYA8GMJRVlASJCtsrJvLMhE8nbKDjkQ8B_7ZYUPWforzJV1mNDb6ibRWJfMUu_RHFG-NxCF1QHeasyOxlpid7ZbQ1tQBcACDZBWE2rLYKDhWD8Kxps0SZJJCZXYx'), 
                   content_type("application/octet-stream"), 
                   accept("application/json"))
  
  # turn the response into a json file
  yelp.json = content(response, "parsed", flatten = TRUE, simplify = TRUE)
  
  # retrieve columns from json structure
  resto.name = data.frame(yelp.json$businesses$name)
  resto.lat = data.frame(yelp.json$businesses$coordinates.latitude)
  resto.lon = data.frame(yelp.json$businesses$coordinates.longitude)
  resto.rating = data.frame(yelp.json$businesses$rating)
  resto.addr = data.frame(yelp.json$businesses$location.address1)
  
  # bind the columns into one dataframe
  yelp.df = cbind(resto.name, resto.rating, resto.addr, resto.lat, resto.lon)  %>%
    as.data.frame()
  
  colnames(yelp.df) <- c("name", "rating", "address", "lat", "lon")
  
  # add in category alias/title (this will give us cuisine information)
  cuisine = yelp.json$businesses$categories
  
  cuis.df <- map_dfr(cuisine, function(x) {
    tibble(
      alias = paste(x$alias, collapse = ", "),
      title = paste(x$title, collapse = ", ")) %>%
      as.data.frame()
  })
  
  yelp.df <- yelp.df %>%
    cbind(cuis.df)
  
  # Replace NA values with ""
  yelp.df <- yelp.df %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
  
  # When creating an empty dataframe, use "" as default value
  if(nrow(yelp.df) == 0) {
    yelp.df <- data.frame(name="", rating=numeric(0), address="", lat=numeric(0), lon=numeric(0), alias = "", title = "", stringsAsFactors=FALSE)
  }
  
  # # return an empty dataframe with the same columns if the request is empty (e.g. the zip runs out of restaurants)
  # if(nrow(yelp.df) == 0) {
  #   yelp.df <- data.frame(name=character(0), rating=numeric(0), address=character(0), lat=numeric(0), lon=numeric(0))
  # }
  # 
   return(yelp.df)
}


#### Get restaurants from yelp ####

# Initialize a named list of empty dataframes
initialize_named_dfs <- function(zips) { 
  empty_df <- data.frame(name=character(0), rating=numeric(0),  address=character(0), lat=numeric(0), lon=numeric(0))
  named_list <- lapply(zips, function(zip) empty_df)
  names(named_list) <- zips
  return(named_list)
}
 
 resto_list_0 <- initialize_named_dfs(zip_list) # Initiate list of restaurant dataframes for each offset (since the query limit is 50, offset = 1 would return 51-100)
 resto_list_1 <- initialize_named_dfs(zip_list) # 10 dfs x 50 restaurants each = a 500 restaurant sample per zip code
 resto_list_2 <- initialize_named_dfs(zip_list)
 resto_list_3 <- initialize_named_dfs(zip_list)

 resto_list_4 <- initialize_named_dfs(zip_list)
 resto_list_5 <- initialize_named_dfs(zip_list)
 resto_list_6 <- initialize_named_dfs(zip_list)
 
 resto_list_7 <- initialize_named_dfs(zip_list)
 resto_list_8 <- initialize_named_dfs(zip_list)


 
 # master list to store the dataframes
 offset_list <- list(resto_list_0, resto_list_1, resto_list_2, resto_list_3,
                     resto_list_4, resto_list_5, resto_list_6, resto_list_7,
                     resto_list_8)
 
# Loop through each offset (think of each offset as a page of results)
for (i in 1:length(zip_list)) {

  # initialize zipnum (this is so we know where we're at in the list of zips)
  zipnum <- 1
  # initialize offset (so we can pull page 1, then page 2, then page 3, etc)
  offset <- i-1
  
  # Loop through each zip code
  for (zip_code in zip_list) {
    print(paste("batch ", offset + 1, ", ", "zip", zipnum, ": ", zip_code, sep = ""))
    
    # Fetch Yelp data for the zip code and store it in the list
    offset_list[[i]][[as.character(zip_code)]] <- get_yelp("restaurant", as.character(zip_code), offset)

    zipnum <- zipnum + 1 #iterate zipnum each loop
  }
  offset <- offset + 50
}


#### Bind dataframe ####

# Combine all dataframes into one dataframe and remove duplicates
cuisine.sf <- map_dfr(offset_list, ~ bind_rows(.x)) %>%
   unique() %>%
   st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
   st_transform("EPSG:26980") %>%
   filter(address != "618 Main St") #remove bakery in SF lol

#### Identify cuisine ####
# Define cuisines and their categories
cuisine_map <- list(
  e_asian = c("szechuan", "chinese", "shanghai", "hot pot", "garden", "cantonese", 
              "dim sum", "sushi", "ramen", "asian", "boba", "korean", "wok", "nam", "kum san"),
  american = c("american", "tex", "juice", "wingstop", "juice", "cafe", "donuts", 
               "pizza", "sandwiches", "burgers", "barbeque", "fast food", "deli", "brunch", "breakfast", "salad", "gastropub", "cole's", "vegan"),
  soul = c("soul food"),
  indian = c("indian"),
  middle_eastern = c("middle east", "halal", "kebab", "kabob", "turkish"),
  european = c("mediterranean", "greek", "italian", "french", "brasserie", "euro"),
  se_asian = c("vietnamese", "thai", "ramen", "laotian", "indonesian"),
  latin = c("mexican", "tapas", "tacos", "burritos", "salvadoran", "peruvian", 
            "brazilian", "portug"))

# Function to find the cuisine based on categories
find_cuisine <- function(title) {
  for(cuisine in names(cuisine_map)) {
    if(any(str_detect(tolower(title), cuisine_map[[cuisine]]))) {
      return(cuisine)
    }
  }
  return("Other") # Default category
}

# Apply the function to your data
cuisine.sf <- cuisine.sf %>%
  mutate(cuis_string = paste(name, title, sep = " ")) %>%
  rowwise() %>%
  mutate(cuisine = find_cuisine(cuis_string))

cuisine.sf$x <- st_coordinates(cuisine.sf)[, 'X']
cuisine.sf$y <- st_coordinates(cuisine.sf)[, 'Y']

cuisine_counts <- cuisine.sf %>% 
  count(cuisine) %>%
  arrange(desc(n))

# Use the ordered names to set factor levels
cuisine.sf <- cuisine.sf %>%
  mutate(cuisine = factor(cuisine, levels = cuisine_counts$cuisine))

#### VISUALS ####
# blue background rectangle for water features
water_rect <- st_as_sfc(st_bbox(bg_zctas), crs = "EPSG:26980")

# map + waffle palette
pal <- c("chartreuse3", "orchid2", "turquoise2", "orange", "brown", "beige", "deeppink3", "blue")

# map
ggplot() + 
  geom_sf(data = water_rect, fill = "midnightblue") +
  geom_sf(data = bg_zctas, fill = "gray8", color = "gray15", size = 0.2) +
  geom_sf(data = rock_zctas, fill = "gray10", color = "gray15", size = 0.2) +
  geom_sf(data = roads, color = "gray12") +
  geom_jitter(data = cuisine.sf, shape = 24, 
          size = 1.5, aes(colour = cuisine, fill = cuisine, x=x, y=y),
          width = 1500, height = 1500) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme_void() + theme(legend.position = "none",
                       legend.text = element_text(color = "gray10"))

# waffle chart
ggplot(data = cuisine_counts, aes(fill = cuisine, values = n)) +
  geom_waffle(na.rm = TRUE, color = NA, flip = TRUE, nrows = 15) + 
  theme_void()

cuisine_waffle <- c("american" = 186, "east asian" = 68, "latin" = 16, "other" = 14, "european" = 9,
       "indian" = 7, "middle eastern" = 6, "southeast asian" = 6)

waffle(cuisine_waffle/2, rows = 11,
       colors = pal)

#### References ####
# MIME types: https://en.wikipedia.org/wiki/Media_type
# HTTR
# yelpr package
# yelp API

