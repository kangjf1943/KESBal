# Statement ----
# the code is to match the air pollution monitor for each ward 

# Package ----
library(dplyr)
library(tidyr)
library(shapefiles)
library(geosphere)

# Functions ----
# function: read attribute table of shapefile of GIS
# name.path: the directory of the shapefile
GetAttrTab <- function(name.path) {
  output.df <- read.shapefile(name.path) %>% 
    # get the attribute table of the *.shp file
    .$dbf %>%  # get a list in this step 
    .$dbf %>%  # get a data.frame in this step 
    rename_with(tolower)
  return(output.df)
}

# function: get monitor for each ward
# how: take the data of the nearest monitor. 
MatchWardMon <- function(centroid.df, monitor.df) {
  # calculate the distance of wards with the existing monitors
  dist.centroid.monitor <- centroid.df %>%  
    select(long, lat) %>% 
    distm(monitor.df[c("monitor_long", "monitor_lat")]) %>%
    as.data.frame() %>% 
    rename_with(~ as.character(monitor.df$sikuchoson)) %>%
    mutate(ward = centroid.df$sikuchoson) %>% 
    pivot_longer(cols = monitor.df$sikuchoson, 
                 names_to = "monitor", values_to = "distance")
  
  # pick up the ward-monitor pairs with minimum distance
  ward.monitor <- dist.centroid.monitor %>% 
    group_by(ward) %>% 
    summarise(min_dist = min(distance)) %>% 
    ungroup() %>% 
    left_join(dist.centroid.monitor, 
              by = c("ward", "min_dist" = "distance")) %>%
    left_join(monitor.df, by = c("monitor" = "sikuchoson")) %>% 
    rename(dist_ward_monitor = min_dist) %>% 
    select(ward, monitor, monitor_id, dist_ward_monitor, 
           monitor_lat, monitor_long)
  return(ward.monitor)
}

# Read Data ----
# get attribute table of centorid of each ward
centroid <- GetAttrTab("GProcData/Ward_centroid") %>% 
  select(sikuchoson, lat, long)

# get attribute table of monitors
co.monitor <- GetAttrTab("GProcData/COMonitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)

# Analysis ----
ward.co.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = co.monitor)

