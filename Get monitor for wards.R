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
# how: process the wards in two steps: for the wards with a monitor, take the data of the monitor for the ward; for the wards without a monitor, take the data of the nearest monitor. 
MatchWardMon <- function(centroid.df, monitor.df) {
  # wards with monitors
  ward.withmon <- left_join(centroid.df, monitor.df, by = "sikuchoson") %>% 
    subset(!is.na(monitor_lat)) %>% 
    rename(ward = sikuchoson) %>% 
    mutate(monitor = ward, dist_ward_monitor = 0)
  
  # match the wards without monitor with the nearest monitors
  # subset the ward without a monitor
  centroid.nomon <- left_join(centroid.df, monitor.df, by = "sikuchoson") %>% 
    subset(is.na(monitor_lat)) %>% 
    select(-monitor_lat, -monitor_long, -monitor_id)
  
  # calculate the distance of those wards with the existing monitors
  dist.centroid.monitor <- 
    distm(centroid.nomon[c("long", "lat")], 
          monitor.df[c("monitor_long", "monitor_lat")]) %>%
    as.data.frame() %>% 
    rename_with(~ as.character(monitor.df$sikuchoson)) %>%
    mutate(ward = centroid.nomon$sikuchoson) %>% 
    pivot_longer(cols = monitor.df$sikuchoson, 
                 names_to = "monitor", values_to = "distance")
  
  # pick up the ward-monitor pairs with minimum distance
  ward.nomon <- dist.centroid.monitor %>% 
    group_by(ward) %>% 
    summarise(min_dist = min(distance)) %>% 
    ungroup() %>% 
    left_join(dist.centroid.monitor, 
              by = c("ward", "min_dist" = "distance")) %>%
    left_join(centroid.nomon, by = c("ward" = "sikuchoson")) %>% 
    # add the latitude and longitude of the monitors
    left_join(monitor.df, by = c("monitor" = "sikuchoson")) %>%
    rename(dist_ward_monitor = min_dist)
  
  # bind the results of wards with a monitor and those without
  ward_monitor <- bind_rows(ward.withmon, ward.nomon) %>% 
    select(ward, lat, long, monitor, monitor_id, monitor_lat, 
           monitor_long, dist_ward_monitor)
  return(ward_monitor)
}

# Read Data ----
# get attribute table of centorid of each ward
centroid <- GetAttrTab("GProcData/Ward_centroid") %>% 
  select(sikuchoson, lat, long)

# get attribute table of CO monitors
co.monitor <- GetAttrTab("GProcData/COMonitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)

# Analysis ----
ward.co.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = co.monitor)

