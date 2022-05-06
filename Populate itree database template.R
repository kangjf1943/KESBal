# Statement ----
# to match the air pollution monitor for each ward 
# then populate the air pollution monitor data to template of i-Tree database 

# Package ----
library(dplyr)
library(tidyr)
library(Hmisc)
library(openxlsx)
library(shapefiles)
library(geosphere)
library(chron)

# Function ----
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
    rename_with(~ as.character(monitor.df$monitor_id)) %>%
    mutate(ward = centroid.df$sikuchoson) %>% 
    pivot_longer(cols = monitor.df$monitor_id, 
                 names_to = "monitor_id", values_to = "distance")
  
  # pick up the ward-monitor pairs with minimum distance
  ward.monitor <- dist.centroid.monitor %>% 
    group_by(ward) %>% 
    summarise(min_dist = min(distance)) %>% 
    ungroup() %>% 
    left_join(dist.centroid.monitor, 
              by = c("ward", "min_dist" = "distance")) %>%
    left_join(monitor.df, by = "monitor_id") %>% 
    rename(dist_ward_monitor = min_dist, monitor = sikuchoson) %>% 
    select(ward, monitor, monitor_id, dist_ward_monitor, 
           monitor_lat, monitor_long)
  return(ward.monitor)
}

# function: to get all the tables of a *.mdb file into a list
# parameters: 
# mdb.name: the name of the *.mdb file
# mdb.dir: the directory of the *.mdb file 
GetMdb <- function(mdb.name, mdb.dir="GRawData/Kang-san2/2019/") {
  # get table names of the *.mdb file
  names.mdb <- mdb.get(paste0(mdb.dir, mdb.name), tables = TRUE)
  
  # a empty list to store the tables of the *.mdb file
  output.list <- vector("list", length = length(names.mdb))
  names(output.list) <- names.mdb
  
  # get all the tables of *.mdb file and store them into the list
  for (i in names.mdb) {
    output.list[[i]] <- mdb.get(paste0(mdb.dir, mdb.name), tables = i)
  }
  return(output.list)
}

# function: process the table from *.mdb database to get ready for the fusion with i-Tree template database based on target location and target pollutant 
# parameters: 
# ward.pollutant.monitor: data indicate which monitor to use for the target ward 
# mdbdata.list: list based on *.mdb data provided by Dr. Hirabayashi 
# name.ward: name of the target location
# name.pollutant: name of the target pollutant 
ProcessMdbTab <- function(ward.pollutant.monitor, mdbdata.list, 
                          name.ward, name.pollutant) {
  # 根据监测点id把要的表格从数据库中摘出来
  name.tar.tab <- 
    ward.pollutant.monitor$monitor_id[which(
      ward.pollutant.monitor$ward == name.ward)]
  tar.tab <- mdbdata.list[[name.tar.tab]] %>% 
    as_tibble()
  
  # parse date and time 
  tar.tab.date <- chron::month.day.year(tar.tab$TimeStamp)
  tar.tab.datetime <- 
    tibble(month = tar.tab.date$month, day = tar.tab.date$day,
           hour = hours(tar.tab$TimeStamp))
  
  # process the target table 
  tar.tab <- tar.tab %>% 
    # join information of date and hour to original table 
    bind_cols(tar.tab.datetime) %>% 
    # add pollutant name and monitor ID
    mutate(spname = name.pollutant, monitor_id_mdb = name.tar.tab) %>% 
    rename(units_mdb = Unit, samplevalue_mdb = SampleValue) %>% 
    rename_with(toupper) %>% 
    select(MONTH, SPNAME, DAY, HOUR, 
           MONITOR_ID_MDB, UNITS_MDB, SAMPLEVALUE_MDB)
  return(tar.tab)
}

# function: join processed table to i-Tree database template 
# bug: need to refine the code 
FillTemplate <- function(processed.tab, enname.ward) {
  # join the table to template 
  itree.template %>% 
    mutate(CITYNAME = enname.ward) %>%
    left_join(processed.tab, by = c("MONTH", "DAY", "HOUR","SPNAME")) %>% 
    # replace the blank columns of template with the data from *.mdb 
    select(-ADDR, -UNITS, -QUANTITY) %>% 
    rename(ADDR = MONITOR_ID_MDB, UNITS = UNITS_MDB, QUANTITY = SAMPLEVALUE_MDB) %>% 
    select(names(itree.template)) %>% 
    return()
}

# function: get MS Excel tables of certain pollution data for each ward 
GetPollutionExcels <- function(ward.pollutant.monitor, 
                               output.dir="RProcData/Add pollution to database/") {
  for (i in ward.pollutant.monitor$ward) {
    ProcessMdbTab(ward.pollutant.monitor, co.mdb.hour, i, "CO") %>% 
      FillTemplate(enname.ward = "Kita-ku") %>% 
      write.xlsx(file = paste0(output.dir, i, ".co.xlsx"
      ))
  }
}


# Read Data ----
# get attribute table of centorid of each ward
centroid <- GetAttrTab("GProcData/Ward_centroid") %>% 
  select(sikuchoson, lat, long)

# get attribute table of monitors location
co.monitor.shp <- GetAttrTab("GProcData/COMonitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)

no2.monitor.shp <- GetAttrTab("GProcData/NO2Monitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)

o3.monitor.shp <- GetAttrTab("GProcData/O3Monitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)

pm25.monitor.shp <- GetAttrTab("GProcData/PM25Monitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)

so2.monitor.shp <- GetAttrTab("GProcData/SO2Monitors_2019_add_ward") %>% 
  mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
  select(sikuchoson, latitude, longitude, monitor_id) %>% 
  rename(monitor_lat = latitude, monitor_long = longitude)






# hourly data of monitors 
co.mdb.hour <- GetMdb(name.file = "CO.mdb")

# match wards with monitors 
ward.co.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = co.monitor.shp)
ward.no2.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = no2.monitor.shp)
ward.o3.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = o3.monitor.shp)
ward.pm25.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = pm25.monitor.shp)
ward.so2.monitor <- MatchWardMon(centroid.df = centroid, monitor.df = so2.monitor.shp)

# data of template for i-Tree database
itree.template <- read.xlsx("RRawData/Air Pollution Template.xlsx") %>% 
  as_tibble()

# Analysis ----




