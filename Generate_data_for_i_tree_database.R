# Statement ----
# Code used to generate the information for adding new location to i-Tree database

# Preparation ----
## Download data ----

## QGIS ----
# Data for latitude, longitude, and elevation of wards: 
# Based on ward map of Kyoto city and centroid tool of QGIS. the detailed steps are: 
# Generate new layer of the centroids of the wards with “Centroid” tool; 
# Use “Field calculator” tool to get the latitude (the expression should be “$y”) and longitude (the expression should be “$x”) of each centroid; 
# Use “Add raster values to points” tool to extract the elevation data from the raster data to the centroid point layer, don’t forget to change the newly added column name to “Elevation”; 
# Then export the attribute table of the centroid layer to Microsoft Excel. 
# Besides, there is a interactive online GIS showing the elevation data of Japan (https://zh-cn.topographic-map.com/maps/eh0b/%E4%BA%AC%E9%83%BD%E5%B8%82/). 
  
# Data for forest coverage calculation: 
# I used 土地利用細分メッシュデータ from 国土交通省 to calculate the vegetation cover of each ward. The steps are: 
# Join the information of “ward”to land use mesh data; 
# Export the attribute table of GRawData/L03-b-16_5235-jgd_GML/L03-b-16_5235.shp to *.csv file to directory "GProcData/". This is the mesh data of land ue of Kyoto and surrounding area.The *.csv file will be used to calculate the forest coverage of each ward. 

# Package ----
library(dplyr)
library(tidyr)
library(Hmisc)
library(openxlsx)
library(shapefiles)
library(geosphere)
library(chron)

# Predefined ----
# the air pollutants covered in the study 
kPollutants <- c("CO", "NO2", "O3", "PM25", "SO2")

# data of template for i-Tree database
itree.template <- read.xlsx("RRawData/Air Pollution Template.xlsx") %>% 
  as_tibble()

# Japanese names and English names of wards
kWardNames <- data.frame(
  ward_ja = c("北区", "上京区", "左京区", "中京区", 
              "東山区", "下京区", "南区", "右京区", 
              "伏見区", "山科区", "西京区"), 
  ward_en = c("Kita-ku", "Kamigyo-ku", "Sakyo-ku", "Nakagyo-ku", 
              "Higashiyama-ku", "Shimogyo-ku", "Minami-ku", "Ukyo-ku", 
              "Fushimi-ku", "Yamashina-ku", "Nishikyo-ku")
)

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
    select(MONTH, SPNAME, DAY, HOUR, MONITOR_ID_MDB, UNITS_MDB, SAMPLEVALUE_MDB)
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
GetPollutionExcels <- function(
  ward.pollutant.monitor, pollutant.mdb.hour, name.pollutant, 
  output.dir="RProcData/AddToITreeDatabase/AddPollution/") {
  # translate of ward names from Japanese to English
  ward.name <- kWardNames
  
  for (i in ward.pollutant.monitor$ward) {
    tar.ward_en <- ward.name$ward_en[which(ward.name$ward_ja == i)]
    ProcessMdbTab(
      ward.pollutant.monitor, pollutant.mdb.hour, i, name.pollutant) %>% 
      FillTemplate(enname.ward = tar.ward_en) %>% 
      write.xlsx(file = paste0(
        output.dir, tar.ward_en, "_", name.pollutant, ".xlsx"))
  }
}

# Read data ----
# get attribute table of centorid of each ward
centroid <- GetAttrTab("GProcData/Kyoto_ward_centroid") %>% 
  select(sikuchoson, lat, long)

# get attribute table of monitor *.shp files into a list
# a empty list to store the attribute tables
monitor.shp.attr <- vector("list", length = length(kPollutants))
names(monitor.shp.attr) <- kPollutants
# get attribute tables and fill them into the list
for (i in names(monitor.shp.attr)) {
  monitor.shp.attr[[i]] <- 
    GetAttrTab(paste0("GProcData/", i, "Monitors_2019_add_ward")) %>% 
    mutate(monitor_id = paste(state, county, siteid, sep = "_")) %>%
    select(sikuchoson, latitude, longitude, monitor_id) %>% 
    rename(monitor_lat = latitude, monitor_long = longitude)
}

# match wards with monitors 
ward.monitor.match <- vector("list", length = length(kPollutants))
names(ward.monitor.match) <- kPollutants
for (i in names(ward.monitor.match)) {
  ward.monitor.match[[i]] <- 
    MatchWardMon(centroid.df = centroid, monitor.df = monitor.shp.attr[[i]])
}

# read daily minimum temperature of Kyoto station in Kyoto city 
sta.temp <- 
  read.csv("RRawData/Kyoto_temperature.csv", skip = 6, header = FALSE) %>% 
  as_tibble() %>% 
  rename_with(~ c("date", "temp", "var_1", "var_2")) %>% 
  select(date, temp)

# hourly data of monitors 
pollutant.mdb.hour <- vector("list", length = length(kPollutants))
names(pollutant.mdb.hour) <- kPollutants
for (i in names(pollutant.mdb.hour)) {
  pollutant.mdb.hour[[i]] <- GetMdb(mdb.name = paste0(i, ".mdb"))
}

# Analysis ----
# prepare new location info
location.info <- 
  # get basic information: ward name and area
  GetAttrTab("GRawData/Kyoto_ward") %>% 
  select(sikuchoson, city_eng, area) %>% 
  # join latitude, longitude, and elevation of the wards 
  left_join(
    # process centroid *.shp file
    GetAttrTab("GProcData/Kyoto_ward_centroid") %>% 
      select(city_eng, lat, long, elevation), by = "city_eng")

# get population data
# make a empty charactor vector to store the results 
ward.population <- character()
# read the MS Excel file of population data 
for (i in location.info$sikuchoson) {
  ward.population <- 
    c(ward.population, 
      read.xlsx("RRawData/Kyoto_population_2019.xlsx", sheet = i)[3, 2])
}
ward.population <- data.frame(sikuchoson = location.info$sikuchoson, 
                              population = ward.population)

# continue to complete the information table 
location.info <- location.info %>% 
  # join population data
  left_join(ward.population, by = "sikuchoson") %>% 
  mutate(
    # add climate region: I compared the temprature data of Atlantic and Kyoto, and decided to use the default value “Mid-Atlantic”. The reason for that default choice is possibly because the area has similar latitude to Kyoto.
    climate_region = "Mid-Atlantic", 
    # Electricity Emissions (kg CO2/kWh): use “0.509”, which is estimated by Dr. Hirabayashi based on local data of Kyoto. 
    electricity_emissions = 0.509, 
    # calculate mean daily minimum temperature then turn to Fahrenheit degree
    mean_minimum_temperature_fahrenheit = mean(sta.temp$temp) * 1.8 + 32, 
    # Leaf On Day of Year: Based on Table S1 of Kang, J., Hirabayashi, S., Shibata, S., 2022. Urban Forest Ecosystem Services Vary with Land Use and Species: A Case Study of Kyoto City. Forests 13, 67, the value is Apri 4th, which is 94th day of the year 2019, while the date for "Leaf Off Day of Year" is November 18th, which is 322nd day of the year 2019.
    leaf_on_day_of_year = 94, 
    leaf_off_day_of_year = 322, 
    # GMT Offset (hours): the value is “9”.
    gmt_offset_hours = 9, 
    # Warm Temperatures: select “Yes”. 
    warm_temperatures = "Yes", 
    # Abundant Rain: select “Yes”. 
    abundant_rain = "Yes"
  )

# calculate the forest coverage 
forest.cover <- read.csv("GProcData/L03-b-16_5235.csv") %>% 
  rename_with(tolower) %>% 
  # keep the data within Kyoto city
  select(土地利用種, sikuchoson) %>% 
  subset(sikuchoson != "") %>%
  # calculate forest cover rate of each ward
  group_by(sikuchoson, 土地利用種) %>% 
  summarise(n = n()) %>% 
  mutate(prop_n = n/sum(n)) %>% 
  ungroup() %>% 
  # keep the proportion of forest
  subset(土地利用種 == 500) %>% 
  rename(forest_coverage = prop_n) %>% 
  select(sikuchoson, forest_coverage)

# continue to complete the location information table 
location.info <- location.info %>% 
  left_join(forest.cover, by = "sikuchoson") %>% 
  mutate(forest_coverage = 
           ifelse(is.na(.$forest_coverage), 0, forest_coverage), 
         abundant_vegetation = 
           ifelse(forest_coverage >= 0.5, "Yes", "No")) %>% 
  mutate(
    # Snow: According to Weather Spark website for Kyoto (https://ja.weatherspark.com/y/143438/%E4%BA%AC%E9%83%BD%E5%B8%82%E3%80%81%E6%97%A5%E6%9C%AC%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E5%B9%B4%E9%96%93%E3%81%AE%E5%B9%B3%E5%9D%87%E7%9A%84%E3%81%AA%E6%B0%97%E5%80%99), the value is “Yes”. 
    snow = "Yes",
    # Ozone State: according to Alexis Ellis of i-Tree company, choose a location that has similar latitude to Kyoto. In this case, I choose “Georgia”.  
    ozone_state = "Georgia", 
    # Add pollution data for this location?: put a tick on the box. 
    add_pollution_data_for_this_location = "put a tick on the box",  
    # Year: “2019”, since both the years of the plant field investigation and the pollution data processed by Dr. Hirabayashi are 2019. 
    year = "2019", 
    # Pollution Data: Processed with R based on database provided by Dr. Hirabayashi. An example see the Excel of Kita-ku ward of Kyoto City. I will upload the code and raw data afterward. One may notice that there is no data for PM 10 in Kyoto, so I just left it blank. 
    pollution_data = "add CO data from 'AddPollution'"
  )

pollution.data.source <- "National Institute for Environmental Studies, Japan"

location.info <- location.info %>% 
  # Pollution Monitor Source: input “National Institute for Environmental Studies, Japan”.
  mutate(pollution_monitor_source = pollution.data.source) %>% 
  # add latitude and longitude of the matched CO monitors 
  # Latitude: The latitude of the nearest monitor is used. I used QGIS and R to match the centroids of wards with the nearest monitor for each air pollutant. 
  # Longitude: The latitude of the nearest monitor is used. I used QGIS and R to match the centroids of wards with the nearest monitor for each air pollutant.
  left_join(ward.monitor.match$CO, by = c("sikuchoson" = "ward")) %>% 
  select(-monitor, -monitor_id, -dist_ward_monitor) %>% 
  # Resubmission: That is the first time we users submit the pollution data of each ward of Kyoto to i-Tree database, so select “No”. 
  mutate(resubmission = "No")

# Export data ----
# export new location basic data
write.xlsx(location.info, "RProcData/AddToITreeDatabase/Location_info.xlsx")

# export ward-monitor match for each pollutant
lapply(ward.monitor.match, function(x) {
  x %>% 
    left_join(kWardNames, by = c("ward" = "ward_ja")) %>% 
    select(-ward) %>% 
    rename(target_ward = ward_en) %>% 
    left_join(kWardNames, by = c("monitor" = "ward_ja")) %>% 
    select(-monitor) %>% 
    rename(datasource_monitor = ward_en) %>% 
    select(target_ward, datasource_monitor, monitor_id, dist_ward_monitor, 
           monitor_lat, monitor_long)
}) %>% 
  write.xlsx(paste0("RProcData/AddToITreeDatabase/", 
                    "Match of wards and monitors for each pollutant.xlsx"))

# export air pollution data for each ward and each pollutant
for (i in c("CO", "NO2", "O3", "SO2")) {
  GetPollutionExcels(ward.pollutant.monitor = ward.monitor.match[[i]], 
                     pollutant.mdb.hour = pollutant.mdb.hour[[i]], 
                     name.pollutant = i)
}
GetPollutionExcels(ward.pollutant.monitor = ward.monitor.match[["PM25"]], 
                   pollutant.mdb.hour = pollutant.mdb.hour[["PM25"]], 
                   name.pollutant = "PM2.5")
