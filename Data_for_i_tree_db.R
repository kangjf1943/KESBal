# Statement ----
# Code used to generate the information for adding new location (in this case, Kyoto), and the air pollution data to i-Tree database. According to the guidance of i-Tree database, the data should be populated into the required template. Thus here I fill the required data into a table with same format with the required template, then manually copy the generated data into the "real template". After that, I upload the data to i-Tree database, or send them to a staff of the David company. 
# The required information for adding a new location in i-Tree database include Continent, Nation, State/Province, State/Province type, County/District, County/District Type, City, Currency, Latitude, Longitude, Elevation (meters), Population, Area in square meters, Climate Region, Electricity Emission (kg CO2/kWh), Mean Minimum Temperature (Fahrenheit), Leaf On Day of Year, Leaf Off Day of Year, GMT Offset (hours), Warm Temperature, Abundant Rain, Abundant Vegetation, Snow, Ozone State, Add pollution data for this location, Year, Pollution Data, Pollution Monitor Source, Latitude, Longitude, Resubmission. I get the data from multiple data source. To understand how I get the information quickly, please search this code with "column-" plus the information of your inquiry, for example, search "column-Continent" to see how I get the information/value of continent. 

# Preparation ----
# Get data for latitude, longitude, and elevation of wards in QGIS: 
# Based on ward map of Kyoto city and centroid tool of QGIS. the detailed steps are: (1) Generate new layer of the centroids of the wards with “Centroid” tool; (2) Use “Field calculator” tool to get the latitude (the expression should be “$y”) and longitude (the expression should be “$x”) of each centroid; (3) Use “Add raster values to points” tool to extract the elevation data from the raster data to the centroid point layer, don’t forget to change the newly added column name to “Elevation”; (4) Then export the attribute table of the centroid layer to Microsoft Excel. 

# Get forest coverage data in QGIS: 
# The "土地利用細分メッシュデータ" from 国土交通省 of Japan is used to calculate the vegetation cover of each ward. The steps are: (1) Join the information of “ward”to land use mesh data; (2) Export the attribute table of the file GRawData/L03-b-16_5235-jgd_GML/L03-b-16_5235.shp to *.csv file to directory "GProcData/". This is the mesh data of land use of Kyoto and surrounding area.The *.csv file will be used to calculate the forest coverage of each ward. 

# Package ----
library(Hmisc)
library(openxlsx)
library(chron)
library(shapefiles)
library(dplyr)
library(tidyr)
library(geosphere)

# Constant ----
# the air pollutants covered in the study 
kPollutants <- c("CO", "NO2", "O3", "PM25", "SO2")

# data of template for i-Tree database
itree.template <- read.xlsx("RRawData/Air_pollution_template.xlsx") %>% 
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
# parameters: 
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
# how: take the data of the nearest monitor
# parameters: 
# centroid.df: data.frame of centroid
# monitor.df: data.frame of monitors 
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
GetMdb <- function(mdb.name, mdb.dir="RRawData/ITreeOutput/") {
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
    # replace the target columns with the new data
    mutate(
      ADDR = ifelse(is.na(MONITOR_ID_MDB), "MonitorID", MONITOR_ID_MDB), 
      UNITS = ifelse(is.na(UNITS_MDB), 7, UNITS_MDB), 
      QUANTITY = ifelse(is.na(UNITS_MDB), -999, SAMPLEVALUE_MDB)
    ) %>%
    # replace the blank columns of template with the data from *.mdb 
    select(-MONITOR_ID_MDB, -UNITS_MDB, -SAMPLEVALUE_MDB) %>% 
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
# fix the unit for PM 2.5
pollutant.mdb.hour$PM25 <- 
  lapply(pollutant.mdb.hour$PM25, 
         function(x) {x %>% mutate(Unit = 1)})
# should eliminate "unit" column in "MonitorInfo" data.frame
pollutant.mdb.hour$PM25$MonitorInfo$Unit <- NULL

# get population data
# make a empty character vector to store the results 
ward.population <- character()
# read the MS Excel file of population data 
for (i in centroid$sikuchoson) {
  ward.population <- 
    c(ward.population, 
      read.xlsx("RRawData/Kyoto_population_2019.xlsx", sheet = i)[3, 2])
}
ward.population <- data.frame(sikuchoson = centroid$sikuchoson, 
                              population = ward.population)

# read and calculate the forest coverage 
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

# Analysis ----
# prepare new location info
location.info <- 
  # get basic information: ward name and area
  GetAttrTab("GRawData/Kyoto_ward") %>% 
  select(sikuchoson, city_eng, area) %>% 
  # column-Latitude, column-Longitude, column-Elevation (meters)
  # join latitude, longitude, and elevation of the wards 
  left_join(
    # process centroid *.shp file
    GetAttrTab("GProcData/Kyoto_ward_centroid") %>% 
      select(city_eng, lat, long, elevation), by = "city_eng") %>% 
  # column-Population: join population data
  left_join(ward.population, by = "sikuchoson") %>% 
  mutate(
    # column-Climate Region: I compared the temperature data of Atlantic and Kyoto, and decided to use the default value “Mid-Atlantic”. The reason for that default choice is possibly because the area has similar latitude to Kyoto.
    climate_region = "Mid-Atlantic", 
    # column-Electricity Emissions (kg CO2/kWh): use “0.509”, which is estimated by Dr. Hirabayashi based on local data of Kyoto. 
    electricity_emissions = 0.509, 
    # column-Mean Minimum Temperature (Fahrenheit): calculate mean daily minimum temperature then turn to Fahrenheit degree
    mean_minimum_temperature_fahrenheit = mean(sta.temp$temp) * 1.8 + 32, 
    # column-Leaf On Day of Year and column-Leaf Off Day of Year: based on Table S1 of Kang, J., Hirabayashi, S., Shibata, S., 2022. Urban Forest Ecosystem Services Vary with Land Use and Species: A Case Study of Kyoto City. Forests 13, 67, the value is Apri 4th, which is 94th day of the year 2019, while the date for "Leaf Off Day of Year" is November 18th, which is 322nd day of the year 2019.
    leaf_on_day_of_year = 94, 
    leaf_off_day_of_year = 322, 
    # column-GMT Offset (hours): the value is “9”.
    gmt_offset_hours = 9, 
    # column-Warm Temperatures: select “Yes”. 
    warm_temperatures = "Yes", 
    # column-Abundant Rain: select “Yes”. 
    abundant_rain = "Yes") %>% 
  left_join(forest.cover, by = "sikuchoson") %>% 
  mutate(
    # column-Abundant Vegetation
    forest_coverage = ifelse(is.na(.$forest_coverage), 0, forest_coverage), 
    abundant_vegetation = ifelse(forest_coverage >= 0.5, "Yes", "No"), 
    # column-Snow: According to Weather Spark website for Kyoto (https://ja.weatherspa
    # rk.com/y/143438/%E4%BA%AC%E9%83%BD%E5%B8%82%E3%80%81%E6%97%A5%E6%9C%AC%E3%
    # 81%AB%E3%81%8A%E3%81%91%E3%82%8B%E5%B9%B4%E9%96%93%E3%81%AE%E5%B9%B3%E5%
    # 9D%87%E7%9A%84%E3%81%AA%E6%B0%97%E5%80%99), the value is “Yes”. 
    snow = "Yes",
    # column-Ozone State: according to Alexis Ellis of i-Tree company, choose a location that has similar latitude to Kyoto. In this case, I choose “Georgia”
    ozone_state = "Georgia", 
    # column-Add pollution data for this location?: put a tick on the box. 
    add_pollution_data_for_this_location = "put a tick on the box",  
    # column-Year: “2019”, since both the years of the plant field investigation and the pollution data processed by Dr. Hirabayashi are 2019. 
    year = "2019", 
    # column-Pollution Data: Processed with R based on database provided by Dr. Hirabayashi. An example see the Excel of Kita-ku ward of Kyoto City. I will upload the code and raw data afterward. One may notice that there is no data for PM 10 in Kyoto, so I just left it blank. 
    pollution_data = paste0(
      "populate CO data of the ward from 'AddPollution' file to template ", 
      "then upload it"
    ), 
    # column-Pollution Monitor Source 
    pollution_monitor_source = 
      "National Institute for Environmental Studies, Japan"
  ) %>% 
    # add latitude and longitude of the matched CO monitors 
    # column-Latitude: The latitude of the nearest monitor is used. I used QGIS and R to match the centroids of wards with the nearest monitor for each air pollutant 
    # column-Longitude: The latitude of the nearest monitor is used. I used QGIS and R to match the centroids of wards with the nearest monitor for each air pollutant
    left_join(ward.monitor.match$CO, by = c("sikuchoson" = "ward")) %>% 
    select(-monitor, -monitor_id, -dist_ward_monitor) %>% 
    # column-Resubmission: That is the first time we users submit the pollution data of each ward of Kyoto to i-Tree database, so select “No”. 
    mutate(resubmission = "No") %>% 
  # tidy data to fit the upload data requirement 
  select(-sikuchoson) %>% 
  rename(latitude = lat, 
         longitude = long, 
         elevation_meters = elevation, 
         area_in_square_meters = area,  # column-Area in square meters
         city = city_eng) %>% 
  # column-City
  mutate(city = gsub("Kyoto-shi, ", "", city)) %>% 
  mutate(
    continent = "Asia",  # column-Continent
    nation = "Japan",  # column-Nation
    state_province = "Kyoto",  # column-State/Province
    state_province_type = "Prefecture",  # column-State/Province Type
    county_district = "Kyoto",  # column-County/District
    county_district_type = "City",  # column-County/District Type
    currency = "Yen"  # column-Currency
  ) %>% 
  select(
    continent, nation, state_province, state_province_type, 
    county_district, county_district_type, city, currency, 
    latitude, longitude, elevation_meters, 
    population, area_in_square_meters, 
    climate_region, electricity_emissions, mean_minimum_temperature_fahrenheit,
    leaf_on_day_of_year, leaf_off_day_of_year, gmt_offset_hours, 
    warm_temperatures, abundant_rain, abundant_vegetation, snow, ozone_state, 
    add_pollution_data_for_this_location, year, pollution_data, 
    pollution_monitor_source, monitor_lat, monitor_long, resubmission)

# Export data ----
# if file name doesn't exist, create one
if(!file.exists("RProcData/AddToITreeDatabase")) {
  dir.create("RProcData/AddToITreeDatabase")
  dir.create("RProcData/AddToITreeDatabase/AddPollution")
}

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
                    "Match_wards_and_monitors_for_each_pollutant.xlsx"))

# export air pollution data for each ward and each pollutant
for (i in c("CO", "NO2", "O3", "SO2")) {
  GetPollutionExcels(ward.pollutant.monitor = ward.monitor.match[[i]], 
                     pollutant.mdb.hour = pollutant.mdb.hour[[i]], 
                     name.pollutant = i)
}
GetPollutionExcels(ward.pollutant.monitor = ward.monitor.match[["PM25"]], 
                   pollutant.mdb.hour = pollutant.mdb.hour[["PM25"]], 
                   name.pollutant = "PM2.5")

# Manual operation ----
# First, input the new location data into i-Tree database based on "RProcData/AddToITreeDatabase/Location_info.xlsx".
# Second, import air pollution data. Populate the data of the tables of "RProcData/AddToITreeDatabase/AddPollution" to original template. I put the processed templates for each ward in "MProcData/AddPollutToITreeDatabase". Then send them to staff of David company, they will kindly help to import the data into the database. Or, alternatively, you can also import the data by yourself. Just for the record, you can also check the file "RProcData/AddToITreeDatabase/Match_wards_and_monitors_for_each_pollutant.xlsx" to see which monitor I used for each ward respectively. 
# For precipitation data, just copy the data of "RRawData/Kyoto_hourly_precipitation_2019.csv" and paste them in the template "RRawData/Rain_2019.xlsx". I put the processed template in "MProcData/AddRainToITreeDatabase/Rain_2019.xlsx". Then, again, you can either send it to staff of David company, or import the data by yourself. 
