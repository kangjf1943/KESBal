library(Hmisc)
library(lubridate)

# Air pollutants removal ----
## Data import ----
### Hourly data ----
# 选取目标表格数据
air_hour <- mdb.get(
  "RawData/ItreeCityLevel/Tree/DryDeposition.mdb", 
  tables = "DryDeposition")
names(air_hour) <- tolower(names(air_hour))
# 删除CO和PM10的数据：没有关于他们的健康影响效益估算
air_hour <- air_hour[which(!air_hour$pollutant %in% c("PM10*", "CO")), ]
# 添加小时数据
air_hour$hour <- rep_len(1:24, length.out = nrow(air_hour))

# 各天内各小时的数据变化
# 计算各类污染物各小时值：取各个监测点的平均值
air_hour <- 
  aggregate(flux ~ pollutant + timestamp + hour, data = air_hour, mean)
# 查看各小时变化
# 因为有明显季节分异：
# ggplot(air_hour) + 
#   geom_point(aes(hour, flux, color = timestamp)) + 
#   facet_wrap(~pollutant, scales = "free")

# 增加周几，月份，季节列
air_hour$weekday <- weekdays(air_hour$timestamp)
air_hour$weekday <- factor(
  air_hour$weekday, 
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
             "Saturday", "Sunday"))

air_hour$month <- month(air_hour$timestamp)

air_hour$season <- "spring"
air_hour$season[which(air_hour$month <= 6 & air_hour$month > 3)] <- "summer"
air_hour$season[which(air_hour$month <= 9 & air_hour$month > 6)] <- "autumn"
air_hour$season[which(air_hour$month <= 12 & air_hour$month > 9)] <- "winter"
air_hour$season <- 
  factor(air_hour$season, levels = c("spring", "summer", "autumn", "winter"))

### Hourly summary ----
air_hour_smry <- vector("list")

air_hour_smry$weekday <- 
  aggregate(flux ~ pollutant + weekday + hour, data = air_hour, median)
air_hour_smry$month <- 
  aggregate(flux ~ pollutant + month + hour, data = air_hour, mean)
air_hour_smry$season <- 
  aggregate(flux ~ pollutant + season + hour, data = air_hour, mean)

### Daily data ----
# 将小时数据叠加成日数据
# 先将各小时的时间累加
air_day <- aggregate(
  flux ~ pollutant + timestamp + season + month + weekday, 
  data = air_hour, sum)

### Monthly data ----
air_month <- aggregate(
  flux ~ pollutant + season + month, 
  data = air_day, sum)

## Analysis ----
### Hourly data ----
# 可视化
# 时间变化及变异：盒形图
# 一周内各天分图：一天内各小时污染物去除率变化
ggplot(air_hour) + 
  geom_boxplot(aes(factor(hour), flux)) + 
  facet_grid(pollutant~weekday, scales = "free")
# 待办：视觉上看没有明显差异，之后可以统计对比一下

# 各月分图：一天内各小时污染物去除率变化
ggplot(air_hour) + 
  geom_boxplot(aes(factor(hour), flux)) + 
  facet_grid(pollutant~month, scales = "free")

# 各季节分图：一天内各小时污染物去除率变化
ggplot(air_hour) + 
  geom_boxplot(aes(factor(hour), flux)) + 
  facet_grid(pollutant~season, scales = "free")

# 条形图：时间变化
# 周几分图：一天内的变化
ggplot(air_hour_smry$weekday) + 
  geom_line(aes(hour, flux, color = factor(weekday))) + 
  facet_wrap(~pollutant, scales = "free")
# 视觉上没什么差异

# 月份分图：一天内的变化
ggplot(air_hour_smry$month) + 
  geom_line(aes(hour, flux, color = factor(month))) + 
  facet_wrap(~pollutant, scales = "free")

# 季节分图：一天内的变化
ggplot(air_hour_smry$season) + 
  geom_line(aes(hour, flux, color = factor(season))) + 
  facet_wrap(~pollutant, scales = "free")

### Daily data ----
# 周几分图
ggplot(air_day) + 
  geom_boxplot(aes(factor(weekday), flux)) + 
  facet_wrap(~pollutant, scales = "free")
# 除了NO2无明显差别

# 月份分图
ggplot(air_day) + 
  geom_boxplot(aes(factor(month), flux)) + 
  facet_wrap(~pollutant, scales = "free")

# 季节分图
ggplot(air_day) + 
  geom_boxplot(aes(factor(season), flux)) + 
  facet_wrap(~pollutant, scales = "free")

### Monthly data ----
ggplot(air_month) + 
  geom_col(aes(month, flux)) + facet_wrap(~pollutant, scales = "free")
