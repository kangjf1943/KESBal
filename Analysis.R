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

# Air pollutant monitor ----
## Data import ----
### Hourly data ----
# 读取空气质量实测值
air_monitor_hour <- mdb.get(
  "RawData/ItreeCityLevel/Tree/AirPollutant.mdb", 
  tables = "AirPollutant")
names(air_monitor_hour) <- tolower(names(air_monitor_hour))
# 删除CO和PM10的数据：没有关于他们的健康影响效益估算
air_monitor_hour <- 
  air_monitor_hour[which(!air_monitor_hour$pollutant %in% c("PM10*", "CO")), ]
# 添加小时数据
air_monitor_hour$hour <- rep_len(1:24, length.out = nrow(air_monitor_hour))

# 各天内各小时的数据变化
# 计算各类污染物各小时值：取各个监测点的平均值
air_monitor_hour <- 
  aggregate(ugm3 ~ pollutant + timestamp + hour, data = air_monitor_hour, mean)

# 增加周几，月份，季节列
air_monitor_hour$weekday <- weekdays(air_monitor_hour$timestamp)
air_monitor_hour$weekday <- factor(
  air_monitor_hour$weekday, 
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
             "Saturday", "Sunday"))

air_monitor_hour$month <- month(air_monitor_hour$timestamp)

air_monitor_hour$season <- "spring"
air_monitor_hour$season[which(air_monitor_hour$month <= 6 & air_monitor_hour$month > 3)] <- "summer"
air_monitor_hour$season[which(air_monitor_hour$month <= 9 & air_monitor_hour$month > 6)] <- "autumn"
air_monitor_hour$season[which(air_monitor_hour$month <= 12 & air_monitor_hour$month > 9)] <- "winter"
air_monitor_hour$season <- 
  factor(air_monitor_hour$season, 
         levels = c("spring", "summer", "autumn", "winter"))

### Hourly summary ----
air_monitor_hour_smry <- vector("list")

air_monitor_hour_smry$weekday <- 
  aggregate(ugm3 ~ pollutant + weekday + hour, data = air_monitor_hour, median)
air_monitor_hour_smry$month <- 
  aggregate(ugm3 ~ pollutant + month + hour, data = air_monitor_hour, mean)
air_monitor_hour_smry$season <- 
  aggregate(ugm3 ~ pollutant + season + hour, data = air_monitor_hour, mean)

### Daily data ----
# 因为是浓度数据，所以将小时数据取平均成日数据
air_monitor_day <- aggregate(
  ugm3 ~ pollutant + timestamp + season + month + weekday, 
  data = air_monitor_hour, mean)

### Monthly data ----
air_monitor_month <- aggregate(
  ugm3 ~ pollutant + season + month, 
  data = air_monitor_day, mean)

## Analysis ----
### Hourly data ----
# 可视化
# 时间变化及变异：盒形图
# 一周内各天分图：一天内各小时污染物去除率变化
ggplot(air_monitor_hour) + 
  geom_boxplot(aes(factor(hour), ugm3)) + 
  facet_grid(pollutant~weekday, scales = "free")
# 待办：视觉上看没有明显差异，之后可以统计对比一下

# 各月分图：一天内各小时污染物去除率变化
ggplot(air_monitor_hour) + 
  geom_boxplot(aes(factor(hour), ugm3)) + 
  facet_grid(pollutant~month, scales = "free")

# 各季节分图：一天内各小时污染物去除率变化
ggplot(air_monitor_hour) + 
  geom_boxplot(aes(factor(hour), ugm3)) + 
  facet_grid(pollutant~season, scales = "free")

# 条形图：时间变化
# 周几分图：一天内的变化
ggplot(air_monitor_hour_smry$weekday) + 
  geom_line(aes(hour, ugm3, color = factor(weekday))) + 
  facet_wrap(~pollutant, scales = "free")
# 注意NO2的双高峰

# 月份分图：一天内的变化
ggplot(air_monitor_hour_smry$month) + 
  geom_line(aes(hour, ugm3, color = factor(month))) + 
  facet_wrap(~pollutant, scales = "free")

# 季节分图：一天内的变化
ggplot(air_monitor_hour_smry$season) + 
  geom_line(aes(hour, ugm3, color = factor(season))) + 
  facet_wrap(~pollutant, scales = "free")

### Daily data ----
# 周几分图
ggplot(air_monitor_day) + 
  geom_boxplot(aes(factor(weekday), ugm3)) + 
  facet_wrap(~pollutant, scales = "free")
# 除了NO2无明显差别

# 月份分图
ggplot(air_monitor_day) + 
  geom_boxplot(aes(factor(month), ugm3)) + 
  facet_wrap(~pollutant, scales = "free")

# 季节分图
ggplot(air_monitor_day) + 
  geom_boxplot(aes(factor(season), ugm3)) + 
  facet_wrap(~pollutant, scales = "free")

### Monthly data ----
ggplot(air_monitor_month) + 
  geom_col(aes(month, ugm3)) + facet_wrap(~pollutant, scales = "free")

# Supply and demand ----
# 空气质量标准有小时、日、年等，根据数据，结合可采取的绿色空间相关措施来看，按日计算比较合理
# 计算每天的空气污染移除需求量
# 需求量 = 实测值 + 移除量 - 标准值
# 全京都市建成区面积147448500平方米
# 树木覆盖率为0.00591%
# 树木覆盖面积为8714.206平方米
# 日本空气质量标准参考环境省数据：
# 英文版：https://www.env.go.jp/en/air/aq/aq.html
# 日文版：https://www.env.go.jp/kijun/taiki.html
# 各种气体ppm和ug/m3的转换系数则采用WHO版本系数：https://uk-air.defra.gov.uk/assets/documents/reports/cat06/0502160851_Conversion_Factors_Between_ppb_and.pdf
# 结果如下，注意有些标准并非基于每日24小时平均值：
# NO2：每日24时平均值不超过0.06ppm = 0.06*1880 = 112.8ug/m3
# O3：每小时平均值不超过0.06ppm = 0.06*1960 = 117.6ug/m3
# PM2.5：每日平均值不超过35ug/m3
# SO2：每日24时平均值不超过0.04ppm = 2620*0.04 = 104.8ug/m3
air_dem_day <- merge(air_day, air_monitor_day, 
  by = c("timestamp", "season", "month", "weekday", "pollutant")
)
air_std <- data.frame(
  pollutant = c("NO2", "O3", "PM2.5", "SO2"), 
  std_ugm3 = c(112.8, 117.6, 35, 104.8)
)
# 需求量计算：基于总重量单位计算
air_dem_day <- merge(air_dem_day, air_std, by = "pollutant")
air_dem_day$demand <- 
  # 探头实测值乘以城市面积和200米高度得到该体积内污染物总量
  air_dem_day$ugm3 * 147448500 * 200 + 
  # 绿植污染物去除量（g/m2）乘以树木面积为该体积内污染物去除总量
  air_dem_day$flux * 10^6 * 8714.206 - 
  # 标准量浓度值乘以城市面积和200米高度得到该体积内污染物总量
  air_dem_day$std_ugm3 *  147448500 * 200
# 若需求量为负数，则说明需求为0
air_dem_day$demand[which(air_dem_day$demand < 0)] <- 0

# 供给和需求之比
air_dem_day$ratio_sply_dem <- 
  air_dem_day$flux * 10^6 * 8714.206 / air_dem_day$demand
air_dem_day$ratio_sply_dem[which(air_dem_day$demand == 0)] <- NA

# 查看月份或季节的满足率
ggplot(air_dem_day) + geom_point(aes(factor(month), ratio_sply_dem)) + 
  facet_wrap(~pollutant)
ggplot(air_dem_day) + geom_point(aes(season, ratio_sply_dem)) + 
  facet_wrap(~pollutant)
# 只有一天的数据是不满足需求的？而且可能还是考虑要移除的异常值？

# 绿植污染物去除量占总污染物的比例呢？
# 待办：这个贡献率计算有问题？
air_dem_day$contrib_green <- 
  air_dem_day$flux * 10^6 * 8714.206 /
  (air_dem_day$flux * 10^6 * 8714.206 + 
     air_dem_day$ugm3 * 147448500 * 200)
ggplot(air_dem_day) + geom_boxplot(aes(season, contrib_green)) + 
  facet_wrap(~pollutant)


