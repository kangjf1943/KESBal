# KESBal

A project aims to evaluate the balance between the demand and supply of key ecosystem services in each ward of Kyoto City. 

## Codebook for Data

### File "GRawData"

The raw data for QGIS. 

**japan_dem_wgs84":**
DEM data of Japan, downloaded from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28762, the file "japan_dem_wgs84.zip". 
It is used to extract the elevation data of each ward of Kyoto city, which is required for adding new location into i-Tree database. 
In the download terms, a citation of the database is required if any article based on this research is published. 
Just for the record, there is another interactive online GIS showing the elevation data of Japan (https://zh-cn.topographic-map.com/maps/eh0b/%E4%BA%AC%E9%83%BD%E5%B8%82/). 

### File "RRawData"

**Kyoto_hourly_precipitation_2019.csv:**
Hourly precipitation of Kyoto in 2019, download from: https://www.data.jma.go.jp/gmd/risk/obsdl/index.php. When downloading data, choose "京都" as target place, "時別値" and "降水量（前1時間）" as target data, "2019.01.01-2019.12.31" as target duration. 
As for "表示オプションを選ぶ", here are the recommendations: 
for "利用上注意が必要なデータの扱い > 統計値を求める際、統計のもととなる資料に大幅な(必要な資料の20%を超える)欠損があるため利用上注意が必要なデータは", choose "値を表示(格納)する。ただし利用上注意が必要なことを示す情報をつける"; 
for "観測環境などの変化の前後で、値が不均質となったデータの扱い > 観測場所の移転、環境の変化、観測方法の変更などにより、その前後で値が不均質となった場合", choose "観測環境などの変化にかかわらず、すべての期間の値を表示(格納)する。ただしデータの不均質を示す情報をつける"; 
for "ダウンロードCSVファイルのデータ仕様", choose "すべて数値で格納(現象あり・なし情報、品質情報は数値で格納)" and "日付リテラルで格納"; 
for "その他", leave all the options blank. 
Then, click "CSVファイルをダウンロード". 

**Kyoto_population_2019.xlsx:** 
The original file name was "Each_Age_2019.xlsx". It is downloaed from: https://www2.city.kyoto.lg.jp/sogo/toukei/Population/Suikei/ > “３　推計人口 年齢別データ” > “令和元年(2019年)10月（xlsx）”. 
Rename it to "Kyoto_population_2019.xlsx". 
It is used for adding new location into i-Tree database. 

**Kyoto_temperature.csv:**
Downloaded from Japan Meteorological Agency (download link: https://www.data.jma.go.jp/risk/obsdl/index.php). On the user interface, choose "京都" for "地点を選ぶ", in the following page, choose target stations "京都". Then choose "日別値" and "日最低気温" under "気温" column for "項目を選ぶ", and January 1st, 2000 to December 31st, 2019 for "期間を選ぶ". 

**Rain_2019.xlsx:**
Template for precipitation data import, download from i-Tree database page. 
