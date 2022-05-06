# Statement ----
# The code is used to get forest cove of each ward

# Package ----
library(dplyr)

# read data
forest.cover <- read.csv("GProcData/L03-b-16_5235.csv") %>% 
  # keep the data within Kyoto city
  select(土地利用種, SIKUCHOSON) %>% 
  subset(SIKUCHOSON != "") %>%
  # calculate forest cover rate of each ward
  group_by(SIKUCHOSON, 土地利用種) %>% 
  summarise(n = n()) %>% 
  mutate(prop_n = n/sum(n)) %>% 
  ungroup() %>% 
  # keep the proportion of forest
  subset(土地利用種 == 500)
