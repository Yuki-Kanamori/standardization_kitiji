require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

dir1 = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata" 
# area ----------------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata/area")
path = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata/area"
files = list.files(path)

area = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i])) %>% filter(和名 == "キチジ") %>% mutate(file = paste0(files[i])) %>% mutate(year = as.numeric(str_sub(file, 5, 8)))
  area = rbind(area, data)
}
summary(area)
unique(area$和名)
colnames(area)
area = area[, c(-1:-3, -51:-166)]
colnames(area)
summary(area)
area = area %>% select(-"測定", -"集計単位", -"集計単位名称")
area = area %>% gather(key = length, value = N, 10:44)
n = area %>% mutate(length2 = as.numeric(str_sub(length, 3, 4)), N2 = N/漁獲効率)
summary(n)
n = n %>% filter(length2 > 3)
