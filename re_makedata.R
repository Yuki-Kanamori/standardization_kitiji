require(tidyverse)
require(openxlsx)
require(maps)
require(mapdata)
require(ggplot2)

dir1 = "/Users/Yuki/Dropbox/sokouo1/ws" 
# catch ---------------------------------------------------------
dir = "/Users/Yuki/Dropbox/sokouo1/ws/catchN"
setwd(dir)
path = dir
files = list.files(path, pattern = ".xlsx")

catch = NULL
for(i in 1:length(files)){
  data = read.xlsx(paste0(files[i]), sheet = 1) %>% select(STATIONコード, 水深, 網次, 和名, 漁獲量, 漁獲尾数) %>% mutate(year = as.numeric(str_sub(files[i], 4, 7)))

  # if(colnames(data) %in% "調査種類"){
  #   data = data %>% select(-"調査種類")
  # }
  
  catch = rbind(catch, data)
}
summary(catch)
catch_kiti = catch %>% filter(和名 == "キチジ")



# area ----------------------------------------------------------
dir = "/Users/Yuki/Dropbox/sokouo1/ws/area"
setwd(dir)
path = dir
files = list.files(path)

area = NULL
for(i in 1:length(files)){
  data = read.xlsx(paste0(files[i]), sheet = 1) %>% select(STATIONコード, 水深, 網次, 巻上開始時緯度, 巻上開始時緯度分, 巻上開始時経度, 巻上開始時経度分, 曳網面積) %>% mutate(year = as.numeric(str_sub(files[i], -8, -5)))
  
  area = rbind(area, data)
}
summary(area)
# area$tag = paste(STATIONコード, 水深，網次, sep = "_")
area = area %>% mutate(tag = paste(STATIONコード, 水深, 網次, sep = "_"))
