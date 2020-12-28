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
catch_kiti = catch %>% filter(和名 == "キチジ") %>% mutate(tag = paste(year, STATIONコード, 水深, 網次, sep = "_"))

# catch_kiti = catch %>% filter(和名 == "キチジ") %>% mutate(tag = paste(STATIONコード, 水深,  sep = "_"))
# colnames(catch_kiti)
# kiti = catch_kiti %>% group_by(year, tag) %>% summarize(catchN = sum(漁獲尾数))
# # %>% mutate(station = str_sub(tag, 1, 1), depth = str_sub(tag, 3, 5))



# area ----------------------------------------------------------
dir = "/Users/Yuki/Dropbox/sokouo1/ws/area"
setwd(dir)
path = dir
files = list.files(path)

area = NULL
for(i in 1:length(files)){
  data = read.xlsx(paste0(files[i]), sheet = 1) %>% select(STATIONコード, 水深, 網次, 巻上開始時緯度, 巻上開始時緯度分, 巻上開始時経度, 巻上開始時経度分, 曳網面積) %>% mutate(year = as.numeric(str_sub(files[i], -9, -6)))
  
  area = rbind(area, data)
}
summary(area)
area = area %>% mutate(tag = paste(year, STATIONコード, 水深, 網次, sep = "_"))

kiti = left_join(catch_kiti, area %>% select(-year, -STATIONコード, -水深, -網次), by = c("tag"))
kiti = kiti %>% mutate(lon = 巻上開始時緯度+巻上開始時緯度分/60, lat = 巻上開始時経度+巻上開始時経度分/60) %>% select(-巻上開始時緯度, -巻上開始時緯度分, -巻上開始時経度, -巻上開始時経度分) %>% mutate(d = 漁獲尾数/曳網面積)

summary(kiti)
check = kiti 
check[is.na(check)] = -100
summary(check)
check2 = check %>% filter(lon == -100)




setwd(dir1)
write.csv(kiti, "kiti.csv", fileEncoding = "CP932")
