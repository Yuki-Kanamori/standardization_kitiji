require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

dir1 = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata" 
# catch ---------------------------------------------------------
dir = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata/catch_raw"
setwd(dir)
path = dir
files = list.files(path)

catch = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i])) %>% filter(和名 == "キチジ") %>% mutate(file = paste0(files[i])) %>% mutate(year = as.numeric(str_sub(file, 6, 9)))
  catch = rbind(catch, data)
}
summary(catch) #NA無し
catch = catch[, c(-1, -6)]
colnames(catch)
colnames(catch) = c("station", "depth", "ami", "in_out", "species", "raw_catch_kg", "raw_N", "file", "year")

setwd(dir1)
write.csv(catch, "raw_catch.csv", fileEncoding = "CP932")



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
area = area[, c(-1:-3, -15:-166)]
colnames(area)
colnames(area) = c("sokutei", "gyokaku_koritu", "nanboku", "syukei_tanni", "syukei_tanni_meisyo", "area_A", "station", "depth", "ami", "swept_area", "raw_N", "file", "year")
summary(area) #NA無し

setwd(dir1)
write.csv(area, "area.csv", fileEncoding = "CP932")


# bind ----------------------------------------------------------
catch2 = catch %>% select(-file) %>% mutate(tag = paste(year, station, depth, ami, sep = "_"))
catch3 = catch2 %>% group_by(tag) %>% summarize(raw_catch_kg = sum(raw_catch_kg))
catch3 = catch3 %>% mutate(year = as.numeric(str_sub(tag, 1, 4)), station = str_sub(tag, 6, 6), depth = as.numeric(str_sub(tag, 8,10)), ami = as.numeric(str_sub(tag, 12, 12)))

area2 = area %>% select(-file) %>% mutate(tag = paste(year, station, depth, ami, sep = "_")) %>% select(-year, -station, -depth, -ami)

#catchとareaの位置情報の数が合わない
length(unique(catch3$tag))
length(unique(area2$tag))

c_tag = data_frame(tag = unique(catch3$tag), data_c = 1)
a_tag = data_frame(tag = unique(area2$tag), data_a = 1)
check = merge(c_tag, a_tag, by = "tag", all = T)
# =>2019年のデータを確認する必要がある！

catch3 = merge(catch3, area2, by = "tag", all = T)
summary(catch3)
catch2$raw_N-area2$raw_N #catchに入っている尾数とareaに入っている尾数のデータが合わない！！！

setwd(dir1)
write.csv(catch3, "catch_area.csv", fileEncoding = "CP932")



# record --------------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata/record")
path = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata/record"
files = list.files(path)

rec = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i])) %>% mutate(file = paste0(files[i])) %>% mutate(year = as.numeric(str_sub(file, 7, 10)))
  rec = rbind(rec, data)
}
colnames(rec)
rec = rec[, c(2:4,9:12,22:23)]
colnames(rec)
colnames(rec) = c("station", "depth", "ami", "la1", "la2", "lo1", "lo2", "file", "year")
rec = rec %>% mutate(lon = lo1+lo2/60, lat = la1+la2/60) %>% select(-la1, -la2, -lo1, -lo2, file)

setwd(dir1)
write.csv(rec, "lonlat_rec.csv", fileEncoding = "CP932")

rec2 = rec %>% mutate(tag = paste(year, station, depth, ami, sep = "_")) %>% select(-depth, -ami, -file)
summary(rec2)


# bind ----------------------------------------------------------
head(catch3,3)
head(rec2,3)

catch4 = merge(catch3, rec2 %>% select(-station, -year), by = "tag")

setwd(dir1)
write.csv(catch4, "catch4.csv", fileEncoding = "CP932")

# # mean density --------------------------------------------------
# dens = catch3
# dens$dens = dens$catch_kg/dens$swept_area
# summary(dens)
# 
# # 網次の扱いが分からない=>平均をとっておく
# dens2 = dens %>% group_by(year, station, depth) %>% summarise(mean_dens = mean(dens)) %>% mutate(tag = paste(year, station, depth, sep = "_")) 
# dens2$year = NULL
# dens2$station = NULL
# 
# # 網次の分だけ緯度経度がダブっているので，平均をとる
# rec3 = rec2 %>% group_by(tag) %>% summarize(lon = mean(lon), lat = mean(lat)) %>% mutate(year = as.numeric(str_sub(tag, 1, 4)), station = str_sub(tag, 6, 6), depth = as.numeric(str_sub(tag, 8, 10))) 
# 
# dens2 = merge(dens2, rec3 %>% select(-depth), by = "tag", all = T)
# summary(dens2)
# 
# setwd(dir1)
# write.csv(dens2, "dens_lonlat.csv", fileEncoding = "CP932")



# # 網次の扱い -------------------------------------------------------------
# # 網次の扱いが分からない=>平均をとっておく
# catch3 = catch3 %>% group_by(year, station, depth) %>% summarise(mean_raw_catch_kg = mean(raw_catch_kg)) %>% mutate(tag = paste(year, station, depth, sep = "_"))
# catch3$year = NULL
# catch3$station = NULL
# 
# # 網次の分だけ緯度経度がダブっているので，平均をとる
# rec3 = rec2 %>% group_by(tag) %>% summarize(lon = mean(lon), lat = mean(lat)) %>% mutate(year = as.numeric(str_sub(tag, 1, 4)), station = str_sub(tag, 6, 6), depth = as.numeric(str_sub(tag, 8, 10))) 
# 
# catch3 = merge(catch3, rec3 %>% select(-depth), by = "tag", all = T)
# summary(catch3)
# 
# setwd(dir1)
# write.csv(dens2, "dens_lonlat.csv", fileEncoding = "CP932")



# add the area in each station and depth ------------------------
dens2 = dens2 %>% select(-tag) %>% mutate(tag = paste(station, depth, sep = "_"))
area3 = area %>% mutate(tag = paste(station, depth, sep = "_")) %>% select(station, depth, tag, area_A)
area_A = area3 %>% distinct(tag, .keep_all = T)

setwd(dir1)
write.csv(area_A, "area_A.csv", fileEncoding = "CP932")

# Aが変だから，2019年のデータを使う
setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata/area")
a2019 = read.csv("area2019.csv")
a2019 = a2019[, c(-1:-3, -15:-166)]
colnames(a2019)
a2019 = a2019 %>% select(面積.Km2., STATIONコード, 水深) %>% dplyr::rename(area_A = 面積.Km2., station = STATIONコード, depth = 水深) %>% na.omit() %>% mutate(tag = paste(station, depth, sep = "_")) %>% distinct(tag, .keep_all = T)
setwd(dir1)
write.csv(a2019, "area_A2019.csv", fileEncoding = "CP932")

st_dep = data_frame(tag = paste(dens2$station, dens2$depth, sep = "_")) %>% distinct(tag, .keep_all = T)
length(unique(st_dep$tag))


# bind(A~Hのみ)
setwd(dir1)
r_area = read.csv("area_revised.csv") %>% mutate(tag = paste(station, depth, sep = "_"))
head(dens2)
dens3 = dens2 %>% select(-tag) %>% mutate(tag = paste(station, depth, sep = "_"))
head(dens3)
head(r_area)

data = inner_join(dens3, r_area, by = "tag")
unique(data$station.x)
unique(data$depth.x)
summary(data)

setwd(dir1)
write.csv(data, "data_vast.csv", fileEncoding = "CP932")

# map -----------------------------------------------------------
# 調査を行なった場所
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 36 & jap$lat < 41.5 & jap$long > 140.6 & jap$long < 143, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
# + coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
t2 + geom_point(data = rec3, aes(x = lon, y = lat), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + theme_bw()
t2 + geom_point(data = data, aes(x = lon, y = lat, colour = factor(depth.x)), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + theme_bw()
# t2 + geom_point(data = rec2, aes(x = lon, y = lat, colour = station), shape = 16, size = 1)+facet_wrap(~year, ncol = 8) + theme_bw()

# 密度
t2 + geom_point(data = data %>% na.omit(), aes(x = lon, y = lat, colour = mean_dens), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+ theme_bw()

# catch only
t2 + geom_point(data = dens3 %>% filter(mean_dens > 0) %>% na.omit(), aes(x = lon, y = lat, colour = mean_dens), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+ theme_bw()
