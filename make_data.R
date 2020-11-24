require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

# catch ---------------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata/catch_raw")
path = "/Users/Yuki/Dropbox/sokouo1/make_VASTdata/catch_raw"
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
colnames(area) = c("sokutei", "gyokaku_koritu", "nanboku", "syukei_tanni", "syukei_tanni_mesyo", "area_A", "station", "depth", "ami", "swept_area", "raw_N", "file", "year")
summary(area) #NA無し
write.csv(area, "area.csv", fileEncoding = "CP932")


# bind ----------------------------------------------------------
catch2 = catch %>% select(-file) %>% mutate(tag = paste(year, station, depth, ami, sep = "_"))
area2 = area %>% select(-file) %>% mutate(tag = paste(year, station, depth, ami, sep = "_")) %>% select(-year, -station, -depth, -ami)

length(unique(catch2$tag))
length(unique(area2$tag))

tag = merge

catch2 = merge(catch2, area2, by = "tag", all = T)
summary(catch2)
catch2$raw_N.x-catch2$raw_N.y #catchに入っている尾数とareaに入っている尾数のデータが合わない！！！

setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata/catch_raw")
write.csv(catch2, "catch_area.csv", fileEncoding = "CP932")



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
write.csv(rec, "lonlat_rec.csv", fileEncoding = "CP932")

rec2 = rec %>% mutate(tag = paste(year, station, depth, sep = "_")) %>% select(-depth, -ami, -file)
summary(rec2)

# mean density --------------------------------------------------
dens = catch2
dens$dens = dens$raw_catch_kg/dens$swept_area
summary(dens)
dens = dens %>% group_by(tag) %>% summarise(dens = sum(dens))
dens = dens %>% mutate(year = as.numeric(str_sub(tag, 1, 4)), station = str_sub(tag, 6, 6), depth = as.numeric(str_sub(tag, 8,10)), ami = as.numeric(str_sub(tag, 12, 12)))

dens2 = dens %>% group_by(year, station, depth) %>% summarise(mean_dens = mean(dens)) %>% mutate(tag = paste(year, station, depth, sep = "_")) 
dens2$year = NULL
dens2$station = NULL
dens2 = merge(dens2, rec2, by = "tag", all = T)
summary(dens2)

write.csv(dens2, "dens_lonlat.csv", fileEncoding = "CP932")



# map -----------------------------------------------------------
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 36 & jap$lat < 41.5 & jap$long > 140.6 & jap$long < 143, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
# + coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
t2 + geom_point(data = rec2, aes(x = lon, y = lat), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + theme_bw()
# t2 + geom_point(data = rec2, aes(x = lon, y = lat, colour = station), shape = 16, size = 1)+facet_wrap(~year, ncol = 8) + theme_bw()


dens3 = dens2 %>% na.omit()
t2 + geom_point(data = dens3, aes(x = lon, y = lat, colour = mean_dens), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+ theme_bw()

# catch only
t2 + geom_point(data = dens3 %>% filter(mean_dens > 0), aes(x = lon, y = lat, colour = mean_dens), shape = 16, size = 1)+facet_wrap(~year, ncol = 12) + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+ theme_bw()