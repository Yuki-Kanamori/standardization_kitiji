require(tidyverse)
require(ggplot2)
require()

vast_output_dirname = "/Users/Yuki/Dropbox/sokouo1/ws/vast2021-01-01_dens_lognorm100"
setwd(dir = vast_output_dirname)
DG = read.csv("Data_Geostat.csv")

# map -----------------------------------------------------------
# 調査を行なった場所
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 36 & jap$lat < 41.5 & jap$long > 140.6 & jap$long < 143, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
# + coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
th = theme(axis.text.x = element_text(angle = 90))
t2 + geom_point(data = DG, aes(x = Lon, y = Lat), shape = 16, size = 1)+facet_wrap(~Year, ncol = 13) + theme_bw() + th
# t2 + geom_point(data = rec2, aes(x = lon, y = lat, colour = station), shape = 16, size = 1)+facet_wrap(~year, ncol = 8) + theme_bw()

# catch N (nominal)
t2 + geom_point(data = DG %>% na.omit(), aes(x = Lon, y = Lat, colour = log(Catch_KG)), shape = 16, size = 1)+facet_wrap(~Year, ncol = 13) + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+ theme_bw() +th +labs(colour = "log (N)")

# catch N (nominal) catch > 0
# DG2 = DG %>% filter(Catch_KG > 0)
# t2 + geom_point(data = DG2 %>% na.omit(), aes(x = Lon, y = Lat, colour = log(Catch_KG)), shape = 16, size = 1)+facet_wrap(~Year, ncol = 13) + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+ theme_bw() + th + labs(colour = "log (catch N)")

