require(xlsx)
require(openxlsx)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(investr)
require(stringr)
require(abind)
require(gridExtra)
require(ggrepel)


# ---------------------------------------------------------------
# previous N ----------------------------------------------------
# ---------------------------------------------------------------
# 
# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")


# ---------------------------------------------------------------
olddata = read.csv("olddata_trawl.csv") 

# combine the catch data from the trawl surveys
old_trawl = olddata %>% filter(data == 'trawl') %>% gather(key = year_tag, value = number, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_trawl)

naa = read.csv("number_at_age.csv")
naa = naa[1:(nrow(naa)-1), 3:ncol(naa)]
naa = apply(naa, 1, sum)
naa = naa %>% data.frame() %>% mutate(age = 0:10) %>% filter(age != 0)
colnames(naa) = c('number', 'age')
naa$year = 2019
summary(naa)
trawl = rbind(old_trawl, naa)
summary(trawl)

temp = ddply(trawl %>% na.omit(), .(year), summarize, totalN = sum(number)) %>% mutate(type = "Nominal")
plot(x = temp$year, y = temp$totalN, type = "b")


# ---------------------------------------------------------------
# VAST N ----------------------------------------------------
# ---------------------------------------------------------------

vast_output_dirname = "/Users/Yuki/Dropbox/sokouo1/ws/vast2021-01-01_dens_lognorm100"
setwd(dir = vast_output_dirname)

# 0.2 load the data -------------------------------------------------
load("Save.RData")
DG = read.csv("Data_Geostat.csv")

category_name = c("kitiji") #カテゴリーの名前（魚種名や銘柄など）

# 2. get dens --------------------------------------------------
# make a data-frame
df_dens = ggvast::get_dens(category_name = category_name)

# 引き延ばし ---------------------------------------------------------
DG2 = DG %>% mutate(tag = paste(Lon, Lat, sep = "_"))
tag = DG2 %>% select(tag, depth, station, knot_i)

df_dens2 = df_dens %>% mutate(tag = paste(lon, lat, sep = "_")) %>% select(-category_name)

check = left_join(df_dens2, tag, by = "tag") #129250
summary(check)

check = check %>% select(-tag) %>% mutate(tag = paste(station, depth, sep = "_"))
setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata")
A = read.csv("area_A.csv") %>% select(-X, -station, -depth) #113
check = left_join(check, A, by = "tag") %>% mutate(extentN = area_A*exp(log_abundance)) #129250
check = check %>% mutate(NS = ifelse(check$lat > 38.50, "N", "S"))

unique(check$station)
remove = c("A", "B", "C", "D", "E", "F", "G", "H")
check2 = check %>% filter(station %in% remove)
unique(check2$station)

d = check %>% dplyr::group_by(year, depth, NS) %>% dplyr::summarize(mean_d = mean(extentN))
yearN = d %>% dplyr::group_by(year) %>% dplyr::summarize(N = sum(mean_d))
plot(x = yearN$year, y = yearN$N, type = "b")


# 年齢分解 ----------------------------------------------------------
dirname = "/Users/Yuki/Dropbox/sokouo1/ws"
setwd(dir = dirname)
require(openxlsx)
al = NULL
for(i in 1:25){
  data = read.xlsx("agelength2.xlsx", sheet = i)
  data[is.na(data)] = 0
  data = data %>% gather(key = age, value =number , 2:ncol(data)) %>% dplyr::rename(length_cm = "体長(cm)") %>% mutate(year = as.numeric(paste0(1995+(i-1))))
  al = rbind(al, data)
}
summary(al)

al_sum = al %>% dplyr::group_by(year) %>% dplyr::summarize(totalN = sum(number))
al = left_join(al, al_sum, by = "year") %>% mutate(freq = number/totalN)
yearN = left_join(yearN, al, by = "year") %>% mutate(decompN = N*freq)

temp2 = yearN %>% dplyr::filter(age != "0") %>% dplyr::group_by(year) %>% dplyr::summarize(totalN = sum(decompN)) %>% mutate(type = "Standardized")
plot(x = temp2$year, y = temp2$totalN, type = "b")



# figure -----------------------------------------------------------
number = rbind(temp, temp2)
levels(number$type)
number$type = factor(number$type, levels = c("Nominal", "Standardized"))

g = ggplot(number, aes(x = year, y = totalN/1000000, colour = type))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
#col_type = c("black", "hotpink1")
lab = labs(x = "年", y = "資源尾数（百万尾）", colour = "")
th = theme(panel.grid.major = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_text(size = rel(1.8)),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
c = scale_colour_manual(values =  c("black", "tomato1"))
trend_n = g+p+l+c+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2019, by = 3), expand = c(0, 0.5))
ggsave(file = "trend_number.png", plot = trend_n, units = "in", width = 11.69, height = 8.27)
