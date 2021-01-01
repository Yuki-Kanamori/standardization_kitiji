

# raw data ------------------------------------------------------
vast_output_dirname = "/Users/Yuki/Dropbox/sokouo1/ws/vast2021-01-01_dens_lognorm100"
setwd(dir = vast_output_dirname)
DG = read.csv("Data_Geostat.csv")

DG = DG %>% mutate(NS = ifelse(DG$Lat > 38.50, "N", "S"))
unique(DG$NS)
summary(DG)
d2 = ddply(DG %>% select(Year, depth, NS, Catch_KG), .(Year, depth, NS), summarize, mean_d = mean(Catch_KG))

dirname = "/Users/Yuki/Dropbox/sokouo1/ws"
setwd(dir = dirname)
A = read.csv("Adata.csv") %>% select(-memo)
d2 = left_join(d2, A, by = c("depth", "NS")) %>% mutate(extentN = A*mean_d)
yearN2 = ddply(d2, .(Year), summarize, N = sum(extentN))
plot(x = yearN2$Year, y = yearN2$N, type = "b")


# 年齢分解 ----------------------------------------------------------
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

al_sum = al %>% group_by(year) %>% summarize(totalN = sum(number))
al = left_join(al, al_sum, by = "year") %>% mutate(freq = number/totalN)
yearN2 = yearN2 %>% dplyr::rename(year = Year)
yearN = left_join(yearN, al, by = "year") %>% mutate(decompN = N*freq)