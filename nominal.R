require(tidyverse)
require(plyr)
require(openxlsx)

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
al = NULL
for(i in 1:25){
  data = read.xlsx("agelength2.xlsx", sheet = i)
  data[is.na(data)] = 0
  data = data %>% gather(key = age, value =number , 2:ncol(data)) %>% dplyr::rename(length_cm = "体長(cm)") %>% mutate(year = as.numeric(paste0(1995+(i-1))))
  al = rbind(al, data)
}
summary(al)

# al_sum = al %>% group_by(year) %>% summarize(totalN = sum(number))
al_sum = ddply(al, .(year), summarize, totalN = sum(number))
head(al)
head(al_sum)
al = left_join(al, al_sum, by = "year") %>% mutate(freq = number/totalN)
yearN2 = yearN2 %>% dplyr::rename(year =Year)
yearN2 = left_join(yearN2, al, by = "year") %>% mutate(decompN = N*freq)


# stock abundance -----------------------------------------------
unique(yearN2$age)
# trawl = yearN2 %>% group_by(year, age) %>% summarize(number = sum(decompN)) %>% filter(age != "0")
trawl = ddply(yearN2, .(year, age), summarize, number = sum(decompN))
trawl = trawl %>% filter(age != 0)
summary(trawl)

summary(al)
length = al %>% mutate(sum_length = (length_cm + 0.5)*number)
require(plyr)
s_length_age = ddply(length, .(year, age), summarize, sum_l = sum(sum_length))
s_number_age = ddply(length, .(year, age), summarize, sum_n = sum(number))

mean_length_weight_at_age = left_join(s_length_age, s_number_age, by = c("year", "age")) %>% 
  mutate(mean_cm = sum_l/sum_n) %>% 
  select(year, age, mean_cm) %>% 
  mutate(mean_mm = mean_cm*10) %>% 
  mutate(weight = (1.86739*10^(-5))*(mean_mm^3.06825547)) 

length = mean_length_weight_at_age %>% select(year, age, mean_mm)
unique(length$age)
length = length %>% mutate(age2 = ifelse(length$age == "5+", 5, ifelse(length$age == "9+", 9, ifelse(length$age == "10+", 10, as.numeric(length$age))))) #with warning
summary(length) #without NA in age2
length = length %>% select(-age) %>% dplyr::rename(age = age2)

setwd(dir = dirname)
catchF = read.csv("catchF.csv") %>% select(-X)

unique(trawl$age)
trawl = trawl %>% mutate(age2 = ifelse(trawl$age == "5+", 5, ifelse(trawl$age == "9+", 9, ifelse(trawl$age == "10+", 10, as.numeric(trawl$age)))))
summary(trawl)
trawl = trawl %>% select(-age) %>% dplyr::rename(age = age2)


### survival rate at age
survival = NULL
for(i in min(trawl$year):(max(trawl$year)-1)){
  # i = min(trawl$year)
  data_lastyr = trawl %>% filter(year == i)
  data_thisyr = trawl %>% filter(year == (i+1))
  data = left_join(data_lastyr, data_thisyr, by = 'age') %>% arrange(age)
  surv = matrix(NA, ncol = 1, nrow = 9)
  
  if(i < 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i == 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = (data$number.y[(j)]+data$number.y[(j+1)]+data$number.y[(j+2)]+data$number.y[(j+3)]+data$number.y[(j+4)]+data$number.y[(j+5)])/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i > 2006){
    if(i > 2013){
      for(j in 2:10){
        if(j < 10){
          surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
        }else{
          surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
        }
      }
    }else{
      for(j in 2:9){
        if(j < 10){
          surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
        }else{
          surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
        }
      }
    }
  }
  survival = rbind(survival, surv)
}
survival = data.frame(surv = survival, year = rep(1996:2019, each = 9), age = rep(2:10))

### selectivity at age
a = 1524.581
b = 0.082366
c = 0.738107

q = NULL
for(i in min(length$year):max(length$year)){
  # i = max(length$year)-1
  data = length %>% filter(age > 1, year == i) %>% arrange(age)
  temp_q = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_q[j, 1] = c/{1+a*exp(-b*data$mean_mm[j])}
  }
  temp_q2 = data.frame(q = temp_q[,1], year = mean(data$year), age = 2:10)
  q = rbind(q, temp_q2)
}  
summary(q)


### weight at age
weight = NULL
for(i in min(length$year):max(length$year)){
  # i = min(length$year)
  data = length %>% filter(age > 1, year == i) %>% arrange(age)
  temp_w = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_w[j, 1] = (1.86739*10^(-5))*data$mean_mm[j]^(3.06825547)
  }
  temp_w2 = data.frame(weight = temp_w[,1], year = mean(data$year), age = 2:10)
  weight = rbind(weight, temp_w2)
}
summary(weight)


### number at age when selectivity changes at age
abund_oct_sel = NULL
for(i in min(trawl$year):max(trawl$year)){
  # i = min(trawl$year)
  data_trawl = trawl %>% filter(year == i)
  data_q = q %>% filter(year == i)
  data_weight = weight %>% filter(year == i)
  data = left_join(data_trawl, data_q, by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  data = left_join(data, data_weight,  by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  
  temp_naa_sel = matrix(NA, ncol = 1, nrow = 9)
  temp_baa_sel = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    #j = 9
    temp_naa_sel[j, 1] = data$number[j]/data$q[j]
  }
  
  for(k in 1:9){
    temp_baa_sel[k, 1] = temp_naa_sel[k, 1]*data$weight[k]*(0.001)^2
  }
  
  temp_abund_oct = data.frame(number_sel = temp_naa_sel[, 1], biomass_sel = temp_baa_sel[, 1], year = mean(data$year), age = 2:10)
  abund_oct_sel = rbind(abund_oct_sel, temp_abund_oct)
}


### fishing rate, F, Z, and survival rate within 2 month
M = 2.5/20 #fixed
abund_jan_forF_notneeded = NULL
fishing_rate = NULL
Z = NULL
survival_2month = NULL

for(i in (min(abund_oct_sel$year)+1):max(abund_oct_sel$year)){
  # i = min(abund_oct_sel$year)+1
  data_oct_sel_last = abund_oct_sel %>% filter(year == (i-1)) %>% na.omit()
  data_catchF_last = catchF %>% filter(year == (i-1))
  data_catchF_this = catchF %>% filter(year == i)
  
  temp_abund_jan = sum(data_oct_sel_last$biomass_sel)*exp(-2/12*0.125)-data_catchF_last$catch/6*exp(-2/12*0.125)
  temp_fishing_rate = data_catchF_this$catch/temp_abund_jan
  temp_f = -log(1-(temp_fishing_rate/exp(-M/2)))
  temp_Z = temp_f + M
  temp_survival_2month = exp(-temp_Z/6)
  
  abund_jan_forF_notneeded_pre = data.frame(biomass = temp_abund_jan, year = i)
  fishing_rate_pre = data.frame(f = temp_f, year = i)
  Z_pre = data.frame(z = temp_Z, year = i)
  survival_2month_pre = data.frame(surv = temp_survival_2month, year = i)
  
  abund_jan_forF_notneeded = rbind(abund_jan_forF_notneeded, abund_jan_forF_notneeded_pre)
  fishing_rate = rbind(fishing_rate, fishing_rate_pre)
  Z = rbind(Z, Z_pre)
  survival_2month = rbind(survival_2month, survival_2month_pre)
}


### abundance in January
est = NULL
for(i in (min(abund_oct_sel$year)+1):(max(abund_oct_sel$year)+1)){
  # i = max(abund_oct_sel$year) #1995
  
  if(i < max(abund_oct_sel$year)+1){
    data_survival = survival_2month %>% filter(year == i)
    data_abund_oct_sel = abund_oct_sel %>% filter(year == (i-1)) %>% arrange(age)
    data_weight = weight %>% filter(year == (i-1)) %>% arrange(age)
    
    temp_number = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    temp_biomass = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    
    for(j in 1:nrow(data_abund_oct_sel)){
      temp_number[j, 1] = data_survival$surv*data_abund_oct_sel$number_sel[j]
    }
    
    for(k in 1:nrow(data_abund_oct_sel)){
      temp_biomass[k, 1] = temp_number[k, 1]*data_weight$weight[k]*(0.001)^2
    }
    
    temp_est = data.frame(number = temp_number[, 1], biomass = temp_biomass[, 1], year = i, age = 2:10)
    est = rbind(est, temp_est)
  }
  
  if(i == max(abund_oct_sel$year)+1){
    data_survival = survival_2month %>% filter(year == (i-1))
    data_abund_oct_sel = abund_oct_sel %>% filter(year == (i-1)) %>% arrange(age)
    data_weight = weight %>% filter(year == (i-1)) %>% arrange(age)
    
    temp_number = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    temp_biomass = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    
    for(j in 1:nrow(data_abund_oct_sel)){
      temp_number[j, 1] = data_survival$surv*data_abund_oct_sel$number_sel[j]
    }
    
    for(k in 1:nrow(data_abund_oct_sel)){
      temp_biomass[k, 1] = temp_number[k, 1]*data_weight$weight[k]*(0.001)^2
    }
    
    temp_est = data.frame(number = temp_number[, 1], biomass = temp_biomass[, 1], year = i, age = 2:10)
    est = rbind(est, temp_est)
  }
}

### catch rate
trend = est %>% select(year, biomass) %>% na.omit() %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(biomass))
catch_rate = left_join(catchF, trend, by = "year") %>% mutate(rate = catch/total*100)
fishing_trend = left_join(catch_rate, fishing_rate, by = "year") %>% select(year, rate, f) %>% gather(key = data, value = value, 2:3) %>% mutate(data2 = ifelse(data == "f", "F値", "漁獲割合"))

mean = fishing_trend %>% filter(data == "rate") %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-1)-3)) %>% select(value)
(mean_fishing_trend = mean(mean$value))


### figures 
### year trend of stock biomass (fig. 10)
trend = est %>% select(year, biomass) %>% na.omit() %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(biomass))
low = (max(trend$total)-min(trend$total))*1/3+min(trend$total)
high = max(trend$total)-(max(trend$total)-min(trend$total))*1/3

g = ggplot(trend, aes(x = year, y = total/1000))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "資源量（トン）", shape = "")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
level_l = geom_hline(yintercept = low/1000, linetype = "dashed", color = "gray50")
level_h = geom_hline(yintercept = high/1000, linetype = "dashed", color = "gray50")
fig10 = g+p+l+lab+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0.03, 0.03))+level_l+level_h
ggsave(file = "fig10_nominal.png", plot = fig10, units = "in", width = 11.69, height = 8.27)



