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



# set directory -------------------------------------------------
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

# stock abundance -----------------------------------------------
trawl = yearN %>% dplyr::group_by(year, age) %>% dplyr::summarize(number = sum(decompN)) %>% filter(age != "0")
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


# ABC -----------------------------------------------------------
f_current = fishing_rate %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-2)-3)) %>% dplyr::summarize(mean(f))

s_current = exp(-(f_current+M))

s1_current = survival %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-2)-3), age == 2) %>% dplyr::summarize(mean(surv))

# number_1old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2, age == 1)%>% dplyr::select(number)/1000 * s1_current
number_1old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2, age == 1)
number_1old_oct_last = number_1old_oct_last$number/1000 * s1_current

number_2old_oct_last = number_1old_oct_last*s1_current
number_2old_jan_this = number_2old_oct_last*survival_2month %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2) %>% select(surv)

number_2old_jan_this_sel = number_2old_jan_this/q %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2, age == 2) %>% select(q)

# the estimated abundance in step 4
abund_abc = est %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-1)) %>% select(number, biomass, year, age) %>% dplyr::rename(number_est = number, biomass_est = biomass)
abund_abc = left_join(abund_abc, weight %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-2)), by = c("age"))

next_year = NULL
for(i in 1:(length(abund_abc$age))){
  # i=1
  if(i == 1){
    temp = number_2old_jan_this_sel
    next_year = rbind(next_year, temp)
  }
  if(i < length(abund_abc$age)){
    temp = abund_abc$number_est[i-1]*s_current/1000
    next_year = abind(next_year, temp, along = 1)
  }
  if(i == length(abund_abc$age)){
    temp = (abund_abc$number_est[i-1]+abund_abc$number_est[i])*s_current/1000
    next_year = abind(next_year, temp, along = 1)
  }
}

abund_abc = abund_abc %>% mutate(next_year_number = next_year) %>% mutate(next_year_biomass = next_year_number*weight/1000)



# forecasting ---------------------------------------------------
retro_year = 5

retro = NULL
for(i in 1:retro_year){
  # fの3年平均
  # i = 1
  f_current = fishing_rate %>% filter(year < ((as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))+1)) %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))-3)) %>% dplyr::summarize(mean(f))
  # 2021 - (2+0) - 3 = 2016 => 2017~2019
  # 2021 - (2+1) - 3 = 2015 => 2016~2019
  
  s_current = exp(-(f_current+M))
  
  s1_current = survival %>% filter(year < ((as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))+1)) %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))-3), age == 2) %>% dplyr::summarize(mean(surv))
  
  # number_1old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2, age == 1)%>% dplyr::select(number)/1000 * s1_current
  number_1old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i), age == 1)
  number_1old_oct_last = number_1old_oct_last$number/1000 * s1_current
  
  number_2old_oct_last = number_1old_oct_last*s1_current
  number_2old_jan_this = number_2old_oct_last*survival_2month %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i)) %>% select(surv)
  
  number_2old_jan_this_sel = number_2old_jan_this/q %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i), age == 2) %>% select(q)
  
  # the estimated abundance in step 4
  abund_abc = est %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-(1+i))) %>% select(number, biomass, year, age) %>% dplyr::rename(number_est = number, biomass_est = biomass)
  abund_abc = left_join(abund_abc, weight %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))), by = c("age"))
  
  next_year = NULL
  for(j in 1:(length(abund_abc$age))){
    # j=1
    if(j == 1){
      temp = number_2old_jan_this_sel
      next_year = rbind(next_year, temp)
    }
    if(j < length(abund_abc$age)){
      temp = abund_abc$number_est[j-1]*s_current/1000
      next_year = abind(next_year, temp, along = 1)
    }
    if(j == length(abund_abc$age)){
      temp = (abund_abc$number_est[j-1]+abund_abc$number_est[j])*s_current/1000
      next_year = abind(next_year, temp, along = 1)
    }
  }
  
  abund_abc = abund_abc %>% mutate(next_year_number = next_year) %>% mutate(next_year_biomass = next_year_number*weight/1000, removed = paste0(i))
  
  # i = 1
  # retro = est %>% filter(year < ((as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))+1))
  
  retro = rbind(retro, abund_abc)
  
}

# retro2 = NULL
# for(i in min(retro$year.x):max(retro$year.x)){ # 2015:2019
#   t = est %>% filter(year < (i+1)) %>% dplyr::group_by(year) %>% na.omit() %>% dplyr::summarize(number = sum(number), biomass = sum(biomass))
#   t2 = retro %>% filter(year.x == i) %>% select(next_year_number, next_year_biomass, removed)
#   t3 = t2 %>% dplyr::summarize(number = sum(next_year_number), biomass = sum(next_year_biomass)) %>% mutate(year = paste0(i+1))
#   
#   t4 = rbind(t, t3) %>% mutate(removed = paste0(min(t2$removed)))
#   retro2 = rbind(retro2, t4)
# }
# 
# r0 = est %>% na.omit %>% dplyr::group_by(year) %>% dplyr::summarize(number = sum(number), biomass = sum(biomass)) %>% mutate(removed = 0)
# retro2 = rbind(retro2, r0)

r0 = NULL
r1 = NULL
for(i in min(retro$year.x):max(retro$year.x)){
  # i = min(retro$year.x)
  t = est %>% filter(year == (i+1)) %>% dplyr::group_by(year) %>% na.omit() %>% dplyr::summarize(number = sum(number), biomass = sum(biomass))
  r0 = rbind(r0, t)
  
  t2 = retro %>% filter(year.x == i)
  # next_year_xxはi+1の予測値
  t2 = t2 %>% mutate(year = as.numeric(year.x)+1) %>% dplyr::group_by(year) %>% dplyr::summarize(e_number = sum(next_year_number), e_biomass = sum(next_year_biomass))
  r1 = rbind(r1, t2)
  
}
retro2 = left_join(r0, r1, by = "year") %>% mutate(bias_b = (e_biomass-biomass)/biomass, bias_n = (e_number/number)/number)
(mean(retro2$bias_b)) #-0.223





