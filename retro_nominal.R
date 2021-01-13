# load the packages -------------------------------------------------------
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

# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")


# step 4; estimation of stock abundance (number & biomass) ---------------------------------------------------------
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

# combine the length data
old_length = olddata %>% filter(data == 'length') %>% gather(key = year_tag, value = mean_mm, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_trawl)

mean_length_weight_at_age = read.csv("mean_length_weight_at_age.csv")
length = mean_length_weight_at_age %>% select(age, mean_mm) %>% mutate(age = as.numeric(age), year = 2019) %>% filter(age > 1)
length = rbind(old_length, length)
summary(length)

# combine the catch data from the fishing
okisoko = read.csv("okisoko.csv")
old_catchF = olddata %>% filter(data == 'catch_fisheries') %>% gather(key = year_tag, value = catch, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data,-age)
catch2019 = data.frame(catch = sum(okisoko$漁獲量の合計)/1000, year = 2019)
catchF = rbind(old_catchF, catch2019)
summary(catchF)


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
    for(j in 2:10){
      if(j < 10){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
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
  data = length %>% filter(year == i) %>% arrange(age)
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
  data = length %>% filter(year == i) %>% arrange(age)
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



# retro ---------------------------------------------------------
# forecasting ---------------------------------------------------
retro_year = 5

retro = NULL
for(i in 1:retro_year){
  # fの3年平均
  # i = 1
  f_current = fishing_rate %>% filter(year < ((as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i))+1)) %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i))-3)) %>% dplyr::summarize(mean(f))
  # 2021 - (2+0) - 3 = 2016 => 2017~2019
  # 2021 - (2+1) - 3 = 2015 => 2016~2019
  
  s_current = exp(-(f_current+M))
  
  s1_current = survival %>% filter(year < ((as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i))+1)) %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i))-3), age == 2) %>% dplyr::summarize(mean(surv))
  
  # number_1old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2, age == 1)%>% dplyr::select(number)/1000 * s1_current
  number_1old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i), age == 1)
  # number_1old_oct_last = number_1old_oct_last$number/1000 * s1_current
  number_1old_oct_last = number_1old_oct_last$number/1000 * s1_current
  
  number_2old_oct_last = number_1old_oct_last*s1_current
  number_2old_jan_this = number_2old_oct_last*survival_2month %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i)) %>% select(surv)
  
  number_2old_jan_this_sel = number_2old_jan_this/q %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i), age == 2) %>% select(q)
  
  # the estimated abundance in step 4
  abund_abc = est %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-(2+i))) %>% select(number, biomass, year, age) %>% dplyr::rename(number_est = number, biomass_est = biomass)
  abund_abc = left_join(abund_abc, weight %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-(3+i))), by = c("age"))
  
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
(mean(retro2$bias_b)) #-0.15
(mean(retro2$bias_n)) #-1.159484e-11
