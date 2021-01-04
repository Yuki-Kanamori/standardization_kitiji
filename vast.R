
# 0. Prepare the data -------------------------------------------
dirname = "/Users/Yuki/Dropbox/sokouo1/ws"
setwd(dir = dirname)

# Packages
require(TMB)
require(VAST)
require(tidyverse)

# read the data
df = read.csv("kiti.csv", fileEncoding = "CP932")
summary(df)
df = df %>%
  # filter(sakana == "C", between(year, 2022, 2026)) %>%
  select(year, station, depth, lon, lat, d, N, area, tag)
summary(df)

lonlat = paste(df$lon, df$lat, sep = "_")
length(unique(lonlat))
st_dep = paste(df$station, df$depth, sep = "_")
length(unique(st_dep))

# 1. Settings ------------------------------------------------------
# 1.1 Version for cpp code
Version = get_latest_version(package = "VAST")
# Version = "VAST_v4_2_0"

# 1.2 Spatial settings
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
Kmeans_Config = list("randomseed" = 1, "nstart" = 100, "iter.max" = 1000)
grid_size_km = 25
n_x = 100

# 1.3 Model settings
FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1) #factor analysis
RhoConfig = c(Beta1 = 0, Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0) #0: fixed, 1: independent, 2:RW, 3:constant, 4:AR
OverdispersionConfig = c("Eta1" = 0, "Eta2" = 0) #overdispersion
ObsModel = c(PosDist = 1, Link = 0)
Options = c(SD_site_density = 0, SD_site_logdensity = 0,
            Calculate_Range = 1, Calculate_evenness = 0, 
            Calculate_effective_area = 1, Calculate_Cov_SE = 0, 
            Calculate_Synchrony = 0, Calculate_Coherence = 0)

# 1.4 Stratification for results
strata.limits = data.frame('STRATA'="All_areas")

# 1.5 Derived objects
Region = "others"

# 1.6 Save settings
DateFile = paste0(getwd(), "/vast", Sys.Date(), "_dens_lognorm", n_x)
dir.create(DateFile)
Record = list(Version = Version, Method = Method, grid_size_km = grid_size_km, n_x = n_x, 
              FieldConfig = FieldConfig, RhoConfig = RhoConfig, OverdispersionConfig = OverdispersionConfig, 
              ObsModel = ObsModel, Kmeans_Config = Kmeans_Config, Region = Region,
              strata.limits = strata.limits) 
setwd(dir = DateFile)
save(Record, file = file.path(DateFile, "Record.RData")) 
capture.output(Record, file = paste0(DateFile, "/Record.txt"))


# 2. Prepare the data ----------------------------------------------
# 2.1 Data-frame
head(df)
# Data_Geostat = df %>% select(year, lon, lat, N, area) %>% rename(Year = year, Lon = lon, Lat = lat, Catch_KG = N, AreaSwept_km2 = area)
Data_Geostat = df %>% select(year, station, depth, lon, lat, d) %>% rename(Year = year, Lon = lon, Lat = lat, Catch_KG = d)

# 2.2 Extrapolation grid
Extrapolation_List = FishStatsUtils::make_extrapolation_info(
  Regio = Region, #zone range in Japan is 51:56
  strata.limits = strata.limits, 
  observations_LL = Data_Geostat[, c("Lat", "Lon")]
)

# 2.3 derived objects for spatio-temporal estimation
Spatial_List = FishStatsUtils::make_spatial_info(
  n_x = n_x,
  Lon = Data_Geostat[, "Lon"], 
  Lat = Data_Geostat[, "Lat"], 
  Extrapolation_List = Extrapolation_List, 
  Method = Method,
  grid_size_km = grid_size_km,
  randomseed = Kmeans_Config[["randomseed"]], 
  nstart = Kmeans_Config[["nstart"]], 
  iter.max = Kmeans_Config[["iter.max"]], 
  #fine_scale = TRUE,
  DirPath = DateFile,
  Save_Results = TRUE)

# 2.4 save Data_Geostat
Data_Geostat = cbind(Data_Geostat, knot_i = Spatial_List[["knot_i"]], zone = Extrapolation_List[["zone"]])
write.csv(Data_Geostat, "Data_Geostat.csv")


# 3. Build and run model ----------------------------------------
TmbData = make_data(
  Version = Version,
  FieldConfig = FieldConfig, 
  OverdispersionConfig = OverdispersionConfig, 
  RhoConfig = RhoConfig, 
  ObsModel = ObsModel, 
  c_iz = rep(0, nrow(Data_Geostat)), 
  b_i = Data_Geostat[, 'Catch_KG'], 
   a_i = rep(1, nrow(Data_Geostat)), #cpue
  #a_i = Data_Geostat[, 'AreaSwept_km2'], # catch and effort
  s_i = Data_Geostat[, 'knot_i'] - 1,
  t_i = Data_Geostat[, 'Year'], 
  spatial_list = Spatial_List, 
  Options = Options,
  Aniso = TRUE
)

TmbList = VAST::make_model(TmbData = TmbData,
                           RunDir = DateFile,
                           Version = Version,
                           RhoConfig = RhoConfig,
                           loc_x = Spatial_List$loc_x,
                           Method = Spatial_List$Method)

Obj = TmbList[["Obj"]]
Opt = TMBhelper::fit_tmb(obj = Obj, 
                         lower = TmbList[["Lower"]], 
                         upper = TmbList[["Upper"]],
                         getsd = TRUE, 
                         savedir = DateFile, 
                         bias.correct = TRUE)
(check = VAST::check_fit(Opt))

Report = Obj$report()
Save = list("Opt" = Opt, 
            "Report" = Report, 
            "ParHat" = Obj$env$parList(Opt$par),
            "TmbData" = TmbData)
save(Save, file = paste0(DateFile,"/Save.RData"))

# 4. Figures -------------------------------------------------------
# 4.1 Plot data
plot_data(Extrapolation_List = Extrapolation_List, Spatial_List = Spatial_List, Data_Geostat = Data_Geostat, PlotDir = DateFile)

# 4.2 Convergence
pander::pandoc.table(Opt$diagnostics[, c('Param','Lower','MLE','Upper','final_gradient')])

# 4.3 Diagnostics for encounter-probability component
Enc_prob = plot_encounter_diagnostic(Report = Report, Data_Geostat = Data_Geostat, DirName = DateFile)

# 4.4 Diagnostics for positive-catch-rate component
Q = plot_quantile_diagnostic(TmbData = TmbData, 
                             Report = Report, 
                             FileName_PP = "Posterior_Predictive",
                             FileName_Phist = "Posterior_Predictive-Histogram", 
                             FileName_QQ = "Q-Q_plot", 
                             FileName_Qhist = "Q-Q_hist", 
                             DateFile = DateFile )

# 4.5 Diagnostics for plotting residuals on a map
MapDetails_List = make_map_info("Region" = Region, 
                                "spatial_list" = Spatial_List, 
                                "Extrapolation_List" = Extrapolation_List)
Year_Set = seq(min(Data_Geostat[,'Year']), max(Data_Geostat[,'Year']))
Years2Include = which(Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

plot_residuals(Lat_i = Data_Geostat[,'Lat'], 
               Lon_i = Data_Geostat[,'Lon'], 
               TmbData = TmbData, 
               Report = Report, 
               Q = Q, 
               savedir = DateFile, 
               spatial_list = Spatial_List, #
               extrapolation_list = Extrapolation_List, #
               MappingDetails = MapDetails_List[["MappingDetails"]], 
               PlotDF = MapDetails_List[["PlotDF"]], 
               MapSizeRatio = MapDetails_List[["MapSizeRatio"]], 
               Xlim = MapDetails_List[["Xlim"]], 
               Ylim = MapDetails_List[["Ylim"]], 
               FileName = DateFile, 
               Year_Set = Year_Set, 
               Years2Include = Years2Include, 
               Rotate = MapDetails_List[["Rotate"]], 
               Cex = MapDetails_List[["Cex"]], 
               Legend = MapDetails_List[["Legend"]], 
               zone = MapDetails_List[["Zone"]], 
               mar = c(0,0,2,0), 
               oma = c(3.5,3.5,0,0), 
               cex = 1.8)

# 4.6 Direction of "geometric anisotropy"
plot_anisotropy(FileName = paste0(DateFile,"Aniso.png"), 
                Report = Report, 
                TmbData = TmbData)

# 4.7 Density surface for each year
Dens_xt = plot_maps(plot_set = c(3), 
                    MappingDetails = MapDetails_List[["MappingDetails"]], 
                    Report = Report, 
                    Sdreport = Opt$SD, 
                    PlotDF = MapDetails_List[["PlotDF"]], 
                    MapSizeRatio = MapDetails_List[["MapSizeRatio"]], 
                    Xlim = MapDetails_List[["Xlim"]], 
                    Ylim = MapDetails_List[["Ylim"]], 
                    FileName = DateFile, 
                    Year_Set = Year_Set, 
                    Years2Include = Years2Include, 
                    Rotate = MapDetails_List[["Rotate"]],
                    Cex = MapDetails_List[["Cex"]], 
                    Legend = MapDetails_List[["Legend"]], 
                    zone = MapDetails_List[["Zone"]], 
                    mar = c(0,0,2,0), 
                    oma = c(3.5,3.5,0,0), 
                    cex = 1.8, 
                    plot_legend_fig = FALSE)

Dens_DF = cbind("Density" = as.vector(Dens_xt), 
                "Year" = Year_Set[col(Dens_xt)], 
                "E_km" = Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], 
                "N_km" = Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'])

pander::pandoc.table(Dens_DF[1:6,], digits=3)

# 4.8 Index of abundance
Index = plot_biomass_index(DirName = DateFile, 
                           TmbData = TmbData, 
                           Sdreport = Opt[["SD"]], 
                           Year_Set = Year_Set, 
                           Years2Include = Years2Include, 
                           use_biascorr=TRUE )
pander::pandoc.table(Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] ) 

# 4.9 Center of gravity and range expansion/contraction
plot_range_index(Report = Report, 
                 TmbData = TmbData, 
                 Sdreport = Opt[["SD"]], 
                 Znames = colnames(TmbData$Z_xm), 
                 PlotDir = DateFile, 
                 Year_Set = Year_Set)


# ggvast --------------------------------------------------------
# require(devtools)
# install_github("Yuki-Kanamori/ggvast")

require(ggvast)

# 0.1 set the directory ---------------------------------------------
vast_output_dirname = "/Users/Yuki/Dropbox/sokouo1/ws/vast2021-01-01_dens_lognorm100"
fig_output_dirname = "/Users/Yuki/Dropbox/sokouo1/ws/vast2021-01-01_dens_lognorm100"
setwd(dir = vast_output_dirname)

# 0.2 load the data -------------------------------------------------
load("Save.RData")
DG = read.csv("Data_Geostat.csv")

# 1. plot index ------------------------------------------------
# vast output
setwd(dir = vast_output_dirname)
vast_index = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standardized")

# nominal
levels(DG$spp) #単一種の時はNULLと出る
category_name = c("kitiji") #カテゴリーの名前（魚種名や銘柄など）

# make a figure
# nominalにはerror barが無いため，geom_errorbarのwarningが出るが問題ない
ggvast::plot_index(vast_index = vast_index,
                   DG = DG,
                   category_name = category_name,
                   fig_output_dirname = fig_output_dirname)



# 2. get dens --------------------------------------------------
# make a data-frame
df_dens = ggvast::get_dens(category_name = category_name)

setwd(fig_output_dirname)
write.csv(df_dens, "est.csv")

# 3. map dens ---------------------------------------------------
#DG2 = DG %>% filter(Catch_KG > 0) #> 0データのみをプロットしたい場合

data = df_dens #VASTの時
#data = DG #ノミナルの時
#data = DG2 #ノミナル>0の時

#require(maps)
#unique(map_data("world")$region)
region = "Japan" #作図する地域を選ぶ
scale_name = "Log density" #凡例　色の違いが何を表しているのかを書く
ncol = 10 #横にいくつ図を並べるか（最大数 = 年数）
shape = 16 #16はclosed dot
size = 1.9 #shapeの大きさ
zoom_out_lon = 1 #mapの拡大・縮小（1がデフォルト，数字が大きくなるほど拡大する．1以下で縮小する）
zoom_out_lat = 1 #mapの拡大・縮小（1がデフォルト，数字が大きくなるほど拡大する．1以下で縮小する）

# make figures
ggvast::map_dens(data = data,
                 region = region,
                 scale_name = scale_name,
                 ncol = ncol,
                 shape = shape,
                 size = size,
                 zoom_out_lon,
                 zoom_out_lat,
                 fig_output_dirname =  fig_output_dirname)



# 引き延ばし ---------------------------------------------------------
DG2 = DG %>% mutate(tag = paste(Lon, Lat, sep = "_"))
tag = DG2 %>% select(tag, depth, station, knot_i)

df_dens2 = df_dens %>% mutate(tag = paste(lon, lat, sep = "_")) %>% select(-category_name)

check = left_join(df_dens2, tag, by = "tag") #129250
summary(check)

# check = check %>% mutate(NS = ifelse(check$lat > 38.50, "N", "S"))
# cn = check %>% filter(NS == "N")
# unique(cn$NS)
# unique(cn$station)
# cnE = cn %>% filter(station == "E")
# 
# d = check %>% dplyr::group_by(year, depth, NS) %>% dplyr::summarize(mean_d = mean(exp(log_abundance)))
# 
# 
# setwd(dir = dirname)
# A = read.csv("Adata.csv") %>% select(-memo)
# d = left_join(d, A, by = c("depth", "NS")) %>% mutate(extentN = A*mean_d)
# yearN = d %>% dplyr::group_by(year) %>% dplyr::summarize(N = sum(extentN))
# plot(x = yearN$year, y = yearN$N, type = "b")

check = check %>% select(-tag) %>% mutate(tag = paste(station, depth, sep = "_"))
setwd("/Users/Yuki/Dropbox/sokouo1/make_VASTdata")
A = read.csv("area_A.csv") %>% select(-X, -station, -depth) #113
check = left_join(check, A, by = "tag") %>% mutate(extentN = area_A*exp(log_abundance)) #129250
check = check %>% mutate(NS = ifelse(check$lat > 38.50, "N", "S"))
d = check %>% dplyr::group_by(year, depth, NS) %>% dplyr::summarize(mean_d = mean(extentN))
yearN = d %>% dplyr::group_by(year) %>% dplyr::summarize(N = sum(mean_d))
plot(x = yearN$year, y = yearN$N, type = "b")


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

al_sum = al %>% dplyr::group_by(year) %>% dplyr::summarize(totalN = sum(number))
al = left_join(al, al_sum, by = "year") %>% mutate(freq = number/totalN)
yearN = left_join(yearN, al, by = "year") %>% mutate(decompN = N*freq)

temp2 = yearN %>% dplyr::filter(age != "0") %>% dplyr::group_by(year) %>% dplyr::summarize(totalN = sum(decompN))
plot(x = temp2$year, y = temp2$totalN, type = "b")



# stock abundance -----------------------------------------------
trawl = yearN %>% group_by(year, age) %>% summarize(number = sum(decompN)) %>% filter(age != "0")
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
lab = labs(x = "年", y = "資源量（千トン）", shape = "")
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
ggsave(file = "fig10_vast.png", plot = fig10, units = "in", width = 11.69, height = 8.27)

check_trend = trend %>% filter(year > 2015)
summary(lm(total/1000 ~ year, data = check_trend))


### year trend of stock number (fig. 11)
est = est %>% mutate(age2 = ifelse(age > 4, "5歳以上", "2-4歳"))
summary(est)

est2 = est
est2[is.na(est2)] = 0
est2 = ddply(est2, .(year, age2), summarize, total = sum(number))
summary(est2)

levels(est2$age2) 
unique(est$age2)
est2$age2 = factor(est2$age2, levels = c("5歳以上", "2-4歳"))
levels(est2$age2)

g = ggplot(est2, aes(x = year, y = total/1000000, fill = age2))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "資源尾数（百万尾）", legend = NULL)
col_age = c("black", "white")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("black", "white"))
fig11 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 200))
ggsave(file = "fig11_vast.png", plot = fig11, units = "in", width = 11.69, height = 8.27)
