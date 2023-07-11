##converting numerical retention data to categorical data to run the RF model
library(tidyverse)
library(ggpubr)
library(purrr)

source("scripts/pulling-covariates-data.R")
source("scripts/ice_data_Lute.R")
source("scripts/pulling-groundwater-data.R")
source("scripts/08_nutrient-loads-sensslopes.R")
source("scripts/pulling-deposition-data.R")

##Reading final dataset for TN ---------------------
data_TN <- read.csv("data/Nretention_df.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid)) %>%
  distinct(lagoslakeid, water_year, .keep_all = TRUE)
  
##N retention

data_TN_final <- merge(data_TN, phosphorus_loads)%>%
  dplyr::select(lagoslakeid, water_year, lake_lon_decdeg , lake_lat_decdeg , totTNload_gm2yr, TN_removal_gNm2yr, fluxTP_kgy)

###Adding covariates

data_clim <- inner_join(data_TN_final, yearly_clim_data, by=c('lagoslakeid', 'water_year'))
data_clim_glcp <- inner_join(data_clim, glcp_sites, by=c('lagoslakeid', 'water_year'))
data_clim_glcp_ts <- inner_join(data_clim_glcp, ts_hydro_us, by=c('lagoslakeid', 'water_year'))
#data_clim_glcp_ts_wq <- left_join(data_clim_glcp_ts, WQ_data_sites, by=c('lagoslakeid', 'water_year'))

#Try join all dfs once

data_all_TN <- purrr::reduce(list(data_clim_glcp_ts, connect_data_lagos_hu12, 
                   lulc_median_lagos, terrain_data_final, 
                   human_data_final, snow_data, drainage_ratio), dplyr::left_join, by = 'lagoslakeid') #%>% I've deleted wq data because those have tons of missing data
  #distinct(lagoslakeid, .keep_all = TRUE) #removed soils

Q1 <- quantile(data_all_TN$TN_removal_gNm2yr, 0.25)
Q3 <- quantile(data_all_TN$TN_removal_gNm2yr, 0.75)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
data_all_TN <- data_all_TN %>%
  dplyr::filter(TN_removal_gNm2yr >= lower_fence & TN_removal_gNm2yr <= upper_fence)

write.csv(data_all_TN, "data/data_all_TN.csv") #948 obs of 175 variables -> 348 obs of 175 variables -> 348 obs of 189 variab -> 283 of 63 vari

##Reading final dataset for TP ---------------------

data_TP <- read.csv("data/Pretention_df.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid)) 

##P retention
data_TP_final <- merge(data_TP, total_Nload)%>%
filter(Pret_coef >0)

data_TP_final2 <- merge(data_TP_final, phosphorus_loads)%>%
  distinct(lagoslakeid, water_year, .keep_all = TRUE)%>%
  dplyr::select(lagoslakeid, water_year, lake_lon_decdeg , lake_lat_decdeg , res_time_yr, total_Pin_ugl, Pret_coef , fluxTP_kgy, totTNload_gm2yr)

###Adding covariates

data_clim <- inner_join(data_TP_final2, yearly_clim_data, by=c('lagoslakeid', 'water_year'))
data_clim_glcp <- inner_join(data_clim, glcp_sites, by=c('lagoslakeid', 'water_year'))
data_clim_glcp_ts <- inner_join(data_clim_glcp, ts_hydro_us, by=c('lagoslakeid', 'water_year'))
#data_clim_glcp_ts_wq <- left_join(data_clim_glcp_ts, WQ_data_sites, by=c('lagoslakeid', 'water_year'))

#Try join all dfs once

data_all_TP <- purrr::reduce(list(data_clim_glcp_ts, connect_data_lagos_hu12, 
                               lulc_median_lagos, terrain_data_final, 
                               human_data_final, snow_data, drainage_ratio), dplyr::left_join, by = 'lagoslakeid') 
#distinct(lagoslakeid, .keep_all = TRUE)
Q1 <- quantile(data_all_TP$Pret_coef, 0.25)
Q3 <- quantile(data_all_TP$Pret_coef, 0.75)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
data_all_TP <- data_all_TP %>%
  dplyr::filter(Pret_coef >= lower_fence & Pret_coef <= upper_fence)

write.csv(data_all_TP, "data/data_all_TP.csv") # 843 obs of 177 variables -> 259 obs of 177 variables -> 272 obs of 191 variables -> 267 obs of 65 variab
