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
hist(data_TN$TN_removal_gNm2yr)
summary(data_TN$TN_removal_gNm2yr)
sort(data_TN$TN_removal_gNm2yr)

#To define the new categorical variable:
data_TN <- within(data_TN, {   
  TN_removal_gNm2yr.cat <- NA # need to initialize variable
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr < 5.237e+05] <- "Low"
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr >= 5.237e+05 & TN_removal_gNm2yr < 1.684e+08] <- "Expected"
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr >= 1.684e+08] <- "High"
} )
str(data_TN)
summary(data_TN$TN_removal_gNm2yr.cat)

data_TN$TN_removal_gNm2yr.cat <- factor(data_TN$TN_removal_gNm2yr.cat, levels = c("High", "Expected", "Low"))
str(data_TN)
summary(data_TN$TN_removal_gNm2yr.cat)

data_TN_final <- merge(data_TN, phosphorus_loads)%>%
  dplyr::select(lagoslakeid, water_year, lake_lon_decdeg , lake_lat_decdeg , totTNload_gm2yr, TN_removal_gNm2yr, TN_removal_gNm2yr.cat, fluxTP_kgy)

###Adding covariates

data_clim <- inner_join(data_TN_final, yearly_clim_data, by=c('lagoslakeid', 'water_year'))
data_clim_glcp <- inner_join(data_clim, glcp_sites, by=c('lagoslakeid', 'water_year'))
data_clim_glcp_ts <- inner_join(data_clim_glcp, ts_hydro_us, by=c('lagoslakeid', 'water_year'))
#data_clim_glcp_ts_wq <- left_join(data_clim_glcp_ts, WQ_data_sites, by=c('lagoslakeid', 'water_year'))

#Try join all dfs once

data_all_TN <- purrr::reduce(list(data_clim_glcp_ts, connect_data_lagos_final, 
                   lulc_median_lagos, soils_data_lagos_final,terrain_data_final, 
                   human_data_final, snow_data, drainage_ratio), dplyr::left_join, by = 'lagoslakeid') #%>% I've deleted wq data because those have tons of missing data
  #distinct(lagoslakeid, .keep_all = TRUE)


write.csv(data_all_TN, "data/data_all_TN.csv") #948 obs of 175 variables -> 348 obs of 175 variables

##Reading final dataset for TP ---------------------

data_TP <- read.csv("data/Pretention_df.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid)) 

##P retention
hist(data_TP$Pret_coef)
summary(data_TP$Pret_coef)
sort(data_TP$Pret_coef)

#To define the new categorical variable:
data_TP <- within(data_TP, {   
  Pret_coef.cat <- NA # need to initialize variable
  Pret_coef.cat[Pret_coef < 5.286] <- "Low"
  Pret_coef.cat[Pret_coef >= 5.286 & Pret_coef < 29.487] <- "Expected"
  Pret_coef.cat[Pret_coef >= 29.487] <- "High"
} )
str(data_TP)
summary(data_TP$Pret_coef.cat)

data_TP$Pret_coef.cat <- factor(data_TP$Pret_coef.cat, levels = c("High", "Expected", "Low"))
str(data_TP)
summary(data_TP$Pret_coef.cat)

data_TP_final <- merge(data_TP, total_Nload)

data_TP_final2 <- merge(data_TP_final, phosphorus_loads)%>%
  distinct(lagoslakeid, water_year, .keep_all = TRUE)%>%
  dplyr::select(lagoslakeid, water_year, lake_lon_decdeg , lake_lat_decdeg , res_time_yr, total_Pin_ugl, Pret_coef , Pret_coef.cat, fluxTP_kgy, totTNload_gm2yr)

###Adding covariates

data_clim <- inner_join(data_TP_final2, yearly_clim_data, by=c('lagoslakeid', 'water_year'))
data_clim_glcp <- inner_join(data_clim, glcp_sites, by=c('lagoslakeid', 'water_year'))
data_clim_glcp_ts <- inner_join(data_clim_glcp, ts_hydro_us, by=c('lagoslakeid', 'water_year'))
#data_clim_glcp_ts_wq <- left_join(data_clim_glcp_ts, WQ_data_sites, by=c('lagoslakeid', 'water_year'))

#Try join all dfs once

data_all_TP <- purrr::reduce(list(data_clim_glcp_ts, connect_data_lagos_final, 
                               lulc_median_lagos, soils_data_lagos_final,terrain_data_final, 
                               human_data_final, snow_data, drainage_ratio), dplyr::left_join, by = 'lagoslakeid') 
#distinct(lagoslakeid, .keep_all = TRUE)

write.csv(data_all_TP, "data/data_all_TP.csv") # 843 obs of 177 variables -> 259 obs of 177 variables
