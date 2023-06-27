##converting numerical retention data to categorical data to run the RF model
library(tidyverse)
library(ggpubr)
library(purrr)

source("scripts/pulling-covariates-data.R")
source("scripts/ice_data_Lute.R")
source("scripts/pulling-groundwater-data.R")

##Reading final dataset
data <- read.csv("data/final_retention_dataset.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid)) 

##P retention
hist(data$Pret_coef)
summary(data$Pret_coef)
sort(data$Pret_coef)

#To define the new categorical variable:
data <- within(data, {   
  Pret_coef.cat <- NA # need to initialize variable
  Pret_coef.cat[Pret_coef < 9] <- "Low"
  Pret_coef.cat[Pret_coef >= 9 & Pret_coef < 19] <- "Normal"
  Pret_coef.cat[Pret_coef >= 19] <- "High"
} )
str(data)
summary(data$Pret_coef.cat)

data$Pret_coef.cat <- factor(data$Pret_coef.cat, levels = c("High", "Normal", "Low"))
str(data)
summary(data$Pret_coef.cat)

##N retention
hist(data$TN_removal_gNm2yr)
summary(data$TN_removal_gNm2yr)
sort(data$TN_removal_gNm2yr)

#To define the new categorical variable:
data <- within(data, {   
  TN_removal_gNm2yr.cat <- NA # need to initialize variable
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr < 13000] <- "Low"
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr >= 13000 & TN_removal_gNm2yr < 397000] <- "Normal"
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr >= 397000] <- "High"
} )
str(data)
summary(data$TN_removal_gNm2yr.cat)

data$TN_removal_gNm2yr.cat <- factor(data$TN_removal_gNm2yr.cat, levels = c("High", "Normal", "Low"))
str(data)
summary(data$TN_removal_gNm2yr.cat)

###testing (Waiting for ice data to include here. Add WQ data as well??)

data_clim <- inner_join(data, yearly_clim_data, by=c('lagoslakeid', 'water_year'))
data_clim_glcp <- inner_join(data_clim, glcp_sites, by=c('lagoslakeid', 'water_year'))
data_clim_glcp_ts <- inner_join(data_clim_glcp, ts_hydro_us, by=c('lagoslakeid', 'water_year'))

#Try join all dfs once

data_all <- purrr::reduce(list(data_clim_glcp_ts, connect_data_lagos_final, 
                   lulc_median_lagos, soils_data_lagos_final,terrain_data_final, 
                   human_data_final), dplyr::left_join, by = 'lagoslakeid') #%>% I've deleted wq data because those have tons of missing data
  #distinct(lagoslakeid, .keep_all = TRUE)


#write.csv(data_all, "data/data_all.csv")

##Adding more covariates (snow and drainage ratio)
data_all <- read.csv("data/data_all.csv")%>%
mutate(lagoslakeid = as.character(lagoslakeid))

data_all_new <- inner_join(data_all, snow_data, by="lagoslakeid")%>%
  inner_join(drainage_ratio, by="lagoslakeid")


