###-----pulling covariates data----###
library(tidyverse)

source("scripts/LAGOS_EDI.R")
d_mm <- d_mm %>% 
  mutate(lagoslakeid=as.character(lagoslakeid))

##weather data and elevation -> monthly precipitation and air temperature from LAGOS-GEO -> transforming to annual data
clim_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_climate_monthly.csv")%>%
  select(zoneid,climate_year, climate_month, climate_tmean_degc, climate_ppt_mm)%>%
  rename(hu12_zoneid = zoneid)

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)%>%
  distinct(lagoslakeid, .keep_all = TRUE) 

study_sites_huc12 <- inner_join(upstream_sites_lagos, d_mm, by="lagoslakeid")%>%
  select(lagoslakeid, hu12_zoneid, lake_elevation_m, lake_centroidstate)

clim_data_sites <- inner_join(study_sites_huc12, clim_data, by="hu12_zoneid")%>%
  filter(climate_year > '2000' & climate_year <'2016')

yearly_clim_data <- clim_data_sites %>%
  group_by(lagoslakeid, lake_elevation_m, lake_centroidstate, hu12_zoneid, climate_year) %>%
  summarize(across(c(climate_tmean_degc:climate_ppt_mm),
                   .fns = list(mean = mean, 
                               #sd = sd,# you can comment these out if you want and just calculate mean/median 
                               #max = max,#
                               #min = min,#
                               median = median), na.rm = T,
                   .names = "{.col}-{.fn}")) #ready to merge with data/final_retention_dataset.csv

yearly_clim_data <- yearly_clim_data %>%
  rename(water_year = climate_year)

rm(clim_data)

#yearly_clim_data %>% 
#  ungroup() %>%
#  pivot_longer(cols = climate_tmean_degc-mean:climate_ppt_mm-median,
#               names_to = c('var','stat'),
               #names_sep = '-')

##Connectivity metrics from LAGOS-GEO
connect_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_connectivity.csv")%>%
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

connect_data_lagos <- inner_join(study_sites_huc12, connect_data, by= "hu12_zoneid")%>%
  dplyr::select(lagoslakeid, hu12_zoneid, variable_name, value, main_feature )%>%
  filter(main_feature == "lakes4ha")%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

connect_data_lagos_final <- connect_data_lagos %>%
  pivot_wider(names_from = variable_name, values_from = value) #ready to merge with data/final_retention_dataset.csv

rm(connect_data)
##Lake characteristics -> area (lake_totalarea_ha), lake depth? huc12_zoneid, from LAGOS-US AND HYDROlakes
#Locus: lake_characteristics.csv file

##LULC: From LAGOS-GEO (zone_landuse.csv only for 2016?)

##Soils from LAGOS-GEO? Ask folks thoughts

##Terrain/landforms from LAGOS-GEO (zone_terrain.csv)

##Human activities from LAGOS-GEO (zone_human.csv)

##GLCP from Meyer et al 2020? (lake surface area, temp, precp and population between 1995-2015)
source("scripts/GLCP_data.R")


# Water quality - look at dataset from Willimans and Labou 2017?? (I was not able to find it. Ask Stephanie)

#Ice/snow dynamics - snow climate metrics from Lute et al 2022 and Global lake and river ice phenology (emailed Josh on Jun 15th about it)?

#Trophic state time-series? Check new Meyer et al dataset