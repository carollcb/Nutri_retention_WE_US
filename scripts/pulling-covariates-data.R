###-----pulling covariates data----###
library(tidyverse)

source("scripts/LAGOS_EDI.R")
source('scripts/LAGOS_EDI_characteristics.R')
source('scripts/09_calculating-TN-retention-timeseries.R')

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)%>%
  distinct(lagoslakeid, .keep_all = TRUE) 

d_mm <- d_mm %>% 
  mutate(lagoslakeid=as.character(lagoslakeid))

study_sites_huc12 <- inner_join(upstream_sites_lagos, d_mm, by="lagoslakeid")%>%
  select(lagoslakeid, hu12_zoneid, lake_elevation_m, lake_centroidstate)


##weather data and elevation -> monthly precipitation and air temperature from LAGOS-GEO -> transforming to annual data
#clim_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_climate_monthly.csv")%>%
  clim_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_climate_monthly.csv")%>%
  select(zoneid,climate_year, climate_month, climate_tmean_degc, climate_ppt_mm)%>%
  rename(hu12_zoneid = zoneid)

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
#connect_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_connectivity.csv")%>%
  connect_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_connectivity.csv")%>% 
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

dt2 <- dt2 %>%
  mutate(lagoslakeid=as.character(lagoslakeid))%>% 
  select(lagoslakeid, lake_totalarea_ha, lake_connectivity_class, lake_connectivity_permanent)

lake_caract_data <- inner_join(upstream_sites_lagos, dt2, by="lagoslakeid")
  
##GLCP from Meyer et al 2020 (lake surface area, temp, precp and population between 1995-2015)
glcp_data <- read.csv("D:/Datasets/Datasets/glcp.csv")%>%
  filter(year > '2000')

##LULC: From LAGOS-GEO (zone_landuse.csv only for 2016?) class legend: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description 
lulc_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_landuse.csv")%>% 
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

lulc_data_lagos <- merge(study_sites_huc12, lulc_data)

rm()
##Soils from LAGOS-GEO? 
soils_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_soils.csv")%>% 
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

soils_data_lagos <- inner_join(study_sites_huc12, soils_data, by= "hu12_zoneid")%>%
  dplyr::select(lagoslakeid, hu12_zoneid, variable_name, value, main_feature )%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

soils_data_lagos_final <- soils_data_lagos %>%
  pivot_wider(names_from = variable_name, values_from = value) #ready to merge with data/final_retention_dataset.csv

#rm(soils_data)

##Terrain/landforms from LAGOS-GEO (zone_terrain.csv)
terrain_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_terrain.csv")%>%
filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

terrain_data_final <- terrain_data %>%
  pivot_wider(names_from = variable_name, values_from = value)

terrain_data_lagos <- inner_join(study_sites_huc12, terrain_data_final, by= "hu12_zoneid")

rm(terrain_data)

##Human activities from LAGOS-GEO (zone_human.csv)
human_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_human.csv")%>%
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

human_data_final <- human_data %>%
  pivot_wider(names_from = variable_name, values_from = value)

human_data_lagos <- inner_join(study_sites_huc12, human_data_final, by= "hu12_zoneid")

rm(human_data)
# Water quality - look at dataset from Willimans and Labou 2017?? (I was not able to find it. Ask Stephanie)

#Ice/snow dynamics - snow climate metrics from Lute et al 2022 

#Trophic state time-series? Check new Meyer et al dataset
ts_hydro_us #hydrolakes + TS dataset