###-----pulling covariates data----###
library(tidyverse)
library(sf)

source("scripts/LAGOS_EDI.R")
source('scripts/LAGOS_EDI_characteristics.R')
source('scripts/09_calculating-TN-retention-timeseries.R')
source('scripts/ice_data_Lute.R')

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
  group_by(lagoslakeid, lake_elevation_m, lake_centroidstate, climate_year) %>%
  summarize(across(c(climate_tmean_degc:climate_ppt_mm),
                   .fns = list(mean = mean, 
                               #sd = sd,# you can comment these out if you want and just calculate mean/median 
                               #max = max,#
                               #min = min,#
                               median = median), na.rm = T,
                   .names = "{.col}-{.fn}")) #ready to merge with data/final_retention_dataset.csv

yearly_clim_data <- yearly_clim_data %>%
  rename(water_year = climate_year, tmean = 'climate_tmean_degc-mean', tmedian = 'climate_tmean_degc-median', prec_mean = 'climate_ppt_mm-mean', prec_median = 'climate_ppt_mm-median')

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
  pivot_wider(names_from = variable_name, values_from = value) %>% #ready to merge with data/final_retention_dataset.csv
select(-main_feature, -hu12_zoneid, -lakes4ha_isolated_pct, -lakes4ha_isolated_ha,	-lakes4ha_isolated_n,-lakes4ha_isolated_nperha,
                           -lakes4ha_headwater_pct,-lakes4ha_headwater_ha,	-lakes4ha_headwater_n,	-lakes4ha_headwater_nperha,
                           -lakes4ha_drainage_pct,-lakes4ha_drainage_ha,	-lakes4ha_drainage_n,	-lakes4ha_drainage_nperha, -lakes4ha_isolatedperm_pct,
       -lakes4ha_isolatedperm_ha,         
        -lakes4ha_isolatedperm_n,           
       -lakes4ha_isolatedperm_nperha,       
       -lakes4ha_headwaterperm_pct,         
       -lakes4ha_headwaterperm_ha ,         
       -lakes4ha_headwaterperm_n ,          
       -lakes4ha_headwaterperm_nperha  ,    
       -lakes4ha_drainageperm_pct ,         
       -lakes4ha_drainageperm_ha ,          
       -lakes4ha_drainageperm_n  ,          
       -lakes4ha_drainageperm_nperha,
- lakes4ha_terminal_pct,              
- lakes4ha_terminal_ha ,              
- lakes4ha_terminal_n ,               
- lakes4ha_terminal_nperha  ,         
- lakes4ha_terminallk_pct ,           
- lakes4ha_terminallk_ha  ,           
- lakes4ha_terminallk_n   ,           
- lakes4ha_terminallk_nperha   ,      
- lakes4ha_terminalperm_pct  ,        
- lakes4ha_terminalperm_ha   ,        
- lakes4ha_terminalperm_n    ,        
- lakes4ha_terminalperm_nperha  ,     
- lakes4ha_terminallkperm_pct  ,      
- lakes4ha_terminallkperm_ha   ,      
- lakes4ha_terminallkperm_n    ,      
- lakes4ha_terminallkperm_nperha) 
  
rm(connect_data)

##GLCP from Meyer et al 2020 (lake surface area, temp, precp and population between 1995-2015)
glcp_data <- read.csv("D:/Datasets/Datasets/glcp.csv")%>%
  filter(year > '2000')

glcp_sites <- right_join(glcp_data, hydrolakes_upstream_sites, by="Hylak_id")%>%
  dplyr::select(lagoslakeid, year, total_precip_mm, mean_annual_temp_k,   pop_sum,seasonal_km2, permanent_km2, total_km2 )%>%
  rename( water_year = year)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  distinct(total_km2, .keep_all = TRUE) 

glcp_sites <- glcp_sites %>%
  select(-pop_sum)

rm(glcp_data)

##LULC: From LAGOS-GEO (zone_landuse.csv only for 2016?) class legend: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description 
lulc_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_landuse.csv")%>% 
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

  
lulc_data_lagos <- merge(study_sites_huc12, lulc_data)%>%
  rename(water_year = year)
  
lulc_median_lagos <- lulc_data_lagos %>%
  group_by(lagoslakeid) %>%
  summarize(across(c(nlcd_openwater11_pct:nlcd_wetemerg95_pct),
                   .fns = list(median = median), na.rm = T,
                   .names = "{.col}-{.fn}"))

lulc_median_lagos <- lulc_median_lagos %>%
  rename(nlcd_openwater11_pct = 'nlcd_openwater11_pct-median', nlcd_icesnow12_pct = 'nlcd_icesnow12_pct-median', nlcd_devopen21_pct='nlcd_devopen21_pct-median',  
         nlcd_devlow22_pct= 'nlcd_devlow22_pct-median',  nlcd_devmed23_pct =  'nlcd_devmed23_pct-median' , nlcd_devhi24_pct =  'nlcd_devhi24_pct-median',   
         nlcd_barren31_pct = 'nlcd_barren31_pct-median', nlcd_fordec41_pct = 'nlcd_fordec41_pct-median',nlcd_forcon42_pct = 'nlcd_forcon42_pct-median',
         nlcd_formix43_pct ='nlcd_formix43_pct-median' ,  nlcd_shrub52_pct = 'nlcd_shrub52_pct-median' ,   
         nlcd_grass71_pct ='nlcd_grass71_pct-median',  nlcd_past81_pct='nlcd_past81_pct-median', nlcd_cultcrop82_pct='nlcd_cultcrop82_pct-median',
         nlcd_wetwood90_pct = 'nlcd_wetwood90_pct-median' ,  nlcd_wetemerg95_pct ='nlcd_wetemerg95_pct-median' )

rm(lulc_data)

##Soils from LAGOS-GEO? 
soils_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_soils.csv")%>% 
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

soils_data_lagos <- inner_join(study_sites_huc12, soils_data, by= "hu12_zoneid")%>%
  dplyr::select(lagoslakeid, hu12_zoneid, variable_name, value, main_feature )%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

soils_data_lagos_final <- soils_data_lagos %>%
  pivot_wider(names_from = variable_name, values_from = value) %>%
  filter(main_feature == "soil")%>%
  select(lagoslakeid, soil_clay_pct, soil_coarse_pct, soil_depthtobedrock_cm, soil_orgcarbon_gperkg, soil_sand_pct,soil_silt_pct)
    #ready to merge with data/final_retention_dataset.csv

rm(soils_data)

#rm(soils_data)

##Terrain/landforms from LAGOS-GEO (zone_terrain.csv)
terrain_data <- read.csv("/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_terrain.csv")%>%
filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

terrain_data_lagos <- inner_join(study_sites_huc12, terrain_data, by= "hu12_zoneid")

terrain_data_final <- terrain_data_lagos %>%
  select(lagoslakeid, variable_name, value)%>%
  group_by(lagoslakeid)%>%
  pivot_wider(names_from = variable_name, values_from = value)

rm(terrain_data)

##Human activities from LAGOS-GEO (zone_human.csv)
human_data <- read.csv("/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_human.csv")%>%
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

human_data_lagos <- inner_join(study_sites_huc12, human_data, by= "hu12_zoneid")

human_data_final <- human_data_lagos %>%
  select(lagoslakeid, variable_name, value)%>%
  group_by(lagoslakeid)%>%
  pivot_wider(names_from = variable_name, values_from = value)

rm(human_data)
# Water quality - look at dataset from Willimans and Labou 2017?? (I was not able to find it. Ask Stephanie)
sites <- upstream_sites_lagos %>%
  select(lagoslakeid)
wq_data <- right_join(dt1_western_summary_2000s, sites, by="lagoslakeid")%>%
 # drop_na()
select(-year, -ton_ugl_median, -microcystin_ugl_median, -salinity_mgl_median)
  
##Ice/snow dynamics - snow climate metrics from Lute et al 2022 - scripts/ice_data_Lute.R
#snow_dur <- st_read("shps/snow_duration_sites.shp")%>%
#  as.data.frame()%>%
#  distinct(lagoslakei, .keep_all = TRUE) %>%
#  dplyr::select(lagoslakei, snowdur)%>%
#  rename(lagoslakeid = lagoslakei)


#Trophic state time-series? Check new Meyer et al dataset
ts_hydro_us <- inner_join(dt1, hydrolakes_upstream_sites, by="Hylak_id")%>%
  filter(year > '2000' & year <'2016')  %>%
  dplyr::select(lagoslakeid, year, categorical_ts, Pour_long, Pour_lat)%>%
  rename(water_year = year)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  distinct(lagoslakeid,water_year, .keep_all = TRUE) 

