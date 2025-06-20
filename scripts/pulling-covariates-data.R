###-----pulling covariates data----###
library(tidyverse)
library(sf)

source("scripts/LAGOS_EDI.R")
source('scripts/LAGOS_EDI_characteristics.R')
source('scripts/09_calculating-TN-retention-timeseries.R')
source('scripts/ice_data_Lute.R')
#source('scripts/05_limno_data_time-series.R')
source('scripts/lake_ts_Meyeretal-dataset.R')

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)%>%
  distinct(lagoslakeid, .keep_all = TRUE) 

d_mm <- d_mm %>% 
  mutate(lagoslakeid=as.character(lagoslakeid))

study_sites_huc12_buff500 <- inner_join(upstream_sites_lagos, d_mm, by="lagoslakeid")%>%
  dplyr::select(lagoslakeid, hu12_zoneid, buff500_zoneid, lake_elevation_m, lake_centroidstate)


##weather data and elevation -> monthly precipitation and air temperature from LAGOS-GEO -> transforming to annual data
#clim_data <- read.csv("/Users/carolinabarbosa/Dropbox/LAGOS_GEO/zone_climate_monthly.csv")%>%
 # clim_data <- read.csv("C:/Users/cbarbosa/Dropbox/LAGOS_GEO/zone_climate_monthly.csv")%>%
    clim_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_climate_monthly.csv")%>% 
  dplyr::select(zoneid,climate_year, climate_month, climate_tmean_degc, climate_ppt_mm)%>%
  rename(hu12_zoneid = zoneid)

clim_data_sites <- inner_join(study_sites_huc12_buff500, clim_data, by="hu12_zoneid")%>%
  filter(climate_year > '2000' & climate_year <'2016')

yearly_clim_data <- clim_data_sites %>%
  group_by(lagoslakeid, lake_elevation_m, lake_centroidstate, climate_year) %>%
  summarize(across(c(climate_tmean_degc:climate_ppt_mm),
                   .fns = list(mean = mean, 
                               #sd = sd,# you can comment these out if you want and just calculate mean/median 
                               #max = max,#
                               #min = min,#
                               #median = median), na.rm = T,
                               sum = sum), na.rm = T,
                   .names = "{.col}-{.fn}")) #ready to merge with data/final_retention_dataset.csv

yearly_clim_data <- yearly_clim_data %>%
  rename(water_year = climate_year, tmean = 'climate_tmean_degc-mean',tsum = 'climate_tmean_degc-sum', prec_mean = 'climate_ppt_mm-mean', prec_total = 'climate_ppt_mm-sum')

yearly_clim_data2 <- yearly_clim_data %>%
  rename(tsum = 'climate_tmean_degc-sum')

#write.csv(yearly_clim_data2, "data/yearly_clim_data2.csv")

rm(clim_data)

#yearly_clim_data %>% 
#  ungroup() %>%
#  pivot_longer(cols = climate_tmean_degc-mean:climate_ppt_mm-median,
#               names_to = c('var','stat'),
               #names_sep = '-')

##Connectivity metrics from LAGOS-GEO - Check why I deleted many connectivity metrics here.
#connect_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_connectivity.csv") %>%
 # filter(spatial_division == 'buff500')

#connect_data_lagos <- inner_join(study_sites_huc12_buff500, connect_data, by= "buff500_zoneid")%>% #buff500_zoneid
#  dplyr::select(lagoslakeid, buff500_zoneid, variable_name, value, main_feature )%>%
 # filter(main_feature == "lakes1ha")%>%
 # mutate(lagoslakeid=as.character(lagoslakeid))

connect_data_hu12 <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_connectivity.csv") %>%
  filter(spatial_division == 'hu12')%>%
  rename(hu12_zoneid = zoneid)

connect_data_lagos <- inner_join(study_sites_huc12_buff500, connect_data_hu12, by= "hu12_zoneid")%>% #buff500_zoneid
  dplyr::select(lagoslakeid, hu12_zoneid, variable_name, value, main_feature )%>%
   #filter(main_feature == "lakes1ha")%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

connect_data_lagos_hu12 <- connect_data_lagos %>%
  pivot_wider(names_from = variable_name, values_from = value) %>% 
  dplyr::select(-main_feature, -hu12_zoneid)%>%
distinct(lagoslakeid, .keep_all = TRUE)%>%
  #dplyr::select(lakes1ha_all_ha, lakes1ha_all_nperha, lakes4ha_all_ha, lakes4ha_all_nperha, lakes10ha_all_ha, lakes10ha_all_nperha)
  dplyr::select(lagoslakeid, lakes1ha_all_nperha, lakes1ha_drainage_nperha,lakes1ha_drainagelk_nperha, lakes1ha_headwater_nperha, lakes1ha_isolated_nperha, lakes1ha_terminal_nperha, lakes1ha_terminallk_nperha)

#connect_data_final <- inner_join(connect_data_lagos_streams, connect_data_lagos_hu12, by="lagoslakeid")

#rm(connect_data)
rm(connect_data_hu12)

##GLCP from Meyer et al 2020 (lake surface area, temp, precp and population between 1995-2015)
glcp_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/glcp.csv")%>%
  filter(year > '2000')

glcp_sites <- right_join(glcp_data, hydrolakes_upstream_sites, by="Hylak_id")%>%
  dplyr::select(lagoslakeid, year, total_precip_mm, mean_annual_temp_k, seasonal_km2, permanent_km2, total_km2 )%>%
  rename( water_year = year)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  distinct(total_km2, .keep_all = TRUE) 

#glcp_sites <- glcp_sites %>%
 # select(-pop_sum)

rm(glcp_data)

##LULC: From LAGOS-GEO (zone_landuse.csv only for 2016?) class legend: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description 
lulc_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_landuse.csv")%>% 
  filter(spatial_division == 'buff500')%>% #'hu12'
  rename(buff500_zoneid = zoneid)

lulc_data_lagos <- merge(study_sites_huc12_buff500, lulc_data)%>%
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
soils_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_soils.csv")%>% 
  filter(spatial_division == 'buff500')%>%
  rename(buff500_zoneid = zoneid)

soils_data_lagos <- inner_join(study_sites_huc12_buff500, soils_data, by= "buff500_zoneid")%>%
  dplyr::select(lagoslakeid, variable_name, value, main_feature )%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

soils_data_lagos_final <- soils_data_lagos %>%
  pivot_wider(names_from = variable_name, values_from = value) %>%
  filter(main_feature == "lith")#%>%
 # select(lagoslakeid, soil_clay_pct, soil_coarse_pct, soil_depthtobedrock_cm, soil_orgcarbon_gperkg, soil_sand_pct,soil_silt_pct)
    #ready to merge with data/final_retention_dataset.csv

rm(soils_data)

#rm(soils_data)

##Terrain/landforms from LAGOS-GEO (zone_terrain.csv) -> cHECK ELEVATION data here!
terrain_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_terrain.csv")%>%
filter(spatial_division == 'buff500')%>%
  rename(buff500_zoneid = zoneid)

terrain_data_lagos <- inner_join(study_sites_huc12_buff500, terrain_data, by= "buff500_zoneid")

terrain_data_final <- terrain_data_lagos %>%
  dplyr::select(lagoslakeid, variable_name, value)%>%
  group_by(lagoslakeid)%>%
  pivot_wider(names_from = variable_name, values_from = value)

rm(terrain_data)

##Human activities from LAGOS-GEO (zone_human.csv)
human_data <- read.csv("/Volumes/Seagate Portable Drive/Datasets/Datasets/LAGOS_GEO/zone_human.csv")%>%
  filter(spatial_division == 'buff500')%>%
  rename(buff500_zoneid = zoneid)

human_data_lagos <- inner_join(study_sites_huc12_buff500, human_data, by= "buff500_zoneid")

human_data_final <- human_data_lagos %>%
  dplyr::select(lagoslakeid, variable_name, value)%>%
  group_by(lagoslakeid)%>%
  pivot_wider(names_from = variable_name, values_from = value)%>%
  dplyr::select(lagoslakeid, dams_n, dams_npersqkm, roads_mperha, canals_mperha)

rm(human_data)
# Water quality - Lagos-Limno-> WQ_data_sites from scripts/05_limno_data_time-series.R
#sites <- upstream_sites_lagos %>%
 # select(lagoslakeid)
#wq_data <- right_join(dt1_western_summary_2000s, sites, by="lagoslakeid")%>%
 # drop_na()
#select(-year, -ton_ugl_median, -microcystin_ugl_median, -salinity_mgl_median)

  
##Ice/snow dynamics - snow climate metrics from Lute et al 2022 - scripts/ice_data_Lute.R

#Trophic state time-series? Check new Meyer et al dataset
ts_hydro_us <- inner_join(dt1, hydrolakes_upstream_sites, by="Hylak_id")%>%
  filter(year > '2000' & year <'2016')  %>%
  dplyr::select(lagoslakeid, year, categorical_ts, mean_prob_dys, mean_prob_eumixo, mean_prob_oligo, Pour_long, Pour_lat)%>%
  rename(water_year = year)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  distinct(lagoslakeid,water_year, .keep_all = TRUE) 

