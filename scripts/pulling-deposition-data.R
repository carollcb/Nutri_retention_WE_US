##Looking into deposition data from LAGOS-GEO

library(tidyverse)

source("scripts/LAGOS_EDI.R")
source("scripts/07_organizing-nutrient-loads-df_newQ.R")
source("scripts/08_nutrient-loads-sensslopes.R")

#lagos_hu12 <- st_read('D:/Datasets/Datasets/LAGOS_GEO/gis_geo_v1.0.gpkg', layer="hu12")%>%
 # rename(zoneid = hu12_zoneid)

#lagos_hu12_df <- as.data.frame(lagos_hu12)

lagos_geo_atm <- read.csv("C:/Users/cbarbosa/Documents/zone_atmosphere.csv")

atm_depos_N <- lagos_geo_atm %>%
  filter(variable_name == "totaldepnitrogen_kgperha" & spatial_division == "hu12")%>%
  rename(hu12_zoneid = zoneid)


atm_depos_N_final <- inner_join(atm_depos_N, d_mm, by= "hu12_zoneid")%>%
  dplyr::select(lagoslakeid, lake_namelagos, lake_lat_decdeg, lake_lon_decdeg, lake_elevation_m, lake_centroidstate, year,variable_name, value)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

##TN stream loads

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 


##filtering only study sites 

atm_depos_N_lakes <- left_join(upstream_sites_lagos, atm_depos_N_final, by= "lagoslakeid")%>%
  rename(water_year = year, totaldepnitrogen_kgperha = value)%>%
  mutate(totaldepnitrogen_kgperha=as.numeric(totaldepnitrogen_kgperha))%>%
  dplyr::select(lagoslakeid, water_year, totaldepnitrogen_kgperha, lake_lon_decdeg, lake_lat_decdeg)

total_Nload <- merge(atm_depos_N_lakes , TN_loads_ts)%>%
  select(lagoslakeid, water_year, totaldepnitrogen_kgperha, flux, lake_lon_decdeg, lake_lat_decdeg)%>%
  mutate(totTNload_gm2yr = ((totaldepnitrogen_kgperha + flux)*0.1))

