## Exploring groundwater data from LAGOS-GEO

#groundwaterrecharge_mmperyr: mean within the zone of annual groundwater recharge calculated as baseflow multiplied by mean annual runoff; data from 1951 to 1980
#baseflowindex_pct: mean within the zone of the percentage of streamflow that can be attributed to groundwater discharge into streams, calculated as baseflow divided by total flow; data from 1951 to 1980
#runoff_inperyr: mean within the zone of annual runoff; data from 1951 to 1980

library(tidyverse)
library(sf)

source("scripts/LAGOS_EDI.R")

lagos_hu12 <- st_read('D:/Datasets/Datasets/LAGOS_GEO/gis_geo_v1.0.gpkg', layer="hu12")
# rename(zoneid = hu12_zoneid) Use shape_area to calculate Q!

lagos_geo_connect <- read.csv("D:/Datasets/Datasets/LAGOS_GEO/zone_connectivity.csv")%>%
  rename(hu12_zoneid = zoneid)

lagos_geo_groundw <- filter(lagos_geo_connect, variable_name %in% c("groundwaterrecharge_mmperyr", "baseflowindex_pct","runoff_inperyr") & spatial_division == "hu12")

lagos_geo_groundw_lagoslakeid <- inner_join(lagos_geo_groundw, d_mm, by= "hu12_zoneid")%>%
  select(lagoslakeid, lake_namelagos, lake_lat_decdeg, lake_lon_decdeg, lake_elevation_m, lake_centroidstate, variable_name, value)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

baseflowindex_pct <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "baseflowindex_pct")%>%
  rename(baseflowindex_pct = value)%>%
  select(lagoslakeid, baseflowindex_pct)

groundwaterrecharge_mmperyr <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "groundwaterrecharge_mmperyr")%>%
  rename(groundwaterrecharge_mmperyr = value)%>%
  select(lagoslakeid, groundwaterrecharge_mmperyr)

runoff_inperyr <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "runoff_inperyr")%>%
  rename(runoff_inperyr = value)%>%
  select(lagoslakeid, runoff_inperyr)

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 

baseflow_lagos <- inner_join(upstream_sites_lagos, baseflowindex_pct , by = "lagoslakeid")
groundwaterrecharge_lagos <- inner_join(baseflow_lagos, groundwaterrecharge_mmperyr , by = "lagoslakeid")%>%
  mutate(groundwaterrecharge_mmperyr= as.numeric(groundwaterrecharge_mmperyr))%>%
  mutate(groundwaterrecharge_myr = groundwaterrecharge_mmperyr / 1000) ##multiplicar pela area da bacia em huc12 pra ter vazao??


