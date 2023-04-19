###Code to pull over Hydrolakes data [ Res_time (days => Vol_total/Disc_avg), Depth_avg (m)] + Wu et al 2022 and LAGOS_lakeid in oder to calculate nutrient retetion based on Vollenweider 1976 and Harrison et al 2009 eq

source("scripts/99_utils.R")

#st_layers("C:/Users/cbarbosa/Documents/HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb")
#hydrolakes_raw <- st_read("C:/Users/cbarbosa/Documents/HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb")%>%
# filter(Country == "United States of America")
#hydrolakes_resT_depthA <- hydrolakes_raw %>%
#  dplyr::select(Lake_name, Res_time, Depth_avg, Pour_long, Pour_lat, Shape_Length, Shape_Area, Shape) #EPSG",4326
#st_write(hydrolakes_resT_depthA, "hydrolakes_NA.shp")

##Trying to filter based on Western shp

#WE_shp <- st_read("D:/Datasets/Datasets/Western.shp") #ESRI 102039 USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
#WE_shp_reproj <- st_transform(WE_shp, 4326)
#hydrolakes_WE <- st_crop(hydrolakes_resT_depthA, WE_shp_reproj)
#df_LCMAP_1985_WY <- as.data.frame(LCMAP_1985_WY, xy=TRUE)

##I've cropped it in QGIs and was also able to merge hydrolakes data and Wu_Lagos_lake data only in QGIs!!

#hydrolakes_West <- st_read("hydrolakes_WE_US.shp")
#hydrolakes_West_df <- as.data.frame(hydrolakes_West, xy=TRUE) %>%
#  rename(lk_lt_d = Pour_lt, lk_ln_d = Por_lng)
#Wu_Lagos_lakes <- st_read("Wu_lagos_lakes.shp")
#Wu_Lagos_lakes_reproj <- st_transform(Wu_Lagos_lakes, 4326)
#Wu_Lagos_lakes_df <- as.data.frame(Wu_Lagos_lakes_reproj, xy=TRUE)
#Wu_Lagos_lakes_hydro <- inner_join(hydrolakes_West_df, Wu_Lagos_lakes_df, by="lk_lt_d")

Wu_Lagos_lakes_hydro <- st_read("Wu_Lagos_lakes_hydro.shp")%>%
  rename(lagoslakeid = lagslkd)

candidate_sites_WE <- readRDS("data/TP_flow_gauges.rds") %>%
  mutate(flow_station_id = as.character(flow_station_id)) %>%
  dplyr::filter(is_lake_gage == "TRUE")%>%
  dplyr::filter(station_id == flow_station_id)%>%
  rename(sites = flow_station_id)

library(mapview)
mapview(Wu_Lagos_lakes_hydro) + mapview(candidate_sites_WE)
  
LAGOS_WE_sp <- st_as_sf(dt1,
                        coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
                        crs=4326) %>%
  filter(lake_centroidstate %in% c("CA", "UT", "NV",
                                   "WA", "OR", "ID",
                                   "MT", "WY", "CO",
                                   "NM", "AZ")) %>%
  mutate(lagoslakeid=factor(lagoslakeid))

ggplot()+
  geom_sf(data= LAGOS_WE_sp)+
  geom_sf(data = candidate_sites_WE, color="red")

mapview(LAGOS_WE_sp) + mapview(candidate_sites_WE, color = "yellow") #procurar code pra aumentar size do segundo

 