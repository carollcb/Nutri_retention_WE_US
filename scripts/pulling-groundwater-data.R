## Exploring groundwater data from LAGOS-GEO

#groundwaterrecharge_mmperyr: mean within the zone of annual groundwater recharge calculated as baseflow multiplied by mean annual runoff; data from 1951 to 1980
#baseflowindex_pct: mean within the zone of the percentage of streamflow that can be attributed to groundwater discharge into streams, calculated as baseflow divided by total flow; data from 1951 to 1980
#runoff_inperyr: mean within the zone of annual runoff; data from 1951 to 1980

library(tidyverse)
library(sf)

source("scripts/LAGOS_EDI.R")
source("scripts/LAGOS_EDI_characteristics.R")

lagos_lakearea <- dt2 %>%
  select(lagoslakeid, lake_totalarea_ha)

lagos_hu12 <- st_read('D:/Datasets/Datasets/LAGOS_GEO/gis_geo_v1.0.gpkg', layer="hu12")
# rename(zoneid = hu12_zoneid) Use shape_area to calculate Q!

lagos_geo_connect <- read.csv("D:/Datasets/Datasets/LAGOS_GEO/zone_connectivity.csv")%>%
  rename(hu12_zoneid = zoneid)

lagos_geo_groundw <- filter(lagos_geo_connect, variable_name %in% c("groundwaterrecharge_mmperyr", "baseflowindex_pct","runoff_inperyr") & spatial_division == "hu12")

lagos_geo_groundw_area <-  inner_join(lagos_geo_groundw, lagos_hu12, by= "hu12_zoneid")


lagos_geo_groundw_lagoslakeid <- inner_join(lagos_geo_groundw_area, d_mm, by= "hu12_zoneid")%>%
  select(lagoslakeid, lake_namelagos, lake_lat_decdeg, lake_lon_decdeg, lake_elevation_m, lake_centroidstate, variable_name, value, Shape_Area, hu12_zoneid)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))%>%
  rename(area_huc12 = Shape_Area)

baseflowindex_pct <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "baseflowindex_pct")%>%
  rename(baseflowindex_pct = value)%>%
  mutate(area_huc12_ha = area_huc12 / 10000)%>%
  select(lagoslakeid, baseflowindex_pct, area_huc12_ha)

baseflowindex_pct_2 <- merge(baseflowindex_pct, lagos_lakearea)

groundwaterrecharge_mmperyr <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "groundwaterrecharge_mmperyr")%>%
  rename(groundwaterrecharge_mmperyr = value)%>%
  select(lagoslakeid, groundwaterrecharge_mmperyr, area_huc12)%>%
  mutate(groundwaterrecharge_mmperyr=as.numeric(groundwaterrecharge_mmperyr))%>%
  mutate(groundwaterrecharge_mperyr = groundwaterrecharge_mmperyr / 1000)%>%
  mutate(Qgroundwaterrecarge_cubmperyr = area_huc12 * groundwaterrecharge_mperyr)


###compare groundwaterrecharge_mmperyr$Qgroundwaterrecarge_cubmperyr with TN_loads_lagos$flowm3y
TN <- data.frame() # create empty dataframe


for(c in 1:length(list_files_TN)) {
  tmp <- read.csv(list_files_TN[c]) |> # read in each csv
    # annoying workaround that I came up with to provide unique IDs
    mutate(id = list_files_TN[c],
           id = gsub('data/results_TN/', '', id),
           id = gsub('/loadflex.csv','',id))
  
  TN <- bind_rows(TN, tmp) # bind into a single dataframe
}


##yearly loads data

nitrogen_loads <- TN %>%
  rename(station_id = id)%>%
  rename(fluxTN_kgy = flux_kgy) 

TN_loads_lagos <- inner_join(upstream_sites_lagos, nitrogen_loads , by= "station_id")%>%
  dplyr::select(lake_namelagos, lagoslakeid, water_year, fluxTN_kgy, flow_m3y)

TN_stream_loads <- TN_loads_lagos %>%
  group_by(lagoslakeid, lake_namelagos) %>%
  summarise(lt_flowcubmperyear = median(flow_m3y))

loadsTN_lagos <- inner_join(groundwaterrecharge_mmperyr, TN_stream_loads, by="lagoslakeid")%>%
  dplyr::select(lake_namelagos, lagoslakeid, lt_flowcubmperyear, Qgroundwaterrecarge_cubmperyr)%>%
  mutate(groundwater_import = Qgroundwaterrecarge_cubmperyr - lt_flowcubmperyear)
  #filter(groundwater_import <0)

###25 lakes groundwater > streamflow, 15 lakes groundwater < streamflow

ggplot()+ 
  geom_boxplot(data=loadsTN_lagos, aes(x=Qgroundwaterrecarge_cubmperyr,y=lake_namelagos),color='red') + 
  geom_boxplot(data=loadsTN_lagos, aes(x=lt_flowcubmperyear,y=lake_namelagos),color='blue') +
  ylab('Lake name')+xlab('Q (m3/s)')+
  ggtitle("Flow influence in the lakes HUC12: Red (groundwater), Blue (streamflow)")

##keeping checking from here!
runoff_inperyr <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "runoff_inperyr")%>%
  rename(runoff_inperyr = value)%>%
  select(lagoslakeid, runoff_inperyr, area_huc12)









upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 

baseflow_lagos <- inner_join(upstream_sites_lagos, baseflowindex_pct , by = "lagoslakeid")
groundwaterrecharge_lagos <- inner_join(baseflow_lagos, groundwaterrecharge_mmperyr , by = "lagoslakeid")%>%
  mutate(groundwaterrecharge_mmperyr= as.numeric(groundwaterrecharge_mmperyr))%>%
  mutate(groundwaterrecharge_myr = groundwaterrecharge_mmperyr / 1000) ##multiply to huc12 area to get Q??


