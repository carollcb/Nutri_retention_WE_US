## Exploring groundwater data from LAGOS-GEO

#baseflowindex_pct: mean within the zone of the percentage of streamflow that can be attributed to groundwater discharge into streams, calculated as baseflow divided by total flow; data from 1951 to 1980
#groundwaterrecharge_mmperyr: mean within the zone of annual groundwater recharge calculated as baseflow multiplied by mean annual runoff; data from 1951 to 1980
#runoff_inperyr: mean within the zone of annual runoff; data from 1951 to 1980

library(tidyverse)
library(sf)
library(mapview)

source("scripts/LAGOS_EDI.R")
source("scripts/LAGOS_EDI_characteristics.R")
source("scripts/06_plottingcorrelations_new.R")

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

#Looking baseflow
baseflowindex_pct <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "baseflowindex_pct")%>%
  rename(baseflowindex_pct = value)%>%
  mutate(area_huc12_ha = area_huc12 / 10000)%>%
  select(lagoslakeid, baseflowindex_pct, area_huc12_ha)

baseflowindex_pct_2 <- merge(baseflowindex_pct, lagos_lakearea)

#Looking recharge
groundwaterrecharge_mmperyr <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "groundwaterrecharge_mmperyr")%>%
  rename(groundwaterrecharge_mmperyr = value)%>%
  select(lagoslakeid, groundwaterrecharge_mmperyr, area_huc12)%>%
  mutate(groundwaterrecharge_mmperyr=as.numeric(groundwaterrecharge_mmperyr))%>%
  mutate(groundwaterrecharge_mperyr = groundwaterrecharge_mmperyr / 1000)

runoff_inperyr <- lagos_geo_groundw_lagoslakeid %>%
  group_by(lagoslakeid, variable_name, value) %>%
  filter(variable_name == "runoff_inperyr")%>%
  rename(runoff_inperyr = value)%>%
  mutate(runoff_inperyr=as.numeric(runoff_inperyr))%>%
  mutate(runoff_mperyr = runoff_inperyr / 39.37)%>%
  select(lagoslakeid, runoff_mperyr)

groundwater <- inner_join(groundwaterrecharge_mmperyr, runoff_inperyr, by="lagoslakeid")%>%
  select(lagoslakeid, groundwaterrecharge_mperyr, runoff_mperyr, area_huc12)%>%
  mutate(baseflow = groundwaterrecharge_mperyr/runoff_mperyr)%>%
  mutate(Qbaseflow_cubmperyr = area_huc12 * baseflow)

###compare groundwaterrecharge_mmperyr$Qgroundwaterrecarge_cubmperyr with TN_loads_lagos$flowm3y
list_files_TN <- paste0('data/results_TN/',list.files(path='data/results_TN/'),'/loadflex.csv')
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
  dplyr::select(lake_namelagos, lagoslakeid, water_year, fluxTN_kgy, flow_m3y, flow_station_area)

TN_stream_loads <- TN_loads_lagos %>%
  group_by(lagoslakeid, lake_namelagos, flow_station_area) %>%
  summarise(lt_flowcubmperyear = median(flow_m3y))%>%
  mutate(flow_station_area=as.numeric(flow_station_area))%>%
  mutate(station_area_sqm = flow_station_area * 1000000)%>%
  mutate(streamflow_mperyr = lt_flowcubmperyear/ station_area_sqm)


loadsTN_lagos <- inner_join(groundwater, TN_stream_loads, by="lagoslakeid")%>%
  dplyr::select(lake_namelagos, lagoslakeid, runoff_mperyr, baseflow, streamflow_mperyr)%>%
  rename(baseflow_mperyr = baseflow)

compare_columns <- function(data, col1, col2) {
  result <- data %>%
    mutate(groundwater_importance = .data[[col1]] > .data[[col2]])
  return(result)
}

loadsTN_lagos <- compare_columns(loadsTN_lagos, "baseflow_mperyr", "streamflow_mperyr")

ggplot()+ 
  geom_boxplot(data=loadsTN_lagos, aes(x=baseflow_mperyr,y=lake_namelagos),color='red') + 
  geom_boxplot(data=loadsTN_lagos, aes(x=streamflow_mperyr,y=lake_namelagos),color='blue') +
  ylab('Lake name')+xlab('Q (m3/s)')+
  ggtitle("Flow influence in the lakes HUC12: Red (baseflow), Blue (streamflow)")



## checking Alice's idea: rel_area = lake catchment/outlet watershed area and drainake_lake = 1 - rel_area
#I've done the overlap analysis in QGis
#From LAGOS-US data: 

#testing
lagos_watersh <- read.csv("D:/Datasets/Datasets/LAGOS-US/lake_watersheds.csv")%>%
  dplyr::select(lagoslakeid, ws_area_ha, nws_area_ha, ws_lat_decdeg,ws_lon_decdeg, nws_lat_decdeg,nws_lon_decdeg)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))
  
lagos_charact <- read.csv("D:/Datasets/Datasets/LAGOS-US/lake_characteristics.csv")%>%
  dplyr::select(lagoslakeid,lake_waterarea_ha)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

lagos_test <- inner_join(lagos_charact, lagos_watersh, by="lagoslakeid")

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) %>% distinct(lagoslakeid, .keep_all = TRUE) 


drainage_ratio <- inner_join(lagos_test,upstream_sites_lagos, by="lagoslakeid" )%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
mutate(ws_drain_ratio = (ws_area_ha/lake_waterarea_ha))%>%
  mutate(nws_drain_ratio = (nws_area_ha/lake_waterarea_ha))%>%
  dplyr::select(lagoslakeid, ws_drain_ratio, nws_drain_ratio)

ggplot(drainage_ratio , aes(x=ws_drain_ratio)) +
  scale_x_log10() + 
  geom_histogram(bins=3, colour="black", fill="brown")+
  ggtitle("Distribution of the drainage-ratio (log) in the study sites")

