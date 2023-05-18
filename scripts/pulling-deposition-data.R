##Looking into deposition data from LAGOS-GEO

library(tidyverse)

source("scripts/LAGOS_EDI.R")

#lagos_hu12 <- st_read('D:/Datasets/Datasets/LAGOS_GEO/gis_geo_v1.0.gpkg', layer="hu12")%>%
 # rename(zoneid = hu12_zoneid)

#lagos_hu12_df <- as.data.frame(lagos_hu12)

lagos_geo_atm <- read.csv("D:/Datasets/Datasets/LAGOS_GEO/zone_atmosphere.csv")

atm_depos_N <- lagos_geo_atm %>%
  filter(variable_name == "totaldepnitrogen_kgperha" & spatial_division == "hu12")%>%
  rename(hu12_zoneid = zoneid)


atm_depos_N_final <- inner_join(atm_depos_N, dt1, by= "hu12_zoneid")%>%
  select(lagoslakeid, lake_namelagos, lake_lat_decdeg, lake_lon_decdeg, lake_elevation_m, lake_centroidstate, year,variable_name, value)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

##TN stream loads

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 


list_files_TN <- paste0('data/results_TN/',list.files(path='data/results_TN/'),'/loadflex.csv') # list full paths
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
  select(lagoslakeid, water_year, fluxTN_kgy)

##filtering only study sites 

atm_depos_N_lakes <- left_join(upstream_sites_lagos, atm_depos_N_final, by= "lagoslakeid")%>%
  rename(water_year = year, totaldepnitrogen_kgperha = value)%>%
  mutate(totaldepnitrogen_kgperha=as.numeric(totaldepnitrogen_kgperha))%>%
  select(lagoslakeid, water_year, totaldepnitrogen_kgperha)

total_Nload <- merge(atm_depos_N_lakes , TN_loads_lagos)%>%
  mutate(totTNload_gm2yr = ((totaldepnitrogen_kgperha + fluxTN_kgy)*0.1))

