library(tidyverse)

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




list_files_TP <- paste0('data/results_TP/',list.files(path='data/results_TP/'),'/loadflex.csv') # list full paths
TP <- data.frame() # create empty dataframe


for(c in 1:length(list_files_TP)) {
  tmp <- read.csv(list_files_TP[c]) |> # read in each csv
    # annoying workaround that I came up with to provide unique IDs
    mutate(id = list_files_TP[c],
           id = gsub('data/results_TP/', '', id),
           id = gsub('/loadflex.csv','',id))
  
  TP <- bind_rows(TP, tmp) # bind into a single dataframe
}

##yearly loads data

phosphorus_loads <- TP %>%
  rename(station_id = id)%>%
  rename(fluxTP_kgy = flux_kgy) 

nitrogen_loads <- TN %>%
  rename(station_id = id)%>%
  rename(fluxTN_kgy = flux_kgy) 


TP_loads_lagos <- inner_join(upstream_sites_lagos, phosphorus_loads , by= "station_id")

TN_loads_lagos <- inner_join(upstream_sites_lagos, nitrogen_loads , by= "station_id")

##Figure out how to merge them together!! 
nutrient_loads_lagos <- right_join(TP_loads_lagos, TN_loads_lagos , by= "station_id")%>%
  group_by(station_id, water_year.x, water_year.y)%>%
  select(station_id, lon.x,   lat.x, lagoslakeid.x,lake_namelagos.x, fluxTP_kgy,fluxTN_kgy, water_year.x)%>%
  rename(lon = lon.x, lat= lat.x, lagoslakeid = lagoslakeid.x, lake_namelagos = lake_namelagos.x, water_year = water_year.x)


##long-term loads data

phosphorus_loads_lt <- TP %>%
  rename(station_id = id)%>%
  rename(fluxTP_kgy = flux_kgy) %>%
  group_by(station_id) %>%
  summarise(lt_fluxTP_kgy = median(fluxTP_kgy))

nitrogen_loads_lt <- TN %>%
  rename(station_id = id)%>%
  rename(fluxTN_kgy = flux_kgy) %>%
  group_by(station_id) %>%
  summarise(lt_fluxTN_kgy = median(fluxTN_kgy))


TP_loads_lagos <- inner_join(upstream_sites_lagos, phosphorus_loads_lt , by= "station_id")

TN_loads_lagos <- inner_join(upstream_sites_lagos, nitrogen_loads_lt , by= "station_id")

lt_nutrient_loads_lagos <- right_join(TP_loads_lagos, TN_loads_lagos , by= "station_id")%>%
  select(station_id, lon.x,   lat.x, lagoslakeid.x,lake_namelagos.x, lt_fluxTP_kgy,lt_fluxTN_kgy)%>%
  rename(lon = lon.x, lat= lat.x, lagoslakeid = lagoslakeid.x, lake_namelagos = lake_namelagos.x)
 
