library(tidyverse)

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 


list_files <- paste0('data/results_new_Q/',
                     list.files(path='data/results_new_Q/'),'/loadflex.csv') # list full paths
TNP <- data.frame() # create empty dataframe


for(c in 1:length(list_files)) {
  tmp <- read.csv(list_files[c]) |> # read in each csv
    # annoying workaround that I came up with to provide unique IDs
    mutate(id = list_files[c],
           id = gsub('data/results_new_Q/', '', id),
           id = gsub('/loadflex.csv','',id))
  
  TNP <- bind_rows(TNP, tmp) # bind into a single dataframe
}

##yearly loads data

nutrient_loads_lagos <- inner_join(upstream_sites_lagos, 
                                   rename(TNP, station_id = id),
                                   by= "station_id") %>%
    rename(fluxTN_kgy = flux_TN_kgy,
           fluxTP_kgy = flux_TP_kgy)


##long-term loads data

lt_nutrient_loads_lagos <- TNP %>%
  rename(station_id = id)%>%
  group_by(station_id) %>%
  summarise(lt_fluxTP_kgy = median(flux_TP_kgy),
            lt_fluxTP_kgy_se = median(flux_TP_kgy_se, na.rm = T),
            lt_fluxTN_kgy = median(flux_TN_kgy),
            lt_fluxTN_kgy_se = median(flux_TN_kgy_se, na.rm = T)) %>%
  inner_join(upstream_sites_lagos, by= "station_id")


phosphorus_loads <- nutrient_loads_lagos
nitrogen_loads <- nutrient_loads_lagos
