###Plottings to check nutrient retention estimation
library(sf)


##Nitrogen
#Plotting N retention (y axis, gm2y-1 based on the eq above) x N loading (x axis, gm2y-1)
#result => r2=1

nitrogen_loads <- list.files(path= "data/results_TN/",pattern = ".csv", full.names = T)%>%
  map(read.csv) %>% 
  data.table::rbindlist()
 

##Phosphorus
#Plotting residence time (yr, y axis) x inflow P conc (x axis, µg l−1)

phosphorus_conc_files <- list.files(path= "data/nwis/",pattern = ".rds", full.names = T)%>%
  map(readRDS) %>% 
    data.table::rbindlist(fill=TRUE)%>%
   mutate(phosphorus_mgl = as.numeric(phosphorus_mgl))
#dplyr::select(site_no,  date_time, flow_cfs, TN_mgl)%>%
#  mutate(TN_mgl = as.numeric(TN_mgl))

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                              colClasses = "character",
                                              stringsAsFactors = FALSE
) 

#Filter hydrolakes to North America and get only res time data
hydrolakes <- st_read("D:/Datasets/Datasets/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp") %>% 

hydrolakes_NA <- hydrolakes %>%  
  filter(Country == "United States of America")%>%
  dplyr::select(Lake_name, Res_time, Pour_long, Pour_lat)%>%
  rename(lake_namelagos = Lake_name)%>%
  st_as_sf(coords= c("Pour_long", "Pour_lat"),
           crs=4326)

st_write(hydrolakes_NA, "shps/hydrolakes_US.shp")

#hydrolakes_upstream_sites <- inner_join(upstream_sites_lagos, hydrolakes_NA, by="lake_namelagos")

#lagos_res_upst <- inner_join(upstream_sites_lagos, Wu_Lagos_lakes_hydro_sp, by="lagoslakeid")
