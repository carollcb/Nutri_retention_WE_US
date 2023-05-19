source("scripts/pulling-deposition-data.R")
source("scripts/lake_ts_Meyeretal-dataset.R")

library(tidyverse)
library(sf)

Nretention <- total_Nload %>%
  mutate(TN_removal_gNm2yr =(10^(-0.27 + (0.82 * log(totTNload_gm2yr)))))%>%
  st_as_sf(coords= c("lake_lon_decdeg", "lake_lat_decdeg"), crs= 4326)

ggplot(Nretention, aes(x=totTNload_gm2yr, y=TN_removal_gNm2yr)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm) #now I'm curious to see this plot categorized by lake trophic status (similar Fig 1 - Finlay et al 2013)

#Merging Hydrolakes coord and Meyer et 2023 dataset
hydrol_us <- st_read("D:/Datasets/Datasets/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp")

hydrol_tot <- as.data.frame(hydrol_us)

ts_hydro_us <- merge(dt1, hydrol_tot)%>%
  select(Hylak_id, year, categorical_ts, Lake_name, Pour_long, Pour_lat)%>%
  st_as_sf(coords= c("Pour_long", "Pour_lat"), crs= 4326)

#Merging Meyer et al 2023 dataset and lagos ids
lagos_ids <- st_read('D:/Datasets/Datasets/LAGOS-US/gis_locus_v1.0_gpkg/gis_locus_v1.0.gpkg', layer="lake")

lagos_ids <- st_transform(lagos_ids, crs=4326)

join_lagos_N_retention <- st_join(lagos_ids, Nretention)

merging_ts_hydro_us_lagos <- st_join(lagos_ids, ts_hydro_us)
#st_make_valid?

#Hylak_id ?
