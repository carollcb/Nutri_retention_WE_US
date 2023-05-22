####-----Calculating TN removal time-series and merging with trophic state dataset

source("scripts/pulling-deposition-data.R")
source("scripts/lake_ts_Meyeretal-dataset.R")

library(tidyverse)
library(sf)
library(mapview)
library(ggmap)

Nretention <- total_Nload %>%
  mutate(TN_removal_gNm2yr =(10^(-0.27 + (0.82 * log(totTNload_gm2yr)))))

Nretention_sp <- Nretention %>%
  st_as_sf(coords= c("lake_lon_decdeg", "lake_lat_decdeg"), crs= 4326)

ggplot(Nretention_sp, aes(x=totTNload_gm2yr, y=TN_removal_gNm2yr)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm) #now I'm curious to see this plot categorized by lake trophic status (similar Fig 1 - Finlay et al 2013)

##----Merging Hydrolakes coord and Meyer et 2023 dataset
#hydrol_us <- st_read("D:/Datasets/Datasets/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp")
hydrol_us <- st_read("shps/joined_hydrolakes_lagos.shp")

hydrol_tot <- as.data.frame(hydrol_us)

ts_hydro_us <- inner_join(dt1, hydrol_tot, by="Hylak_id")%>%
  select(Hylak_id,lagoslakei, year, categorical_ts, Lake_name, Pour_long, Pour_lat)%>%
  rename(lagoslakeid = lagoslakei, water_year = year)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))
  #st_as_sf(coords= c("Pour_long", "Pour_lat"), crs= 4326)

#mapview(Nretention, size = 5) + mapview(ts_hydro_us)

Nretention_ts <- merge(Nretention, ts_hydro_us)

by(Nretention_ts,factor(Nretention_ts$categorical_ts),summary)

ggplot(Nretention_ts, aes(y=TN_removal_gNm2yr, x=categorical_ts)) +
  geom_boxplot(aes(fill = categorical_ts), outlier.shape = NA)  + geom_jitter(height = 0, width = 0.1)+
  scale_y_continuous(limits = quantile(Nretention_ts$TN_removal_gNm2yr, c(0.1, 0.9)))+
  theme_bw()

Nretention_ts_sp <- Nretention_ts%>%
  st_as_sf(coords= c("lake_lon_decdeg", "lake_lat_decdeg"), crs= 4326)

#p <- ggmap(get_stamenmap(bbox=c(-125, 25, -100, 50), zoom = 5, 
 #                        maptype='toner',
  #                      # maptype ='terrain',
   #                      color = 'color'))

#p + geom_point(aes(x = lake_lon_decdeg, y = lake_lat_decdeg,  color = categorical_ts, size = TN_removal_gNm2yr), data = Nretention_ts) +
 # theme(legend.position="left")+
 # scale_size(breaks=c(10000,1000000, 1000000000),guide="legend")
