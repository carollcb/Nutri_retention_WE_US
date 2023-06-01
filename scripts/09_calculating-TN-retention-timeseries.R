####-----Calculating TN removal time-series and merging with trophic state dataset

source("scripts/pulling-deposition-data.R")
source("scripts/lake_ts_Meyeretal-dataset.R")

library(tidyverse)
library(sf)
library(mapview)
library(ggmap)
library(patchwork)

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

g1 <- ggplot(Nretention_ts, aes(y=TN_removal_gNm2yr, x=categorical_ts)) +
  geom_boxplot(aes(fill = categorical_ts), outlier.shape = NA)  + geom_jitter(height = 0, width = 0.1)+
  scale_y_continuous(limits = quantile(Nretention_ts$TN_removal_gNm2yr, c(0.1, 0.9)))+
  ggtitle("N removal in gNm-2yr-1 based on lakes and reservoirs trophic state")+
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

####-----Calculating TP retention coefficient time-series and merging with trophic state dataset
upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
)

##Wait for the daily interpolated P conc data(Alice comment: "make sure the loads consider daily discharge (interpolated P concentrations * daily discharge)
phosphorus_conc_files <- list.files(path= "data/new_Q_TP_TN/",pattern = ".rds", full.names = T)%>%
  map(readRDS) %>% 
  data.table::rbindlist(fill=TRUE)%>%
  rename(flow_station_id = site_no)

phosphorus_conc_files$year <- year(phosphorus_conc_files$date_time)

phosphorus_conc_yrs <- phosphorus_conc_files %>%
  na.omit() %>%
  group_by(phosphorus_mgl, date_time, flow_station_id) %>%
  dplyr::summarize(interpolated_TP = approxfun(phosphorus_mgl))%>%
  na.omit()%>%
  mutate(annual_median_TP_ugl = annual_median_TP*1000)%>%
  mutate(q_m3s = flow_cfs /35.315)


P_retention <- phosphorus_conc_yrs%>%
  group_by(station_id, year) %>%
  mutate(Pin = (sum(phosphorus_mgl)/(sum(q_m3s)))) %>%
  mutate(Pret_coef = ((1-(1.43/Pin))*((Pin)/(1 + (res_time_yr^0.5)))^0.88))%>% #nondimensional?
  #rename(flow_station_id = site_no)
  na.omit()


#Merging with ts data - check it again later!
Pretention_lakes <- left_join(upstream_sites_lagos, upstream_conc_hydro, by= "station_id")%>%
  rename(water_year = year)%>%
  mutate(Pret_coef=as.numeric(Pret_coef))%>%
  select(lagoslakeid, water_year, Pret_coef, annual_median_TP_ugl)%>% 
  #rename(flow_station_id = site_no)
  na.omit()

Pretention_ts <- merge(Pretention_lakes, ts_hydro_us)

by(Pretention_ts,factor(Pretention_ts$categorical_ts),summary)

g2 <- ggplot(Pretention_ts, aes(y=Pret_coef, x=categorical_ts)) +
  geom_boxplot(aes(fill = categorical_ts), outlier.shape = NA)  + geom_jitter(height = 0, width = 0.1)+
  #scale_y_continuous(limits = quantile(Pretention_ts$Pret_coef, c(0.1, 0.9)))+
  ggtitle("P retention coeficient based on lakes and reservoirs trophic state")+
  theme_bw()

g1 + g2
