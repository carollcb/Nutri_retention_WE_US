####-----Calculating TN removal time-series and merging with trophic state dataset

source("scripts/pulling-deposition-data.R")
source("scripts/lake_ts_Meyeretal-dataset.R")
source("scripts/06_plottingcorrelations_new.R")

library(tidyverse)
library(sf)
library(mapview)
library(ggmap)
library(patchwork)
library(cowplot)

Nretention <- total_Nload %>%
  mutate(TN_removal_gNm2yr =(10^(-0.27 + (0.82 * log(totTNload_gm2yr)))))

write.csv(Nretention, "data/Nretention_df.csv") #34 sites

Nretention_sp <- Nretention %>%
  st_as_sf(coords= c("lake_lon_decdeg", "lake_lat_decdeg"), crs= 4326)

ggplot(Nretention_sp, aes(x=totTNload_gm2yr, y=TN_removal_gNm2yr)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm) #now I'm curious to see this plot categorized by lake trophic status (similar Fig 1 - Finlay et al 2013)

##----Merging Hydrolakes coord and Meyer et 2023 dataset
#hydrol_us <- st_read("D:/Datasets/Datasets/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp")
#hydrol_us <- st_read("shps/joined_hydrolakes_lagos_Final.shp")
#hydrol_us <- st_read("D:/nutri_ret_shps/joined_hydrolakes_lagos_Final.shp")

hydrol_tot <- as.data.frame(hydrolakes_upstream_sites)

ts_hydro_us <- inner_join(dt1, hydrol_tot, by="Hylak_id")%>%
  dplyr::select(Hylak_id,lagoslakeid, year, categorical_ts, Lake_name, Pour_long, Pour_lat)%>%
  rename( water_year = year)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))
  #st_as_sf(coords= c("Pour_long", "Pour_lat"), crs= 4326)

#mapview(Nretention, size = 5) + mapview(ts_hydro_us)

Nretention_ts <- merge(Nretention, ts_hydro_us)%>%
  filter(lagoslakeid!= 457120) %>%
  distinct(lagoslakeid, water_year, .keep_all = T)%>%
  na.omit()

by(Nretention_ts,factor(Nretention_ts$categorical_ts),summary)

g1 <- ggplot(Nretention_ts, aes(x = categorical_ts, y = log(TN_removal_gNm2yr))) +
    geom_violin(aes(fill = categorical_ts), outlier.shape = NA) +
    geom_jitter( height = 0, width = 0.1) +
  stat_summary(fun.y = median, geom = "point", shape = 20, size = 3, color = "red", position = position_dodge(0.75)) +  # Add median points
  scale_fill_manual(values = c("#009E73", "#56B4E9")) +
    scale_color_gradient(low = "#FEFE62", high = "#D35FB7") +  # Color by log10 Pretention values
  #ggtitle("Distribution of the TN retention values based on TS") +
  theme_bw()+
  theme(legend.position = "none")


#ggsave("figures/TN_retention_lakes-trophic-state.png", width=8, height=6,units="in", dpi=300)

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

#Merging with ts data 
Pretention_ts <- merge(upstream_Pconc_hydro_lt, ts_hydro_us)%>%
  filter(lagoslakeid!= 457120) %>%
  distinct(lagoslakeid, water_year, .keep_all = T)%>%
  na.omit()

write.csv(upstream_Pconc_hydro_lt, "data/Pretention_df.csv") #24 sites

by(Pretention_ts,factor(Pretention_ts$categorical_ts),summary)

g2 <- ggplot(Pretention_ts, aes(x = categorical_ts, y = log(Pret_coef))) +
  geom_violin(aes(fill = categorical_ts), outlier.shape = NA) +
  geom_jitter( height = 0, width = 0.1) +
  stat_summary(fun.y = median, geom = "point", shape = 20, size = 3, color = "red", position = position_dodge(0.75)) +  # Add median points
  scale_fill_manual(values = c("#009E73", "#56B4E9")) +
  scale_color_gradient(low = "#FEFE62", high = "#D35FB7") +  # Color by log10 Pretention values
  #ggtitle("Distribution of the TP retention values based on TS") +
  theme_bw()+
  theme(legend.position = "none")

oligo_lakes <- Pretention_ts %>%
  group_by(lagoslakeid)%>%
filter(categorical_ts =="oligo")%>%
  add_count() %>%
  filter(n>=10) %>% #11
summarize(similar_rows = n())
  
eu_lakes <- Pretention_ts %>%
  group_by(lagoslakeid)%>%
  filter(categorical_ts =="eu/mixo")%>%
  add_count() %>%
  filter(n>=10) %>%#1
summarize(similar_rows = n())

multi <- plot_grid(g1,g2, ncol=2)
ggsave("figures/N-P-retention-TS.png", width=8, height=6,units="in", dpi=300)

#bottom <- plot_grid(g1,g2, labels=c('A','B'), ncol=2) #nest plots at the bottom row
#bottom #view the multi-panel figure


##Saving Final file
test <- test %>%
  select(water_year, lagoslakeid,  res_time_yr, total_Pin_ugl, totTNload_gm2yr, TN_removal_gNm2yr, Pret_coef, lake_lon_decdeg, lake_lat_decdeg)

test_sp <- test %>%  
  st_as_sf(coords= c("lake_lon_decdeg", "lake_lat_decdeg"), crs= 4326)

write.csv(test, "data/final_retention_dataset.csv")
#library(plainview)

# mapview + object
#mapview(test_sp, zcol="TN_removal_gNm2yr", at = seq(0, 242094243, 5000), legend = TRUE) 
