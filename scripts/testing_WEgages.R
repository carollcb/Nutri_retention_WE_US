library(raster)
library(rgdal)
library(ggplot2)
library(sf)
library(ggmap)
library(mapview)
library(tidyverse)

#WE_map <- st_read("D:/Datasets/Datasets/Western.shp")

###reading wq sparrow data
#TP
wq_tp <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/wq_tp.txt") #(sub-weekly) raw phosphorus concentration data from long-term gages

west <- c("pacific", "southwest", "midwest")
wq_tp_WE <- wq_tp %>%
  filter(region == west) 

#reading gauges
gage_tp <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/sitegage_tp.txt") #attributes of long-term gages incl. location, id, name, and drainage area

gage_flow <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/sitegage_q.txt") #attributes of long-term gages incl. location, id, name, and drainage area

#merging dfs and cutting only WE-US available data
#gage_west <- inner_join(wq_tp, gage_tp, by="station_id")

TP_gages_west <- gage_tp %>% #based only in long-term gages with TP data
  #group_by(lat, lon) %>%
  #filter(lon>-122&lon<-100 | lat>=28&lat<=50)
  filter(between(lon, -125, -100) & between(lat, 25, 50))%>%
  st_as_sf( coords= c("lon", "lat"),
                                   crs=4269)

# ---- detect and explore lake associated gages ----
if (!file.exists("data/test_allsparrow.rds")) {
  are_lake_gages <- jstapply(TP_gages_west$flow_station_id, function(x) tryCatch(is_lake_gage(x), error = function(e) FALSE))
  TP_gages_west$is_lake_gage <- unlist(lapply(are_lake_gages, function(x) tryCatch(x$is_lake_gage, error = function(e) FALSE)))
  saveRDS(TP_gages_west, "data/test_allsparrow.rds")
}
sitegage_tp_all <- readRDS("data/TP_flow_gauges.rds") #87 matches

TP_flow_gauges <- sitegage_tp_all %>%
  dplyr::filter(is_lake_gage == "TRUE")

sf::st_write(TP_flow_gauges, "TP_flow_gauges.gpkg", append = FALSE)

write.csv(TP_flow_gauges, "TP_gages_sparrow.csv")

ggplot()+
  geom_sf(data = true_test)+
  ggtitle("Map of TP gage locations in West US")#+
  #coord_sf(xlim=st_bbox(WE_map)[c(1,3)], ylim=st_bbox(WE_map)[c(2,4)], crs=st_crs(WE_map))

mapview(TN_flow_gauges, col.regions = "blue", color = "grey40") + mapview(TP_flow_gauges, col.regions = "brown") 

#data frequency
gauges_freq <- gage_west %>%
  group_by(station_id) %>%
  count(station_id) 

list_WETPgauges <- gauges_freq %>%
  dplyr::select(station_id)

numb_WEgauges <- nrow(gauges_freq)
#print
cat("Number of SPARROW gauges in Western US are :", numb_WEgauges)

avrg_n <- mean(gauges_freq$n)

ggplot(gauges_freq, aes(x=n)) + geom_histogram()+
  ggtitle("Frequency of monthly TP data in WE-US between 1999 and 2014")+ 
  geom_vline(aes(xintercept=avrg_n, color="red"),linetype="dashed")

#TN
wq_tn <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/wq_tn.txt")

wq_tn_WE <- wq_tn %>%
  filter(region == west) 

#reading gauges
gage_tn <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/sitegage_tn.txt")

#merging dfs and cutting only WE-US available data
#gage_west_tn <- inner_join(wq_tn_WE, gage_tn, by="station_id")

TN_gage_west <- gage_tn %>%
  #group_by(lat, lon) %>%
  #filter(lon>-122&lon<-100 | lat>=28&lat<=50)
  filter(between(lon, -125, -100) & between(lat, 25, 50))%>%
  st_as_sf( coords= c("lon", "lat"),
            crs=4269)

# ---- detect and explore lake associated gages ----
if (!file.exists("data/test_allsparrow_TN.rds")) {
  are_lake_gages <- jstapply(TN_gage_west$flow_station_id, function(x) tryCatch(is_lake_gage(x), error = function(e) FALSE))
  TN_gage_west$is_lake_gage <- unlist(lapply(are_lake_gages, function(x) tryCatch(x$is_lake_gage, error = function(e) FALSE)))
  saveRDS(TN_gage_west, "data/test_allsparrow_TN.rds")
}
sitegage_tn_all <- readRDS("data/TN_flow_gauges.rds") # 64 matches

TN_flow_gauges <- sitegage_tn_all %>%
  dplyr::filter(is_lake_gage == "TRUE")

#plot_gage_west_tn <- st_as_sf(gage_west_tn,
                          # coords= c("lon", "lat"),
                          # crs=4269)

sf::st_write(TN_flow_gauges, "TN_flow_gauges.gpkg", append = FALSE)

ggplot()+
  geom_sf(data = plot_gage_west_tn)+
  ggtitle("Map of TN gage locations in West US")#+
#coord_sf(xlim=st_bbox(WE_map)[c(1,3)], ylim=st_bbox(WE_map)[c(2,4)], crs=st_crs(WE_map))

mapview(plot_gage_west, color = "yellow") + mapview(plot_gage_west_tn, color = "blue")

ggmap(get_stamenmap(bbox=c(-125, 25, -100, 50), zoom = 5, 
                    maptype='toner'))+
  geom_point(data = gage_west, aes(x = lon, y = lat ),color="purple", size= 5, shape=21)+
  geom_point(data = gage_west_tn, aes(x = lon, y = lat ),color="red", size= 3, shape=21)+
  ggtitle("SPARROW gages with TN (R) and TP (P) data in WE-US")+
  theme_classic()  

#data frequency
gauges_freq_TN <- gage_west_tn %>%
  group_by(station_id) %>%
  count(station_id) 

numb_WEgauges_TN <- nrow(gauges_freq_TN)
#print
cat("Number of TN SPARROW gauges in Western US are :", numb_WEgauges_TN)

avrg_n2 <- mean(gauges_freq_TN$n)

ggplot(gauges_freq_TN, aes(x=n)) + geom_histogram()+
  ggtitle("Frequency of monthly TN data in WE-US between 1999 and 2014")+ 
  geom_vline(aes(xintercept=avrg_n2, color="red"),linetype="dashed")                  
                  
  
### Look into Sparrow output

load_est_tp <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/load_est_tp.txt") %>% #607 TP loads
  #filter(region == west) %>%
  filter(between(lon, -125, -100) & between(lat, 25, 50))%>%
  st_as_sf( coords= c("lon", "lat"),
            crs=4269)

mapview(load_est_tp)

sf::st_write(load_est_tp, "WE_load_est_TP.gpkg", append = FALSE)

load_est_tn <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/load_est_tn.txt") %>% #453 TN loads
  #filter(region == west) %>%
  filter(between(lon, -125, -100) & between(lat, 25, 50))%>%
  st_as_sf( coords= c("lon", "lat"),
            crs=4269)

load_est_tn_tp <- st_intersection(load_est_tp, load_est_tn)

mapview(load_est_tn, zcol="flow_station_id")+ mapview(load_est_tp)

sf::st_write(load_est_tn, "WE_load_est_TN.gpkg", append = FALSE)

#joining TP_flow_gauges with SPARROW estimated load
sparrow_out_TP_flow_gauges <- st_join(TP_flow_gauges, load_est_tp) #89

#joining TN_flow_gauges with SPARROW estimated load
sparrow_out_TN_flow_gauges <- st_join(TN_flow_gauges, load_est_tn) #66

#checking available data
TP_flow_gauges_sites <- TP_flow_gauges %>%
  summarize(station_id) 

testinf_flow_data <- read.csv("D:/Datasets/Datasets/SPARROW_allUSA/flow_files_all/01074520.rdb")
 
TP_flow_gauges_sites_df <- as.data.frame(TP_flow_gauges_sites)

wq_tp_sites <- unique(wq_tp$station_id)

wq_tp_sites <- as.data.frame(wq_tp_sites) %>%
  rename(station_id = wq_tp_sites)

wp_tp_TP_flow_gauges <- sitegage_tn_all %>%
  dplyr::filter(is_lake_gage == "TRUE")

available_data_tp <- inner_join(wq_tp_sites, TP_flow_gauges_sites_df, by="station_id")

mapview(TP_flow_gauges)+ mapview(lagos_lakes)
