##---Testing time-series analysis on TP and TN annual loads----##
library(tidyverse)
library(patchwork)
library(Kendall)
library(broom) 
library(trend)
library(zip)
library(purrr)
library(ggpubr)
library(sf)
library(mapview)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)

# source("scripts/07_organizing-nutrient-loads-df.R")
source("scripts/07_organizing-nutrient-loads-df_newQ.R")
source("scripts/LAGOS_EDI.R")

#For TP loads
TP_loads_ts <- nutrient_loads_lagos_TP %>%
  group_by(lagoslakeid, water_year)%>%
  dplyr::mutate(total_fluxTP_kgy = sum(fluxTP_kgy))%>%
  dplyr::select(station_id, lagoslakeid, water_year, total_fluxTP_kgy) %>%
  mutate(nutrient="TP") %>%
  rename(flux=total_fluxTP_kgy)%>%
  drop_na()


#For TN loads
TN_loads_ts <- nutrient_loads_lagos_TN %>%
  group_by(lagoslakeid, water_year)%>%
  dplyr::mutate(total_fluxTN_kgy = sum(fluxTN_kgy))%>%
  dplyr::select(station_id, lagoslakeid, water_year, total_fluxTN_kgy) %>%
  mutate(nutrient="TN") %>%
  rename(flux=total_fluxTN_kgy)%>%
  drop_na()


#All loads
all_loads_ts <- bind_rows(TP_loads_ts, TN_loads_ts) %>%
  group_by(station_id, lagoslakeid, nutrient) %>%
  add_count() %>%
  filter(n>=10)%>% #exclude sites with less than 10 years of data
  filter(lagoslakeid!= 457120) 

##########################################################################################
##Functions for calculating sens slopes and intercepts for plotting later
##########################################################################################
map_sens <- function(df) {
  sens.slope(df$flux)
}


sens_slope <- function(mod) {
  mod$estimate[[1]]
}

#https://kevintshoemaker.github.io/NRES-746/TimeSeries_all.html
#For getting sens intercept, helpful for plotting later
map_zyp <- function(df) {
  zyp::zyp.sen(flux ~ water_year, df)
}


sens_intercept <- function(mod) {
  mod$coefficients[[1]] # pull out y-int estimate for ploting
}


## Trends in TN and TP
nutrient_loads_nested <- all_loads_ts %>%
  group_by(lagoslakeid, nutrient) %>%
  nest() %>%
  mutate(
    sens = map(data, map_sens),
    sens_sum = map(sens, broom::glance),
    slope = map(sens, sens_slope),
    zyp_mod = map(data, map_zyp),
    intercept = map(zyp_mod, sens_intercept)
  )

## Un-nest trends in TN and TP
nutrient_loads_unnested = unnest(nutrient_loads_nested, c(sens_sum, slope, nutrient)) %>%
  mutate(
    Trend = case_when(
      p.value <= 0.05 & slope >= 0 ~ 'increasing',
      p.value <= 0.05 & slope <= 0 ~ 'decreasing',
      p.value > 0.05 ~ 'no trend'
    ),
    Trend = factor(Trend,
                       levels = c('no trend',
                                  'increasing',
                                  'decreasing'))
  )

#Which sites are trending?
trending_sites <- nutrient_loads_unnested %>%
  ungroup() %>%
  filter(!Trend=="no trend") %>%
  dplyr::select(lagoslakeid, nutrient, slope, Trend)

flux_slope_intercept <- nutrient_loads_unnested %>%
  filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
  dplyr::select(lagoslakeid, slope, intercept) %>%
  mutate(intercept=as.numeric(intercept)) %>%
  ungroup()

#Plot them together
ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(lagoslakeid %in% trending_sites$lagoslakeid),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
  facet_wrap(nutrient~lagoslakeid, scales="free")  +
  # facet_grid(lagoslakeid~nutrient, scales = "free_y") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept,
              mapping=aes(intercept = intercept, slope = slope, group=lagoslakeid,
                          color=nutrient)) + #MK estimate trend
  geom_smooth(data = all_loads_ts %>%
                filter(lagoslakeid %in% trending_sites$lagoslakeid), method="lm", se=F,
              mapping=aes(y=flux,x=water_year), color="black", linetype="dashed")+
  scale_color_manual(values=c("darkblue","gold"))+
  scale_fill_manual(values=c("darkblue","gold"))+
  labs(y="Annual nutrient loads (kgy-1)", x="Water Year")


#Plot them separately

#Total P
ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
               filter(nutrient=="TP") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
  facet_wrap(.~lagoslakeid, scales="free_y")  +
  # facet_grid(lagoslakeid~nutrient, scales = "free_y") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TP"),
              mapping=aes(intercept = intercept, slope = slope, group=lagoslakeid,
                          color=nutrient)) + #MK estimate trend
  # geom_smooth(data = all_loads_ts %>%
  #               filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
  #               filter(nutrient=="TP"), method="lm", se=F,
  #             mapping=aes(y=flux,x=water_year), color="black", linetype="dashed")+ #LM trend-- you can see how these differ with the outliers
  scale_color_manual(values=c("darkblue","gold"))+
  scale_fill_manual(values=c("darkblue","gold"))+
  labs(y="Annual TP loads (kgy-1)", x="Water Year")+
  theme_pubr()+
  theme(legend.position = "none",
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))
ggsave("figures/TP_load_trends.png", width=8, height=6,units="in", dpi=300)


#Total N
ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
               filter(nutrient=="TN") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
  facet_wrap(.~lagoslakeid, scales="free_y")  +
  # facet_grid(lagoslakeid~nutrient, scales = "free_y") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TN"),
              mapping=aes(intercept = intercept, slope = slope, group=lagoslakeid,
                          color=nutrient)) + #MK estimate trend
  # geom_smooth(data = all_loads_ts %>%
  #               filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
  #               filter(nutrient=="TN"), method="lm", se=F,
  #             mapping=aes(y=flux,x=water_year), color="black", linetype="dashed")+ #LM trend-- you can see how these differ with the outliers
  scale_color_manual(values=c("gold"))+
  scale_fill_manual(values=c("gold"))+
  labs(y="Annual TN loads (kgy-1)", x="Water Year")+
  theme_pubr()+
  theme(legend.position = "none",
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))
ggsave("figures/TN_load_trends.png", width=8, height=6,units="in", dpi=300)


##testing plotting per station_id
max_TN <- all_loads_ts %>%
  filter(nutrient=="TN") %>%
  filter(!station_id=="14206500") 

max_TNloads <- max(max_TN$flux)

max_TP <- all_loads_ts %>%
  filter(nutrient=="TP") 

max_TPloads <- max(max_TP$flux)

#g1 <- 
  ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
               filter(nutrient=="TN") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
 facet_wrap(.~lagoslakeid, scales="free_y")  +
  # facet_grid(lagoslakeid~nutrient, scales = "free") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TN"),
              mapping=aes(intercept = intercept, slope = slope, group=lagoslakeid,
                          color=nutrient))+

#g2 <- ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(lagoslakeid %in% trending_sites$lagoslakeid) %>%
               filter(nutrient=="TP") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
   # scale_y_continuous("TN flux",  sec.axis = sec_axis(~ . *2), name = "TP loads") +
  facet_wrap(.~lagoslakeid, scales="free_y")  +
 # facet_grid(lagoslakeid~nutrient, scales = "free") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TP"),
              mapping=aes(intercept = intercept, slope = slope, group=lagoslakeid,
                          color=nutrient))+
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . * (max_TPloads/ max_TNloads),
                                           name = 'TN flux')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(color = "#4B92DB"),
          axis.text.y.right = element_text(color = "red"),
          legend.position="right") +
    ylab('TP flux')
#g1 + g2


    
##merging trend sites with df

ts_TNtrends_lakes <- left_join(upstream_sites_lagos, nutrient_loads_unnested, by="lagoslakeid")%>%
  group_by(lagoslakeid) %>%
    filter(nutrient=="TN") %>%
   dplyr::select(lake_namelagos, lagoslakeid, Trend)%>%
    distinct()%>%
  na.omit() 

ts_TPtrends_lakes <- left_join(upstream_sites_lagos, nutrient_loads_unnested, by="lagoslakeid")%>%
  group_by(lagoslakeid) %>%
  filter(nutrient=="TP") %>%
  dplyr::select(lake_namelagos, lagoslakeid, Trend)%>%
  distinct()%>%
  na.omit()

filter(ts_TNtrends_lakes, Trend == "no trend")
filter(ts_TPtrends_lakes, Trend == "no trend")

d_mm <- d_mm %>%
  mutate(lagoslakeid= as.character(lagoslakeid))

ts_TNtrends_lakes_sp <- left_join(d_mm, ts_TNtrends_lakes, by="lagoslakeid")%>%
               st_as_sf( coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
                       crs=4326) %>%
  filter(!Trend=="no trend") 

ts_TPtrends_lakes_sp <- left_join(d_mm, ts_TPtrends_lakes, by="lagoslakeid")%>%
  st_as_sf( coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
            crs=4326) %>%
  filter(!Trend=="no trend") 

ts_TPtrends_lakes_sp_2 <- right_join(d_mm, ts_TPtrends_lakes, by="lagoslakeid")%>%
  st_as_sf( coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
            crs=4326)  

ts_TNtrends_lakes_sp_2 <- right_join(d_mm, ts_TNtrends_lakes, by="lagoslakeid")%>%
  st_as_sf( coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
            crs=4326) 

map_TP <- mapview(ts_TPtrends_lakes_sp_2, zcol = "Trend")
map_TN <- mapview(ts_TNtrends_lakes_sp_2, zcol = "Trend")

mapview(ts_TNtrends_lakes_sp_2, zcol = "Trend", col.regions = c("grey", "navy", "red"), map.types = "OpenTopoMap") 
  mapview(ts_TPtrends_lakes_sp_2, zcol = "Trend")

p <- st_bbox(c(xmin = -125, xmax = 25, ymin = -100, ymax = 50), 
             crs = st_crs(ts_TNtrends_lakes_sp_2)) |> 
  st_as_sfc()

##new map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
US <- ggplot(data = world) +
  geom_sf(color = "black", fill = "white") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125, -68), ylim = c(15, 50))

map_TN <- US +
  #geom_point(data = ts_TPtrends_lakes_sp_2, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, color= Trend), shape=18, size=3)+
  geom_sf(data = ts_TNtrends_lakes_sp_2, aes(color= Trend, shape = Trend), size=3)+
  coord_sf(xlim = c(-125, -68), ylim = c(15, 50))+
  # ggtitle("Study sites location and elevation in meters")+
  labs(x= "longitude", y="latitude", color = "Legend")+
  ggtitle("TN trends") +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#661100")) +
  scale_shape_manual(values=c(4,1,12))+
  theme(legend.position = "bottom")
  

map_TP <- US +
  #geom_point(data = ts_TPtrends_lakes_sp_2, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, color= Trend), shape=18, size=3)+
  geom_sf(data = ts_TPtrends_lakes_sp_2, aes(color= Trend, shape = Trend), size=3)+
  coord_sf(xlim = c(-125, -68), ylim = c(15, 50))+
  # ggtitle("Study sites location and elevation in meters")+
  labs(x= "longitude", y="latitude", color = "Legend")+
  ggtitle("TP trends") +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#661100")) +
  scale_shape_manual(values=c(4,1,12))+
  theme(legend.position = "bottom")

library(ggpubr)
multi_map <- ggarrange(map_TN, map_TP, 
                      # labels = c("A", "B"),
                       ncol=2, nrow = 1,
                       common.legend = TRUE)
multi_plot <- annotate_figure(multi_map,
                              top = text_grob("Spatial distribution of TN and TP loads trends", color = "black", face = "bold", size = 11))
multi_plot
ggsave("map_fig2.png", width=8, height=6,units="in", dpi=300)
