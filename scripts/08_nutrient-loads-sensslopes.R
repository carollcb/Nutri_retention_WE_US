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

# source("scripts/07_organizing-nutrient-loads-df.R")
source("scripts/07_organizing-nutrient-loads-df_newQ.R")

#For TP loads
TP_loads_ts <- phosphorus_loads %>%
  dplyr::select(station_id, water_year, fluxTP_kgy) %>%
  mutate(nutrient="TP") %>%
  rename(flux=fluxTP_kgy)

#For TN loads
TN_loads_ts <- nitrogen_loads %>%
  dplyr::select(station_id, water_year, fluxTN_kgy) %>%
  mutate(nutrient="TN") %>%
  rename(flux=fluxTN_kgy)

#All loads
all_loads_ts <- bind_rows(TP_loads_ts, TN_loads_ts) %>%
  group_by(station_id, nutrient) %>%
  add_count() %>%
  filter(n>=10) #exclude sites with less than 10 years of data


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
  group_by(station_id, nutrient) %>%
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
  dplyr::select(station_id, nutrient, slope, Trend)

flux_slope_intercept <- nutrient_loads_unnested %>%
  filter(station_id %in% trending_sites$station_id) %>%
  dplyr::select(station_id, slope, intercept) %>%
  mutate(intercept=as.numeric(intercept)) %>%
  ungroup()

#Plot them together
ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(station_id %in% trending_sites$station_id),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
  facet_wrap(nutrient~station_id, scales="free")  +
  # facet_grid(station_id~nutrient, scales = "free_y") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept,
              mapping=aes(intercept = intercept, slope = slope, group=station_id,
                          color=nutrient)) + #MK estimate trend
  geom_smooth(data = all_loads_ts %>%
                filter(station_id %in% trending_sites$station_id), method="lm", se=F,
              mapping=aes(y=flux,x=water_year), color="black", linetype="dashed")+
  scale_color_manual(values=c("darkblue","gold"))+
  scale_fill_manual(values=c("darkblue","gold"))+
  labs(y="Annual nutrient loads (kgy-1)", x="Water Year")


#Plot them separately

#Total P
ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(station_id %in% trending_sites$station_id) %>%
               filter(nutrient=="TP") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
  facet_wrap(.~station_id, scales="free_y")  +
  # facet_grid(station_id~nutrient, scales = "free_y") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TP"),
              mapping=aes(intercept = intercept, slope = slope, group=station_id,
                          color=nutrient)) + #MK estimate trend
  # geom_smooth(data = all_loads_ts %>%
  #               filter(station_id %in% trending_sites$station_id) %>%
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
               filter(station_id %in% trending_sites$station_id) %>%
               filter(nutrient=="TN") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
  facet_wrap(.~station_id, scales="free_y")  +
  # facet_grid(station_id~nutrient, scales = "free_y") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TN"),
              mapping=aes(intercept = intercept, slope = slope, group=station_id,
                          color=nutrient)) + #MK estimate trend
  # geom_smooth(data = all_loads_ts %>%
  #               filter(station_id %in% trending_sites$station_id) %>%
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
               filter(station_id %in% trending_sites$station_id) %>%
               filter(nutrient=="TN") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
 facet_wrap(.~station_id, scales="free_y")  +
  # facet_grid(station_id~nutrient, scales = "free") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TN"),
              mapping=aes(intercept = intercept, slope = slope, group=station_id,
                          color=nutrient))+

#g2 <- ggplot() +
  geom_point(data = all_loads_ts %>%
               filter(station_id %in% trending_sites$station_id) %>%
               filter(nutrient=="TP") %>%
               mutate(water_year=round(water_year, 0),
                      water_year=as.integer(water_year)),
             aes(y = flux, x=water_year, fill=nutrient),
             col = "black", shape=21)+
   # scale_y_continuous("TN flux",  sec.axis = sec_axis(~ . *2), name = "TP loads") +
  facet_wrap(.~station_id, scales="free_y")  +
 # facet_grid(station_id~nutrient, scales = "free") + #You might like the facet_wrap version better
  geom_abline(flux_slope_intercept %>%
                filter(nutrient=="TP"),
              mapping=aes(intercept = intercept, slope = slope, group=station_id,
                          color=nutrient))+
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . * (max_TPloads/ max_TNloads),
                                           name = 'TN flux')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(color = "#4B92DB"),
          axis.text.y.right = element_text(color = "red"),
          legend.position="right") +
    ylab('TP flux')
#g1 + g2

  
  
  d2 <- gather(d1, 'var', 'val', stones:revenue) %>% 
    mutate(val = if_else(var == 'revenue', as.double(val), val / (max_stones / max_revenue)))
  
  ggplot(mapping = aes(clarity, val)) +
    geom_bar(aes(fill = cut), filter(d2, var == 'revenue'), stat = 'identity') +
    geom_point(data = filter(d2, var == 'stones'), col = 'red') +
    facet_grid(~cut) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . * (max_stones / max_revenue),
                                           name = 'number of stones'),
                       labels = dollar) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(color = "#4B92DB"),
          axis.text.y.right = element_text(color = "red"),
          legend.position="bottom") +
    ylab('revenue')  
    
##merging trend sites with df

ts_TNtrends_lakes <- left_join(upstream_sites_lagos, nutrient_loads_unnested, by="station_id")%>%
    filter(nutrient=="TN") %>%
  group_by(lagoslakeid) %>%
  select(lake_namelagos, lagoslakeid, Trend, lon, lat)%>%
  distinct()%>%
  na.omit() 

ts_TPtrends_lakes <- left_join(upstream_sites_lagos, nutrient_loads_unnested, by="station_id")%>%
  filter(nutrient=="TP") %>%
  group_by(lagoslakeid) %>%
  select(station_id, lake_namelagos, lagoslakeid, Trend, lon, lat)%>%
  distinct()%>%
  na.omit() 

ts_trends_lakes <- left_join(upstream_sites_lagos, nutrient_loads_unnested, by="station_id")%>%
  group_by(lagoslakeid) %>%
  select(lake_namelagos, lagoslakeid, Trend, lon, lat)%>%
  distinct()%>%
  na.omit() 

filter(ts_TNtrends_lakes, Trend == "no trend")
filter(ts_TPtrends_lakes, Trend == "no trend")

ts_trends_lakes_sp <- ts_trends_lakes %>%
               st_as_sf( coords= c("lon", "lat"),
                       crs=4326) %>%
  filter(!Trend=="no trend") 

mapview(ts_trends_lakes_sp, zcol = "Trend") 

ggplot()+
  geom_sf(aes(color = Trend), data = ts_TPtrends_lakes_sp) +
  geom_sf(aes(color = Trend), data = ts_TNtrends_lakes_sp) +
  theme(legend.position="left")

##Comparing some LAGOS-Limno data and loads trends

loads_trends_onsite_concTP <- left_join(TP_loads_ts, upstream_gauges_lakes_all, by="station_id")%>%
  filter(nutrient == "TP" & station_id == "09352900")


ggplot(loads_trends_onsite_concTP) +
  geom_point(aes(x=year, y=log(tp_ugl_median)), col = "red", size=3, shape=16)+
  geom_point(aes(x=water_year, y=log(flux)),
             col = "black",size=2, shape=15)+
  theme_bw()


