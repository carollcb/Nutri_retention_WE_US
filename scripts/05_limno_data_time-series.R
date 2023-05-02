### looking at the LAGOS-Limno time-series and merging with candidate_sites info

#Loading packages
library(dplyr)
library(lubridate)
library(sf)
library(mapview)

#Data sources
#source("scripts/04_compile_LAGOS-limno-reservoir-connectivity-data.R")
#limno_summary <- read.csv("data/limno_summary.csv", colClasses = "character") %>%
#  select(lagoslakeid, year,temp_degc_median, tn_ugl_median, 
#         tp_ugl_median, lake_elevation_m, lake_centroidstate, epanutr_zoneid)

#limno_nutri<-read.csv("C:/Users/carol/OneDrive/Documentos/LIMNO_v2.1/site_nutrientsalgae_epi.csv")
limno_nutri<-read.csv("C:/westernLimno/LIMNO_v2.1/site_nutrientsalgae_epi.csv")

limno_tp_tn <- limno_nutri %>%
  mutate(event_date=lubridate::ymd(event_date),
         year=lubridate::year(event_date),
         year=factor(year),
         lagoslakeid=factor(lagoslakeid))%>%
  select(lagoslakeid, event_date, tn_ugl, tp_ugl)
  
limno_tp_tn_yrs  <- limno_tp_tn %>%
  mutate(year=lubridate::year(event_date),
year=factor(year))%>%
  group_by(lagoslakeid, year) %>%
  summarize_at(c("tn_ugl","tp_ugl"), list(median = function(x) median(x,na.rm=T),
                                                     n=length))%>%
  filter(!(is.na(tp_ugl_median) &
             is.na(tn_ugl_median))) 

limno_tp_tn_2000 <- limno_tp_tn_yrs %>%
  mutate(year=as.numeric(as.character(year))) %>%
  filter(year>=2000)

candidate_sites <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                            colClasses = "character",
                            stringsAsFactors = FALSE) 

upstream_gauges_lakes_all <- inner_join(limno_tp_tn_2000, candidate_sites, by="lagoslakeid") #%>%
  #distinct(station_id)
 # st_as_sf(coords= c("lon", "lat"),
       #    crs=4326)

#mapview(upstream_gauges_lakes_all)

##Merge with Wu dataset and see what lakes will overlap??
