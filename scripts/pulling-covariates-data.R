###-----pulling covariates data----###
source("scripts/LAGOS_EDI.R")

##weather data -> monthly precipitation and air temperature from LAGOS-GEO -> tranforming to annual data
clim_data <- read.csv("D:/Datasets/Datasets/LAGOS_GEO/zone_climate_monthly.csv")%>%
  select(zoneid,climate_year, climate_month, climate_tmean_degc, climate_ppt_mm)

yearly_clim_data <- clim_data %>%
  group_by(zoneid, climate_year) %>%
  summarize(across(c(climate_tmean_degc:climate_ppt_mm),
                   .fns = list(mean = mean, 
                               sd = sd,# you can comment these out if you want and just calculate mean/median 
                               max = max,#
                               min = min,#
                               median = median), na.rm = T,
                   .names = "{.col}-{.fn}")) 

yearly_clim_data %>%
  rename(hu12_zoneid = zoneid)

yearly_clim_data_lagos <- inner_join(yearly_clim_data, d_mm, by= "hu12_zoneid")%>%
  dplyr::select(lagoslakeid, lake_namelagos, lake_lat_decdeg, lake_lon_decdeg, lake_elevation_m, lake_centroidstate, climate_year,climate_tmean_degc-mean, climate_tmean_degc-max, climate_tmean_degc-min, climate_ppt_mm-mean, climate_ppt_mm-max, climate_ppt_mm-min)%>%
  mutate(lagoslakeid=as.character(lagoslakeid))

#yearly_clim_data %>% 
#  ungroup() %>%
#  pivot_longer(cols = climate_tmean_degc-mean:climate_ppt_mm-median,
#               names_to = c('var','stat'),
               #names_sep = '-')

#Lake networks and connectivity metrics from LAGOS-US

# Lake characteristics -> area, elevation, lake depth? from LAGOS-US AND HYDROlakes

# LULC: From NLCD-USGS 2019 and LCMAP-USGS?

# Water quality - look at dataset from Willimans and Labou 2017??

#Ice/snow dynamics - snow climate metrics from Lute et al 2022 and Global lake and river ice phenology (ask Josh)?

#Trophic state time-series? Check new Meyer et al dataset