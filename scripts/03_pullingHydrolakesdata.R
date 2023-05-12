###Code to pull over Hydrolakes data [ Res_time (days => Vol_total/Disc_avg), Depth_avg (m)] + Wu et al 2022 and LAGOS_lakeid in oder to calculate nutrient retetion based on Vollenweider 1976 and Harrison et al 2009 eq
source("scripts/99_utils.R")
source("scripts/LAGOS_EDI.R")

#st_layers("C:/Users/cbarbosa/Documents/HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb")
#hydrolakes_raw <- st_read("C:/Users/cbarbosa/Documents/HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb")%>%
# filter(Country == "United States of America")
#hydrolakes_resT_depthA <- hydrolakes_raw %>%
#  dplyr::select(Lake_name, Res_time, Depth_avg, Pour_long, Pour_lat, Shape_Length, Shape_Area, Shape) #EPSG",4326
#st_write(hydrolakes_resT_depthA, "hydrolakes_NA.shp")

##Trying to crop based on Western shp

#WE_shp <- st_read("D:/Datasets/Datasets/Western.shp") #ESRI 102039 USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
#WE_shp_reproj <- st_transform(WE_shp, 4326)
#hydrolakes_WE <- st_crop(hydrolakes_resT_depthA, WE_shp_reproj)
#df_LCMAP_1985_WY <- as.data.frame(LCMAP_1985_WY, xy=TRUE)

##I've cropped it in QGIs and was also able to merge hydrolakes data and Wu_Lagos_lake data only in QGIs!!

#hydrolakes_West <- st_read("hydrolakes_WE_US.shp")
#hydrolakes_West_df <- as.data.frame(hydrolakes_West, xy=TRUE) %>%
#  rename(lk_lt_d = Pour_lt, lk_ln_d = Por_lng)
#Wu_Lagos_lakes <- st_read("Wu_lagos_lakes.shp")
#Wu_Lagos_lakes_reproj <- st_transform(Wu_Lagos_lakes, 4326)
#Wu_Lagos_lakes_df <- as.data.frame(Wu_Lagos_lakes_reproj, xy=TRUE)
#Wu_Lagos_lakes_hydro <- inner_join(hydrolakes_West_df, Wu_Lagos_lakes_df, by="lk_lt_d")

wu_nla <- read.csv("data/NLAmodel_output_Western.csv")
epa_nla <- read.csv("data/nla2012_wide_siteinfo_08232016.csv")
lagos_info <- read.csv("data/lagos_dataset_info.csv")%>%
  dplyr::select(lagoslakeid, nhdplusv2_comid, lake_nhdid, lake_lat_decdeg, lake_lon_decdeg, lake_centroidstate) %>%
  rename(COMID2012 = nhdplusv2_comid) %>%
  filter(lake_centroidstate %in% c("CA", "UT", "NV",
                                   "WA", "OR", "ID",
                                   "MT", "WY", "CO",
                                   "NM", "AZ")) %>%
  mutate(lagoslakeid=factor(lagoslakeid))

epa_nla <- epa_nla %>%
  rename(ID=SITE_ID) 

wu_epa_nla <- inner_join(epa_nla, wu_nla, by = "ID")

wu_epa_nla <- wu_epa_nla%>%
  group_by(VISIT_NO=1)

#filtering only interesting variables
wu_epa_nla <- wu_epa_nla %>%
  dplyr::select(ID, UID, AREA_HA, COMID2012, NARS_NAME, Area, Chla, TNmean, TPmean, N_Load, N_EN, N_DE, N_Outflow, P_Load, P_EN, P_DE, P_Outflow)

wu_epa_nla_LAGOS_locus <- right_join(lagos_info, wu_epa_nla, by = "COMID2012") %>%
  drop_na()%>%
mutate(NetPRet = log(P_EN - P_DE), NetNRet = log(N_EN - N_DE)) %>%
  mutate(EnrDepP_ratio = (P_EN/P_DE), EnrDepN_ratio = (N_EN/N_DE))%>%
  mutate(Pret = (P_DE - P_EN), Nret = (N_DE - N_EN))

#Wu + EPA + LAGOS-US Locus = 158 lakes
#n_distinct(wu_epa_nla_LAGOS_locus$lagoslakeid) = 157 lakes

#Wu + LAGOS-US Locus + Hydrolakes = 158 lakes
Wu_Lagos_lakes_hydro_sp <- st_read("shps/Wu_Lagos_lakes_hydro.shp")%>% #all lakes
  rename(lagoslakeid = lagslkd)%>%
  mutate(log_ResTim = log(Res_tim)) 

mapview(Wu_Lagos_lakes_hydro_sp, zcol = "EnrDepP_ratio")

#transforming in df and pulling only lakes with N and P retention (Pret >0) = 101 lakes!!
Wu_Lagos_lakes_hydro_df <- Wu_Lagos_lakes_hydro_sp %>%
  #as_tibble() %>% #no longer sf
  as.data.frame() %>%
  dplyr::select(lagoslakeid, lk_lt_d, lk_ln_d, P_DE, P_EN, N_DE, N_EN, Res_tim, EnrDepP_ratio, EnrDepN_ratio, log_ResTim) %>% 
  #filter(Pret >0, Nret>0) %>%
  rename(lat = lk_lt_d, lon= lk_ln_d) %>%
  drop_na() 

library(ggmap)

#140 lakes with N and P retention and residence time info
p <- ggmap(get_stamenmap(bbox=c(-125, 25, -100, 50), zoom = 5, 
                         #maptype='toner'))
                         maptype ='terrain',
                         color = 'color'))
p + geom_point(aes(x = lon, y = lat,  col = log_ResTim, size = Pret), data = Wu_Lagos_lakes_hydro_df) +
  theme(legend.position="left")

# create scatterplot
ggplot(Wu_Lagos_lakes_hydro, aes(x = EnrDepP_ratio, y = log_ResTim)) +
  geom_point()

Wu_Lagos_lakes_hydro_few <- Wu_Lagos_lakes_hydro_sp %>%
  #as_tibble() %>% #no longer sf
  as.data.frame() %>%
  dplyr::select(lk_lt_d, lk_ln_d, P_DE, P_EN, N_DE, N_EN, Res_tim, EnrDepP_ratio, EnrDepN_ratio, log_ResTim)%>%
 # mutate(Res_tim_yr = Res_tim/365) %>% #changing from days to years
  mutate(Pret = (P_DE - P_EN), Nret = (N_DE - N_EN)) %>% 
    #filter(Pret >0, Nret>0) %>%
  rename(lat = lk_lt_d, lon= lk_ln_d) %>%
  drop_na() 

library(corrplot)
M = cor(Wu_Lagos_lakes_hydro_few)
corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust')
corrplot(M, order = 'hclust', addrect = 2)

 