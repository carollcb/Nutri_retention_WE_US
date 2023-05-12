##Wu + LAGOS data

##Wu data units: g/day! 
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
  mutate(NetPRet_Wu = log(P_EN - P_DE), NetNRet_Wu = log(N_EN - N_DE)) %>%
  mutate(EnrDepP_ratio = (P_EN/P_DE), EnrDepN_ratio = (N_EN/N_DE))%>%
  mutate(Pret = (P_DE - P_EN), Nret = (N_DE - N_EN))%>%
  distinct()

##TN and TP loads -> merge with lagos

list_files_TN <- paste0('data/results_TN/',list.files(path='data/results_TN/'),'/loadflex.csv') # list full paths
TN <- data.frame() # create empty dataframe


for(c in 1:length(list_files_TN)) {
  tmp <- read.csv(list_files_TN[c]) |> # read in each csv
    # annoying workaround that I came up with to provide unique IDs
    mutate(id = list_files_TN[c],
           id = gsub('data/results_TN/', '', id),
           id = gsub('/loadflex.csv','',id))
  
  TN <- bind_rows(TN, tmp) # bind into a single dataframe
}




list_files_TP <- paste0('data/results_TP/',list.files(path='data/results_TP/'),'/loadflex.csv') # list full paths
TP <- data.frame() # create empty dataframe


for(c in 1:length(list_files_TP)) {
  tmp <- read.csv(list_files_TP[c]) |> # read in each csv
    # annoying workaround that I came up with to provide unique IDs
    mutate(id = list_files_TP[c],
           id = gsub('data/results_TP/', '', id),
           id = gsub('/loadflex.csv','',id))
  
  TP <- bind_rows(TP, tmp) # bind into a single dataframe
}

##joining loads data

phosphorus_loads_lt <- TP %>%
  rename(station_id = id)%>%
  rename(fluxTP_kgy = flux_kgy) %>%
  group_by(station_id) %>%
  summarise(lt_fluxTP_kgy = median(fluxTP_kgy))

nitrogen_loads_lt <- TN %>%
  rename(station_id = id)%>%
  rename(fluxTN_kgy = flux_kgy) %>%
  group_by(station_id) %>%
  summarise(lt_fluxTN_kgy = median(fluxTN_kgy))

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 

TP_loads_lagos <- inner_join(upstream_sites_lagos, phosphorus_loads_lt , by= "station_id")

TN_loads_lagos <- inner_join(upstream_sites_lagos, nitrogen_loads_lt , by= "station_id")

nutrient_loads_lagos <- right_join(TP_loads_lagos, TN_loads_lagos , by= "station_id")%>%
  select(station_id, lon.x,   lat.x, lagoslakeid.x,lake_namelagos.x, lt_fluxTP_kgy,lt_flux_kgy)%>%
  rename(lon = lon.x, lat= lat.x, lagoslakeid = lagoslakeid.x, lake_namelagos = lake_namelagos.x, lt_fluxTN_kgy = lt_flux_kgy)%>%
  mutate(lt_fluxTN_gday = lt_fluxTN_kgy* 2.74, lt_fluxTP_gday = lt_fluxTP_kgy*2.74) %>%
mutate(NetPRet = log(lt_fluxTP_gday), NetNRet = log(lt_fluxTN_gday))

#join and compare Wu and ours

Wu_ours_test <- inner_join(wu_epa_nla_LAGOS_locus, nutrient_loads_lagos, by="lagoslakeid") #0 coincidence lakes
