##looking loadflex results for TP upstream loads

#Looking at Pload data
candidate_sites_WE <- readRDS("data/TP_flow_gauges.rds") %>%
  mutate(flow_station_id = as.character(flow_station_id)) %>%
  dplyr::filter(is_lake_gage == "TRUE")%>%
  dplyr::filter(station_id == flow_station_id)%>%
  rename(sites = flow_station_id)

class(wu_epa_nla_LAGOS_locus)
class(candidate_sites_WE)

wu_epa_nla_LAGOS_locus_sp <- st_as_sf(wu_epa_nla_LAGOS_locus,
                        coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
                        crs=4326) %>%
  filter(lake_centroidstate %in% c("CA", "UT", "NV",
                                   "WA", "OR", "ID",
                                   "MT", "WY", "CO",
                                   "NM", "AZ"))

mapview(wu_epa_nla_LAGOS_locus_sp) + mapview(candidate_sites_WE, color= "red")

LAGOS_WE_sp <- st_as_sf(dt1,
                        coords= c("lake_lon_decdeg", "lake_lat_decdeg"),
                        crs=4326) %>%
  filter(lake_centroidstate %in% c("CA", "UT", "NV",
                                   "WA", "OR", "ID",
                                   "MT", "WY", "CO",
                                   "NM", "AZ")) %>%
  mutate(lagoslakeid=factor(lagoslakeid))

ggplot()+
  geom_sf(data= LAGOS_WE_sp)+
  geom_sf(data = candidate_sites_WE, color="red")

mapview(LAGOS_WE_sp) + mapview(candidate_sites_WE, color = "yellow") #procurar code pra aumentar size do segundo


#Visually I was able to see that this upstream gauge corresponds to lagoslakeid = 450977
site_09196500 <- read.csv("data/results/09196500/loadflex.csv") %>%
  as.data.frame()%>%
  mutate(fluxP_gday = flux_kgy*2.74)
