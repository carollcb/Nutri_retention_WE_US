library(ggmap)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(tidyverse)

p <- ggmap(get_stamenmap(bbox=c(-125, 25, -100, 50), zoom = 5, 
                         maptype='terrain'))

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 

study_sites <- upstream_sites_lagos %>%
  distinct(lagoslakeid, .keep_all=TRUE)

dt2 <- dt2 %>%
  mutate(lagoslakeid = as.character(lagoslakeid))

study_sites_2 <- inner_join(study_sites, dt2, by="lagoslakeid")

d_mm <- d_mm %>%
  mutate(lagoslakeid = as.character(lagoslakeid))

study_sites_3 <- inner_join(study_sites_2, d_mm, by="lagoslakeid")%>%
  select(lagoslakeid, lake_totalarea_ha, lake_lon_decdeg, lake_lat_decdeg)

#write.csv(study_sites_3, "data/study_sites_final.csv")

sites <- read.csv("data/study_sites_final.csv")#,
                  #colClasses = "character",
                 # stringsAsFactors = FALSE)

custom_palette <- c("L" = "red", "R" = "blue")

#p+
ggplot()+
  geom_polygon(data=WEUS, mapping=aes(x=long, y=lat, group=group),color="black", fill=NA) + 
  geom_point(data = sites, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, color= categ), shape=18, size=3)+
 # ggtitle("Study sites location and elevation in meters")+
  labs(x= "longitude", y="latitude", color = "Category")+
  scale_color_manual(values = custom_palette) +
  theme(legend.position = "left")+
  theme(panel.background = element_rect(fill=NA))

p+
  geom_point(data = study_sites_huc12, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, size=lake_elevation_m),color="#006CD1", shape=16)+
  # ggtitle("Study sites location and elevation in meters")+
  labs(x= "longitude", y="latitude", size="Elevation (m):")+
  theme(legend.position = "left")

ggsave("figures/map_fig1.png", width=8, height=6,units="in", dpi=300)


data_lt <- data %>%
  group_by(lagoslakeid)%>%
  summarise(TN_removal_gNm2yr_lt = median(TN_removal_gNm2yr), Pret_coef_lt = median(Pret_coef))

data_map <- inner_join(data_lt, study_sites_huc12, by="lagoslakeid")

p+
  #geom_point(data = data_map, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, size=lake_elevation_m, color=Pret_coef_lt), shape=16)+
  geom_point(data = data_map, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, size=lake_elevation_m, color=TN_removal_gNm2yr_lt), shape=16)+
  # ggtitle("Study sites location and elevation in meters")+
  labs(x= "longitude", y="latitude", size="Elevation (m):")+
  theme(legend.position = "left")

data_all_final <- read.csv("data/data_all_final.csv")%>%
  select(-X)%>%
  group_by(lagoslakeid)%>%
  summarise(TN_removal_gNm2yr_lt = median(TN_removal_gNm2yr), Pret_coef_lt = median(Pret_coef),
            across(.cols = everything(), .fns = ~first(.)))%>%
  select(lagoslakeid, TN_removal_gNm2yr_lt, Pret_coef_lt, tmean,tmedian,prec_mean,prec_median,                       
         total_precip_mm,mean_annual_temp_k, snowdur,snow_free_days,ann_max_swe, lake_lon_decdeg, lake_lat_decdeg)%>%
  drop_na()

p+
  geom_point(data = data_all_final, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, size=ann_max_swe, color=Pret_coef_lt), shape=16)+
  #geom_point(data = data_all_final, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, size=snowdur, color=TN_removal_gNm2yr_lt), shape=16)+
  # ggtitle("Study sites location and elevation in meters")+
  labs(x= "longitude", y="latitude", size="")+
  theme(legend.position = "left")

library(corrplot)

data_all_TP <- read.csv("data/data_all_TP.csv")

data_all_final_P <- read.csv("data/data_all_final.csv")%>%
  select(-X)%>%
  select(lagoslakeid,Pret_coef, tmean,prec_mean,                       
         total_precip_mm,mean_annual_temp_k, snowdur,snow_free_days,ann_max_swe, lake_lon_decdeg, lake_lat_decdeg)%>%
  drop_na()

## TN
data_all_TN <- read.csv("data/data_all_TN.csv")
data_all_final_TN <- full_join(data_all_TP, data_all_TN, by=c("lagoslakeid", "water_year"))%>%
  drop_na()%>%
  #mutate(Pret_coef_log = log(Pret_coef))%>%
  mutate(TN_removal_gNm2yr_log = log(TN_removal_gNm2yr))%>%
  #group_by(lagoslakeid)%>%
  #summarise(TN_removal_gNm2yr_log_lt = median(TN_removal_gNm2yr_log), Pret_coef_log_lt = median(Pret_coef_log),
  #      across(.cols = everything(), .fns = ~first(.)))%>%
  select(TN_removal_gNm2yr_log, lakes1ha_drainagelk_nperha.x, fluxTP_kgy.x,nlcd_barren31_pct.x, total_precip_mm.x ,total_km2.x,
         nlcd_past81_pct.x, topographicwetness.x,ann_max_swe.x,nws_drain_ratio.x, nlcd_forcon42_pct.x)%>%
  rename(fluxTP_kgy = fluxTP_kgy.x, nlcd_barren31_pct = nlcd_barren31_pct.x, total_precip_mm = total_precip_mm.x ,
         total_km2 = total_km2.x,nlcd_past81_pct = nlcd_past81_pct.x, topographicwetness = topographicwetness.x,
         ann_max_swe = ann_max_swe.x,nws_drain_ratio = nws_drain_ratio.x, nlcd_forcon42_pct = nlcd_forcon42_pct.x, lakes1ha_drainagelk_nperha = lakes1ha_drainagelk_nperha.x)

cor_matrix_TN <- cor(data_all_final_TN)

# Plot the correlation matrix using corrplot
corrplot(cor_matrix_TN, method = "color")

data_all_final_TP <- full_join(data_all_TP, data_all_TN, by=c("lagoslakeid", "water_year"))%>%
  drop_na()%>%
  mutate(Pret_coef_log = log(Pret_coef))%>%
 # mutate(TN_removal_gNm2yr_log = log(TN_removal_gNm2yr))%>%
  #group_by(lagoslakeid)%>%
  #summarise(TN_removal_gNm2yr_log_lt = median(TN_removal_gNm2yr_log), Pret_coef_log_lt = median(Pret_coef_log),
  #      across(.cols = everything(), .fns = ~first(.)))%>%
  select(Pret_coef_log, nlcd_shrub52_pct.x, total_precip_mm.x ,total_km2.x,tmean.x,
         totTNload_gm2yr.x,ann_max_swe.x,snowdur.x, prec_mean.x, nws_drain_ratio.x, nlcd_forcon42_pct.x)%>%
  rename(nlcd_shrub52_pct = nlcd_shrub52_pct.x, total_precip_mm = total_precip_mm.x ,prec_mean = prec_mean.x,
         total_km2 = total_km2.x, tmean = tmean.x,ann_max_swe = ann_max_swe.x,nws_drain_ratio = nws_drain_ratio.x, nlcd_forcon42_pct = nlcd_forcon42_pct.x,
         snowdur = snowdur.x, totTNload_gm2yr = totTNload_gm2yr.x)

cor_matrix_TP <- cor(data_all_final_TP)
# Plot the correlation matrix using corrplot
c1 <- corrplot(cor_matrix_TP, method = "circle")
c2 <- corrplot(cor_matrix_TN, method = "color")

#Look at those in https://www.rdocumentation.org/packages/corrplot/versions/0.92/topics/corrplot
res1 = cor.mtest(cor_matrix_TP, conf.level = 0.95)
res2 = cor.mtest(cor_matrix_TN, conf.level = 0.95)

corrplot(cor_matrix_TN, low = res2$uppCI, upp = res2$uppCI,
         plotCI = 'circle', addg = 'grey20', cl.pos = 'n')


corrplot(cor_matrix_TN, method="circle",   
         diag=FALSE,  
         type="upper", order="hclust", 
         #title=title, 
         addCoef.col = "black", # Add coefficient of correlation
         # Combine with significance
         sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0) # http://stackoverflow.com/a/14754408/54964
)
v2 <- corrplot(cor_matrix_TN, low = res2$lowCI, upp = res2$uppCI,
         col = c('white', 'black'), bg = 'gold2', order = 'AOE',
         plotCI = 'square', addg = NULL, cl.pos = 'n')

v1 <- corrplot(cor_matrix_TP, low = res1$lowCI, upp = res1$uppCI,
               col = c('white', 'black'), bg = 'gold2', order = 'AOE',
               plotCI = 'square', addg = NULL, cl.pos = 'n')

corrplot(cor_matrix_TN, low = res2$lowCI, upp = res2$uppCI, order = 'hclust', rect.col = 'navy', plotCI = 'rect', cl.pos = 'n',
         title="Correlation among top predictors of N retention", mar=c(0,0,1,0))
corrplot(cor_matrix_TP, low = res1$lowCI, upp = res1$uppCI, order = 'hclust', rect.col = 'navy', plotCI = 'rect', cl.pos = 'n',
  title="Correlation among top predictors of P retention", mar=c(0,0,1,0))


