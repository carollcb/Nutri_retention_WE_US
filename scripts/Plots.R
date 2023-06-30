library(ggmap)
library(patchwork)
library(ggplot2)

p <- ggmap(get_stamenmap(bbox=c(-125, 25, -100, 50), zoom = 5, 
                         maptype='terrain'))

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
cor_matrix <- cor(data_all_final)

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "color")

data_all_final_P <- read.csv("data/data_all_final.csv")%>%
  select(-X)%>%
  select(lagoslakeid,Pret_coef, tmean,prec_mean,                       
         total_precip_mm,mean_annual_temp_k, snowdur,snow_free_days,ann_max_swe, lake_lon_decdeg, lake_lat_decdeg)%>%
  drop_na()

data_all_final_N <- read.csv("data/data_all_final.csv")%>%
  select(-X)%>%
  select(lagoslakeid,TN_removal_gNm2yr, tmean,prec_mean,                       
         total_precip_mm,mean_annual_temp_k, snowdur,snow_free_days,ann_max_swe, lake_lon_decdeg, lake_lat_decdeg)%>%
  drop_na()

cor_matrixP <- cor(data_all_final_P)
cor_matrixN <- cor(data_all_final_N)

# Plot the correlation matrix using corrplot
c1 <- corrplot(cor_matrixP, method = "circle")
c2 <- corrplot(cor_matrixN, method = "color")

#Look at those in https://www.rdocumentation.org/packages/corrplot/versions/0.92/topics/corrplot
res1 = cor.mtest(cor_matrixP, conf.level = 0.95)
res2 = cor.mtest(cor_matrixN, conf.level = 0.99)

corrplot(cor_matrixP, low = res1$uppCI, upp = res1$uppCI,
         plotCI = 'circle', addg = 'grey20', cl.pos = 'n')

corrplot(cor_matrixP, low = res1$lowCI, upp = res1$uppCI,
         col = c('white', 'black'), bg = 'gold2', order = 'AOE',
         plotCI = 'square', addg = NULL, cl.pos = 'n')

corrplot(cor_matrixP, low = res1$lowCI, upp = res1$uppCI, order = 'hclust',
         rect.col = 'navy', plotCI = 'rect', cl.pos = 'n')
