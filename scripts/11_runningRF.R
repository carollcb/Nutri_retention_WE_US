# Load the required library
library(randomForest)
#library(randomForestExplainer)
library(tidyverse)
library(patchwork)
library(vip)
library(pdp)

####RF for N retention ------------------------------------------------------------------------
data_all_TN <- read.csv("data/data_all_TN_new.csv")%>%
mutate(lagoslakeid = as.character(lagoslakeid))%>%
  mutate(nlcd_developed = sum(nlcd_devhi24_pct, nlcd_devlow22_pct, nlcd_devmed23_pct))%>%
  select(-nlcd_devhi24_pct, -nlcd_devlow22_pct, -nlcd_devmed23_pct) %>%
  mutate(TS = ifelse(categorical_ts == "oligo", 1, ifelse(categorical_ts == "eu/mixo", 2, NA)))%>%
  select(-mean_prob_oligo, -mean_prob_eumixo, -mean_prob_dys, -lake_elevation_m, -tmean, -prec_mean)


input_seasonality <- read.csv("data/is.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  select(-X)

data_all_TN_2 <- inner_join(data_all_TN, input_seasonality, by="lagoslakeid")
N_retention<-data_all_TN_2 %>%
  drop_na()%>%
  mutate(TN_removal_gNm2yr_log = log(TN_removal_gNm2yr))%>%
  #retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
  #mutate(across(contains("streams_"), as.numeric))%>%
  #mutate(across(contains("lith_"), as.numeric))%>%
  mutate(across(contains("glaciatedlatewisc_pct"), as.numeric))%>%
  mutate(across(contains("elevation_"), as.numeric))%>%
  mutate(across(contains("tri_m"), as.numeric))%>%
  mutate(across(contains("topographicwetness"), as.numeric))%>%
  mutate(across(contains("landform_"), as.numeric))%>%
  mutate(across(contains("dams_"), as.numeric))%>%
  mutate(across(contains("sems_"), as.numeric))%>%
  mutate(across(contains("npdes_"), as.numeric))%>%
  mutate(across(contains("mines_"), as.numeric))%>%
  mutate(across(contains("_median"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-X, -categorical_ts, -seasonal_km2, -TN_removal_gNm2yr, -mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -totTNload_gm2yr, -permanent_km2, -total_precip_mm)%>%
  filter(TN_removal_gNm2yr_log>0)

yearly_clim_data <- read.csv("data/yearly_clim_data2.csv")

yearly_clim_data2 <- yearly_clim_data %>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  select(-X)

N_retention_final <- inner_join(N_retention, yearly_clim_data2, by=c('lagoslakeid', 'water_year')) %>%
  select(-tsum, -prec_mean, -water_year, -lake_centroidstate, -fluxTP_kgy)
#258 obs of 151 variables -> 258 obs of 140 varia -> 229 obs of 140 variab
# Set the seed for reproducibility
set.seed(123)

# Build the random forest model
#rf_model <- randomForest(predictors, target, ntree = 500, importance = TRUE)
rf_model_N2 <- randomForest(TN_removal_gNm2yr_log ~ ., data = N_retention_final, importance = TRUE)

# Get the variable importance
var_importance <- importance(rf_model_N2)

v1 <- vip(rf_model_N, num_features = 10, aesthetics = list(color = "grey35", fill = "#E1BE6A", size = 0.8)) + ggtitle("Variable importance for TN retention")

impToPlot <- importance(rf_model_N, scale=FALSE)
dotchart(sort(impToPlot[,1]), xlab="IncNodePurity")

b1 <- varImpPlot(rf_model_N, n.var = 7, type = 2, main ="Variable Importance for TN retention")

N_ret_pred <- N_retention %>%
  select(lagoslakeid, water_year, TN_removal_gNm2yr_log, fluxTP_kgy, nlcd_barren31_pct, total_precip_mm, nlcd_past81_pct, ann_max_swe, nws_drain_ratio, total_km2)%>%
  group_by(lagoslakeid)%>%
  summarise_at(vars(TN_removal_gNm2yr_log:total_km2), list(mean = mean, sd = sd))%>%
  rename(SWE = ann_max_swe_mean, ups_fluxTP= fluxTP_kgy_mean, barren_land = nlcd_barren31_pct_mean, pasture_land = nlcd_past81_pct_mean, drain_ratio = nws_drain_ratio_mean, total_area = total_km2_mean, total_area_sd = total_km2_sd,tot_precip = total_precip_mm_mean) 
  
N_ret_pred_new <- N_retention %>%
  select(lagoslakeid, TN_removal_gNm2yr_log, fluxTP_kgy, nlcd_barren31_pct, IS, nlcd_past81_pct, ann_max_swe, lakes1ha_drainagelk_nperha , total_km2)%>%
  #group_by(lagoslakeid)%>%
  #summarise_at(vars(TN_removal_gNm2yr_log:total_km2), list(mean = mean, sd = sd))%>%
  rename(SWE = ann_max_swe, ups_fluxTP= fluxTP_kgy, barren_land = nlcd_barren31_pct, pasture_land = nlcd_past81_pct,drainage_1halakes = lakes1ha_drainagelk_nperha , total_area = total_km2) 

####RF for P retention ------------------------------------------------------------------------
data_all_TP <- read.csv("data/data_all_TP_new.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  mutate(nlcd_developed = sum(nlcd_devhi24_pct, nlcd_devlow22_pct, nlcd_devmed23_pct))%>%
  mutate(TS = ifelse(categorical_ts == "oligo", 1, ifelse(categorical_ts == "eu/mixo", 2, NA)))%>%
  select(-mean_prob_oligo, -mean_prob_eumixo, -mean_prob_dys, -lake_elevation_m, -tmean, -prec_mean)

data_all_TP_2 <- inner_join(data_all_TP, input_seasonality, by="lagoslakeid")
P_retention<-data_all_TP_2 %>%
  mutate(Pret_coef_log = log(Pret_coef))%>%
  drop_na()%>%
  #retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
  #mutate(across(contains("streams_"), as.numeric))%>%
 # mutate(across(contains("soil_"), as.numeric))%>%
  mutate(across(contains("glaciatedlatewisc_pct"), as.numeric))%>%
  mutate(across(contains("elevation_"), as.numeric))%>%
  mutate(across(contains("tri_m"), as.numeric))%>%
  mutate(across(contains("topographicwetness"), as.numeric))%>%
  mutate(across(contains("landform_"), as.numeric))%>%
  mutate(across(contains("dams_"), as.numeric))%>%
  mutate(across(contains("sems_"), as.numeric))%>%
  mutate(across(contains("npdes_"), as.numeric))%>%
  mutate(across(contains("mines_"), as.numeric))%>%
  mutate(across(contains("_median"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-X, -seasonal_km2, -res_time_yr,-mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -total_Pin_ugl, -Pret_coef, -permanent_km2,-fluxTP_kgy, -total_precip_mm,-categorical_ts)

P_retention_final <- inner_join(P_retention, yearly_clim_data2, by=c('lagoslakeid', 'water_year')) %>%
  select(-tsum, -prec_mean, -water_year, -lake_centroidstate, -totTNload_gm2yr)                
#229 obs of 146 variables -> 229 obs of 135 variab
# Set the seed for reproducibility
set.seed(123)

# Build the random forest model
#rf_model_TP <- randomForest(predictors, target, ntree = 500, importance = TRUE)
rf_model_TP_2 <- randomForest(Pret_coef_log ~ ., data = P_retention_final, importance = TRUE)

b2 <- varImpPlot(rf_model_TP_2, n.var = 7, type = 2, main ="Variable Importance for TP retention")

P_ret_pred <- P_retention %>%
  select(lagoslakeid, water_year, Pret_coef_log, prec_mean, totTNload_gm2yr, ann_max_swe, nlcd_forcon42_pct, snowdur, nws_drain_ratio, nlcd_shrub52_pct)%>%
group_by(lagoslakeid)%>%
  summarise_at(vars(Pret_coef_log:nlcd_shrub52_pct), list(mean = mean, sd = sd))%>%
  rename(SWE = ann_max_swe_mean, ups_fluxTN= totTNload_gm2yr_mean, ups_fluxTN_sd= totTNload_gm2yr_sd, shrub_land = nlcd_shrub52_pct_mean, evergforest_land = nlcd_forcon42_pct_mean, drain_ratio = nws_drain_ratio_mean, prec_mean = prec_mean_mean, prec_sd = prec_mean_sd,snow_dur = snowdur_mean) 

P_ret_pred_new <- P_retention %>%
  select(lagoslakeid, water_year, Pret_coef_log, prec_mean, totTNload_gm2yr, ann_max_swe, nlcd_forcon42_pct, snowdur, nws_drain_ratio, nlcd_shrub52_pct)%>%
  #group_by(lagoslakeid)%>%
  #summarise_at(vars(Pret_coef_log:nlcd_shrub52_pct), list(mean = mean, sd = sd))%>%
  rename(SWE = ann_max_swe, ups_fluxTN= totTNload_gm2yr, shrub_land = nlcd_shrub52_pct, evergforest_land = nlcd_forcon42_pct, drain_ratio = nws_drain_ratio, prec_mean = prec_mean, snow_dur = snowdur) 

# Get the variable importance
var_importance <- importance(rf_model_TP_2)

varImpPlot(rf_model_TP_2)

grid.arrange()

v2 <-vip(rf_model_TP_2, num_features = 10, aesthetics = list(color = "grey35", fill = "#994F00", size = 0.8)) + ggtitle("Variable importance for P retention")

v1 / v2

correlation_precip <- cor(N_retention$total_precip_mm, N_retention$prec_mean) #no

ggplot(N_retention, aes(x=total_precip_mm, y=prec_mean)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)


#####PDPs----------------
###TN
# Using randomForest's partialPlot function
p1 <- partialPlot(rf_model_N2, pred.data = N_retention_final, x.var = "TS")
p2 <- partialPlot(rf_model_N2, pred.data = N_retention_final, x.var = "total_km2")
p4 <- partialPlot(rf_model_N2, pred.data = N_retention_final, x.var = "prec_total") ###OLHAR ESSE EXEMPLO!!
p3 <- partialPlot(rf_model_N2, pred.data = N_retention_final, x.var = "tmean")
p5 <- partialPlot(rf_model_N, pred.data = N_retention, x.var = "mean_prob_oligo")
p6 <- partialPlot(rf_model_N, pred.data = N_retention, x.var = "mean_prob_eumixo")
p7 <- partialPlot(rf_model_N, pred.data = N_retention, x.var = "")
#library(patchwork)
par(mfrow= c(2,2))
plot(p1, type="l", ylab="log(TN removal)", xlab="Categorical tropic state")
plot(p2, type="l", ylab="log(TN removal)", xlab="Sites total water area (km2)")
plot(p4, type="l", ylab="log(TN removal)", xlab="Annual total precipitation (mm)")
plot(p3, type="l", ylab="log(TN removal)", xlab="Annual air temperature (decC)")
#plot(p5, type="l", ylab="log(TN removal)", xlab="oligo lakes")

#plot1 <- c1 + c2/ c3 + c4

par(mfrow= c(1,2))
plot(p5, type="l", ylab="log(TN removal)", xlab="oligo lakes")
plot(p6, type="l", ylab="log(TN removal)", xlab="eutrophic lakes")
#plot(p7, type="l", ylab="log(TN removal)", xlab="Sites total water area (km2)")

###TP
# Using randomForest's partialPlot function
pp1 <- partialPlot(rf_model_TP_2, pred.data = P_retention_final, x.var = "prec_total")
pp2 <- partialPlot(rf_model_TP_2, pred.data = P_retention_final, x.var = "TS")
pp3 <- partialPlot(rf_model_TP_2, pred.data = P_retention_final, x.var = "total_km2") 
pp4 <- partialPlot(rf_model_TP_2, pred.data = P_retention_final, x.var = "tmean")
pp5 <- partialPlot(rf_model_TP_2, pred.data = P_retention, x.var = "snowdur")
pp6 <- partialPlot(rf_model_TP_2, pred.data = P_retention, x.var = "nws_drain_ratio")
pp7 <- partialPlot(rf_model_TP_2, pred.data = P_retention, x.var = "nlcd_shrub52_pct")

par(mfrow= c(2,2))
plot(pp1, type="l", ylab="log(TP retention)", xlab="Annual total precipitation (mm)")
#plot(pp2, type="l", ylab="log(TP retention)", xlab="Upstream TN loads (gm2yr-1)")
plot(pp3, type="l", ylab="log(TP retention)", xlab="Sites total water area (km2)")
plot(pp4, type="l", ylab="log(TP retention)", xlab="Annual air temperature (decC)")

par(mfrow= c(2,2))
plot(pp5, type="l", ylab="log(TP retention)", xlab="Snow duration (d)")
plot(pp6, type="l", ylab="log(TP retention)", xlab="Catchment/lake ratio")
plot(pp7, type="l", ylab="log(TP retention)", xlab="Shrub and scrub areas (%)")

#pdp package
pdp::partial(rf_model_N, pred.var = c("total_precip_mm","nlcd_past81_pct"), grid.resolution = 40, 
             plot=TRUE, chull = TRUE, progress = TRUE)

pdp::partial(rf_model_N, pred.var = "total_precip_mm",
             plot=TRUE, rug = TRUE)

rf_model_N %>% 
  pdp::partial(pred.var = c("fluxTP_kgy")) %>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(fluxTP_kgy)))

# Compute partial dependence data for fluxTP_kgy, nlcd_barren31_pct, total_precip_mm, ann_max_swe
pd <- pdp::partial(rf_model_N, pred.var = c("fluxTP_kgy", "nlcd_past81_pct"))
# Default PDP
pdp1 <- plotPartial(pd)
# Add contour lines and use a different color palette
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp2 <- plotPartial(pd, contour = TRUE, col.regions = rwb)
# 3-D surface
pdp3 <- plotPartial(pd, levelplot = FALSE, zlab = "TN_removal_gNm2yr_log", drape = TRUE,
                    colorkey = TRUE, main = "PDP for TN retention based on percentage of pasture areas and upstream TP fluxes")
# Figure 3
grid.arrange(pdp1, pdp2, pdp3, ncol = 3)
pdp::partial(rf_model_N, pred.var = c("fluxTP_kgy", "ann_max_swe"), plot = TRUE, chull = TRUE)

#Calculate correlation coefficients with the response variable using dplyr
library(tidyr)
library(purrr)

# Create a function to calculate correlation and p-value
correlation_function <- function(x) {
  cor_test <- cor.test(x, P_ret_pred_new$Pret_coef_log)
  return(list(correlation = cor_test$estimate, p_value = cor_test$p.value))
}

correlation_function2 <- function(x) {
  cor_test <- cor.test(x, N_ret_pred_new$TN_removal_gNm2yr_log)
  return(list(correlation = cor_test$estimate, p_value = cor_test$p.value))
}

# Calculate correlation coefficients and p-values
correlation_df <- N_ret_pred_new %>%
  select(-lagoslakeid) 

correlation_TN <- correlation_df %>%
  map(correlation_function2) %>%
  enframe(name = "Column_Name", value = "Correlation_PValue") %>%
  unnest_wider(Correlation_PValue) %>%
  drop_na()

# Print the result
correlation_df <- correlation_df %>%
  filter(p_value<0.05)

correlation_df_new <- N_ret_pred_new %>%
  select(-TN_removal_gNm2yr_log, -lagoslakeid, -water_year) %>%
  map(correlation_function2) %>%
  enframe(name = "Column_Name", value = "Correlation_PValue") %>%
  unnest_wider(Correlation_PValue) %>%
  drop_na()


# Optionally, we can plot the correlations using a bar chart
correlation_df_2 <-correlation_df_new %>%
  mutate(Color = ifelse(correlation <0, "green","red")) %>%
  ggplot(aes(x = Column_Name, y = correlation, fill = Color)) +
  geom_col(color = "grey35")+
  #theme(aspect.ratio = 2/1)+
  #geom_bar(stat = "identity", color = "grey35", fill = "#E1BE6A", width = 0.4) +
  labs(title = "Correlation with TN retention", x = "Top predictors", y = "Correlation Coefficient")+
  theme(legend.position="none")

correlation_df_2

##TP

#Calculate correlation coefficients with the response variable using dplyr
correlation_df_TP <- P_ret_pred %>%
  select(-Pret_coef_log_mean, - lagoslakeid, -Pret_coef_log_sd) %>%
  summarise(across(everything(), ~ cor(.x, P_ret_pred$Pret_coef_log_mean))) %>%
  pivot_longer(everything(), names_to = "Column_Name", values_to = "Correlation")%>%
  drop_na()

# Print the result
print(correlation_df_TP)


correlationP_df <- P_ret_pred %>%
  select(-Pret_coef_log_mean, - lagoslakeid, -Pret_coef_log_sd) %>%
  map(correlation_function) %>%
  enframe(name = "Column_Name", value = "Correlation_PValue") %>%
  unnest_wider(Correlation_PValue) %>%
  drop_na()

correlationP_df_new <- P_ret_pred_new %>%
  select(- lagoslakeid, -Pret_coef_log, -water_year) %>%
  map(correlation_function) %>%
  enframe(name = "Column_Name", value = "Correlation_PValue") %>%
  unnest_wider(Correlation_PValue) %>%
  drop_na()


# Optionally, you can plot the correlations using a bar chart
correlation_df_3 <-correlationP_df_new%>%
  mutate(Color = ifelse(correlation <0, "green","red")) %>%
  ggplot(aes(x = Column_Name, y = correlation, fill = Color)) +
  geom_col(color = "grey35")+
  #theme(aspect.ratio = 2/1)+
  #geom_bar(stat = "identity", color = "grey35", fill = "#E1BE6A", width = 0.4) +
  labs(title = "Correlation with TP retention", x = "Top predictors", y = "Correlation Coefficient")+
  theme(legend.position="none")

grid.arrange(v1,b2, ncol=1)

grid.arrange(b1, b2)


##Looking trends in TN and TP retention over time
ggplot(data=N_retention_final)+ 
  geom_line(aes(x=water_year, y=TN_removal_gNm2yr_log, color= lagoslakeid))
