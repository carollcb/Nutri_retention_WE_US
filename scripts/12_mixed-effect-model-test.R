##Mixed-effects model analysis
library(lme4)
library(devtools)
library(arm)

#PS: including a large number of predictors in a model can lead to challenges related to model interpretability, computational performance, 
#and potential issues with multicollinearity. It is important to carefully consider the relevance and significance of each predictor and 
#explore techniques like variable selection or dimensionality reduction if necessary.

data_all_TP <- read.csv("data/data_all_TP.csv")
P_retention<-data_all_TP %>%
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
  dplyr::select(-X, -seasonal_km2, -res_time_yr,-mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -total_Pin_ugl, -Pret_coef, -permanent_km2,-fluxTP_kgy)

P_retention$water_year <- as.factor(P_retention$water_year)
P_retention$lagoslakeid <- as.factor(P_retention$lagoslakeid)
str(P_retention)
# Construct the formula 
model_TP <- lmer(Pret_coef_log ~ nlcd_shrub52_pct + totTNload_gm2yr + ann_max_swe  +snowdur + prec_mean  + 
                nlcd_forcon42_pct + nws_drain_ratio  + tmean + total_precip_mm + total_km2 +
                (1|lagoslakeid) + (1|water_year), data=P_retention)
display(model_TP) #coef.est: estimated effect and coef.se: standard errors associated with the estimated coefficients. 
#AIC: The Akaike Information Criterion (AIC) is a measure of model fit that balances goodness of fit and model complexity. 
#Lower AIC values indicate better-fitting models.
#The Deviance Information Criterion (DIC) is another measure of model fit that takes into account both the goodness of fit and model complexity. 
#Lower DIC values indicate better-fitting models

# View the model summary
summary(model_TP)

# View the summary statistics of fixed effects
summary(model_TP)$coefficients

####N retention -----------------------------
data_all_TN <- read.csv("data/data_all_TN.csv")
N_retention<-data_all_TN %>%
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
  dplyr::select(-X, -seasonal_km2, -TN_removal_gNm2yr,-mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -totTNload_gm2yr, -permanent_km2)

N_retention$water_year <- as.factor(N_retention$water_year)
N_retention$lagoslakeid <- as.factor(N_retention$lagoslakeid)
str(N_retention)

# Construct the formula 
model <- lmer(TN_removal_gNm2yr_log ~ fluxTP_kgy + nlcd_barren31_pct + total_precip_mm + total_km2 + 
                nlcd_past81_pct + topographicwetness + ann_max_swe + nws_drain_ratio + nlcd_forcon42_pct +
                (1|lagoslakeid) + (1|water_year), data=N_retention)
display(model) #coef.est: estimated effect and coef.se: standard errors associated with the estimated coefficients. 
#AIC: The Akaike Information Criterion (AIC) is a measure of model fit that balances goodness of fit and model complexity. 
#Lower AIC values indicate better-fitting models.
#The Deviance Information Criterion (DIC) is another measure of model fit that takes into account both the goodness of fit and model complexity. 
#Lower DIC values indicate better-fitting models

# View the model summary
summary(model)

# View the summary statistics of fixed effects
summary(model)$coefficients
