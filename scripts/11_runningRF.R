# Load the required library
library(randomForest)
library(tidyverse)
library(patchwork)

####RF for N retention ------------------------------------------------------------------------
#data_all_TN <- read.csv("data/data_all_TN.csv")
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
  dplyr::select(-seasonal_km2, -TN_removal_gNm2yr,-water_year, -mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -totTNload_gm2yr, -permanent_km2)

#258 obs of 151 variables -> 258 obs of 140 varia -> 229 obs of 140 variab
# Set the seed for reproducibility
set.seed(123)

# Read the test dataset (assuming it has both predictors and the target variable)
test_data <- N_retention

# Separate the predictors and the target variable
predictors <- test_data[, -ncol(test_data)]
target <- test_data$TN_removal_gNm2yr_log

# Build the random forest model
rf_model <- randomForest(predictors, target, ntree = 500, importance = TRUE)

# Get the variable importance
var_importance <- importance(rf_model)

v1 <- vip(rf_model, num_features = 10, aesthetics = list(color = "grey35", fill = "#E1BE6A", size = 0.8)) + ggtitle("Variable importance for N retention")


####RF for P retention ------------------------------------------------------------------------
#data_all_TP <- read.csv("data/data_all_TP.csv")
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
  dplyr::select(-seasonal_km2, -water_year, -res_time_yr,-mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -lagoslakeid, -total_Pin_ugl, -Pret_coef, -permanent_km2,-fluxTP_kgy)
                
#229 obs of 146 variables -> 229 obs of 135 variab
# Set the seed for reproducibility
set.seed(123)

# Read the test dataset (assuming it has both predictors and the target variable)
test_data <- P_retention

# Separate the predictors and the target variable
predictors <- test_data[, -ncol(test_data)]
target <- test_data$Pret_coef_log

# Build the random forest model
rf_model <- randomForest(predictors, target, ntree = 500, importance = TRUE)

# Get the variable importance
var_importance <- importance(rf_model)

v2 <- vip(rf_model, num_features = 10, aesthetics = list(color = "grey35", fill = "#994F00", size = 0.8)) + ggtitle("Variable importance for P retention")

v1 / v2

correlation_precip <- cor(N_retention$total_precip_mm, N_retention$prec_mean) #no

ggplot(N_retention, aes(x=total_precip_mm, y=prec_mean)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)


