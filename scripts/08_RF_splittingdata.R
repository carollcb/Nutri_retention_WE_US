##RF splitting subset of data
library(randomForest)
require(tidyverse)
require(caret)
library(paletteer)
require(ggplot2)

N_retention_final_3_imputed <- read.csv("data/TNretention_df.csv")
P_retention_final_3_imputed <- read.csv("data/TPret_final_data_2.csv")

####TN retention -----------------------
N_retention_final_4 <- N_retention_final_3_imputed %>%
  select(-X) %>%
  mutate(TN_removal_gNm2yr_log = log(TN_removal_gNm2yr))

set.seed(111)
lagoslakeid <- unique(N_retention_final_4$lagoslakeid)
train_lagoslakeid <- sample(lagoslakeid, size = 0.8 * length(lagoslakeid))
train <- N_retention_final_4 %>% filter(lagoslakeid %in% train_lagoslakeid)
validate <- N_retention_final_4 %>% filter(!lagoslakeid %in% train_lagoslakeid)

# train <- N_retention_final_4 %>%
#   group_by(lagoslakeid) %>%
#   sample_frac(.8) %>% #80% of data will be used for training
#   ungroup() 
# 
# validate <- N_retention_final_4 %>%
#   anti_join(train)

set.seed(111)
rF_tn <- randomForest(TN_removal_gNm2yr_log ~ . -lagoslakeid, data = train, mtry = 17, ntree =500, importance = TRUE)
print(rF_tn)

# Calculate RMSE and R-squared
rF_tn_pred <- predict(rF_tn, validate)
predicted <- rF_tn_pred
actual <- validate$TN_removal_gNm2yr_log

# RMSE
rmse <- sqrt(mean((predicted - actual)^2))

# R-squared
r_squared <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))

# Metrics
cat("RMSE: ", rmse, "\nR-squared: ", r_squared)

##Plot
# 1. Obtain the predicted values from the Random Forest model
#rF_tn_pred <- predict(rF_tn, newdata = validate)

# 2. Create a data frame to hold the predicted and actual values
comparison_data <- data.frame(
  Actual = validate$TN_removal_gNm2yr_log,
  Predicted = rF_tn_pred
)

# Plot the predicted vs actual values
ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +         # Scatter plot
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") +  # Add perfect fit line
  labs(title = "Predicted vs Actual TN Removal",
       x = "Actual TN Removal (log scale)",
       y = "Predicted TN Removal (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

####TP retention -----------------------
P_retention_final_4 <- P_retention_final_3_imputed %>%
  select(-nlcd_barren31_pct,-nlcd_shrub52_pct, -nlcd_past81_pct, -nlcd_openwater11_pct, -nlcd_wetwood90_pct)

set.seed(111)
train <- P_retention_final_4 %>%
  group_by(lagoslakeid) %>%
  sample_frac(.8) %>% #80% of data will be used for training
  ungroup() 

validate <- P_retention_final_4 %>%
  anti_join(train)

set.seed(111)
rF_tp <- randomForest(Pret_coef_log ~ . -lagoslakeid, data = train, mtry = 17, ntree =500, importance = TRUE)
print(rF_tp)
rF_tp_pred <- predict(rF_tp, validate)

# Calculate RMSE and R-squared
predicted <- rF_tp_pred
actual <- validate$Pret_coef_log

# RMSE
rmse <- sqrt(mean((predicted - actual)^2))

# R-squared
r_squared <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))

# Metrics
cat("RMSE: ", rmse, "\nR-squared: ", r_squared)

##Plot
# 1. Obtain the predicted values from the Random Forest model
rF_tp_pred <- predict(rF_tp, newdata = validate)

# 2. Create a data frame to hold the predicted and actual values
comparison_data <- data.frame(
  Actual = validate$Pret_coef_log,
  Predicted = rF_tp_pred
)

# Plot the predicted vs actual values
ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +         # Scatter plot
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") +  # Add perfect fit line
  labs(title = "Predicted vs Actual TP retention",
       x = "Actual TP retention (log scale)",
       y = "Predicted TP retention (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

##Postprocessing analysis
library(randomForestExplainer)
library(ggpubr)

# importance_frame 
importance_frame_tn <- measure_importance(rF_tn)
importance_final_tn <- importance_frame_tn %>%
  filter(p_value<=0.05)%>%
  select(variable, mean_min_depth, no_of_nodes, node_purity_increase, p_value)

importance_frame_tp <- measure_importance(rF_tp)
importance_final_tp <- importance_frame_tp %>%
  filter(p_value<=0.05)%>%
  select(variable, mean_min_depth, no_of_nodes, node_purity_increase, p_value)

# plot_multi_way_importance(forest, size_measure = "no_of_nodes") # gives the same result as below but takes longer
plot_TN <- plot_multi_way_importance(importance_frame_tn, x_measure = "mean_min_depth", y_measure = "no_of_nodes", size_measure = "p_value",
                                     no_of_labels =5, main = "Multi-way importance plot for TN retention")

plot_TP <- plot_multi_way_importance(importance_frame_tp, x_measure = "mean_min_depth", y_measure = "no_of_nodes", size_measure = "p_value",
                                     no_of_labels =5, main = "Multi-way importance plot for TP retention")
multi_plot <- ggarrange(plot_TN, plot_TP, 
                        # labels = c("A", "B"),
                        ncol=2, nrow = 1,
                        common.legend = FALSE)
multi_plot
