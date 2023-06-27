library(randomForest)
library(caret)
library(tidyverse)
library(patchwork)
library(tidymodels)
library(themis)
library(ggthemes)
library(missRanger)

#data_all <- read.csv("data/data_all.csv")%>%
data_all <- data_all_new %>%
  dplyr::select(-X, -lake_centroidstate )%>%
  mutate(TN_removal_gNm2yr.cat = ifelse(TN_removal_gNm2yr.cat == "Regular", "Expected", TN_removal_gNm2yr.cat))%>%
  mutate(Pret_coef.cat = ifelse(Pret_coef.cat == "Normal", "Expected", Pret_coef.cat))

#getting data
retention<-data_all %>%
  na.omit()

retention <- retention %>%
  mutate(across(contains("lakes4ha_"), as.numeric))%>%
  mutate(across(contains("soil_"), as.numeric))%>%
  mutate(across(contains("glaciatedlatewisc_pct"), as.numeric))%>%
  mutate(across(contains("elevation_"), as.numeric))%>%
  mutate(across(contains("tri_m"), as.numeric))%>%
  mutate(across(contains("topographicwetness"), as.numeric))%>%
  mutate(across(contains("landform_"), as.numeric))%>%
  mutate(across(contains("dams_"), as.numeric))%>%
  mutate(across(contains("sems_"), as.numeric))%>%
  mutate(across(contains("npdes_"), as.numeric))%>%
  mutate(across(contains("mines_"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-Pret_coef, -TN_removal_gNm2yr, -water_year, -mean_annual_temp_k, -tmedian, -prec_median)

str(retention)
#write.csv(retention, "data/covariates_list.csv")
#The dataset contain 799 obs. of  128 variables. 

###TN_removal_gNm2yr.cat considered as response variables. TN_removal_gNm2yr.cat variable should be a factor variable.

retention$TN_removal_gNm2yr.cat <- as.factor(retention$TN_removal_gNm2yr.cat)
table(retention$TN_removal_gNm2yr.cat) 

#Lets start with random seed so the outcome will be repeatable and store train and test data.

##From Bella's paper:
set.seed(123)
split_d <- initial_split(retention, strata = TN_removal_gNm2yr.cat, prop=0.60)
train_d <- training(split_d)%>%  mutate_if(is.numeric, round, digits=2) 
test_d<- testing(split_d)%>%  mutate_if(is.numeric, round, digits=2) 
## I doubled checked and at least 25% of each Trend group is set aside for validation
val_d <- validation_split(train_d, 
                          strata = TN_removal_gNm2yr.cat, 
                          prop = 0.8)

cores <- parallel::detectCores()
cores
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")
rf_recipe <- 
  recipe(TN_removal_gNm2yr.cat ~ ., data = train_d) %>%#Unlike MLR, doesn't require dummy or Expectedized predictor variables
  update_role(lagoslakeid, new_role = "ID")  #Specify that this is an identifier
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)
#TRAIN AND TUNE THE MODEL
rf_mod #we  have 2 hyperparameters for tuning
rf_mod %>%    
  parameters() 
#Use a space-filling design to tune, with 25 candidate models
set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_d,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))


#Top 5 models
rf_res %>%
  show_best(metric = "roc_auc")
autoplot(rf_res)+theme_few()+geom_smooth(method="lm")
#Looks like roc_auc decreaes as minimum node size increases, similar pattern for # randomly selected predictors

rf_best <-
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best #selects best model based on roc_auc # A tibble: 1 × 3
#mtry min_n .config              
#<int> <int> <chr>                
#1    57     7 Preprocessor1_Model01

#Calculate the data needed to plot the ROC curve. Possible after tuning with control_grid(save_pred=TRUE)
rf_res %>% 
  collect_predictions()

#To filter the predictions for only our best random forest model, we can use the parameters argument and pass it our tibble with the best hyperparameter values from tuning, which we called rf_best:
rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(TN_removal_gNm2yr.cat, .pred_Expected:.pred_Low) %>% 
  mutate(model = "Random Forest") 

#So start by rebuilding parsnip model object from scratch and add a new argument (impurity) to get VI scores
# the last model
last_rf_mod <- 
  rand_forest(mtry = 4, min_n = 3, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(split_d)

last_rf_fit %>% 
  collect_metrics()  #1 accuracy multiclass     0.971 Preprocessor1_Model1
#2 roc_auc  hand_till      0.990 Preprocessor1_Model1

#Get VI scores
library(vip)
library(gridExtra)
vip_plot<-last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 15)+
  ggtitle("Variables importance score for TN retention")

vip_plot


#### Figure S2. CM and ROC curve
#Plot ROC curve
last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(TN_removal_gNm2yr.cat, .pred_Expected:.pred_Low) %>% 
  autoplot()


fit_rf<-as.data.frame(last_rf_fit %>%
                        pluck(".predictions"))

require(multiROC)


#true_label <- data.frame(dummy(test_d$TN_removal_gNm2yr.cat))
true_label <- data.frame(model.matrix(~ TN_removal_gNm2yr.cat - 1, data = test_d))

colnames(true_label) <- c("High","Low","Expected")
colnames(true_label) <- paste(colnames(true_label), "_true", sep="")

rf_pred <- fit_rf[,1:3]
colnames(rf_pred) <- c("High","Low","Expected")
colnames(rf_pred) <- paste(colnames(rf_pred), "_pred_RF", sep="")


final_df <- cbind(true_label, rf_pred)

roc_res <- multi_roc(final_df, force_diag=T)
plot_roc_df <- plot_roc_data(roc_res)

aucs <- plot_roc_df %>%
  dplyr::select(AUC, Method, Group) %>%
  filter(!Group %in% c('Micro','Macro'))%>%
  distinct()

ROC<-plot_roc_df %>%
  filter(!Group %in% c('Micro','Macro'))%>%
  ggplot(., aes(x=1-Specificity,y=Sensitivity,color = Group)) +
  geom_step() +
  geom_text(data=aucs[aucs$Group=='Low',], aes(x=0.2,y=1, label=paste0('AUC = ',round(AUC,2))), show.legend = FALSE, size=3) +
  geom_text(data=aucs[aucs$Group=='Expected',], aes(x=0.2,y=.95, label=paste0('AUC = ',round(AUC,2))), show.legend = FALSE, size=3) +
  geom_text(data=aucs[aucs$Group=='High',], aes(x=0.2,y=.9, label=paste0('AUC = ',round(AUC,2))), show.legend = FALSE, size=3) +
 # scale_color_manual(values = trendColors_a) +
  geom_abline(slope=1,intercept=0, linetype="dashed")+
  ggtitle("ROC curve for the RF classification model of TN retention")+
  theme_few()

ROC
# Confusion matrix

confMatRF<-confusionMatrix(
  factor(fit_rf$'.pred_class'), 
  factor(test_d$TN_removal_gNm2yr.cat)
)

confMatRF #accuracy: 0.835

#Plot CM
# plt<-as.data.frame(confMatRF$table)
# accuracy <- confMatRF$overall
# plt$Prediction <- factor(plt$Prediction)
# 
# ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
#     scale_x_discrete(expand = c(0, 0))+ #remove white space
#   scale_y_discrete(expand = c(0, 0))+ #remove white space
#     geom_tile(color="black") + geom_text(aes(label=Freq)) +
#     scale_fill_gradient(low="white", high="#009194") +
#     labs(y = "Reference",x = "Prediction")+
#     labs(title = paste0("Accuracy: ",round(accuracy,2))) +
#     theme(axis.title = element_blank(),
#           axis.text.y=element_text(angle=45))

#Alt. way--
conf_mat_RF <- cvms::confusion_matrix(targets = test_d$TN_removal_gNm2yr.cat,
                                      predictions = fit_rf$'.pred_class')
tableRF<-data.frame(conf_mat_RF$`Confusion Matrix`)

plotTableRF <- tableRF %>%
  group_by(Target) %>%
  mutate(prop = N/sum(N),
         prop = round(prop,2))

plotTableRF$Match <- ifelse(plotTableRF$Prediction == plotTableRF$Target, 'Match',
                            ifelse(!plotTableRF$Prediction == plotTableRF$Target, 'No Match', 'Error'))


g1 <- ggplot(data = plotTableRF,
       mapping = aes(x = Target, y = Prediction, fill=N)) +
  geom_tile(color="black") +
  scale_x_discrete(expand = c(0, 0))+ #remove white space
  scale_y_discrete(expand = c(0, 0))+ #remove white space
  scale_fill_gradient(low="white", high="#3a787a",
                      name="Frequency") +
  geom_text(aes(label = paste0("n=",N)), vjust = .5,  alpha = 1, size=3) +
  geom_text(aes(label = paste0("prop.=",prop)), vjust = 2.0,  alpha = 1, size=2) +
  theme_few() +
  theme(axis.title = element_blank(),
        axis.text.y=element_text(angle=45))+
   #labs(title = paste0("Accuracy: ",round(accuracy,2)))
ggtitle("Confusion matrix for the testing data of the TN retention")


#g1 +  ROC

#dev.off()

#### Figure 5. Boxplots of top predictors

#Change labels from pos/neg to something a little more intuitive
retention <- retention %>%
  mutate(TN_removal_gNm2yr.cat=recode(TN_removal_gNm2yr.cat,
                          "High"="High TN retention",
                          "Low"="Low TN retention"))

#Extract VI scores for plot labels 
#VI_TN_load <-vip_plot$data %>%  pluck(2,2)
VI_P_ret <-vip_plot$data %>%  pluck(2,3)
VI_P_loads<-vip_plot$data %>%  pluck(2,4)
VI_totalkm<-vip_plot$data %>%  pluck(2,5)
VI_agency_desig<-vip_plot$data %>%  pluck(2,9)
VI_annualT <-vip_plot$data %>%  pluck(2,10)

#P retention
P_reg <-
  retention %>% filter(TN_removal_gNm2yr.cat == "Regular") %>% summarize(median(Pret_coef)) %>%
  pull()
B<-  ggplot(retention, aes(x = TN_removal_gNm2yr.cat, y = Pret_coef, fill = TN_removal_gNm2yr.cat)) +
  # geom_violin()+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(fill = TN_removal_gNm2yr.cat),
    shape = 21,
    size = 1.5,
    alpha = 0.6,
    position = position_jitter(0.2)
  ) +
  geom_hline(yintercept = (P_reg), linetype = "dashed") +
 # scale_fill_manual(values = trendColors_new) +
  guides(fill = "none") +
  theme_few() +
  theme(
    # axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  ) +
  scale_y_log10(labels = scales::label_number_si(),
                name = "P retention")+
  labs(title=paste0("VI = ", round(VI_P_ret, 3)))

#P_loads
P_reg <-
  retention %>% filter(TN_removal_gNm2yr.cat == "Regular") %>% summarize(median(total_Pin_ugl)) %>%
  pull()
C<-  ggplot(retention, aes(x = TN_removal_gNm2yr.cat, y = total_Pin_ugl, fill = TN_removal_gNm2yr.cat)) +
  # geom_violin()+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(fill = TN_removal_gNm2yr.cat),
    shape = 21,
    size = 1.5,
    alpha = 0.6,
    position = position_jitter(0.2)
  ) +
  geom_hline(yintercept = (P_reg), linetype = "dashed") +
  # scale_fill_manual(values = trendColors_new) +
  guides(fill = "none") +
  theme_few() +
  theme(
    # axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  ) +
  scale_y_log10(labels = scales::label_number_si(),
                name = "P loads")+
  labs(title=paste0("VI = ", round(VI_P_loads, 4)))

#totalkm2
P_reg <-
  retention %>% filter(TN_removal_gNm2yr.cat == "Regular") %>% summarize(median(total_km2)) %>%
  pull()
D<-  ggplot(retention, aes(x = TN_removal_gNm2yr.cat, y = total_km2, fill = TN_removal_gNm2yr.cat)) +
  # geom_violin()+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(fill = TN_removal_gNm2yr.cat),
    shape = 21,
    size = 1.5,
    alpha = 0.6,
    position = position_jitter(0.2)
  ) +
  geom_hline(yintercept = (P_reg), linetype = "dashed") +
  # scale_fill_manual(values = trendColors_new) +
  guides(fill = "none") +
  theme_few() +
  theme(
    # axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  ) +
  scale_y_log10(labels = scales::label_number_si(),
                name = "Total water")+
  labs(title=paste0("VI = ", round(VI_totalkm, 5)))

#% Agency designation
P_reg <-
  retention %>% filter(TN_removal_gNm2yr.cat == "Regular") %>% summarize(median(agency_designation_pct)) %>%
  pull()
E<-  ggplot(retention, aes(x = TN_removal_gNm2yr.cat, y = agency_designation_pct, fill = TN_removal_gNm2yr.cat)) +
  # geom_violin()+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(fill = TN_removal_gNm2yr.cat),
    shape = 21,
    size = 1.5,
    alpha = 0.6,
    position = position_jitter(0.2)
  ) +
  geom_hline(yintercept = (P_reg), linetype = "dashed") +
  # scale_fill_manual(values = trendColors_new) +
  guides(fill = "none") +
  theme_few() +
  theme(
    # axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  ) +
  scale_y_log10(labels = scales::label_number_si(),
                name = "Pct of protected areas")+
  labs(title=paste0("VI = ", round(VI_agency_desig, 9)))

#Mean annual temp k
P_reg <-
  retention %>% filter(TN_removal_gNm2yr.cat == "Regular") %>% summarize(median(mean_annual_temp_k)) %>%
  pull()
F<-  ggplot(retention, aes(x = TN_removal_gNm2yr.cat, y = mean_annual_temp_k, fill = TN_removal_gNm2yr.cat)) +
  # geom_violin()+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(fill = TN_removal_gNm2yr.cat),
    shape = 21,
    size = 1.5,
    alpha = 0.6,
    position = position_jitter(0.2)
  ) +
  geom_hline(yintercept = (P_reg), linetype = "dashed") +
  # scale_fill_manual(values = trendColors_new) +
  guides(fill = "none") +
  theme_few() +
  theme(
    # axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  ) +
  scale_y_log10(labels = scales::label_number_si(),
                name = "Mean annual basin-level temperature")+
  labs(title=paste0("VI = ", round(VI_annualT, 10)))



(B / C / D / E / F) + 
  plot_annotation(tag_levels = 'A')  & theme(legend.position = 'none',
                                             legend.text = element_text(size=9))

###Pret_coef.cat considered as response variables. Pret_coef.cat variable should be a factor variable.
data_all <- data_all_new %>%
  dplyr::select(-X, -lake_centroidstate )%>%
  mutate(TN_removal_gNm2yr.cat = ifelse(TN_removal_gNm2yr.cat == "Regular", "Expected", TN_removal_gNm2yr.cat))%>%
  mutate(Pret_coef.cat = ifelse(Pret_coef.cat == "Normal", "Expected", Pret_coef.cat))

#getting data
retention<-data_all %>%
  na.omit()

retention <- retention %>%
  mutate(across(contains("lakes4ha_"), as.numeric))%>%
  mutate(across(contains("soil_"), as.numeric))%>%
  mutate(across(contains("glaciatedlatewisc_pct"), as.numeric))%>%
  mutate(across(contains("elevation_"), as.numeric))%>%
  mutate(across(contains("tri_m"), as.numeric))%>%
  mutate(across(contains("topographicwetness"), as.numeric))%>%
  mutate(across(contains("landform_"), as.numeric))%>%
  mutate(across(contains("dams_"), as.numeric))%>%
  mutate(across(contains("sems_"), as.numeric))%>%
  mutate(across(contains("npdes_"), as.numeric))%>%
  mutate(across(contains("mines_"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-Pret_coef, -TN_removal_gNm2yr, -water_year, -mean_annual_temp_k, -tmedian, -prec_median)

retention$Pret_coef.cat <- as.factor(retention$Pret_coef.cat)
table(retention$Pret_coef.cat) 

set.seed(123)
split_d <- initial_split(retention, strata = Pret_coef.cat, prop=0.60)
train_d <- training(split_d)%>%  mutate_if(is.numeric, round, digits=2) 
test_d<- testing(split_d)%>%  mutate_if(is.numeric, round, digits=2) 
## I doubled checked and at least 25% of each Trend group is set aside for validation
val_d <- validation_split(train_d, 
                          strata = Pret_coef.cat, 
                          prop = 0.8)

cores <- parallel::detectCores()
cores
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")
rf_recipe <- 
  recipe(Pret_coef.cat ~ ., data = train_d) %>%#Unlike MLR, doesn't require dummy or Expectedized predictor variables
  update_role(lagoslakeid, new_role = "ID")  #Specify that this is an identifier
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)
#TRAIN AND TUNE THE MODEL
rf_mod #we  have 2 hyperparameters for tuning
rf_mod %>%    
  parameters() 
#Use a space-filling design to tune, with 25 candidate models
set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_d,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))


#Top 5 models
rf_res %>%
  show_best(metric = "roc_auc")
autoplot(rf_res)+theme_few()+geom_smooth(method="lm")
#Looks like roc_auc decreaes as minimum node size increases, similar pattern for # randomly dplyr::dplyr::selected predictors

rf_best <-
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best #dplyr::dplyr::selects best model based on roc_auc # A tibble: 1 × 3
#mtry min_n .config              
#<int> <int> <chr>                
#1    57     7 Preprocessor1_Model01 -> Model5

#Calculate the data needed to plot the ROC curve. Possible after tuning with control_grid(save_pred=TRUE)
rf_res %>% 
  collect_predictions()

#To filter the predictions for only our best random forest model, we can use the parameters argument and pass it our tibble with the best hyperparameter values from tuning, which we called rf_best:
rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(Pret_coef.cat, .pred_Expected:.pred_Low) %>% 
  mutate(model = "Random Forest") 

#So start by rebuilding parsnip model object from scratch and add a new argument (impurity) to get VI scores
# the last model
last_rf_mod <- 
  rand_forest(mtry = 4, min_n = 3, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(split_d)

last_rf_fit %>% 
  collect_metrics()  #1 accuracy multiclass     0.921 Preprocessor1_Model1
#2 roc_auc  hand_till      0.993 Preprocessor1_Model1

#Get VI scores
library(vip)
library(gridExtra)
vip_plot2<-last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 15)+
  ggtitle("Variables importance score for TP retention")

vip_plot2


#### Figure S2. CM and ROC curve
#Plot ROC curve
last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(Pret_coef.cat, .pred_Expected:.pred_Low) %>% 
  autoplot()


fit_rf<-as.data.frame(last_rf_fit %>%
                        pluck(".predictions"))

require(multiROC)


#true_label <- data.frame(dummy(test_d$Pret_coef.cat))
true_label <- data.frame(model.matrix(~ Pret_coef.cat - 1, data = test_d))

colnames(true_label) <- c("High","Low","Expected")
colnames(true_label) <- paste(colnames(true_label), "_true", sep="")

rf_pred <- fit_rf[,1:3]
colnames(rf_pred) <- c("High","Low","Expected")
colnames(rf_pred) <- paste(colnames(rf_pred), "_pred_RF", sep="")


final_df <- cbind(true_label, rf_pred)

roc_res <- multi_roc(final_df, force_diag=T)
plot_roc_df <- plot_roc_data(roc_res)

aucs <- plot_roc_df %>%
  dplyr::select(AUC, Method, Group) %>%
  filter(!Group %in% c('Micro','Macro'))%>%
  distinct()

ROC2<-plot_roc_df %>%
  filter(!Group %in% c('Micro','Macro'))%>%
  ggplot(., aes(x=1-Specificity,y=Sensitivity,color = Group)) +
  geom_step() +
  geom_text(data=aucs[aucs$Group=='Low',], aes(x=0.2,y=1, label=paste0('AUC = ',round(AUC,2))), show.legend = FALSE, size=3) +
  geom_text(data=aucs[aucs$Group=='Expected',], aes(x=0.2,y=.95, label=paste0('AUC = ',round(AUC,2))), show.legend = FALSE, size=3) +
  geom_text(data=aucs[aucs$Group=='High',], aes(x=0.2,y=.9, label=paste0('AUC = ',round(AUC,2))), show.legend = FALSE, size=3) +
  # scale_color_manual(values = trendColors_a) +
  geom_abline(slope=1,intercept=0, linetype="dashed")+
  ggtitle("ROC curve for the RF classification model of TP retention")+
  theme_few()
ROC2
# Confusion matrix

confMatRF<-confusionMatrix(
  factor(fit_rf$'.pred_class'), 
  factor(test_d$Pret_coef.cat)
)

confMatRF #accuracy: 0.9312

#Plot CM
# plt<-as.data.frame(confMatRF$table)
# accuracy <- confMatRF$overall
# plt$Prediction <- factor(plt$Prediction)
# 
# ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
#     scale_x_discrete(expand = c(0, 0))+ #remove white space
#   scale_y_discrete(expand = c(0, 0))+ #remove white space
#     geom_tile(color="black") + geom_text(aes(label=Freq)) +
#     scale_fill_gradient(low="white", high="#009194") +
#     labs(y = "Reference",x = "Prediction")+
#     labs(title = paste0("Accuracy: ",round(accuracy,2))) +
#     theme(axis.title = element_blank(),
#           axis.text.y=element_text(angle=45))

#Alt. way--
conf_mat_RF <- cvms::confusion_matrix(targets = test_d$Pret_coef.cat,
                                      predictions = fit_rf$'.pred_class')
tableRF<-data.frame(conf_mat_RF$`Confusion Matrix`)

plotTableRF <- tableRF %>%
  group_by(Target) %>%
  mutate(prop = N/sum(N),
         prop = round(prop,2))

plotTableRF$Match <- ifelse(plotTableRF$Prediction == plotTableRF$Target, 'Match',
                            ifelse(!plotTableRF$Prediction == plotTableRF$Target, 'No Match', 'Error'))


g2 <- ggplot(data = plotTableRF,
             mapping = aes(x = Target, y = Prediction, fill=N)) +
  geom_tile(color="black") +
  scale_x_discrete(expand = c(0, 0))+ #remove white space
  scale_y_discrete(expand = c(0, 0))+ #remove white space
  scale_fill_gradient(low="white", high="#634044",
                      name="Frequency") +
  geom_text(aes(label = paste0("n=",N)), vjust = .5,  alpha = 1, size=3) +
  geom_text(aes(label = paste0("prop.=",prop)), vjust = 2.0,  alpha = 1, size=2) +
  theme_few() +
  theme(axis.title = element_blank(),
        axis.text.y=element_text(angle=45))+
#labs(title = paste0("Accuracy: ",round(accuracy,2)))+
ggtitle("Confusion matrix for the testing data of the TP retention")


#g2 +  ROC

g1 / g2

vip_plot / vip_plot2

ROC / ROC2
