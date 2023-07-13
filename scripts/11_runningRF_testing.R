library(randomForest)
library(caret)
library(tidyverse)
library(patchwork)
library(tidymodels)
library(themis)
library(ggthemes)
library(missRanger)
library(vip) 

###For N retention ------------------------------------------------------------

#getting data
#data_all_TN <- read.csv("data/data_all_TN.csv")

N_retention<-data_all_TN %>%
  drop_na()%>%
  mutate(TN_removal_gNm2yr_log = log(TN_removal_gNm2yr))%>%
#retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
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
  mutate(across(contains("_median"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-TN_removal_gNm2yr.cat, -water_year, -mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate)

str(N_retention)
#write.csv(retention, "data/covariates_list.csv")
#The dataset contain 258 obs. of  167 variables. 

###TN_removal_gNm2yr.cat considered as response variables. TN_removal_gNm2yr.cat variable should be a factor variable.

N_retention$TN_removal_gNm2yr.cat <- as.factor(N_retention$TN_removal_gNm2yr.cat)
table(N_retention$TN_removal_gNm2yr.cat) 

#Lets start with random seed so the outcome will be repeatable and store train and test data.

##Baes on Bella's paper:
set.seed(123)
split_d <- initial_split(N_retention, strata = TN_removal_gNm2yr.cat, prop=0.60)
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
  roc_curve(TN_removal_gNm2yr.cat, .pred_High:.pred_Low) %>% 
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
  vip(num_features = 11)+
  ggtitle("Variables importance score for TN retention")

vip_plot


#### Figure S2. CM and ROC curve
#Plot ROC curve
last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(TN_removal_gNm2yr.cat, .pred_High:.pred_Low) %>% 
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

confMatRF #accuracy: 0.90

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
ggtitle("Confusion matrix for the N retention")


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

#getting data
P_retention<-data_all_TP %>%
  drop_na()%>%
  #retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
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
  mutate(across(contains("_median"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-water_year, -mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -Pret_coef, -total_Pin_ugl)

str(P_retention)

P_retention$Pret_coef.cat <- as.factor(P_retention$Pret_coef.cat)
table(P_retention$Pret_coef.cat) 

set.seed(123)
split_d <- initial_split(P_retention, strata = Pret_coef.cat, prop=0.60)
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
  roc_curve(Pret_coef.cat, .pred_High:.pred_Low) %>% 
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
# ML global interpretation
library(gridExtra)
vip_plot2<-last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 11)+
  ggtitle("Variables importance score for TP retention")

vip_plot2


#### Figure S2. CM and ROC curve
#Plot ROC curve
last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(Pret_coef.cat, .pred_High:.pred_Low) %>% 
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

confMatRF #accuracy: 0.74

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
ggtitle("Confusion matrix for the P retention")


#g2 +  ROC

g1 / g2
ggsave("figures/confusion_matrixs.png", width=8, height=6,units="in", dpi=300)

vip_plot / vip_plot2
ggsave("figures/vip_plot.png", width=8, height=6,units="in", dpi=300)

ROC / ROC2
ggsave("figures/ROC_plot.png", width=8, height=6,units="in", dpi=300)

##Using lime package to better interpreter the results and running another test --------------------- Don't need to run from here on
library(lime)       # ML local interpretation
library(pdp)        # ML global interpretation
library(h2o)
library(ggplot2)

h2o.init()


P_retention_lime<-data_all_TP %>%
  drop_na()%>%
  #retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
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
  mutate(across(contains("_median"), as.numeric))%>%
  mutate(categorical_ts=as.factor(categorical_ts))%>%
  dplyr::select(-water_year, -mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -lagoslakeid)

P_retention_lime$Pret_coef.cat <- as.factor(P_retention_lime$Pret_coef.cat)

str(P_retention_lime)
# create h2o objects for modeling
index <- 1:100
train_obs <- P_retention_lime[-index, ]
local_obs <- P_retention_lime[index, ]

y <- "Pret_coef.cat"
x <- setdiff(names(train_obs), y)
train_obs.h2o <- as.h2o(train_obs)
local_obs.h2o <- as.h2o(local_obs)

fit.caret <- train(
  Pret_coef.cat ~ ., 
  data = train_obs, 
  method = 'ranger',
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneLength = 1,
  importance = 'impurity'
)

# create h2o models
h2o_rf <- h2o.randomForest(x, y, training_frame = train_obs.h2o)
h2o_glm <- h2o.glm(x, y, training_frame = train_obs.h2o, family = "binomial")
h2o_gbm <- h2o.gbm(x, y, training_frame = train_obs.h2o)

fit.ranger <- ranger::ranger(
  Pret_coef.cat ~ ., 
  data = train_obs, 
  importance = 'impurity',
  probability = TRUE
)

##Global interpretation
#Variable importance quantifies the global contribution of each input variable to the predictions of a machine learning model. 
#Variable importance measures rarely give insight into the average direction that a variable affects a response function. They simply state the magnitude of a variable’s relationship with the response as compared to other variables used in the model.

vip(fit.ranger) + ggtitle("ranger: RF")

#After the most globally relevant variables have been identified, the next step is to attempt to understand how the response variable changes based on these variables. 
#For this we can use partial dependence plots (PDPs) and individual conditional expectation (ICE) curves. These techniques plot the change in the predicted value as specified feature(s) vary over their marginal distribution. Consequently, we can gain some local understanding how the reponse variable changes across the distribution of a particular variable but this still only provides a global understanding of this relationships across all observed data.

# built-in PDP support in H2O (it didn't work)
h2o.partialPlot(h2o_rf, data = train_obs.h2o, cols = "totTNload_gm2yr")

fit.ranger %>%
  partial(pred.var = "total_Pin_ugl", grid.resolution = 25, ice = TRUE) %>%
  autoplot(rug = TRUE, train = train_obs, alpha = 0.1, center = TRUE)

#lime (local interpretation)
explainer_caret <- lime(train_obs, fit.caret, n_bins = 5)
class(explainer_caret)
summary(explainer_caret)

explanation_caret <- explain(
  local_obs, 
  explainer_caret, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 5, 
  feature_select = "highest_weights",
  labels = "Yes"
)

tibble::glimpse(explanation_caret)

plot_explanations(explanation_caret)

# tune LIME algorithm
explanation_caret <- explain(
  x = local_obs, 
  explainer = explainer_caret, 
  n_permutations = 500,
  dist_fun = "manhattan",
  kernel_width = 3,
  n_features = 10, 
  feature_select = "lasso_path",
  labels = "Yes"
)

plot_explanations(explanation_caret)

#supported models
# get the model class
class(fit.ranger)
## [1] "ranger"

# need to create custom model_type function
model_type.ranger <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  
  return("classification")
}

model_type(fit.ranger)
## [1] "classification"

# need to create custom predict_model function
predict_model.ranger <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata)
  return(as.data.frame(pred$predictions))
}

predict_model(fit.ranger, newdata = local_obs)

explainer_ranger <- lime(train_obs, fit.ranger, n_bins = 5)
explanation_ranger <- explain(local_obs, explainer_ranger, n_features = 5, n_labels = 2, kernel_width = .1)
plot_explanations(explanation_ranger, ncol = 2) + ggtitle("Local interpretation based on the VI values")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )
  
