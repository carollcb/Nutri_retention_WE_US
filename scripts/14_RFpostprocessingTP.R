###Postprocessing of the RF results
library(randomForest)
library(randomForestExplainer)

source("scripts/11_runningRF.R")
#TN RF: rf_model_N

rf_model_TP_2

#To obtain the distribution of minimal depth we pass our forest to the function min_depth_distribution
min_depth_frame <- min_depth_distribution(rf_model_TP_2)
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame)
plot2 <- plot_min_depth_distribution(min_depth_frame, mean_sample = "top_trees", k = 3)

plot_new2 <- plot2 +  
  ggtitle("TP retention") + 
  scale_fill_brewer(palette = "YlOrBr")+
  #ggsci::scale_fill_rickandmorty()
  scale_x_discrete(labels = c("Annual total precipitation", "Total water area","Annual air temperature")
                   #limits = c("% of pasture areas", "Upstream TP concentrations"))
  )

ggsave("figures/plot_mean_depth_TP.png", plot = plot_new2, width = 10, height = 7, dpi = 300)

# importance_frame 
importance_frame2 <- measure_importance(rf_model_TP_2)
importance_final_TP <- importance_frame2 %>%
  filter(p_value<=0.05)%>%
  select(variable, mean_min_depth, no_of_nodes, node_purity_increase, p_value)


# plot_multi_way_importance(forest, size_measure = "no_of_nodes") # gives the same result as below but takes longer
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 7)

plot_TP <- plot_multi_way_importance(importance_frame2, x_measure = "mean_min_depth", y_measure = "no_of_nodes", size_measure = "p_value", main = "Multi-way importance plot for TP retention")

ggsave("figures/plot_metrics_TP.png", plot = plot_TP, width = 10, height = 7, dpi = 300)

library(ggpubr)
multi_plot <- ggarrange(plot_TN, plot_TP, 
                       # labels = c("A", "B"),
                       ncol=2, nrow = 1,
                       common.legend = FALSE)
multi_plot
ggsave("figures/plot_metrics_TN_TP.png", width = 8, height = 6, dpi = 300)


# plot_importance_ggpairs(forest) # gives the same result as below but takes longer
plot_importance_ggpairs(importance_frame)

#We pass the result together with or forest to the min_depth_interactions function to obtain a data frame containing information on mean conditional minimal depth of variables with respect to each element of vars (missing values are filled analogously as for unconditional minimal depth, in one of three ways specified by mean_sample)
interactions_frame <- min_depth_interactions(rf_model_TP_2)
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

# plot_min_depth_interactions(forest) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
plot_min_depth_interactions(interactions_frame)

interactions_frame2 <- min_depth_interactions(rf_model_TP_2, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
plot_min_depth_interactions(interactions_frame2)

#Prediction of the forest on a grid
plot_predict_interaction(rf_model_TP_2, P_retention, "mean_prob_oligo", "tmean")

#Explain the forest
explain_forest(rf_model_TP_2, interactions = TRUE, data = P_retention)

#Distribution of covariates
average_TN <- P_retention_final %>%
  group_by(lagoslakeid) %>%
  summarise(aver_TN = mean(totTNload_gm2yr))

h3 <- hist(average_TN$aver_TN)
h3
