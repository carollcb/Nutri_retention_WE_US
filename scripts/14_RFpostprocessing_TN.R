###Postprocessing of the RF results
library(randomForest)
library(randomForestExplainer)

source("scripts/11_runningRF.R")
#TN RF: rf_model_N

rf_model_N

#To obtain the distribution of minimal depth we pass our forest to the function min_depth_distribution
min_depth_frame <- min_depth_distribution(rf_model_N)
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame)
plot <- plot_min_depth_distribution(min_depth_frame, mean_sample = "top_trees", k = 3)

plot_new <- plot +  
  ggtitle("TN retention") + 
  scale_fill_brewer(palette = "viridis")+
  #ggsci::scale_fill_rickandmorty()
  scale_x_discrete(labels = c("Total water area", "Annual total precipitation", "Annual air temperature")
                   #limits = c("% of pasture areas", "Upstream TP concentrations"))
)

ggsave("figures/plot_mean_depth_TN.png", plot = plot_new, width = 10, height = 7, dpi = 300)

# importance_frame 
importance_frame <- measure_importance(rf_model_N2)
importance_final <- importance_frame %>%
  filter(p_value<=0.05)%>%
  select(variable, mean_min_depth, no_of_nodes, node_purity_increase, p_value)

# plot_multi_way_importance(forest, size_measure = "no_of_nodes") # gives the same result as below but takes longer
plot_multi_way_importance(importance_final, size_measure = "no_of_nodes")

plot_TN <- plot_multi_way_importance(importance_frame, x_measure = "mean_min_depth", y_measure = "no_of_nodes", size_measure = "p_value",
                                     main = "Multi-way importance plot for TN retention")

ggsave("figures/plot_metrics_TN.png", plot = plot_TN, width = 10, height = 7, dpi = 300)

# plot_importance_ggpairs(forest) # gives the same result as below but takes longer
plot_importance_ggpairs(importance_frame)


Fi#We pass the result together with or forest to the min_depth_interactions function to obtain a data frame containing information on mean conditional minimal depth of variables with respect to each element of vars (missing values are filled analogously as for unconditional minimal depth, in one of three ways specified by mean_sample)
interactions_frame <- min_depth_interactions(rf_model_N)
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

# plot_min_depth_interactions(forest) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
plot_min_depth_interactions(interactions_frame)

interactions_frame2 <- min_depth_interactions(rf_model_N, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
plot_min_depth_interactions(interactions_frame2)

#Prediction of the forest on a grid
plot_predict_interaction(rf_model_N, N_retention, "mean_prob_oligo", "tmean")

#Explain the forest
explain_forest(rf_model_N, interactions = TRUE, data = N_retention)

#Distribution of covariates
average_prec_total <- N_retention_final %>%
  group_by(lagoslakeid) %>%
  summarise(aver_prec = mean(prec_total))

h1 <- hist(average_prec_total$aver_prec, breaks = 15)
h1

average_air <- N_retention_final %>%
  group_by(lagoslakeid) %>%
  summarise(aver_air = mean(tmean))

h2 <- hist(average_air$aver_air)
h2

average_TP <- N_retention_final %>%
  group_by(lagoslakeid) %>%
  summarise(aver_TP = mean(fluxTP_kgy))

h3 <- hist(average_TP$aver_TP, breaks=10)
h3
