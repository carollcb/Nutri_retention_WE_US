library(randomForest)
library(randomForestExplainer)
library(ggplot2)
library(ggrepel)

# Sample dataset with local importance enabled
set.seed(42)
#iris_rf <- randomForest(Pret_coef_log ~ ., data = P_retention_final_3_imputed, ntree = 500, keep.inbag = TRUE, localImp = TRUE)
iris_rf <- rF_tp
# Extract variable importance metrics
importance_frame <- measure_importance(iris_rf)

# Get total nodes in the forest
total_nodes <- sum(iris_rf$forest$ndbigtree, na.rm = TRUE)

# Calculate variable selection frequency
importance_frame$selection_frequency <- importance_frame$no_of_nodes / total_nodes

# Rename mean minimal depth column for clarity
importance_frame$avg_first_occurrence <- importance_frame$mean_min_depth

# Print variable selection frequency and average first occurrence
print(importance_frame[, c("variable", "selection_frequency", "avg_first_occurrence")])

# Plot variable selection frequency vs. average first occurrence
tp <- ggplot(importance_frame, aes(x = avg_first_occurrence, y = selection_frequency)) +
  geom_point(aes(color = p_value < 0.05), alpha = 0.7, size = 5) +  # Color by significance
  geom_text_repel(data = subset(importance_frame, p_value < 0.05), 
                  aes(label = variable), box.padding = 1.5, max.overlaps = Inf) +  # Apply repel only for p < 0.05
  geom_text(data = subset(importance_frame, p_value >= 0.05), 
            aes(label = variable), vjust = -0.5, hjust = -0.5, size = 3) +  # Regular text for p >= 0.05
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "gray50"), 
                     labels = c("> 0.05", "< 0.05")) +  # Custom color scale
  labs(x = "Average First Occurrence (Mean Min Depth)",  
       y = "Variable Selection Frequency",
       color = "p-value",
       title = "Multi-way Importance Plot for TP Retention") +
  theme_minimal()

tn <- ggplot(importance_frame, aes(x = avg_first_occurrence, y = selection_frequency)) +
  geom_point(aes(color = p_value < 0.05), alpha = 0.7, size = 5) +  # Color by significance
  geom_text_repel(data = subset(importance_frame, p_value < 0.05), 
                  aes(label = variable), box.padding = 1.5, max.overlaps = Inf) +  # Apply repel only for p < 0.05
  geom_text(data = subset(importance_frame, p_value >= 0.05), 
            aes(label = variable), vjust = -0.5, hjust = -0.5, size = 3) +  # Regular text for p >= 0.05
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "gray50"), 
                     labels = c("> 0.05", "< 0.05")) +  # Custom color scale
  labs(x = "Average First Occurrence (Mean Min Depth)",  
       y = "Variable Selection Frequency",
       color = "p-value",
       title = "Multi-way Importance Plot for TN Retention") +
  theme_minimal()

# Adjust axis text size for both plots
tn <- tn + theme(axis.title = element_text(size = 12),  # Reduce axis title size
                 axis.text = element_text(size = 10))   # Reduce tick label size

tp <- tp + theme(axis.title = element_text(size = 12),  
                 axis.text = element_text(size = 10))  

# Arrange plots with merged legend
multi_plot <- ggarrange(tn, tp, 
                        ncol = 1, nrow = 2, 
                        common.legend = TRUE,  # Merge legends
                        legend = "bottom")  # Place legend at the bottom

# Print the final plot
print(multi_plot)
ggsave("figures/Fig4_new.png", width=8, height=6,units="in", dpi=300)
