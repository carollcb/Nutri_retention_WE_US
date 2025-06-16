###PDPs-----------------
#PDPs can be used to graphically examine the dependence of the response on low cardinality subsets
#of the features, accounting for the average effect of the other predictors. 

library(pdp)
library(cowplot)
library(ggplot2)
n <- partial(rf_model_N2, pred.var = "tmean", plot = TRUE) #TN
p <- partial(rf_model_TP_2, pred.var = "Pret_coef_log", plot = TRUE) #TP

##Nitrogen-------------------------------------
rf_model_N <- randomForest(TN_removal_gNm2yr_log ~ ., data = N_retention, ntree = 500, importance = TRUE)
varImpPlot(rf_model_N)
partialPlot(rf_model_N, pred.var = "totTNload_gm2yr", plot = TRUE)

# Compute partial dependence data for lstat and rm
pd <- pdp::partial(boston.rf, pred.var = c("lstat", "rm"))
# Default PDP
pdp1 <- plotPartial(pd)
# Add contour lines and use a different color palette
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp2 <- plotPartial(pd, contour = TRUE, col.regions = rwb)
# 3-D surface
pdp3 <- plotPartial(pd, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
                    colorkey = TRUE, screen = list(z = -20, x = -60))
# Figure 3
grid.arrange(pdp1, pdp2, pdp3, ncol = 3)



n <- pdp::partial(rf_model_N2, pred.var = c("tmean", "prec_total"))
plotPartial(n)

p1 <- partial(rf_model_N2, pred.var = "tmean", plot = TRUE) 
p2 <- partial(rf_model_N2, pred.var = "prec_total", plot = TRUE)
p3 <- partial(rf_model_N2, pred.var = "TS", plot = TRUE)

p1 + ylab("TN retention")

library(ggpubr)
multi_plot <- ggarrange(pp1, pp2,
                       # labels = c("A", "B"),
                       ncol=2, nrow = 1)#,
                       #common.legend = TRUE)



# Generate partial dependence data without plotting
p2_data <- partial(rf_model_N2, pred.var = "tmean", plot = FALSE)

# Create a ggplot2 plot from the partial dependence data
p2_plot <- autoplot(p2_data)

# Customize the y-axis label
pp1 <- p2_plot + ylab("log(TN retention)") + xlab("Annual air temp. (C)")  + #xlab("Annual precipitation (mm/yr)") + #xlab("Annual air temp. (C)")
theme_minimal()#+ 
  #theme(panel.border = element_rect(color = "black", fill = NA, size = 1))  # Add a black border

  
###TP
c1 <- partial(rf_model_TP_2, pred.var = "tmean", plot = TRUE) 
c2 <- partial(rf_model_TP_2, pred.var = "prec_total", plot = TRUE)
c3 <- partial(rf_model_TP_2, pred.var = "TS", plot = TRUE)
c4 <- partial(rf_model_TP_2, pred.var = "ann_max_swe", plot = TRUE)

library(ggpubr)
multi_plot_2 <- ggarrange(cc1, cc2,cc4,
                        # labels = c("A", "B"),
                        ncol=2, nrow = 2)#,
#common.legend = TRUE)

multi_plot_3 <- ggarrange(pp1, pp2, cc1, cc2, cc4,
                          # labels = c("A", "B"),
                          ncol=2, nrow = 3)
ggsave("figures/pdp_final.png", width = 8, height = 6, dpi = 300)


# Generate partial dependence data without plotting
c1_data <- partial(rF_tn, pred.var = "tmean", plot = FALSE)

# Create a ggplot2 plot from the partial dependence data
c1_plot <- autoplot(c1_data)

# Customize the y-axis label
cc5 <- c1_plot + ylab("log(TN retention)") +  xlab("Annual air. temp (C)") +
  theme_minimal()

density_plot <- ggplot(N_retention_final_3_imputed, aes(x = tmean)) + 
  geom_density(fill = "lightblue", alpha = 0.7) + 
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

# Create y-axis density plot (aligned to the right)
y_density_plot <- ggplot(P_retention_final_3_imputed, aes(x = Pret_coef_log)) +  
  geom_density(fill = "lightblue", alpha = 0.7) +  
  coord_flip() +  # Flip to align with the y-axis
  theme_minimal() +  
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank())

# Align and combine the PDP and density plots using cowplot
combined_plot_5 <- plot_grid(density_plot, cc5, 
                           ncol = 1, align = "v", 
                           rel_heights = c(1, 3))
combined_plot_3 <- plot_grid(density_plot, 
                             plot_grid(cc3, y_density_plot, ncol = 2, rel_widths = c(3, 1)),  
                             ncol = 1, align = "v", rel_heights = c(1, 3))

# Display the combined plot
print(combined_plot_3)

#combined_plot_1 = TP, prec
#combined_plot_2 = TP, tmean
#combined_plot_3 = TP, IS
#combined_plot_4 = TN, prec
#combined_plot_5 = TN, tmean
#combined_plot_6 = TN, IS

multi_plot_final <- ggarrange(combined_plot_4, combined_plot_5,combined_plot_6, combined_plot_1, combined_plot_2,combined_plot_3,
                          # labels = c("A", "B"),
                          ncol=3, nrow = 2)
ggsave("figures/pdp_final_2025.png", width = 8, height = 6, dpi = 300)
