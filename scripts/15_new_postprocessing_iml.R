##Running iml (interpretable ML in R) package => https://cran.r-project.org/web/packages/iml/vignettes/intro.html
library(iml)
library(patchwork)
library(viridis)
library(cowplot)

##TN retention

#X <- N_retention_final[which(names(N_retention_final) != "TN_removal_gNm2yr_log")]
predictor <- Predictor$new(rf_model_N2, data = N_retention_final_3)

imp <- FeatureImp$new(predictor, loss = "rmse")
plot(imp)
imp

plot(Interaction$new(predictor, feature="TS"))
ale1 <- plot(FeatureEffect$new(predictor,c("prec_total","tmean"),method="ale",grid.size=c(50,50)))+
  scale_fill_viridis(option="rainbow") +
  ggtitle("2D-ALE plot on TN retention") 
ale2 <- plot(FeatureEffect$new(predictor_2,c("prec_total","tmean"),method="ale",grid.size=c(50,50)))+
  scale_fill_viridis(option="rainbow") +
  ggtitle("2D-ALE plot on TP retention")

multi <- plot_grid(ale1,ale2, ncol=1)
ggsave("figures/ale_TN_TP_new.png", width=8, height=6,units="in", dpi=300)

tmean <- FeatureEffect$new(predictor, feature = "tmean")
tmean$plot()

prec <- FeatureEffect$new(predictor, feature = "prec_total")#, method = "pdp")
prec$plot()

barren <- FeatureEffect$new(predictor, feature = "nlcd_barren31_pct")
barren$plot()

all_tn <- FeatureEffects$new(predictor, features = c("tmean", "prec_total"))
all_tn$plot()
ggsave("figures/ALE_TN.png", width=8, height=6,units="in", dpi=300)

##2D plot!!
eff = FeatureEffect$new(predictor, feature = c("tmean", "prec_total"), method = "ale", grid.size = 20)
eff$plot()+
  #geom_tile(color="viridis")
  scale_fill_viridis(option="rainbow") + 
  ggtitle("2D-ALE plot on TN retention") +
  #geom_text(aes(label = .ale))+
  theme_bw()

viridis_palette <- rainbow(10)  # Choose number of colors as needed

# Update the color of the lines in the plot
for (i in seq_along(eff$plotData$ale_lines)) {
  eff$plotData$ale_lines[[i]]$line$color <- viridis_palette[i]
}

# Re-plot
eff$plot()
#interact <- Interaction$new(predictor)
#plot(interact)

##TP retention
#rf_model_TP_2 <- randomForest(Pret_coef_log ~ ., data = P_retention_final)
#X1 <- P_retention_final[which(names(P_retention_final) != "Pret_coef_log")]
predictor_2 <- Predictor$new(rf_model_TP_2, data = P_retention_final_3)

imp <- FeatureImp$new(predictor_2, loss = "mae")
plot(imp)

pdp1 <- plot(FeatureEffect$new(predictor,c("prec_total", "tmean"),method="pdp",grid.size=c(30,30)))+
  #geom_smooth(method="loess")
  ggtitle("2D-PDP plot on TN retention")

pdp2 <- plot(FeatureEffect$new(predictor_2,c("prec_total", "tmean"),method="pdp",grid.size=c(30,30)))+
  #geom_smooth(method="loess")
  ggtitle("2D-PDP plot on TP retention")

multi <- plot_grid(pdp1,pdp2, ncol=1)
ggsave("figures/pdp_TN_TP.png", width=8, height=6,units="in", dpi=300)


tmean_tp <- FeatureEffects$new(predictor_2, features = c("tmean", "prec_total", "TS"))
tmean_tp$plot()

prec_tp <- FeatureEffect$new(predictor_2, feature = "prec_total")#, method = "pdp")
prec_tp$plot()

ts_tp <- FeatureEffect$new(predictor_2, feature = "TS")
ts_tp$plot()

all_tp <- FeatureEffects$new(predictor_2, features = c("tmean", "prec_total"))
all_tp$plot


ggsave("figures/ALE_TP.png", width=8, height=6,units="in", dpi=300)

eff_TP = FeatureEffect$new(predictor_2, feature = c("tmean", "prec_total"), method = "ale", grid.size = 20)
eff_TP$plot()

eff$plot(features = c("lstat", "crim"))

library(cowplot)
ALE1=eff$plot()+
  #geom_tile(color="viridis")
  scale_fill_viridis(option="rainbow") + 
  ggtitle("2D-ALE plot on TN retention") +
  theme_bw()
ALE2=eff_TP$plot()+
  #geom_tile(color="viridis")
  scale_fill_viridis(option="rainbow") +
  ggtitle("2D-ALE plot on TP retention") +
  theme_bw()
multi <- plot_grid(ALE1,ALE2, ncol=1)
ggsave("figures/ALE_TN_TP.png", width=8, height=6,units="in", dpi=300)
