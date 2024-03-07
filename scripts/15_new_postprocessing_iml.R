##Running iml (interpretable ML in R) package => https://cran.r-project.org/web/packages/iml/vignettes/intro.html
library(iml)

##TN retention

#X <- N_retention_final[which(names(N_retention_final) != "TN_removal_gNm2yr_log")]
predictor <- Predictor$new(rf_model_N2, data = N_retention_final)

imp <- FeatureImp$new(predictor, loss = "mae")
plot(imp)

tmean <- FeatureEffect$new(predictor, feature = "tmean")
tmean$plot()

prec <- FeatureEffect$new(predictor, feature = "prec_total")#, method = "pdp")
prec$plot()

barren <- FeatureEffect$new(predictor, feature = "nlcd_barren31_pct")
barren$plot()

all_tn <- FeatureEffects$new(predictor, features = c("tmean", "prec_total", "nlcd_barren31_pct"))
all_tn$plot()
ggsave("figures/ALE_TN.png", width=8, height=6,units="in", dpi=300)

#interact <- Interaction$new(predictor)
#plot(interact)

##TP retention
rf_model_TP_2 <- randomForest(Pret_coef_log ~ ., data = P_retention_final)
#X1 <- P_retention_final[which(names(P_retention_final) != "Pret_coef_log")]
predictor_2 <- Predictor$new(rf_model_TP_2, data = P_retention_final)

imp <- FeatureImp$new(predictor_2, loss = "mae")
plot(imp)

tmean_tp <- FeatureEffects$new(predictor_2, features = c("tmean", "prec_total", "TS"))
tmean_tp$plot()

prec_tp <- FeatureEffect$new(predictor_2, feature = "prec_total")#, method = "pdp")
prec_tp$plot()

ts_tp <- FeatureEffect$new(predictor_2, feature = "TS")
ts_tp$plot()

all_tp <- FeatureEffects$new(predictor_2, features = c("tmean", "prec_total"))
all_tp$plot


ggsave("figures/ALE_TP.png", width=8, height=6,units="in", dpi=300)


eff$plot(features = c("lstat", "crim"))
