source("scripts/08_RF_splittingdata.R")

library(corrplot)
library(dplyr)
library(cowplot)
library(viridis)
library(ggExtra)
library(ggpubr)
library(patchwork)
library(colorblindr)

N_plot <- N_retention_final_3_imputed %>%
  select(lagoslakeid, tmean, prec_total, TS, ann_max_swe, TN_removal_gNm2yr_log)%>% #water_year
  #dplyr::rename(barren_pct=nlcd_barren31_pct)%>%
 # mutate(lagoslakeid = as.factor(lagoslakeid))%>%
 # mutate(water_area_m2 = lake_waterarea_ha*10000)%>%
  #mutate(lake_vol_m3 = water_area_m2 * lake_meandepth_m)%>%
  filter(prec_total<1000)

P_plot <- P_retention_final_3_imputed %>%
  select(lagoslakeid, tmean,  prec_total, TS, Pret_coef_log)%>%
  #mutate(lagoslakeid = as.factor(lagoslakeid))%>%
  #mutate(water_area_m2 = lake_waterarea_ha*10000)%>%
  #mutate(lake_vol_m3 = water_area_m2 * lake_meandepth_m)%>%
  
  filter(prec_total<1000)

all_plot = merge(N_plot, P_plot)

# Replace 0 values with NA
all_plot[all_plot == 0] <- NA

p =ggplot()+
  geom_point(data=all_plot, aes(x=prec_total, y=tmean, color=Pret_coef_log), size=2)+
  labs(x="Annual total precipitation (mm/yr)", y="Annual air temperature (C)", title="b")+
  scale_color_viridis(name="log(TP retention)") + theme_bw()+ theme(legend.position="bottom")

n = ggplot(data=all_plot, aes(x=prec_total, y=tmean, color=TN_removal_gNm2yr_log))+
  geom_point(size=2)+
  # stat_ellipse(aes(group = cluster), type = "norm", linetype = 2, size = 1, color = "black") +
  scale_color_viridis(name="log(TN retention)") + 
  labs(x="Annual total precipitation (mm/yr)", y="Annual air temperature (C)", title="a")+
  theme_bw()+ theme(legend.position="bottom")

n1 = ggplot()+
  geom_point(data=N_plot, aes(x=tmean, y=TN_removal_gNm2yr_log, color=prec_total), size=5)+
  scale_color_viridis(name="Annual precipitation (mm/yr)") + 
  labs(x="Annual air temperature (C)", y="log(TN retention)", title="b")+
  theme_bw()+ theme(legend.position="bottom")

# Add normalized marginal histograms
n_with_hist <- ggMarginal(
  n, 
  type = "density", 
  margins = "both", 
  size = 5, 
  binwidth = 0.2, 
  fill = "grey", 
  normalize = TRUE
)

p_with_hist <- ggMarginal(
  p, 
  type = "density", 
  margins = "both", 
  size = 5, 
  binwidth = 0.2, 
  fill = "grey", 
  normalize = TRUE
)

#n_with_hist / p_with_hist

ggarrange(n_with_hist, p_with_hist, ncol=2, nrow=1)
#ggsave("figures/fig5_new_final.png", width = 8, height = 6, dpi = 300)

##Testing Josh's ideas
#It might be interesting to do this again but with 
#local characteristics. For example, you could look at volume and precip or depth and temp. 
#The visuals may help us with some of the TN patterns, but may also be interesting for the TP patterns. 

all_plot_lt <- all_plot %>%
  group_by(lagoslakeid)%>%
  summarise(across(everything(), mean, na.rm = TRUE))

pp =ggplot(all_plot, aes(x=prec_total, y=Pret_coef_log, color=tmean)) +
  geom_point(size=3) +
  # stat_ellipse(aes(group = cluster), type = "norm", linetype = 2, size = 1, color = "black") +
  scale_color_gradient(low= "#E1BE6A", high= "#990000", name="Annual air. temp(C)") + 
  labs(x="Annual precipitation (mm/yr)", y="log(TP retention)", title="b") +
  theme_bw() +
  theme(legend.position="right")+
  geom_smooth(method="lm", show.legend = FALSE)


nn = ggplot(all_plot, aes(x=prec_total, y=TN_removal_gNm2yr_log, color=tmean)) +
  geom_point(size=3) +
  # stat_ellipse(aes(group = cluster), type = "norm", linetype = 2, size = 1, color = "black") +
  scale_color_gradient(low= "#E1BE6A", high= "#990000", name="Annual air. temp(C)") + 
  labs(x="Annual precipitation (mm/yr)", y="log(TN retention)", title="a") +
  theme_bw() +
  theme(legend.position="right")+
  geom_smooth(method="lm", show.legend = FALSE) 

gg =ggplot(all_plot, aes(x=ann_max_swe, y=Pret_coef_log, color=prec_total)) +
  geom_point(size=5) +
  # stat_ellipse(aes(group = cluster), type = "norm", linetype = 2, size = 1, color = "black") +
  scale_color_viridis(name="Annual precipitation (mm)") + 
  labs(x="M=Annual maximum SWE (mm)", y="log(TP retention)", title="b") +
  theme_bw() +
  theme(legend.position="right")+
  geom_smooth(method="lm", show.legend = FALSE)

#ggarrange(n_with_hist, p_with_hist, nn, pp, ncol=2, nrow=2)
combined_plot <- ggarrange(nn, pp,
                           ncol = 2, nrow = 1,
                           common.legend = TRUE,
                           legend = "right")  # or use "bottom" if preferred

# Save the output
ggsave("figures/fig5.png", plot = combined_plot,
       width = 10, height = 6, dpi = 300)  
