##---Testing time-series analysis on TP and TN annual loads----##
library(tidyverse)
library(patchwork)
library(Kendall)
library(broom) 
library(trend)
library(zip)

#For TP loads
TP_loads_ts <- TP_loads %>%
  dplyr::select(station_id, water_year, fluxTP_kgy)
  
#Mann-Kendall test
mk <-trend::mk.test(TP_loads_ts$fluxTP_kgy)
mk #Trend is significant! p<0.05
mk_df <- glance(mk) 
View(mk_df)

sens <-trend::sens.slope(TP_loads_ts$fluxTP_kgy)
print(sens)
glance(sens)

#Extract slope estimate manually
slopeEst <-as.numeric(sens[[1]]) 

sens_test <- zyp::zyp.sen(fluxTP_kgy ~ water_year, TP_loads_ts)
print(sens_test) # inspect

yInt <-sens_test$coefficients[[1]] # pull out y-int estimate for plotting

p1 <- TP_loads_ts %>%
  ggplot(aes(y = fluxTP_kgy, x = water_year)) +
  geom_point(col = "black", fill="grey50", shape=21)+
  geom_smooth(method = "lm", se = FALSE, color="red") + #Linear model
    geom_abline(intercept = yInt, slope = slopeEst, color="blue") + #MK estimate trend
    labs(title="TP_loads trends",
       y="Annual TP loads (kgy-1)", x="Water Year")

#For TN loads
TN_loads_ts <- TN_loads %>%
  dplyr::select(station_id, water_year, fluxTN_kgy)

#Mann-Kendall test
mk <-trend::mk.test(TN_loads_ts$fluxTN_kgy)
mk #Trend is significant! p<0.05
mk_df <- glance(mk) 
View(mk_df)

sens <-trend::sens.slope(TN_loads_ts$fluxTN_kgy)
print(sens)
glance(sens)

#Extract slope estimate manually
slopeEst <-as.numeric(sens[[1]]) 

sens_test <- zyp::zyp.sen(fluxTN_kgy ~ water_year, TN_loads_ts)
print(sens_test) # inspect

yInt <-sens_test$coefficients[[1]] # pull out y-int estimate for ploting

p2 <- TN_loads_ts %>%
  ggplot(aes(y = fluxTN_kgy, x = water_year)) +
  geom_point(col = "black", fill="grey50", shape=21)+
  geom_smooth(method = "lm", se = FALSE, color="red") + #Linear model
  geom_abline(intercept = yInt, slope = slopeEst, color="blue") + #MK estimate trend
  labs(title="TN_loads trends",
       y="Annual TN loads (kgy-1)", x="Water Year")

p1 + p2
