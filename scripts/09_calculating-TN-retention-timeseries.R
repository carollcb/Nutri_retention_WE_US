source("scripts/pulling-deposition-data.R")

library(tidyverse)

Nretention <- total_Nload %>%
  mutate(TN_removal_gNm2yr =(10^(-0.27 + (0.82 * log(totTNload_gm2yr)))))

ggplot(Nretention, aes(x=totTNload_gm2yr, y=TN_removal_gNm2yr)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm) #now I'm curious to see this plot categorized by lake trophic status (similar Fig 1 - Finlay et al 2013)

