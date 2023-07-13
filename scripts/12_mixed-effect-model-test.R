##Mixed-effects model analysis
library(lme4)
library(devtools)
library(arm)
library(tidyverse)

#PS: including a large number of predictors in a model can lead to challenges related to model interpretability, computational performance, 
#and potential issues with multicollinearity. It is important to carefully consider the relevance and significance of each predictor and 
#explore techniques like variable selection or dimensionality reduction if necessary.

data_all_TP <- read.csv("data/data_all_TP.csv")
P_retention<-data_all_TP %>%
  mutate(Pret_coef_log = log(Pret_coef))%>%
  drop_na()%>%
  #retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
  #mutate(across(contains("streams_"), as.numeric))%>%
  # mutate(across(contains("soil_"), as.numeric))%>%
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
  dplyr::select(-X, -seasonal_km2, -res_time_yr,-mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -total_Pin_ugl, -Pret_coef, -permanent_km2,-fluxTP_kgy)

P_retention$water_year <- as.factor(P_retention$water_year)
P_retention$lagoslakeid <- as.factor(P_retention$lagoslakeid)
str(P_retention)

densityPlot(~P_retention$Pret_coef_log) #Ben's code

# Construct the formula 
model_TP <- lmer(Pret_coef_log ~ nlcd_shrub52_pct + totTNload_gm2yr + ann_max_swe  +snowdur + prec_mean  + 
                nlcd_forcon42_pct + nws_drain_ratio  + tmean + total_precip_mm + total_km2 +
                (1|lagoslakeid) + (1|water_year), data=P_retention)
display(model_TP) #coef.est: estimated effect and coef.se: standard errors associated with the estimated coefficients. 
#AIC: The Akaike Information Criterion (AIC) is a measure of model fit that balances goodness of fit and model complexity. 
#Lower AIC values indicate better-fitting models.
#The Deviance Information Criterion (DIC) is another measure of model fit that takes into account both the goodness of fit and model complexity. 
#Lower DIC values indicate better-fitting models

# View the summary statistics of fixed effects
summary(model_TP)$coefficients

###Ben's example
qqnorm(resid(model_TP)) # model residuals are normally distributed
plot(resid(model_TP) ~ fitted(model_TP)) # a little bit of a funnel, but mostly okay. These two things suggest that the model residuals are fine, and you're significance testing should be okay

# let's first just look at the basic model output, ignore the fixed effects significance tests for now since they are calculated in a different way
summary(model_TP)

# the only things you really want to pay attention to from this (for now) are the random effects residual variance components and the observation numbers
# the random effects residual variance components tell you how much of the remaining variation is explained by each random effect, after accounting for your fixed effects. So for instance, CruiseNo has an estimated residual variance of 0.268026 and if you add up all of the residual variance components, you get ~ 0.396067, meaning that of all of the variation not explained by your fixed effects, 67.67% is explained just by the differences among the cruises. That's a lot. However, you'd imagine there was a lot of variation among the cruises in things like cloudiness, rain, etc. that will probably all affect all of the stations during a cruise. So intuitively, it makes sense that a lot of the variation not explained by the fixed effects is explained by just the variation among cruises. 
# the other thing you want to pay attention to are the number of observations listed. You should see the total number of observations as well as the number of groups (e.g. 25 years, 14 station codes). Together these things will determine your denominator degrees of freedom

# okay now let's do some hypothesis testing using the Kenward-Roger demoninator degrees of freedom approximation. This is the most conservative F-test for mixed-effects models and it's the one I prefer when possible. 
anova(model_TP, ddf = 'Kenward-Roger') 
# significant Site Type and carps * site type effects.  
# notice the "DenDF" column is different than a typical anova table. Denominator degrees of freedom are weird for mixed effects models and the Kenward-Rogers degrees of freedom are one way of estimating them. For your interactive effects here, I would have guessed that they should have been close to, but less than, the number of station:year combinations, which was 347. From this analysis, the degrees of freedom are ~ 301 which seems about right. 

# now post-hoc testing is a little trickier. You've got some options. You can spend time looking for how to do a TukeyHSD test, perhaps in the package multcomp, though I'm not sure. 
# you could plot your data as the fitted means +/- bootstrapped 95% confidence intervals, and if they don't overlap, that's an extremely conservative post-hoc test. This isn't the easiest thing in the world to do, but can be done
# what I would advocate I guess is running the analysis separately for each site type and looking to see if some of the site types are significantly different pre-post carp invasion versus not. That was a finding from your thesis as I recall. My understanding is that this is called a simple main effects test. An example of how to do one of these is:

lsmeans(model_TP, pairwise ~ nlcd_forcon42_pct|nws_drain_ratio, adjust = 'none')


####N retention -----------------------------
data_all_TN <- read.csv("data/data_all_TN.csv")
N_retention<-data_all_TN %>%
  drop_na()%>%
  mutate(TN_removal_gNm2yr_log = log(TN_removal_gNm2yr))%>%
  #retention <- retention %>%
  mutate(across(contains("lakes1ha_"), as.numeric))%>%
  #mutate(across(contains("streams_"), as.numeric))%>%
  #mutate(across(contains("lith_"), as.numeric))%>%
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
  dplyr::select(-X, -seasonal_km2, -TN_removal_gNm2yr,-mean_annual_temp_k, -tmedian, -prec_median, -lake_centroidstate, -totTNload_gm2yr, -permanent_km2)

N_retention$water_year <- as.factor(N_retention$water_year)
N_retention$lagoslakeid <- as.factor(N_retention$lagoslakeid)
str(N_retention)

# Construct the formula 
model <- lmer(TN_removal_gNm2yr_log ~ fluxTP_kgy + nlcd_barren31_pct + total_precip_mm + total_km2 + 
                nlcd_past81_pct + topographicwetness + ann_max_swe + nws_drain_ratio + nlcd_forcon42_pct +
                (1|lagoslakeid) + (1|water_year), data=N_retention)
display(model) #coef.est: estimated effect and coef.se: standard errors associated with the estimated coefficients. 
#AIC: The Akaike Information Criterion (AIC) is a measure of model fit that balances goodness of fit and model complexity. 
#Lower AIC values indicate better-fitting models.
#The Deviance Information Criterion (DIC) is another measure of model fit that takes into account both the goodness of fit and model complexity. 
#Lower DIC values indicate better-fitting models

# View the model summary
summary(model)

# View the summary statistics of fixed effects
summary(model)$coefficients
