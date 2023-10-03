##Code to calculate IS from Bella

library(tidyverse)

## Calc input seasonality (IS) as in Hammond et al. 2023 doi.org/10.1029/2022EF003092 
#A high value of IS means much of the sites surface water input occurs within a short period, whereas a low value of IS means that surface water input is more evenly distributed throughout the year 


#Get annual precip first
annual_precip <- clim_data_sites %>%
  select(lagoslakeid, climate_year, climate_month, climate_ppt_mm) %>%
  pivot_wider(names_from = climate_month, values_from = climate_ppt_mm) %>%
  rowwise() %>%
  mutate(annual = sum(c_across(c(3:14)), na.rm = T)) %>%
  ungroup() %>%
  select(lagoslakeid, climate_year, annual)

#calculate input seasonality
input_seasonality <- clim_data_sites %>%
  select(lagoslakeid, climate_year, climate_month, climate_ppt_mm) %>% 
  left_join(., annual_precip) %>%
  mutate(paren = abs(climate_ppt_mm - (annual/12))) %>%
  group_by(lagoslakeid, climate_year) %>%
  summarize(sum_paren = sum(paren),
            annual = mean(annual)) %>%
  mutate(IS = (1/annual) * sum_paren,
         lagoslakeid = as.character(as.numeric(lagoslakeid))) %>%
  group_by(lagoslakeid) %>%
  summarize(IS=median(IS, na.rm=TRUE))

write.csv(input_seasonality, "data/is.csv")
