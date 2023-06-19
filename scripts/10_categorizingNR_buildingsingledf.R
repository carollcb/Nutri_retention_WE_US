##converting numerical retention data to categorical data to run the RF model
library(tidyverse)

source("scripts/pulling-covariates-data.R")

##Reading final dataset
data <- read.csv("data/final_retention_dataset.csv")%>%
  mutate(lagoslakeid = as.character(lagoslakeid)) 

##P retention
hist(data$Pret_coef)
summary(data$Pret_coef)
sort(data$Pret_coef)

#To define the new categorical variable:
data <- within(data, {   
  Pret_coef.cat <- NA # need to initialize variable
  Pret_coef.cat[Pret_coef < 9] <- "Low"
  Pret_coef.cat[Pret_coef >= 9 & Pret_coef < 19] <- "Regular"
  Pret_coef.cat[Pret_coef >= 19] <- "High"
} )
str(data)
summary(data$Pret_coef.cat)

data$Pret_coef.cat <- factor(data$Pret_coef.cat, levels = c("High", "Regular", "Low"))
str(data)
summary(data$Pret_coef.cat)

##N retention
hist(data$TN_removal_gNm2yr)
summary(data$TN_removal_gNm2yr)
sort(data$TN_removal_gNm2yr)

#To define the new categorical variable:
data <- within(data, {   
  TN_removal_gNm2yr.cat <- NA # need to initialize variable
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr < 13000] <- "Low"
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr >= 13000 & TN_removal_gNm2yr < 397000] <- "Regular"
  TN_removal_gNm2yr.cat[TN_removal_gNm2yr >= 397000] <- "High"
} )
str(data)
summary(data$TN_removal_gNm2yr.cat)

data$TN_removal_gNm2yr.cat <- factor(data$TN_removal_gNm2yr.cat, levels = c("High", "Regular", "Low"))
str(data)
summary(data$TN_removal_gNm2yr.cat)

###testing
#join to hydrolakes first to get Hylak_id and then proceed with steps below


data_clim <- inner_join(data, yearly_clim_data, by=c('lagoslakeid', 'water_year'))

data_clim_connect <- inner_join(data_clim, connect_data_lagos_final, by="lagoslakeid")
