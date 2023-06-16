##converting numerical retention data to categorical data to run the RF model
library(tidyverse)

source("scripts/pulling-covariates-data.R")

##Reading final dataset
data <- read.csv("data/final_retention_dataset.csv")

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
upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)%>%
  distinct(lagoslakeid, .keep_all = TRUE) 

hydrolakes_lagos <- st_read("shps/joined_hydrolakes_lagos_Final.shp") %>% 
  rename(lagoslakeid = lagoslakei)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  mutate(res_time_yr = Res_time/365)

hydrolakes_upstream_sites <- inner_join(upstream_sites_lagos, hydrolakes_lagos, by="lagoslakeid")
