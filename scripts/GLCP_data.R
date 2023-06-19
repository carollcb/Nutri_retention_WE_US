# Package ID: edi.394.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Global lake area, climate, and population dataset.
# Data set creator:  Stephanie Labou - Center for Environmental Research, Education, & Outreach, Washington
#State University 
# Data set creator:  Michael Meyer - School of the Environment, Washington State University 
# Data set creator:  Matthew Brousil - Center for Environmental Research, Education, & Outreach, Washington
#State University 
# Data set creator:  Alli Cramer - School of the Environment, Washington State University 
# Data set creator:  Bradley Luff - School of the Environment, Washington State University 
# Contact:  Stephanie Labou -  Center for Environmental Research, Education, & Outreach, Washington
#State University  - 
  # Contact:  Michael Meyer -  School of the Environment, Washington State University  - michael.f.meyer@wsu.edu
  # Contact:  Matthew Brousil -  Center for Environmental Research, Education, & Outreach, Washington
 # State University  - matthew.brousil@wsu.edu
# Contact:  Alli Cramer -  School of the Environment, Washington State University  - allison.cramer@wsu.edu
# Contact:  Bradley Luff -  School of the Environment, Washington State University  - bradley.luff@wsu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/394/4/b3af2a6d3205ede2469d6d6ba410c101" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year",     
                 "Hylak_id",     
                 "centr_lat",     
                 "centr_lon",     
                 "continent",     
                 "country",     
                 "bsn_lvl",     
                 "HYBAS_ID",     
                 "mean_monthly_precip_mm",     
                 "total_precip_mm",     
                 "mean_annual_temp_k",     
                 "pop_sum",     
                 "seasonal_km2",     
                 "permanent_km2",     
                 "total_km2"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)
if (class(dt1$Hylak_id)!="factor") dt1$Hylak_id<- as.factor(dt1$Hylak_id)
if (class(dt1$centr_lat)=="factor") dt1$centr_lat <-as.numeric(levels(dt1$centr_lat))[as.integer(dt1$centr_lat) ]               
if (class(dt1$centr_lat)=="character") dt1$centr_lat <-as.numeric(dt1$centr_lat)
if (class(dt1$centr_lon)=="factor") dt1$centr_lon <-as.numeric(levels(dt1$centr_lon))[as.integer(dt1$centr_lon) ]               
if (class(dt1$centr_lon)=="character") dt1$centr_lon <-as.numeric(dt1$centr_lon)
if (class(dt1$continent)!="factor") dt1$continent<- as.factor(dt1$continent)
if (class(dt1$country)!="factor") dt1$country<- as.factor(dt1$country)
if (class(dt1$bsn_lvl)!="factor") dt1$bsn_lvl<- as.factor(dt1$bsn_lvl)
if (class(dt1$HYBAS_ID)!="factor") dt1$HYBAS_ID<- as.factor(dt1$HYBAS_ID)
if (class(dt1$mean_monthly_precip_mm)=="factor") dt1$mean_monthly_precip_mm <-as.numeric(levels(dt1$mean_monthly_precip_mm))[as.integer(dt1$mean_monthly_precip_mm) ]               
if (class(dt1$mean_monthly_precip_mm)=="character") dt1$mean_monthly_precip_mm <-as.numeric(dt1$mean_monthly_precip_mm)
if (class(dt1$total_precip_mm)=="factor") dt1$total_precip_mm <-as.numeric(levels(dt1$total_precip_mm))[as.integer(dt1$total_precip_mm) ]               
if (class(dt1$total_precip_mm)=="character") dt1$total_precip_mm <-as.numeric(dt1$total_precip_mm)
if (class(dt1$mean_annual_temp_k)=="factor") dt1$mean_annual_temp_k <-as.numeric(levels(dt1$mean_annual_temp_k))[as.integer(dt1$mean_annual_temp_k) ]               
if (class(dt1$mean_annual_temp_k)=="character") dt1$mean_annual_temp_k <-as.numeric(dt1$mean_annual_temp_k)
if (class(dt1$pop_sum)=="factor") dt1$pop_sum <-as.numeric(levels(dt1$pop_sum))[as.integer(dt1$pop_sum) ]               
if (class(dt1$pop_sum)=="character") dt1$pop_sum <-as.numeric(dt1$pop_sum)
if (class(dt1$seasonal_km2)=="factor") dt1$seasonal_km2 <-as.numeric(levels(dt1$seasonal_km2))[as.integer(dt1$seasonal_km2) ]               
if (class(dt1$seasonal_km2)=="character") dt1$seasonal_km2 <-as.numeric(dt1$seasonal_km2)
if (class(dt1$permanent_km2)=="factor") dt1$permanent_km2 <-as.numeric(levels(dt1$permanent_km2))[as.integer(dt1$permanent_km2) ]               
if (class(dt1$permanent_km2)=="character") dt1$permanent_km2 <-as.numeric(dt1$permanent_km2)
if (class(dt1$total_km2)=="factor") dt1$total_km2 <-as.numeric(levels(dt1$total_km2))[as.integer(dt1$total_km2) ]               
if (class(dt1$total_km2)=="character") dt1$total_km2 <-as.numeric(dt1$total_km2)

# Convert Missing Values to NA for non-dates

dt1$year <- ifelse((trimws(as.character(dt1$year))==trimws("NA")),NA,dt1$year)               
suppressWarnings(dt1$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year))==as.character(as.numeric("NA"))),NA,dt1$year))
dt1$Hylak_id <- as.factor(ifelse((trimws(as.character(dt1$Hylak_id))==trimws("NA")),NA,as.character(dt1$Hylak_id)))
dt1$centr_lat <- ifelse((trimws(as.character(dt1$centr_lat))==trimws("NA")),NA,dt1$centr_lat)               
suppressWarnings(dt1$centr_lat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$centr_lat))==as.character(as.numeric("NA"))),NA,dt1$centr_lat))
dt1$centr_lon <- ifelse((trimws(as.character(dt1$centr_lon))==trimws("NA")),NA,dt1$centr_lon)               
suppressWarnings(dt1$centr_lon <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$centr_lon))==as.character(as.numeric("NA"))),NA,dt1$centr_lon))
dt1$continent <- as.factor(ifelse((trimws(as.character(dt1$continent))==trimws("NA")),NA,as.character(dt1$continent)))
dt1$country <- as.factor(ifelse((trimws(as.character(dt1$country))==trimws("NA")),NA,as.character(dt1$country)))
dt1$bsn_lvl <- as.factor(ifelse((trimws(as.character(dt1$bsn_lvl))==trimws("NA")),NA,as.character(dt1$bsn_lvl)))
dt1$HYBAS_ID <- as.factor(ifelse((trimws(as.character(dt1$HYBAS_ID))==trimws("NA")),NA,as.character(dt1$HYBAS_ID)))
dt1$mean_monthly_precip_mm <- ifelse((trimws(as.character(dt1$mean_monthly_precip_mm))==trimws("NA")),NA,dt1$mean_monthly_precip_mm)               
suppressWarnings(dt1$mean_monthly_precip_mm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_monthly_precip_mm))==as.character(as.numeric("NA"))),NA,dt1$mean_monthly_precip_mm))
dt1$total_precip_mm <- ifelse((trimws(as.character(dt1$total_precip_mm))==trimws("NA")),NA,dt1$total_precip_mm)               
suppressWarnings(dt1$total_precip_mm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$total_precip_mm))==as.character(as.numeric("NA"))),NA,dt1$total_precip_mm))
dt1$mean_annual_temp_k <- ifelse((trimws(as.character(dt1$mean_annual_temp_k))==trimws("NA")),NA,dt1$mean_annual_temp_k)               
suppressWarnings(dt1$mean_annual_temp_k <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_annual_temp_k))==as.character(as.numeric("NA"))),NA,dt1$mean_annual_temp_k))
dt1$pop_sum <- ifelse((trimws(as.character(dt1$pop_sum))==trimws("NA")),NA,dt1$pop_sum)               
suppressWarnings(dt1$pop_sum <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pop_sum))==as.character(as.numeric("NA"))),NA,dt1$pop_sum))
dt1$seasonal_km2 <- ifelse((trimws(as.character(dt1$seasonal_km2))==trimws("NA")),NA,dt1$seasonal_km2)               
suppressWarnings(dt1$seasonal_km2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$seasonal_km2))==as.character(as.numeric("NA"))),NA,dt1$seasonal_km2))
dt1$permanent_km2 <- ifelse((trimws(as.character(dt1$permanent_km2))==trimws("NA")),NA,dt1$permanent_km2)               
suppressWarnings(dt1$permanent_km2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$permanent_km2))==as.character(as.numeric("NA"))),NA,dt1$permanent_km2))
dt1$total_km2 <- ifelse((trimws(as.character(dt1$total_km2))==trimws("NA")),NA,dt1$total_km2)               
suppressWarnings(dt1$total_km2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$total_km2))==as.character(as.numeric("NA"))),NA,dt1$total_km2))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year)
summary(Hylak_id)
summary(centr_lat)
summary(centr_lon)
summary(continent)
summary(country)
summary(bsn_lvl)
summary(HYBAS_ID)
summary(mean_monthly_precip_mm)
summary(total_precip_mm)
summary(mean_annual_temp_k)
summary(pop_sum)
summary(seasonal_km2)
summary(permanent_km2)
summary(total_km2) 
# Get more details on character variables

summary(as.factor(dt1$Hylak_id)) 
summary(as.factor(dt1$continent)) 
summary(as.factor(dt1$country)) 
summary(as.factor(dt1$bsn_lvl)) 
summary(as.factor(dt1$HYBAS_ID))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/394/4/c3ca4da728dcdaca1176fc52a641bd0e" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Hylak_id",     
                 "year",     
                 "no_obvs_km2",     
                 "not_water_km2",     
                 "no_data_to_not_water",     
                 "no_data_to_seasonal",     
                 "no_data_to_permanent",     
                 "no_data_to_total"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Hylak_id)!="factor") dt2$Hylak_id<- as.factor(dt2$Hylak_id)
if (class(dt2$year)=="factor") dt2$year <-as.numeric(levels(dt2$year))[as.integer(dt2$year) ]               
if (class(dt2$year)=="character") dt2$year <-as.numeric(dt2$year)
if (class(dt2$no_obvs_km2)=="factor") dt2$no_obvs_km2 <-as.numeric(levels(dt2$no_obvs_km2))[as.integer(dt2$no_obvs_km2) ]               
if (class(dt2$no_obvs_km2)=="character") dt2$no_obvs_km2 <-as.numeric(dt2$no_obvs_km2)
if (class(dt2$not_water_km2)=="factor") dt2$not_water_km2 <-as.numeric(levels(dt2$not_water_km2))[as.integer(dt2$not_water_km2) ]               
if (class(dt2$not_water_km2)=="character") dt2$not_water_km2 <-as.numeric(dt2$not_water_km2)
if (class(dt2$no_data_to_not_water)=="factor") dt2$no_data_to_not_water <-as.numeric(levels(dt2$no_data_to_not_water))[as.integer(dt2$no_data_to_not_water) ]               
if (class(dt2$no_data_to_not_water)=="character") dt2$no_data_to_not_water <-as.numeric(dt2$no_data_to_not_water)
if (class(dt2$no_data_to_seasonal)=="factor") dt2$no_data_to_seasonal <-as.numeric(levels(dt2$no_data_to_seasonal))[as.integer(dt2$no_data_to_seasonal) ]               
if (class(dt2$no_data_to_seasonal)=="character") dt2$no_data_to_seasonal <-as.numeric(dt2$no_data_to_seasonal)
if (class(dt2$no_data_to_permanent)=="factor") dt2$no_data_to_permanent <-as.numeric(levels(dt2$no_data_to_permanent))[as.integer(dt2$no_data_to_permanent) ]               
if (class(dt2$no_data_to_permanent)=="character") dt2$no_data_to_permanent <-as.numeric(dt2$no_data_to_permanent)
if (class(dt2$no_data_to_total)=="factor") dt2$no_data_to_total <-as.numeric(levels(dt2$no_data_to_total))[as.integer(dt2$no_data_to_total) ]               
if (class(dt2$no_data_to_total)=="character") dt2$no_data_to_total <-as.numeric(dt2$no_data_to_total)

# Convert Missing Values to NA for non-dates

dt2$no_data_to_not_water <- ifelse((trimws(as.character(dt2$no_data_to_not_water))==trimws("Inf")),NA,dt2$no_data_to_not_water)               
suppressWarnings(dt2$no_data_to_not_water <- ifelse(!is.na(as.numeric("Inf")) & (trimws(as.character(dt2$no_data_to_not_water))==as.character(as.numeric("Inf"))),NA,dt2$no_data_to_not_water))
dt2$no_data_to_not_water <- ifelse((trimws(as.character(dt2$no_data_to_not_water))==trimws("NA")),NA,dt2$no_data_to_not_water)               
suppressWarnings(dt2$no_data_to_not_water <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$no_data_to_not_water))==as.character(as.numeric("NA"))),NA,dt2$no_data_to_not_water))
dt2$no_data_to_seasonal <- ifelse((trimws(as.character(dt2$no_data_to_seasonal))==trimws("Inf")),NA,dt2$no_data_to_seasonal)               
suppressWarnings(dt2$no_data_to_seasonal <- ifelse(!is.na(as.numeric("Inf")) & (trimws(as.character(dt2$no_data_to_seasonal))==as.character(as.numeric("Inf"))),NA,dt2$no_data_to_seasonal))
dt2$no_data_to_seasonal <- ifelse((trimws(as.character(dt2$no_data_to_seasonal))==trimws("NA")),NA,dt2$no_data_to_seasonal)               
suppressWarnings(dt2$no_data_to_seasonal <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$no_data_to_seasonal))==as.character(as.numeric("NA"))),NA,dt2$no_data_to_seasonal))
dt2$no_data_to_permanent <- ifelse((trimws(as.character(dt2$no_data_to_permanent))==trimws("Inf")),NA,dt2$no_data_to_permanent)               
suppressWarnings(dt2$no_data_to_permanent <- ifelse(!is.na(as.numeric("Inf")) & (trimws(as.character(dt2$no_data_to_permanent))==as.character(as.numeric("Inf"))),NA,dt2$no_data_to_permanent))
dt2$no_data_to_permanent <- ifelse((trimws(as.character(dt2$no_data_to_permanent))==trimws("NA")),NA,dt2$no_data_to_permanent)               
suppressWarnings(dt2$no_data_to_permanent <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$no_data_to_permanent))==as.character(as.numeric("NA"))),NA,dt2$no_data_to_permanent))
dt2$no_data_to_total <- ifelse((trimws(as.character(dt2$no_data_to_total))==trimws("Inf")),NA,dt2$no_data_to_total)               
suppressWarnings(dt2$no_data_to_total <- ifelse(!is.na(as.numeric("Inf")) & (trimws(as.character(dt2$no_data_to_total))==as.character(as.numeric("Inf"))),NA,dt2$no_data_to_total))
dt2$no_data_to_total <- ifelse((trimws(as.character(dt2$no_data_to_total))==trimws("NA")),NA,dt2$no_data_to_total)               
suppressWarnings(dt2$no_data_to_total <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$no_data_to_total))==as.character(as.numeric("NA"))),NA,dt2$no_data_to_total))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Hylak_id)
summary(year)
summary(no_obvs_km2)
summary(not_water_km2)
summary(no_data_to_not_water)
summary(no_data_to_seasonal)
summary(no_data_to_permanent)
summary(no_data_to_total) 
# Get more details on character variables

summary(as.factor(dt2$Hylak_id))
detach(dt2)               




