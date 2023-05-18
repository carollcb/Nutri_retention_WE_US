# Package ID: edi.1395.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: National-scale, remotely sensed lake trophic state (LTS-US) 1984-2020.
# Data set creator: Dr Michael Meyer - U.S. Geological Survey 
# Data set creator: Dr Simon Topp - U.S. Geological Survey 
# Data set creator: Dr. Tyler King - U.S. Geological Survey 
# Data set creator: Dr Robert Ladwig - Center for Limnology 
# Data set creator: Dr Rachel Pilla - Oak Ridge National Lab 
# Data set creator: Dr Hilary Dugan - Center for Limnology 
# Data set creator: Dr Jack Eggleston - U.S. Geological Survey 
# Data set creator: Dr Stephanie Hampton - Carnegie Institution for Science 
# Data set creator: Dr Dina Leech - Longwood University 
# Data set creator: Dr Isabella Oleksy - University of Wyoming 
# Data set creator:  Jesse Ross - U.S. Geological Survey 
# Data set creator: Dr Matthew Ross - Colorado State University 
# Data set creator: Dr R Woolway - Bangor University 
# Data set creator: Dr Xiao Yang - Southern Methodist University 
# Data set creator:  Matthew Brousil - Colorado State University 
# Data set creator: Dr Kate Fickas - U.S. Geological Survey 
# Data set creator: Dr Julie Padowski - Washington State University 
# Data set creator: Dr Amina Pollard - U.S. Environmental Protection Agency 
# Data set creator: Dr Jianning Ren - University of Nevada - Reno 
# Data set creator: Dr Jacob Zwart - U.S. Geological Survey 
# Metadata Provider: Dr Michael Meyer - U.S. Geological Survey 
# Contact: Dr Michael Meyer - Research Geographer U.S. Geological Survey  - mfmeyer@usgs.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1395/1/2408a538ea395572153e16b189ccacc6" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Hylak_id",     
                 "year",     
                 "mean_prob_dys",     
                 "var_prob_dys",     
                 "mean_prob_eumixo",     
                 "var_prob_eumixo",     
                 "mean_prob_oligo",     
                 "var_prob_oligo",     
                 "categorical_ts"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Hylak_id)=="factor") dt1$Hylak_id <-as.numeric(levels(dt1$Hylak_id))[as.integer(dt1$Hylak_id) ]               
if (class(dt1$Hylak_id)=="character") dt1$Hylak_id <-as.numeric(dt1$Hylak_id)
if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)
if (class(dt1$mean_prob_dys)=="factor") dt1$mean_prob_dys <-as.numeric(levels(dt1$mean_prob_dys))[as.integer(dt1$mean_prob_dys) ]               
if (class(dt1$mean_prob_dys)=="character") dt1$mean_prob_dys <-as.numeric(dt1$mean_prob_dys)
if (class(dt1$var_prob_dys)=="factor") dt1$var_prob_dys <-as.numeric(levels(dt1$var_prob_dys))[as.integer(dt1$var_prob_dys) ]               
if (class(dt1$var_prob_dys)=="character") dt1$var_prob_dys <-as.numeric(dt1$var_prob_dys)
if (class(dt1$mean_prob_eumixo)=="factor") dt1$mean_prob_eumixo <-as.numeric(levels(dt1$mean_prob_eumixo))[as.integer(dt1$mean_prob_eumixo) ]               
if (class(dt1$mean_prob_eumixo)=="character") dt1$mean_prob_eumixo <-as.numeric(dt1$mean_prob_eumixo)
if (class(dt1$var_prob_eumixo)=="factor") dt1$var_prob_eumixo <-as.numeric(levels(dt1$var_prob_eumixo))[as.integer(dt1$var_prob_eumixo) ]               
if (class(dt1$var_prob_eumixo)=="character") dt1$var_prob_eumixo <-as.numeric(dt1$var_prob_eumixo)
if (class(dt1$mean_prob_oligo)=="factor") dt1$mean_prob_oligo <-as.numeric(levels(dt1$mean_prob_oligo))[as.integer(dt1$mean_prob_oligo) ]               
if (class(dt1$mean_prob_oligo)=="character") dt1$mean_prob_oligo <-as.numeric(dt1$mean_prob_oligo)
if (class(dt1$var_prob_oligo)=="factor") dt1$var_prob_oligo <-as.numeric(levels(dt1$var_prob_oligo))[as.integer(dt1$var_prob_oligo) ]               
if (class(dt1$var_prob_oligo)=="character") dt1$var_prob_oligo <-as.numeric(dt1$var_prob_oligo)
if (class(dt1$categorical_ts)!="factor") dt1$categorical_ts<- as.factor(dt1$categorical_ts)

# Convert Missing Values to NA for non-dates

dt1$Hylak_id <- ifelse((trimws(as.character(dt1$Hylak_id))==trimws("NA")),NA,dt1$Hylak_id)               
suppressWarnings(dt1$Hylak_id <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Hylak_id))==as.character(as.numeric("NA"))),NA,dt1$Hylak_id))
dt1$year <- ifelse((trimws(as.character(dt1$year))==trimws("NA")),NA,dt1$year)               
suppressWarnings(dt1$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year))==as.character(as.numeric("NA"))),NA,dt1$year))
dt1$mean_prob_dys <- ifelse((trimws(as.character(dt1$mean_prob_dys))==trimws("NA")),NA,dt1$mean_prob_dys)               
suppressWarnings(dt1$mean_prob_dys <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_prob_dys))==as.character(as.numeric("NA"))),NA,dt1$mean_prob_dys))
dt1$var_prob_dys <- ifelse((trimws(as.character(dt1$var_prob_dys))==trimws("NA")),NA,dt1$var_prob_dys)               
suppressWarnings(dt1$var_prob_dys <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$var_prob_dys))==as.character(as.numeric("NA"))),NA,dt1$var_prob_dys))
dt1$mean_prob_eumixo <- ifelse((trimws(as.character(dt1$mean_prob_eumixo))==trimws("NA")),NA,dt1$mean_prob_eumixo)               
suppressWarnings(dt1$mean_prob_eumixo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_prob_eumixo))==as.character(as.numeric("NA"))),NA,dt1$mean_prob_eumixo))
dt1$var_prob_eumixo <- ifelse((trimws(as.character(dt1$var_prob_eumixo))==trimws("NA")),NA,dt1$var_prob_eumixo)               
suppressWarnings(dt1$var_prob_eumixo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$var_prob_eumixo))==as.character(as.numeric("NA"))),NA,dt1$var_prob_eumixo))
dt1$mean_prob_oligo <- ifelse((trimws(as.character(dt1$mean_prob_oligo))==trimws("NA")),NA,dt1$mean_prob_oligo)               
suppressWarnings(dt1$mean_prob_oligo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_prob_oligo))==as.character(as.numeric("NA"))),NA,dt1$mean_prob_oligo))
dt1$var_prob_oligo <- ifelse((trimws(as.character(dt1$var_prob_oligo))==trimws("NA")),NA,dt1$var_prob_oligo)               
suppressWarnings(dt1$var_prob_oligo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$var_prob_oligo))==as.character(as.numeric("NA"))),NA,dt1$var_prob_oligo))
dt1$categorical_ts <- as.factor(ifelse((trimws(as.character(dt1$categorical_ts))==trimws("NA")),NA,as.character(dt1$categorical_ts)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Hylak_id)
summary(year)
summary(mean_prob_dys)
summary(var_prob_dys)
summary(mean_prob_eumixo)
summary(var_prob_eumixo)
summary(mean_prob_oligo)
summary(var_prob_oligo)
summary(categorical_ts) 
# Get more details on character variables

summary(as.factor(dt1$categorical_ts))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1395/1/95b484f3bf81cbe035e27ee51df4df32" 
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
                 "prob_dys_mlr",     
                 "prob_eumixo_mlr",     
                 "prob_oligo_mlr",     
                 "prob_dys_mlp",     
                 "prob_eumixo_mlp",     
                 "prob_oligo_mlp",     
                 "prob_dys_xgb",     
                 "prob_eumixo_xgb",     
                 "prob_oligo_xgb"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Hylak_id)=="factor") dt2$Hylak_id <-as.numeric(levels(dt2$Hylak_id))[as.integer(dt2$Hylak_id) ]               
if (class(dt2$Hylak_id)=="character") dt2$Hylak_id <-as.numeric(dt2$Hylak_id)
if (class(dt2$year)=="factor") dt2$year <-as.numeric(levels(dt2$year))[as.integer(dt2$year) ]               
if (class(dt2$year)=="character") dt2$year <-as.numeric(dt2$year)
if (class(dt2$prob_dys_mlr)=="factor") dt2$prob_dys_mlr <-as.numeric(levels(dt2$prob_dys_mlr))[as.integer(dt2$prob_dys_mlr) ]               
if (class(dt2$prob_dys_mlr)=="character") dt2$prob_dys_mlr <-as.numeric(dt2$prob_dys_mlr)
if (class(dt2$prob_eumixo_mlr)=="factor") dt2$prob_eumixo_mlr <-as.numeric(levels(dt2$prob_eumixo_mlr))[as.integer(dt2$prob_eumixo_mlr) ]               
if (class(dt2$prob_eumixo_mlr)=="character") dt2$prob_eumixo_mlr <-as.numeric(dt2$prob_eumixo_mlr)
if (class(dt2$prob_oligo_mlr)=="factor") dt2$prob_oligo_mlr <-as.numeric(levels(dt2$prob_oligo_mlr))[as.integer(dt2$prob_oligo_mlr) ]               
if (class(dt2$prob_oligo_mlr)=="character") dt2$prob_oligo_mlr <-as.numeric(dt2$prob_oligo_mlr)
if (class(dt2$prob_dys_mlp)=="factor") dt2$prob_dys_mlp <-as.numeric(levels(dt2$prob_dys_mlp))[as.integer(dt2$prob_dys_mlp) ]               
if (class(dt2$prob_dys_mlp)=="character") dt2$prob_dys_mlp <-as.numeric(dt2$prob_dys_mlp)
if (class(dt2$prob_eumixo_mlp)=="factor") dt2$prob_eumixo_mlp <-as.numeric(levels(dt2$prob_eumixo_mlp))[as.integer(dt2$prob_eumixo_mlp) ]               
if (class(dt2$prob_eumixo_mlp)=="character") dt2$prob_eumixo_mlp <-as.numeric(dt2$prob_eumixo_mlp)
if (class(dt2$prob_oligo_mlp)=="factor") dt2$prob_oligo_mlp <-as.numeric(levels(dt2$prob_oligo_mlp))[as.integer(dt2$prob_oligo_mlp) ]               
if (class(dt2$prob_oligo_mlp)=="character") dt2$prob_oligo_mlp <-as.numeric(dt2$prob_oligo_mlp)
if (class(dt2$prob_dys_xgb)=="factor") dt2$prob_dys_xgb <-as.numeric(levels(dt2$prob_dys_xgb))[as.integer(dt2$prob_dys_xgb) ]               
if (class(dt2$prob_dys_xgb)=="character") dt2$prob_dys_xgb <-as.numeric(dt2$prob_dys_xgb)
if (class(dt2$prob_eumixo_xgb)=="factor") dt2$prob_eumixo_xgb <-as.numeric(levels(dt2$prob_eumixo_xgb))[as.integer(dt2$prob_eumixo_xgb) ]               
if (class(dt2$prob_eumixo_xgb)=="character") dt2$prob_eumixo_xgb <-as.numeric(dt2$prob_eumixo_xgb)
if (class(dt2$prob_oligo_xgb)=="factor") dt2$prob_oligo_xgb <-as.numeric(levels(dt2$prob_oligo_xgb))[as.integer(dt2$prob_oligo_xgb) ]               
if (class(dt2$prob_oligo_xgb)=="character") dt2$prob_oligo_xgb <-as.numeric(dt2$prob_oligo_xgb)

# Convert Missing Values to NA for non-dates

dt2$Hylak_id <- ifelse((trimws(as.character(dt2$Hylak_id))==trimws("NA")),NA,dt2$Hylak_id)               
suppressWarnings(dt2$Hylak_id <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Hylak_id))==as.character(as.numeric("NA"))),NA,dt2$Hylak_id))
dt2$year <- ifelse((trimws(as.character(dt2$year))==trimws("NA")),NA,dt2$year)               
suppressWarnings(dt2$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$year))==as.character(as.numeric("NA"))),NA,dt2$year))
dt2$prob_dys_mlr <- ifelse((trimws(as.character(dt2$prob_dys_mlr))==trimws("NA")),NA,dt2$prob_dys_mlr)               
suppressWarnings(dt2$prob_dys_mlr <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_dys_mlr))==as.character(as.numeric("NA"))),NA,dt2$prob_dys_mlr))
dt2$prob_eumixo_mlr <- ifelse((trimws(as.character(dt2$prob_eumixo_mlr))==trimws("NA")),NA,dt2$prob_eumixo_mlr)               
suppressWarnings(dt2$prob_eumixo_mlr <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_eumixo_mlr))==as.character(as.numeric("NA"))),NA,dt2$prob_eumixo_mlr))
dt2$prob_oligo_mlr <- ifelse((trimws(as.character(dt2$prob_oligo_mlr))==trimws("NA")),NA,dt2$prob_oligo_mlr)               
suppressWarnings(dt2$prob_oligo_mlr <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_oligo_mlr))==as.character(as.numeric("NA"))),NA,dt2$prob_oligo_mlr))
dt2$prob_dys_mlp <- ifelse((trimws(as.character(dt2$prob_dys_mlp))==trimws("NA")),NA,dt2$prob_dys_mlp)               
suppressWarnings(dt2$prob_dys_mlp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_dys_mlp))==as.character(as.numeric("NA"))),NA,dt2$prob_dys_mlp))
dt2$prob_eumixo_mlp <- ifelse((trimws(as.character(dt2$prob_eumixo_mlp))==trimws("NA")),NA,dt2$prob_eumixo_mlp)               
suppressWarnings(dt2$prob_eumixo_mlp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_eumixo_mlp))==as.character(as.numeric("NA"))),NA,dt2$prob_eumixo_mlp))
dt2$prob_oligo_mlp <- ifelse((trimws(as.character(dt2$prob_oligo_mlp))==trimws("NA")),NA,dt2$prob_oligo_mlp)               
suppressWarnings(dt2$prob_oligo_mlp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_oligo_mlp))==as.character(as.numeric("NA"))),NA,dt2$prob_oligo_mlp))
dt2$prob_dys_xgb <- ifelse((trimws(as.character(dt2$prob_dys_xgb))==trimws("NA")),NA,dt2$prob_dys_xgb)               
suppressWarnings(dt2$prob_dys_xgb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_dys_xgb))==as.character(as.numeric("NA"))),NA,dt2$prob_dys_xgb))
dt2$prob_eumixo_xgb <- ifelse((trimws(as.character(dt2$prob_eumixo_xgb))==trimws("NA")),NA,dt2$prob_eumixo_xgb)               
suppressWarnings(dt2$prob_eumixo_xgb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_eumixo_xgb))==as.character(as.numeric("NA"))),NA,dt2$prob_eumixo_xgb))
dt2$prob_oligo_xgb <- ifelse((trimws(as.character(dt2$prob_oligo_xgb))==trimws("NA")),NA,dt2$prob_oligo_xgb)               
suppressWarnings(dt2$prob_oligo_xgb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$prob_oligo_xgb))==as.character(as.numeric("NA"))),NA,dt2$prob_oligo_xgb))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Hylak_id)
summary(year)
summary(prob_dys_mlr)
summary(prob_eumixo_mlr)
summary(prob_oligo_mlr)
summary(prob_dys_mlp)
summary(prob_eumixo_mlp)
summary(prob_oligo_mlp)
summary(prob_dys_xgb)
summary(prob_eumixo_xgb)
summary(prob_oligo_xgb) 
# Get more details on character variables

detach(dt2)               


