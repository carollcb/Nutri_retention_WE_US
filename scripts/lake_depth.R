# Package ID: edi.1043.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US DEPTH v1.0: Data module of observed maximum and mean lake depths for a subset of lakes in the conterminous U.S..
# Data set creator:  Jemma Stachelek - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Jessica DÃ­az VÃ¡zquez - Michigan State University 
# Data set creator:  Arika Hawkins - Michigan State University 
# Data set creator:  Ellie Phillips - Michigan State University 
# Data set creator:  Allie Shoffner - Michigan State University 
# Data set creator:  Ian McCullough - Michigan State University 
# Data set creator:  Katelyn King - Michigan State University 
# Data set creator:  Jake Namovich - Michigan State University 
# Data set creator:  Lindsie Egedy - Michigan State University 
# Data set creator:  Maggie Haite - Michigan State University 
# Data set creator:  Patrick Hanly - Michigan State University 
# Data set creator:  Katherine Webster - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Data set creator:  Patricia Soranno - Michigan State University 
# Metadata Provider:  Jemma Stachelek - Michigan State University 
# Metadata Provider:  Patrick Hanly - Michigan State University 
# Metadata Provider:  Ian McCullough - Michigan State University 
# Metadata Provider:  Katherine Webster - Michigan State University 
# Metadata Provider:  Patricia Soranno - Michigan State University 
# Contact:  Jemma Stachelek -  Michigan State University  - stachel2@msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1043/1/f6ffb2325156008edffbea5b6c1ea24c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lagoslakeid",     
                 "lake_namegnis",     
                 "lake_states",     
                 "lake_depth_state",     
                 "lake_lat_decdeg",     
                 "lake_lon_decdeg",     
                 "lake_maxdepth_m",     
                 "lake_meandepth_m",     
                 "lake_waterarea_ha",     
                 "lake_depth_sourcename",     
                 "lake_depth_sourceurl",     
                 "lake_maxdepth_effort",     
                 "lake_meandepth_effort"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lagoslakeid)=="factor") dt1$lagoslakeid <-as.numeric(levels(dt1$lagoslakeid))[as.integer(dt1$lagoslakeid) ]               
if (class(dt1$lagoslakeid)=="character") dt1$lagoslakeid <-as.numeric(dt1$lagoslakeid)
if (class(dt1$lake_namegnis)!="factor") dt1$lake_namegnis<- as.factor(dt1$lake_namegnis)
if (class(dt1$lake_states)!="factor") dt1$lake_states<- as.factor(dt1$lake_states)
if (class(dt1$lake_depth_state)!="factor") dt1$lake_depth_state<- as.factor(dt1$lake_depth_state)
if (class(dt1$lake_lat_decdeg)=="factor") dt1$lake_lat_decdeg <-as.numeric(levels(dt1$lake_lat_decdeg))[as.integer(dt1$lake_lat_decdeg) ]               
if (class(dt1$lake_lat_decdeg)=="character") dt1$lake_lat_decdeg <-as.numeric(dt1$lake_lat_decdeg)
if (class(dt1$lake_lon_decdeg)=="factor") dt1$lake_lon_decdeg <-as.numeric(levels(dt1$lake_lon_decdeg))[as.integer(dt1$lake_lon_decdeg) ]               
if (class(dt1$lake_lon_decdeg)=="character") dt1$lake_lon_decdeg <-as.numeric(dt1$lake_lon_decdeg)
if (class(dt1$lake_maxdepth_m)=="factor") dt1$lake_maxdepth_m <-as.numeric(levels(dt1$lake_maxdepth_m))[as.integer(dt1$lake_maxdepth_m) ]               
if (class(dt1$lake_maxdepth_m)=="character") dt1$lake_maxdepth_m <-as.numeric(dt1$lake_maxdepth_m)
if (class(dt1$lake_meandepth_m)=="factor") dt1$lake_meandepth_m <-as.numeric(levels(dt1$lake_meandepth_m))[as.integer(dt1$lake_meandepth_m) ]               
if (class(dt1$lake_meandepth_m)=="character") dt1$lake_meandepth_m <-as.numeric(dt1$lake_meandepth_m)
if (class(dt1$lake_waterarea_ha)=="factor") dt1$lake_waterarea_ha <-as.numeric(levels(dt1$lake_waterarea_ha))[as.integer(dt1$lake_waterarea_ha) ]               
if (class(dt1$lake_waterarea_ha)=="character") dt1$lake_waterarea_ha <-as.numeric(dt1$lake_waterarea_ha)
if (class(dt1$lake_depth_sourcename)!="factor") dt1$lake_depth_sourcename<- as.factor(dt1$lake_depth_sourcename)
if (class(dt1$lake_depth_sourceurl)!="factor") dt1$lake_depth_sourceurl<- as.factor(dt1$lake_depth_sourceurl)
if (class(dt1$lake_maxdepth_effort)!="factor") dt1$lake_maxdepth_effort<- as.factor(dt1$lake_maxdepth_effort)
if (class(dt1$lake_meandepth_effort)!="factor") dt1$lake_meandepth_effort<- as.factor(dt1$lake_meandepth_effort)

# Convert Missing Values to NA for non-dates

dt1$lake_namegnis <- as.factor(ifelse((trimws(as.character(dt1$lake_namegnis))==trimws("NULL")),NA,as.character(dt1$lake_namegnis)))
dt1$lake_maxdepth_m <- ifelse((trimws(as.character(dt1$lake_maxdepth_m))==trimws("NA")),NA,dt1$lake_maxdepth_m)               
suppressWarnings(dt1$lake_maxdepth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_maxdepth_m))==as.character(as.numeric("NA"))),NA,dt1$lake_maxdepth_m))
dt1$lake_meandepth_m <- ifelse((trimws(as.character(dt1$lake_meandepth_m))==trimws("NA")),NA,dt1$lake_meandepth_m)               
suppressWarnings(dt1$lake_meandepth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_meandepth_m))==as.character(as.numeric("NA"))),NA,dt1$lake_meandepth_m))
dt1$lake_depth_sourcename <- as.factor(ifelse((trimws(as.character(dt1$lake_depth_sourcename))==trimws("NULL")),NA,as.character(dt1$lake_depth_sourcename)))
dt1$lake_depth_sourceurl <- as.factor(ifelse((trimws(as.character(dt1$lake_depth_sourceurl))==trimws("NULL")),NA,as.character(dt1$lake_depth_sourceurl)))
dt1$lake_meandepth_effort <- as.factor(ifelse((trimws(as.character(dt1$lake_meandepth_effort))==trimws("NULL")),NA,as.character(dt1$lake_meandepth_effort)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lagoslakeid)
summary(lake_namegnis)
summary(lake_states)
summary(lake_depth_state)
summary(lake_lat_decdeg)
summary(lake_lon_decdeg)
summary(lake_maxdepth_m)
summary(lake_meandepth_m)
summary(lake_waterarea_ha)
summary(lake_depth_sourcename)
summary(lake_depth_sourceurl)
summary(lake_maxdepth_effort)
summary(lake_meandepth_effort) 
# Get more details on character variables

summary(as.factor(dt1$lake_namegnis)) 
summary(as.factor(dt1$lake_states)) 
summary(as.factor(dt1$lake_depth_state)) 
summary(as.factor(dt1$lake_depth_sourcename)) 
summary(as.factor(dt1$lake_depth_sourceurl)) 
summary(as.factor(dt1$lake_maxdepth_effort)) 
summary(as.factor(dt1$lake_meandepth_effort))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1043/1/44c1aa06d126f79d6a68604e193b083a" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "table_name",     
                 "variable_name_new",     
                 "variable_description",     
                 "variable_name_group",     
                 "taxonomy_type",     
                 "taxonomy_division",     
                 "taxonomy_main_feature",     
                 "taxonomy_subgroup",     
                 "taxonomy_units",     
                 "units",     
                 "missing_values",     
                 "in_lagosne",     
                 "lagosne_table_name",     
                 "lagosne_variable_name",     
                 "variable_source_code1",     
                 "variable_source_code2",     
                 "methods_tool",     
                 "source_value_code",     
                 "data_type",     
                 "precision",     
                 "domain",     
                 "column_index"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$table_name)!="factor") dt2$table_name<- as.factor(dt2$table_name)
if (class(dt2$variable_name_new)!="factor") dt2$variable_name_new<- as.factor(dt2$variable_name_new)
if (class(dt2$variable_description)!="factor") dt2$variable_description<- as.factor(dt2$variable_description)
if (class(dt2$variable_name_group)!="factor") dt2$variable_name_group<- as.factor(dt2$variable_name_group)
if (class(dt2$taxonomy_type)!="factor") dt2$taxonomy_type<- as.factor(dt2$taxonomy_type)
if (class(dt2$taxonomy_division)!="factor") dt2$taxonomy_division<- as.factor(dt2$taxonomy_division)
if (class(dt2$taxonomy_main_feature)!="factor") dt2$taxonomy_main_feature<- as.factor(dt2$taxonomy_main_feature)
if (class(dt2$taxonomy_subgroup)!="factor") dt2$taxonomy_subgroup<- as.factor(dt2$taxonomy_subgroup)
if (class(dt2$taxonomy_units)!="factor") dt2$taxonomy_units<- as.factor(dt2$taxonomy_units)
if (class(dt2$units)!="factor") dt2$units<- as.factor(dt2$units)
if (class(dt2$missing_values)!="factor") dt2$missing_values<- as.factor(dt2$missing_values)
if (class(dt2$in_lagosne)!="factor") dt2$in_lagosne<- as.factor(dt2$in_lagosne)
if (class(dt2$lagosne_table_name)!="factor") dt2$lagosne_table_name<- as.factor(dt2$lagosne_table_name)
if (class(dt2$lagosne_variable_name)!="factor") dt2$lagosne_variable_name<- as.factor(dt2$lagosne_variable_name)
if (class(dt2$variable_source_code1)!="factor") dt2$variable_source_code1<- as.factor(dt2$variable_source_code1)
if (class(dt2$variable_source_code2)!="factor") dt2$variable_source_code2<- as.factor(dt2$variable_source_code2)
if (class(dt2$methods_tool)!="factor") dt2$methods_tool<- as.factor(dt2$methods_tool)
if (class(dt2$source_value_code)!="factor") dt2$source_value_code<- as.factor(dt2$source_value_code)
if (class(dt2$data_type)!="factor") dt2$data_type<- as.factor(dt2$data_type)
if (class(dt2$precision)=="factor") dt2$precision <-as.numeric(levels(dt2$precision))[as.integer(dt2$precision) ]               
if (class(dt2$precision)=="character") dt2$precision <-as.numeric(dt2$precision)
if (class(dt2$domain)!="factor") dt2$domain<- as.factor(dt2$domain)
if (class(dt2$column_index)=="factor") dt2$column_index <-as.numeric(levels(dt2$column_index))[as.integer(dt2$column_index) ]               
if (class(dt2$column_index)=="character") dt2$column_index <-as.numeric(dt2$column_index)

# Convert Missing Values to NA for non-dates

dt2$taxonomy_subgroup <- as.factor(ifelse((trimws(as.character(dt2$taxonomy_subgroup))==trimws("NULL")),NA,as.character(dt2$taxonomy_subgroup)))
dt2$taxonomy_units <- as.factor(ifelse((trimws(as.character(dt2$taxonomy_units))==trimws("NULL")),NA,as.character(dt2$taxonomy_units)))
dt2$units <- as.factor(ifelse((trimws(as.character(dt2$units))==trimws("NULL")),NA,as.character(dt2$units)))
dt2$lagosne_table_name <- as.factor(ifelse((trimws(as.character(dt2$lagosne_table_name))==trimws("NULL")),NA,as.character(dt2$lagosne_table_name)))
dt2$lagosne_variable_name <- as.factor(ifelse((trimws(as.character(dt2$lagosne_variable_name))==trimws("NULL")),NA,as.character(dt2$lagosne_variable_name)))
dt2$variable_source_code1 <- as.factor(ifelse((trimws(as.character(dt2$variable_source_code1))==trimws("NULL")),NA,as.character(dt2$variable_source_code1)))
dt2$variable_source_code2 <- as.factor(ifelse((trimws(as.character(dt2$variable_source_code2))==trimws("NULL")),NA,as.character(dt2$variable_source_code2)))
dt2$methods_tool <- as.factor(ifelse((trimws(as.character(dt2$methods_tool))==trimws("NULL")),NA,as.character(dt2$methods_tool)))
dt2$source_value_code <- as.factor(ifelse((trimws(as.character(dt2$source_value_code))==trimws("NULL")),NA,as.character(dt2$source_value_code)))
dt2$domain <- as.factor(ifelse((trimws(as.character(dt2$domain))==trimws("NULL")),NA,as.character(dt2$domain)))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(table_name)
summary(variable_name_new)
summary(variable_description)
summary(variable_name_group)
summary(taxonomy_type)
summary(taxonomy_division)
summary(taxonomy_main_feature)
summary(taxonomy_subgroup)
summary(taxonomy_units)
summary(units)
summary(missing_values)
summary(in_lagosne)
summary(lagosne_table_name)
summary(lagosne_variable_name)
summary(variable_source_code1)
summary(variable_source_code2)
summary(methods_tool)
summary(source_value_code)
summary(data_type)
summary(precision)
summary(domain)
summary(column_index) 
# Get more details on character variables

summary(as.factor(dt2$table_name)) 
summary(as.factor(dt2$variable_name_new)) 
summary(as.factor(dt2$variable_description)) 
summary(as.factor(dt2$variable_name_group)) 
summary(as.factor(dt2$taxonomy_type)) 
summary(as.factor(dt2$taxonomy_division)) 
summary(as.factor(dt2$taxonomy_main_feature)) 
summary(as.factor(dt2$taxonomy_subgroup)) 
summary(as.factor(dt2$taxonomy_units)) 
summary(as.factor(dt2$units)) 
summary(as.factor(dt2$missing_values)) 
summary(as.factor(dt2$in_lagosne)) 
summary(as.factor(dt2$lagosne_table_name)) 
summary(as.factor(dt2$lagosne_variable_name)) 
summary(as.factor(dt2$variable_source_code1)) 
summary(as.factor(dt2$variable_source_code2)) 
summary(as.factor(dt2$methods_tool)) 
summary(as.factor(dt2$source_value_code)) 
summary(as.factor(dt2$data_type)) 
summary(as.factor(dt2$domain))
detach(dt2)               

