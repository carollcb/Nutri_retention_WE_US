library(data.table)
# EDI ---------------------------------------------------------------------


#Pull from EDI
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1

# Package ID: edi.854.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical characteristics of lakes and their watersheds in the conterminous U.S..
# Data set creator:  Nicole Smith - Michigan State University 
# Data set creator:  Katherine Webster - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Data set creator:  Patricia Soranno - Michigan State University 
# Contact:  Kendra Cheruvelil -  Michigan State University  - ksc@msu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/854/1/007ca4f5ec02bb5809fc661dcfa7a903") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: lakeinformation 
lakeinformation <-read_delim(infile1  
                             ,delim=","   
                             ,skip=1 
                             ,quote='"'  
                             , col_names=c( 
                               "lagoslakeid",   
                               "lake_nhdid",   
                               "lake_nhdfcode",   
                               "lake_nhdftype",   
                               "lake_reachcode",   
                               "lake_namegnis",   
                               "lake_namelagos",   
                               "lake_onlandborder",   
                               "lake_ismultipart",   
                               "lake_missingws",   
                               "lake_shapeflag",   
                               "lake_lat_decdeg",   
                               "lake_lon_decdeg",   
                               "lake_elevation_m",   
                               "lake_centroidstate",   
                               "lake_states",   
                               "lake_county",   
                               "lake_countyfips",   
                               "lake_huc12",   
                               "buff100_zoneid",   
                               "buff500_zoneid",   
                               "ws_zoneid",   
                               "nws_zoneid",   
                               "hu12_zoneid",   
                               "hu8_zoneid",   
                               "hu4_zoneid",   
                               "county_zoneid",   
                               "state_zoneid",   
                               "epanutr_zoneid",   
                               "omernik3_zoneid",   
                               "wwf_zoneid",   
                               "mlra_zoneid",   
                               "bailey_zoneid",   
                               "neon_zoneid"   ), 
                             col_types=list(
                               col_number() ,  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(), 
                               col_number() , 
                               col_number() , 
                               col_number() ,  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character(),  
                               col_character()), 
                             na=c(" ",".","NA")  )


# Convert Missing Values to NA for individual vectors 
lakeinformation$lagoslakeid <- ifelse((trimws(as.character(lakeinformation$lagoslakeid))==trimws("NA")),NA,lakeinformation$lagoslakeid)               
suppressWarnings(lakeinformation$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lagoslakeid))==as.character(as.numeric("NA"))),NA,lakeinformation$lagoslakeid))
lakeinformation$lake_nhdid <- ifelse((trimws(as.character(lakeinformation$lake_nhdid))==trimws("NA")),NA,lakeinformation$lake_nhdid)               
suppressWarnings(lakeinformation$lake_nhdid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_nhdid))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_nhdid))
lakeinformation$lake_nhdfcode <- ifelse((trimws(as.character(lakeinformation$lake_nhdfcode))==trimws("NA")),NA,lakeinformation$lake_nhdfcode)               
suppressWarnings(lakeinformation$lake_nhdfcode <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_nhdfcode))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_nhdfcode))
lakeinformation$lake_nhdftype <- ifelse((trimws(as.character(lakeinformation$lake_nhdftype))==trimws("NA")),NA,lakeinformation$lake_nhdftype)               
suppressWarnings(lakeinformation$lake_nhdftype <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_nhdftype))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_nhdftype))
lakeinformation$lake_reachcode <- ifelse((trimws(as.character(lakeinformation$lake_reachcode))==trimws("NA")),NA,lakeinformation$lake_reachcode)               
suppressWarnings(lakeinformation$lake_reachcode <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_reachcode))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_reachcode))
lakeinformation$lake_namegnis <- ifelse((trimws(as.character(lakeinformation$lake_namegnis))==trimws("NA")),NA,lakeinformation$lake_namegnis)               
suppressWarnings(lakeinformation$lake_namegnis <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_namegnis))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_namegnis))
lakeinformation$lake_namelagos <- ifelse((trimws(as.character(lakeinformation$lake_namelagos))==trimws("NA")),NA,lakeinformation$lake_namelagos)               
suppressWarnings(lakeinformation$lake_namelagos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_namelagos))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_namelagos))
lakeinformation$lake_onlandborder <- ifelse((trimws(as.character(lakeinformation$lake_onlandborder))==trimws("NA")),NA,lakeinformation$lake_onlandborder)               
suppressWarnings(lakeinformation$lake_onlandborder <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_onlandborder))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_onlandborder))
lakeinformation$lake_ismultipart <- ifelse((trimws(as.character(lakeinformation$lake_ismultipart))==trimws("NA")),NA,lakeinformation$lake_ismultipart)               
suppressWarnings(lakeinformation$lake_ismultipart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_ismultipart))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_ismultipart))
lakeinformation$lake_missingws <- ifelse((trimws(as.character(lakeinformation$lake_missingws))==trimws("NA")),NA,lakeinformation$lake_missingws)               
suppressWarnings(lakeinformation$lake_missingws <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_missingws))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_missingws))
lakeinformation$lake_shapeflag <- ifelse((trimws(as.character(lakeinformation$lake_shapeflag))==trimws("NA")),NA,lakeinformation$lake_shapeflag)               
suppressWarnings(lakeinformation$lake_shapeflag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_shapeflag))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_shapeflag))
lakeinformation$lake_lat_decdeg <- ifelse((trimws(as.character(lakeinformation$lake_lat_decdeg))==trimws("NA")),NA,lakeinformation$lake_lat_decdeg)               
suppressWarnings(lakeinformation$lake_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_lat_decdeg))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_lat_decdeg))
lakeinformation$lake_lon_decdeg <- ifelse((trimws(as.character(lakeinformation$lake_lon_decdeg))==trimws("NA")),NA,lakeinformation$lake_lon_decdeg)               
suppressWarnings(lakeinformation$lake_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_lon_decdeg))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_lon_decdeg))
lakeinformation$lake_elevation_m <- ifelse((trimws(as.character(lakeinformation$lake_elevation_m))==trimws("NA")),NA,lakeinformation$lake_elevation_m)               
suppressWarnings(lakeinformation$lake_elevation_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_elevation_m))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_elevation_m))
lakeinformation$lake_centroidstate <- ifelse((trimws(as.character(lakeinformation$lake_centroidstate))==trimws("NA")),NA,lakeinformation$lake_centroidstate)               
suppressWarnings(lakeinformation$lake_centroidstate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_centroidstate))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_centroidstate))
lakeinformation$lake_states <- ifelse((trimws(as.character(lakeinformation$lake_states))==trimws("NA")),NA,lakeinformation$lake_states)               
suppressWarnings(lakeinformation$lake_states <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_states))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_states))
lakeinformation$lake_county <- ifelse((trimws(as.character(lakeinformation$lake_county))==trimws("NA")),NA,lakeinformation$lake_county)               
suppressWarnings(lakeinformation$lake_county <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_county))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_county))
lakeinformation$lake_countyfips <- ifelse((trimws(as.character(lakeinformation$lake_countyfips))==trimws("NA")),NA,lakeinformation$lake_countyfips)               
suppressWarnings(lakeinformation$lake_countyfips <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_countyfips))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_countyfips))
lakeinformation$lake_huc12 <- ifelse((trimws(as.character(lakeinformation$lake_huc12))==trimws("NA")),NA,lakeinformation$lake_huc12)               
suppressWarnings(lakeinformation$lake_huc12 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$lake_huc12))==as.character(as.numeric("NA"))),NA,lakeinformation$lake_huc12))
lakeinformation$buff100_zoneid <- ifelse((trimws(as.character(lakeinformation$buff100_zoneid))==trimws("NA")),NA,lakeinformation$buff100_zoneid)               
suppressWarnings(lakeinformation$buff100_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$buff100_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$buff100_zoneid))
lakeinformation$buff500_zoneid <- ifelse((trimws(as.character(lakeinformation$buff500_zoneid))==trimws("NA")),NA,lakeinformation$buff500_zoneid)               
suppressWarnings(lakeinformation$buff500_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$buff500_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$buff500_zoneid))
lakeinformation$ws_zoneid <- ifelse((trimws(as.character(lakeinformation$ws_zoneid))==trimws("NA")),NA,lakeinformation$ws_zoneid)               
suppressWarnings(lakeinformation$ws_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$ws_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$ws_zoneid))
lakeinformation$nws_zoneid <- ifelse((trimws(as.character(lakeinformation$nws_zoneid))==trimws("NA")),NA,lakeinformation$nws_zoneid)               
suppressWarnings(lakeinformation$nws_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$nws_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$nws_zoneid))
lakeinformation$hu12_zoneid <- ifelse((trimws(as.character(lakeinformation$hu12_zoneid))==trimws("NA")),NA,lakeinformation$hu12_zoneid)               
suppressWarnings(lakeinformation$hu12_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$hu12_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$hu12_zoneid))
lakeinformation$hu8_zoneid <- ifelse((trimws(as.character(lakeinformation$hu8_zoneid))==trimws("NA")),NA,lakeinformation$hu8_zoneid)               
suppressWarnings(lakeinformation$hu8_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$hu8_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$hu8_zoneid))
lakeinformation$hu4_zoneid <- ifelse((trimws(as.character(lakeinformation$hu4_zoneid))==trimws("NA")),NA,lakeinformation$hu4_zoneid)               
suppressWarnings(lakeinformation$hu4_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$hu4_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$hu4_zoneid))
lakeinformation$county_zoneid <- ifelse((trimws(as.character(lakeinformation$county_zoneid))==trimws("NA")),NA,lakeinformation$county_zoneid)               
suppressWarnings(lakeinformation$county_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$county_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$county_zoneid))
lakeinformation$state_zoneid <- ifelse((trimws(as.character(lakeinformation$state_zoneid))==trimws("NA")),NA,lakeinformation$state_zoneid)               
suppressWarnings(lakeinformation$state_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$state_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$state_zoneid))
lakeinformation$epanutr_zoneid <- ifelse((trimws(as.character(lakeinformation$epanutr_zoneid))==trimws("NA")),NA,lakeinformation$epanutr_zoneid)               
suppressWarnings(lakeinformation$epanutr_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$epanutr_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$epanutr_zoneid))
lakeinformation$omernik3_zoneid <- ifelse((trimws(as.character(lakeinformation$omernik3_zoneid))==trimws("NA")),NA,lakeinformation$omernik3_zoneid)               
suppressWarnings(lakeinformation$omernik3_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$omernik3_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$omernik3_zoneid))
lakeinformation$wwf_zoneid <- ifelse((trimws(as.character(lakeinformation$wwf_zoneid))==trimws("NA")),NA,lakeinformation$wwf_zoneid)               
suppressWarnings(lakeinformation$wwf_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$wwf_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$wwf_zoneid))
lakeinformation$mlra_zoneid <- ifelse((trimws(as.character(lakeinformation$mlra_zoneid))==trimws("NA")),NA,lakeinformation$mlra_zoneid)               
suppressWarnings(lakeinformation$mlra_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$mlra_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$mlra_zoneid))
lakeinformation$bailey_zoneid <- ifelse((trimws(as.character(lakeinformation$bailey_zoneid))==trimws("NA")),NA,lakeinformation$bailey_zoneid)               
suppressWarnings(lakeinformation$bailey_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$bailey_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$bailey_zoneid))
lakeinformation$neon_zoneid <- ifelse((trimws(as.character(lakeinformation$neon_zoneid))==trimws("NA")),NA,lakeinformation$neon_zoneid)               
suppressWarnings(lakeinformation$neon_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakeinformation$neon_zoneid))==as.character(as.numeric("NA"))),NA,lakeinformation$neon_zoneid))


# Observed issues when reading the data. An empty list is good!
problems(lakeinformation) 
# Here is the structure of the input data tibble: 
glimpse(lakeinformation) 
# And some statistical summaries of the data 
summary(lakeinformation) 
# Get more details on character variables

summary(as.factor(lakeinformation$lake_nhdid)) 
summary(as.factor(lakeinformation$lake_nhdfcode)) 
summary(as.factor(lakeinformation$lake_nhdftype)) 
summary(as.factor(lakeinformation$lake_reachcode)) 
summary(as.factor(lakeinformation$lake_namegnis)) 
summary(as.factor(lakeinformation$lake_namelagos)) 
summary(as.factor(lakeinformation$lake_onlandborder)) 
summary(as.factor(lakeinformation$lake_ismultipart)) 
summary(as.factor(lakeinformation$lake_missingws)) 
summary(as.factor(lakeinformation$lake_shapeflag)) 
summary(as.factor(lakeinformation$lake_centroidstate)) 
summary(as.factor(lakeinformation$lake_states)) 
summary(as.factor(lakeinformation$lake_county)) 
summary(as.factor(lakeinformation$lake_countyfips)) 
summary(as.factor(lakeinformation$lake_huc12)) 
summary(as.factor(lakeinformation$buff100_zoneid)) 
summary(as.factor(lakeinformation$buff500_zoneid)) 
summary(as.factor(lakeinformation$ws_zoneid)) 
summary(as.factor(lakeinformation$nws_zoneid)) 
summary(as.factor(lakeinformation$hu12_zoneid)) 
summary(as.factor(lakeinformation$hu8_zoneid)) 
summary(as.factor(lakeinformation$hu4_zoneid)) 
summary(as.factor(lakeinformation$county_zoneid)) 
summary(as.factor(lakeinformation$state_zoneid)) 
summary(as.factor(lakeinformation$epanutr_zoneid)) 
summary(as.factor(lakeinformation$omernik3_zoneid)) 
summary(as.factor(lakeinformation$wwf_zoneid)) 
summary(as.factor(lakeinformation$mlra_zoneid)) 
summary(as.factor(lakeinformation$bailey_zoneid)) 
summary(as.factor(lakeinformation$neon_zoneid)) 


infile2 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/854/1/fd7fe936d290a12bc6dbf5c41047849e") 
infile2 <-sub("^https","http",infile2)
# This creates a tibble named: lakecharacteristics 
lakecharacteristics <-read_delim(infile2  
                                 ,delim=","   
                                 ,skip=1 
                                 ,quote='"'  
                                 , col_names=c( 
                                   "lagoslakeid",   
                                   "lake_waterarea_ha",   
                                   "lake_totalarea_ha",   
                                   "lake_islandarea_ha",   
                                   "lake_perimeter_m",   
                                   "lake_islandperimeter_m",   
                                   "lake_shorelinedevfactor",   
                                   "lake_mbgconhull_length_m",   
                                   "lake_mbgconhull_width_m",   
                                   "lake_mbgconhull_orientation_deg",   
                                   "lake_mbgrect_length_m",   
                                   "lake_mbgrect_width_m",   
                                   "lake_mbgrect_arearatio",   
                                   "lake_meanwidth_m",   
                                   "lake_connectivity_class",   
                                   "lake_connectivity_fluctuates",   
                                   "lake_connectivity_permanent",   
                                   "lake_lakes4ha_upstream_ha",   
                                   "lake_lakes4ha_upstream_n",   
                                   "lake_lakes1ha_upstream_ha",   
                                   "lake_lakes1ha_upstream_n",   
                                   "lake_lakes10ha_upstream_n",   
                                   "lake_lakes10ha_upstream_ha",   
                                   "lake_glaciatedlatewisc"   ), 
                                 col_types=list(
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() ,  
                                   col_character(),  
                                   col_character(),  
                                   col_character(), 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() , 
                                   col_number() ,  
                                   col_character()), 
                                 na=c(" ",".","NA")  )


# Convert Missing Values to NA for individual vectors 
lakecharacteristics$lagoslakeid <- ifelse((trimws(as.character(lakecharacteristics$lagoslakeid))==trimws("NA")),NA,lakecharacteristics$lagoslakeid)               
suppressWarnings(lakecharacteristics$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lagoslakeid))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lagoslakeid))
lakecharacteristics$lake_waterarea_ha <- ifelse((trimws(as.character(lakecharacteristics$lake_waterarea_ha))==trimws("NA")),NA,lakecharacteristics$lake_waterarea_ha)               
suppressWarnings(lakecharacteristics$lake_waterarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_waterarea_ha))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_waterarea_ha))
lakecharacteristics$lake_totalarea_ha <- ifelse((trimws(as.character(lakecharacteristics$lake_totalarea_ha))==trimws("NA")),NA,lakecharacteristics$lake_totalarea_ha)               
suppressWarnings(lakecharacteristics$lake_totalarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_totalarea_ha))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_totalarea_ha))
lakecharacteristics$lake_islandarea_ha <- ifelse((trimws(as.character(lakecharacteristics$lake_islandarea_ha))==trimws("NA")),NA,lakecharacteristics$lake_islandarea_ha)               
suppressWarnings(lakecharacteristics$lake_islandarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_islandarea_ha))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_islandarea_ha))
lakecharacteristics$lake_perimeter_m <- ifelse((trimws(as.character(lakecharacteristics$lake_perimeter_m))==trimws("NA")),NA,lakecharacteristics$lake_perimeter_m)               
suppressWarnings(lakecharacteristics$lake_perimeter_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_perimeter_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_perimeter_m))
lakecharacteristics$lake_islandperimeter_m <- ifelse((trimws(as.character(lakecharacteristics$lake_islandperimeter_m))==trimws("NA")),NA,lakecharacteristics$lake_islandperimeter_m)               
suppressWarnings(lakecharacteristics$lake_islandperimeter_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_islandperimeter_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_islandperimeter_m))
lakecharacteristics$lake_shorelinedevfactor <- ifelse((trimws(as.character(lakecharacteristics$lake_shorelinedevfactor))==trimws("NA")),NA,lakecharacteristics$lake_shorelinedevfactor)               
suppressWarnings(lakecharacteristics$lake_shorelinedevfactor <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_shorelinedevfactor))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_shorelinedevfactor))
lakecharacteristics$lake_mbgconhull_length_m <- ifelse((trimws(as.character(lakecharacteristics$lake_mbgconhull_length_m))==trimws("NA")),NA,lakecharacteristics$lake_mbgconhull_length_m)               
suppressWarnings(lakecharacteristics$lake_mbgconhull_length_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_mbgconhull_length_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_mbgconhull_length_m))
lakecharacteristics$lake_mbgconhull_width_m <- ifelse((trimws(as.character(lakecharacteristics$lake_mbgconhull_width_m))==trimws("NA")),NA,lakecharacteristics$lake_mbgconhull_width_m)               
suppressWarnings(lakecharacteristics$lake_mbgconhull_width_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_mbgconhull_width_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_mbgconhull_width_m))
lakecharacteristics$lake_mbgconhull_orientation_deg <- ifelse((trimws(as.character(lakecharacteristics$lake_mbgconhull_orientation_deg))==trimws("NA")),NA,lakecharacteristics$lake_mbgconhull_orientation_deg)               
suppressWarnings(lakecharacteristics$lake_mbgconhull_orientation_deg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_mbgconhull_orientation_deg))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_mbgconhull_orientation_deg))
lakecharacteristics$lake_mbgrect_length_m <- ifelse((trimws(as.character(lakecharacteristics$lake_mbgrect_length_m))==trimws("NA")),NA,lakecharacteristics$lake_mbgrect_length_m)               
suppressWarnings(lakecharacteristics$lake_mbgrect_length_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_mbgrect_length_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_mbgrect_length_m))
lakecharacteristics$lake_mbgrect_width_m <- ifelse((trimws(as.character(lakecharacteristics$lake_mbgrect_width_m))==trimws("NA")),NA,lakecharacteristics$lake_mbgrect_width_m)               
suppressWarnings(lakecharacteristics$lake_mbgrect_width_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_mbgrect_width_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_mbgrect_width_m))
lakecharacteristics$lake_mbgrect_arearatio <- ifelse((trimws(as.character(lakecharacteristics$lake_mbgrect_arearatio))==trimws("NA")),NA,lakecharacteristics$lake_mbgrect_arearatio)               
suppressWarnings(lakecharacteristics$lake_mbgrect_arearatio <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_mbgrect_arearatio))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_mbgrect_arearatio))
lakecharacteristics$lake_meanwidth_m <- ifelse((trimws(as.character(lakecharacteristics$lake_meanwidth_m))==trimws("NA")),NA,lakecharacteristics$lake_meanwidth_m)               
suppressWarnings(lakecharacteristics$lake_meanwidth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_meanwidth_m))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_meanwidth_m))
lakecharacteristics$lake_connectivity_class <- ifelse((trimws(as.character(lakecharacteristics$lake_connectivity_class))==trimws("NA")),NA,lakecharacteristics$lake_connectivity_class)               
suppressWarnings(lakecharacteristics$lake_connectivity_class <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_connectivity_class))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_connectivity_class))
lakecharacteristics$lake_connectivity_fluctuates <- ifelse((trimws(as.character(lakecharacteristics$lake_connectivity_fluctuates))==trimws("NA")),NA,lakecharacteristics$lake_connectivity_fluctuates)               
suppressWarnings(lakecharacteristics$lake_connectivity_fluctuates <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_connectivity_fluctuates))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_connectivity_fluctuates))
lakecharacteristics$lake_connectivity_permanent <- ifelse((trimws(as.character(lakecharacteristics$lake_connectivity_permanent))==trimws("NA")),NA,lakecharacteristics$lake_connectivity_permanent)               
suppressWarnings(lakecharacteristics$lake_connectivity_permanent <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_connectivity_permanent))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_connectivity_permanent))
lakecharacteristics$lake_lakes4ha_upstream_ha <- ifelse((trimws(as.character(lakecharacteristics$lake_lakes4ha_upstream_ha))==trimws("NA")),NA,lakecharacteristics$lake_lakes4ha_upstream_ha)               
suppressWarnings(lakecharacteristics$lake_lakes4ha_upstream_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_lakes4ha_upstream_ha))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_lakes4ha_upstream_ha))
lakecharacteristics$lake_lakes4ha_upstream_n <- ifelse((trimws(as.character(lakecharacteristics$lake_lakes4ha_upstream_n))==trimws("NA")),NA,lakecharacteristics$lake_lakes4ha_upstream_n)               
suppressWarnings(lakecharacteristics$lake_lakes4ha_upstream_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_lakes4ha_upstream_n))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_lakes4ha_upstream_n))
lakecharacteristics$lake_lakes1ha_upstream_ha <- ifelse((trimws(as.character(lakecharacteristics$lake_lakes1ha_upstream_ha))==trimws("NA")),NA,lakecharacteristics$lake_lakes1ha_upstream_ha)               
suppressWarnings(lakecharacteristics$lake_lakes1ha_upstream_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_lakes1ha_upstream_ha))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_lakes1ha_upstream_ha))
lakecharacteristics$lake_lakes1ha_upstream_n <- ifelse((trimws(as.character(lakecharacteristics$lake_lakes1ha_upstream_n))==trimws("NA")),NA,lakecharacteristics$lake_lakes1ha_upstream_n)               
suppressWarnings(lakecharacteristics$lake_lakes1ha_upstream_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_lakes1ha_upstream_n))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_lakes1ha_upstream_n))
lakecharacteristics$lake_lakes10ha_upstream_n <- ifelse((trimws(as.character(lakecharacteristics$lake_lakes10ha_upstream_n))==trimws("NA")),NA,lakecharacteristics$lake_lakes10ha_upstream_n)               
suppressWarnings(lakecharacteristics$lake_lakes10ha_upstream_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_lakes10ha_upstream_n))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_lakes10ha_upstream_n))
lakecharacteristics$lake_lakes10ha_upstream_ha <- ifelse((trimws(as.character(lakecharacteristics$lake_lakes10ha_upstream_ha))==trimws("NA")),NA,lakecharacteristics$lake_lakes10ha_upstream_ha)               
suppressWarnings(lakecharacteristics$lake_lakes10ha_upstream_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_lakes10ha_upstream_ha))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_lakes10ha_upstream_ha))
lakecharacteristics$lake_glaciatedlatewisc <- ifelse((trimws(as.character(lakecharacteristics$lake_glaciatedlatewisc))==trimws("NA")),NA,lakecharacteristics$lake_glaciatedlatewisc)               
suppressWarnings(lakecharacteristics$lake_glaciatedlatewisc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakecharacteristics$lake_glaciatedlatewisc))==as.character(as.numeric("NA"))),NA,lakecharacteristics$lake_glaciatedlatewisc))


# Observed issues when reading the data. An empty list is good!
problems(lakecharacteristics) 
# Here is the structure of the input data tibble: 
glimpse(lakecharacteristics) 
# And some statistical summaries of the data 
summary(lakecharacteristics) 
# Get more details on character variables

summary(as.factor(lakecharacteristics$lake_connectivity_class)) 
summary(as.factor(lakecharacteristics$lake_connectivity_fluctuates)) 
summary(as.factor(lakecharacteristics$lake_connectivity_permanent)) 
summary(as.factor(lakecharacteristics$lake_glaciatedlatewisc)) 

lakecharacteristics <- lakecharacteristics %>%
  dplyr::select(lagoslakeid, lake_perimeter_m, lake_totalarea_ha,
                lake_connectivity_class, lake_connectivity_fluctuates, lake_connectivity_permanent,
                lake_lakes4ha_upstream_ha, lake_lakes4ha_upstream_n, lake_lakes1ha_upstream_ha,
                lake_lakes1ha_upstream_n, lake_lakes10ha_upstream_n, lake_lakes10ha_upstream_ha, 
                lake_glaciatedlatewisc)



infile3 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/854/1/8bd86b94234a21a74991eca7bd9ab883") 
infile3 <-sub("^https","http",infile3)
# This creates a tibble named: lakewatersheds 
lakewatersheds <-read_delim(infile3  
                            ,delim=","   
                            ,skip=1 
                            ,quote='"'  
                            , col_names=c( 
                              "lagoslakeid",   
                              "ws_zoneid",   
                              "nws_zoneid",   
                              "ws_subtype",   
                              "ws_equalsnws",   
                              "ws_onlandborder",   
                              "ws_oncoast",   
                              "ws_inusa_pct",   
                              "ws_includeshu4inlet",   
                              "ws_ismultipart",   
                              "ws_sliverflag",   
                              "nws_onlandborder",   
                              "nws_oncoast",   
                              "nws_inusa_pct",   
                              "nws_includeshu4inlet",   
                              "nws_ismultipart",   
                              "ws_states",   
                              "ws_focallakewaterarea_ha",   
                              "ws_area_ha",   
                              "ws_perimeter_m",   
                              "ws_lake_arearatio",   
                              "ws_mbgconhull_length_m",   
                              "ws_mbgconhull_width_m",   
                              "ws_mbgconhull_orientation_deg",   
                              "ws_meanwidth_m",   
                              "ws_lat_decdeg",   
                              "ws_lon_decdeg",   
                              "nws_states",   
                              "nws_focallakewaterarea_ha",   
                              "nws_area_ha",   
                              "nws_perimeter_m",   
                              "nws_lake_arearatio",   
                              "nws_mbgconhull_length_m",   
                              "nws_mbgconhull_width_m",   
                              "nws_mbgconhull_orientation_deg",   
                              "nws_meanwidth_m",   
                              "nws_lat_decdeg",   
                              "nws_lon_decdeg"   ), 
                            col_types=list(
                              col_number() ,  
                              col_character(),  
                              col_character(),  
                              col_character(),  
                              col_character(),  
                              col_character(),  
                              col_character(), 
                              col_number() ,  
                              col_character(),  
                              col_character(),  
                              col_character(),  
                              col_character(),  
                              col_character(), 
                              col_number() ,  
                              col_character(),  
                              col_character(),  
                              col_character(), 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() ,  
                              col_character(), 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() , 
                              col_number() ), 
                            na=c(" ",".","NA")  )


# Convert Missing Values to NA for individual vectors 
lakewatersheds$lagoslakeid <- ifelse((trimws(as.character(lakewatersheds$lagoslakeid))==trimws("NA")),NA,lakewatersheds$lagoslakeid)               
suppressWarnings(lakewatersheds$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$lagoslakeid))==as.character(as.numeric("NA"))),NA,lakewatersheds$lagoslakeid))
lakewatersheds$ws_zoneid <- ifelse((trimws(as.character(lakewatersheds$ws_zoneid))==trimws("NA")),NA,lakewatersheds$ws_zoneid)               
suppressWarnings(lakewatersheds$ws_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_zoneid))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_zoneid))
lakewatersheds$nws_zoneid <- ifelse((trimws(as.character(lakewatersheds$nws_zoneid))==trimws("NA")),NA,lakewatersheds$nws_zoneid)               
suppressWarnings(lakewatersheds$nws_zoneid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_zoneid))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_zoneid))
lakewatersheds$ws_subtype <- ifelse((trimws(as.character(lakewatersheds$ws_subtype))==trimws("NA")),NA,lakewatersheds$ws_subtype)               
suppressWarnings(lakewatersheds$ws_subtype <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_subtype))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_subtype))
lakewatersheds$ws_equalsnws <- ifelse((trimws(as.character(lakewatersheds$ws_equalsnws))==trimws("NA")),NA,lakewatersheds$ws_equalsnws)               
suppressWarnings(lakewatersheds$ws_equalsnws <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_equalsnws))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_equalsnws))
lakewatersheds$ws_onlandborder <- ifelse((trimws(as.character(lakewatersheds$ws_onlandborder))==trimws("NA")),NA,lakewatersheds$ws_onlandborder)               
suppressWarnings(lakewatersheds$ws_onlandborder <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_onlandborder))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_onlandborder))
lakewatersheds$ws_oncoast <- ifelse((trimws(as.character(lakewatersheds$ws_oncoast))==trimws("NA")),NA,lakewatersheds$ws_oncoast)               
suppressWarnings(lakewatersheds$ws_oncoast <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_oncoast))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_oncoast))
lakewatersheds$ws_inusa_pct <- ifelse((trimws(as.character(lakewatersheds$ws_inusa_pct))==trimws("NA")),NA,lakewatersheds$ws_inusa_pct)               
suppressWarnings(lakewatersheds$ws_inusa_pct <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_inusa_pct))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_inusa_pct))
lakewatersheds$ws_includeshu4inlet <- ifelse((trimws(as.character(lakewatersheds$ws_includeshu4inlet))==trimws("NA")),NA,lakewatersheds$ws_includeshu4inlet)               
suppressWarnings(lakewatersheds$ws_includeshu4inlet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_includeshu4inlet))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_includeshu4inlet))
lakewatersheds$ws_ismultipart <- ifelse((trimws(as.character(lakewatersheds$ws_ismultipart))==trimws("NA")),NA,lakewatersheds$ws_ismultipart)               
suppressWarnings(lakewatersheds$ws_ismultipart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_ismultipart))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_ismultipart))
lakewatersheds$ws_sliverflag <- ifelse((trimws(as.character(lakewatersheds$ws_sliverflag))==trimws("NA")),NA,lakewatersheds$ws_sliverflag)               
suppressWarnings(lakewatersheds$ws_sliverflag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_sliverflag))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_sliverflag))
lakewatersheds$nws_onlandborder <- ifelse((trimws(as.character(lakewatersheds$nws_onlandborder))==trimws("NA")),NA,lakewatersheds$nws_onlandborder)               
suppressWarnings(lakewatersheds$nws_onlandborder <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_onlandborder))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_onlandborder))
lakewatersheds$nws_oncoast <- ifelse((trimws(as.character(lakewatersheds$nws_oncoast))==trimws("NA")),NA,lakewatersheds$nws_oncoast)               
suppressWarnings(lakewatersheds$nws_oncoast <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_oncoast))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_oncoast))
lakewatersheds$nws_inusa_pct <- ifelse((trimws(as.character(lakewatersheds$nws_inusa_pct))==trimws("NA")),NA,lakewatersheds$nws_inusa_pct)               
suppressWarnings(lakewatersheds$nws_inusa_pct <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_inusa_pct))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_inusa_pct))
lakewatersheds$nws_includeshu4inlet <- ifelse((trimws(as.character(lakewatersheds$nws_includeshu4inlet))==trimws("NA")),NA,lakewatersheds$nws_includeshu4inlet)               
suppressWarnings(lakewatersheds$nws_includeshu4inlet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_includeshu4inlet))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_includeshu4inlet))
lakewatersheds$nws_ismultipart <- ifelse((trimws(as.character(lakewatersheds$nws_ismultipart))==trimws("NA")),NA,lakewatersheds$nws_ismultipart)               
suppressWarnings(lakewatersheds$nws_ismultipart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_ismultipart))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_ismultipart))
lakewatersheds$ws_states <- ifelse((trimws(as.character(lakewatersheds$ws_states))==trimws("NA")),NA,lakewatersheds$ws_states)               
suppressWarnings(lakewatersheds$ws_states <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_states))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_states))
lakewatersheds$ws_focallakewaterarea_ha <- ifelse((trimws(as.character(lakewatersheds$ws_focallakewaterarea_ha))==trimws("NA")),NA,lakewatersheds$ws_focallakewaterarea_ha)               
suppressWarnings(lakewatersheds$ws_focallakewaterarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_focallakewaterarea_ha))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_focallakewaterarea_ha))
lakewatersheds$ws_area_ha <- ifelse((trimws(as.character(lakewatersheds$ws_area_ha))==trimws("NA")),NA,lakewatersheds$ws_area_ha)               
suppressWarnings(lakewatersheds$ws_area_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_area_ha))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_area_ha))
lakewatersheds$ws_perimeter_m <- ifelse((trimws(as.character(lakewatersheds$ws_perimeter_m))==trimws("NA")),NA,lakewatersheds$ws_perimeter_m)               
suppressWarnings(lakewatersheds$ws_perimeter_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_perimeter_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_perimeter_m))
lakewatersheds$ws_lake_arearatio <- ifelse((trimws(as.character(lakewatersheds$ws_lake_arearatio))==trimws("NA")),NA,lakewatersheds$ws_lake_arearatio)               
suppressWarnings(lakewatersheds$ws_lake_arearatio <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_lake_arearatio))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_lake_arearatio))
lakewatersheds$ws_mbgconhull_length_m <- ifelse((trimws(as.character(lakewatersheds$ws_mbgconhull_length_m))==trimws("NA")),NA,lakewatersheds$ws_mbgconhull_length_m)               
suppressWarnings(lakewatersheds$ws_mbgconhull_length_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_mbgconhull_length_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_mbgconhull_length_m))
lakewatersheds$ws_mbgconhull_width_m <- ifelse((trimws(as.character(lakewatersheds$ws_mbgconhull_width_m))==trimws("NA")),NA,lakewatersheds$ws_mbgconhull_width_m)               
suppressWarnings(lakewatersheds$ws_mbgconhull_width_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_mbgconhull_width_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_mbgconhull_width_m))
lakewatersheds$ws_mbgconhull_orientation_deg <- ifelse((trimws(as.character(lakewatersheds$ws_mbgconhull_orientation_deg))==trimws("NA")),NA,lakewatersheds$ws_mbgconhull_orientation_deg)               
suppressWarnings(lakewatersheds$ws_mbgconhull_orientation_deg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_mbgconhull_orientation_deg))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_mbgconhull_orientation_deg))
lakewatersheds$ws_meanwidth_m <- ifelse((trimws(as.character(lakewatersheds$ws_meanwidth_m))==trimws("NA")),NA,lakewatersheds$ws_meanwidth_m)               
suppressWarnings(lakewatersheds$ws_meanwidth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_meanwidth_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_meanwidth_m))
lakewatersheds$ws_lat_decdeg <- ifelse((trimws(as.character(lakewatersheds$ws_lat_decdeg))==trimws("NA")),NA,lakewatersheds$ws_lat_decdeg)               
suppressWarnings(lakewatersheds$ws_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_lat_decdeg))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_lat_decdeg))
lakewatersheds$ws_lon_decdeg <- ifelse((trimws(as.character(lakewatersheds$ws_lon_decdeg))==trimws("NA")),NA,lakewatersheds$ws_lon_decdeg)               
suppressWarnings(lakewatersheds$ws_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$ws_lon_decdeg))==as.character(as.numeric("NA"))),NA,lakewatersheds$ws_lon_decdeg))
lakewatersheds$nws_states <- ifelse((trimws(as.character(lakewatersheds$nws_states))==trimws("NA")),NA,lakewatersheds$nws_states)               
suppressWarnings(lakewatersheds$nws_states <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_states))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_states))
lakewatersheds$nws_focallakewaterarea_ha <- ifelse((trimws(as.character(lakewatersheds$nws_focallakewaterarea_ha))==trimws("NA")),NA,lakewatersheds$nws_focallakewaterarea_ha)               
suppressWarnings(lakewatersheds$nws_focallakewaterarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_focallakewaterarea_ha))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_focallakewaterarea_ha))
lakewatersheds$nws_area_ha <- ifelse((trimws(as.character(lakewatersheds$nws_area_ha))==trimws("NA")),NA,lakewatersheds$nws_area_ha)               
suppressWarnings(lakewatersheds$nws_area_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_area_ha))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_area_ha))
lakewatersheds$nws_perimeter_m <- ifelse((trimws(as.character(lakewatersheds$nws_perimeter_m))==trimws("NA")),NA,lakewatersheds$nws_perimeter_m)               
suppressWarnings(lakewatersheds$nws_perimeter_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_perimeter_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_perimeter_m))
lakewatersheds$nws_lake_arearatio <- ifelse((trimws(as.character(lakewatersheds$nws_lake_arearatio))==trimws("NA")),NA,lakewatersheds$nws_lake_arearatio)               
suppressWarnings(lakewatersheds$nws_lake_arearatio <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_lake_arearatio))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_lake_arearatio))
lakewatersheds$nws_mbgconhull_length_m <- ifelse((trimws(as.character(lakewatersheds$nws_mbgconhull_length_m))==trimws("NA")),NA,lakewatersheds$nws_mbgconhull_length_m)               
suppressWarnings(lakewatersheds$nws_mbgconhull_length_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_mbgconhull_length_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_mbgconhull_length_m))
lakewatersheds$nws_mbgconhull_width_m <- ifelse((trimws(as.character(lakewatersheds$nws_mbgconhull_width_m))==trimws("NA")),NA,lakewatersheds$nws_mbgconhull_width_m)               
suppressWarnings(lakewatersheds$nws_mbgconhull_width_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_mbgconhull_width_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_mbgconhull_width_m))
lakewatersheds$nws_mbgconhull_orientation_deg <- ifelse((trimws(as.character(lakewatersheds$nws_mbgconhull_orientation_deg))==trimws("NA")),NA,lakewatersheds$nws_mbgconhull_orientation_deg)               
suppressWarnings(lakewatersheds$nws_mbgconhull_orientation_deg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_mbgconhull_orientation_deg))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_mbgconhull_orientation_deg))
lakewatersheds$nws_meanwidth_m <- ifelse((trimws(as.character(lakewatersheds$nws_meanwidth_m))==trimws("NA")),NA,lakewatersheds$nws_meanwidth_m)               
suppressWarnings(lakewatersheds$nws_meanwidth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_meanwidth_m))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_meanwidth_m))
lakewatersheds$nws_lat_decdeg <- ifelse((trimws(as.character(lakewatersheds$nws_lat_decdeg))==trimws("NA")),NA,lakewatersheds$nws_lat_decdeg)               
suppressWarnings(lakewatersheds$nws_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_lat_decdeg))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_lat_decdeg))
lakewatersheds$nws_lon_decdeg <- ifelse((trimws(as.character(lakewatersheds$nws_lon_decdeg))==trimws("NA")),NA,lakewatersheds$nws_lon_decdeg)               
suppressWarnings(lakewatersheds$nws_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lakewatersheds$nws_lon_decdeg))==as.character(as.numeric("NA"))),NA,lakewatersheds$nws_lon_decdeg))


# Observed issues when reading the data. An empty list is good!
problems(lakewatersheds) 
# Here is the structure of the input data tibble: 
glimpse(lakewatersheds) 
# And some statistical summaries of the data 
summary(lakewatersheds) 
# Get more details on character variables

summary(as.factor(lakewatersheds$ws_zoneid)) 
summary(as.factor(lakewatersheds$nws_zoneid)) 
summary(as.factor(lakewatersheds$ws_subtype)) 
summary(as.factor(lakewatersheds$ws_equalsnws)) 
summary(as.factor(lakewatersheds$ws_onlandborder)) 
summary(as.factor(lakewatersheds$ws_oncoast)) 
summary(as.factor(lakewatersheds$ws_includeshu4inlet)) 
summary(as.factor(lakewatersheds$ws_ismultipart)) 
summary(as.factor(lakewatersheds$ws_sliverflag)) 
summary(as.factor(lakewatersheds$nws_onlandborder)) 
summary(as.factor(lakewatersheds$nws_oncoast)) 
summary(as.factor(lakewatersheds$nws_includeshu4inlet)) 
summary(as.factor(lakewatersheds$nws_ismultipart)) 
summary(as.factor(lakewatersheds$ws_states)) 
summary(as.factor(lakewatersheds$nws_states)) 
# 
infile4 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/854/1/5488e333ce818597fa3dbfc9b4e0c131")
infile4 <-sub("^https","http",infile4)
# This creates a tibble named: dt4
dt4 <-read_delim(infile4
                 ,delim=","
                 ,skip=1
                 ,quote='"'
                 , col_names=c(
                   "lagoslakeid",
                   "lake_nhdid",
                   "lake_reachcode",
                   "lake_namegnis",
                   "lake_namelagos",
                   "lake_county",
                   "lake_countyfips",
                   "lake_lat_decdeg",
                   "lake_lon_decdeg",
                   "lake_centroidstate",
                   "nhdhr_area_sqkm",
                   "nhdhr_fdate",
                   "nhdhr_gnisid",
                   "lagosus_legacysiteid",
                   "lagosus_legacysitelabel",
                   "lagosus_legacyprogram",
                   "wqp_monitoringlocationidentifier",
                   "wqp_monitoringlocationname",
                   "wqp_providername",
                   "nhdplusv2_comid",
                   "nhdplusv2_reachcode",
                   "nhdplusv2_area_sqkm",
                   "lagosne_lagoslakeid",
                   "lagosne_legacysiteid",
                   "nla2007_siteid",
                   "nla2012_siteid",
                   "nhdplusv2_lakes_n",
                   "lagosne_lakes_n",
                   "wqp_sites_n",
                   "lagosus_legacyids_n"   ),
                 col_types=list(
                   col_number() ,
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_number() ,
                   col_number() ,
                   col_character(),
                   col_number() ,
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_character(),
                   col_number() ,
                   col_number() ,
                   col_character(),
                   col_character(),
                   col_character(),
                   col_number() ,
                   col_number() ,
                   col_number() ,
                   col_number() ),
                 na=c(" ",".","NA")  )


# Convert Missing Values to NA for individual vectors
dt4$lagoslakeid <- ifelse((trimws(as.character(dt4$lagoslakeid))==trimws("NA")),NA,dt4$lagoslakeid)
suppressWarnings(dt4$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagoslakeid))==as.character(as.numeric("NA"))),NA,dt4$lagoslakeid))
dt4$lake_nhdid <- ifelse((trimws(as.character(dt4$lake_nhdid))==trimws("NA")),NA,dt4$lake_nhdid)
suppressWarnings(dt4$lake_nhdid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_nhdid))==as.character(as.numeric("NA"))),NA,dt4$lake_nhdid))
dt4$lake_reachcode <- ifelse((trimws(as.character(dt4$lake_reachcode))==trimws("NA")),NA,dt4$lake_reachcode)
suppressWarnings(dt4$lake_reachcode <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_reachcode))==as.character(as.numeric("NA"))),NA,dt4$lake_reachcode))
dt4$lake_namegnis <- ifelse((trimws(as.character(dt4$lake_namegnis))==trimws("NA")),NA,dt4$lake_namegnis)
suppressWarnings(dt4$lake_namegnis <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_namegnis))==as.character(as.numeric("NA"))),NA,dt4$lake_namegnis))
dt4$lake_namelagos <- ifelse((trimws(as.character(dt4$lake_namelagos))==trimws("NA")),NA,dt4$lake_namelagos)
suppressWarnings(dt4$lake_namelagos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_namelagos))==as.character(as.numeric("NA"))),NA,dt4$lake_namelagos))
dt4$lake_county <- ifelse((trimws(as.character(dt4$lake_county))==trimws("NA")),NA,dt4$lake_county)
suppressWarnings(dt4$lake_county <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_county))==as.character(as.numeric("NA"))),NA,dt4$lake_county))
dt4$lake_countyfips <- ifelse((trimws(as.character(dt4$lake_countyfips))==trimws("NA")),NA,dt4$lake_countyfips)
suppressWarnings(dt4$lake_countyfips <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_countyfips))==as.character(as.numeric("NA"))),NA,dt4$lake_countyfips))
dt4$lake_lat_decdeg <- ifelse((trimws(as.character(dt4$lake_lat_decdeg))==trimws("NA")),NA,dt4$lake_lat_decdeg)
suppressWarnings(dt4$lake_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_lat_decdeg))==as.character(as.numeric("NA"))),NA,dt4$lake_lat_decdeg))
dt4$lake_lon_decdeg <- ifelse((trimws(as.character(dt4$lake_lon_decdeg))==trimws("NA")),NA,dt4$lake_lon_decdeg)
suppressWarnings(dt4$lake_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_lon_decdeg))==as.character(as.numeric("NA"))),NA,dt4$lake_lon_decdeg))
dt4$lake_centroidstate <- ifelse((trimws(as.character(dt4$lake_centroidstate))==trimws("NA")),NA,dt4$lake_centroidstate)
suppressWarnings(dt4$lake_centroidstate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_centroidstate))==as.character(as.numeric("NA"))),NA,dt4$lake_centroidstate))
dt4$nhdhr_area_sqkm <- ifelse((trimws(as.character(dt4$nhdhr_area_sqkm))==trimws("NA")),NA,dt4$nhdhr_area_sqkm)
suppressWarnings(dt4$nhdhr_area_sqkm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdhr_area_sqkm))==as.character(as.numeric("NA"))),NA,dt4$nhdhr_area_sqkm))
dt4$nhdhr_fdate <- ifelse((trimws(as.character(dt4$nhdhr_fdate))==trimws("NA")),NA,dt4$nhdhr_fdate)
suppressWarnings(dt4$nhdhr_fdate <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdhr_fdate))==as.character(as.numeric("NA"))),NA,dt4$nhdhr_fdate))
dt4$nhdhr_gnisid <- ifelse((trimws(as.character(dt4$nhdhr_gnisid))==trimws("NA")),NA,dt4$nhdhr_gnisid)
suppressWarnings(dt4$nhdhr_gnisid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdhr_gnisid))==as.character(as.numeric("NA"))),NA,dt4$nhdhr_gnisid))
dt4$lagosus_legacysiteid <- ifelse((trimws(as.character(dt4$lagosus_legacysiteid))==trimws("NA")),NA,dt4$lagosus_legacysiteid)
suppressWarnings(dt4$lagosus_legacysiteid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosus_legacysiteid))==as.character(as.numeric("NA"))),NA,dt4$lagosus_legacysiteid))
dt4$lagosus_legacysitelabel <- ifelse((trimws(as.character(dt4$lagosus_legacysitelabel))==trimws("NA")),NA,dt4$lagosus_legacysitelabel)
suppressWarnings(dt4$lagosus_legacysitelabel <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosus_legacysitelabel))==as.character(as.numeric("NA"))),NA,dt4$lagosus_legacysitelabel))
dt4$lagosus_legacyprogram <- ifelse((trimws(as.character(dt4$lagosus_legacyprogram))==trimws("NA")),NA,dt4$lagosus_legacyprogram)
suppressWarnings(dt4$lagosus_legacyprogram <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosus_legacyprogram))==as.character(as.numeric("NA"))),NA,dt4$lagosus_legacyprogram))
dt4$wqp_monitoringlocationidentifier <- ifelse((trimws(as.character(dt4$wqp_monitoringlocationidentifier))==trimws("NA")),NA,dt4$wqp_monitoringlocationidentifier)
suppressWarnings(dt4$wqp_monitoringlocationidentifier <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$wqp_monitoringlocationidentifier))==as.character(as.numeric("NA"))),NA,dt4$wqp_monitoringlocationidentifier))
dt4$wqp_monitoringlocationname <- ifelse((trimws(as.character(dt4$wqp_monitoringlocationname))==trimws("NA")),NA,dt4$wqp_monitoringlocationname)
suppressWarnings(dt4$wqp_monitoringlocationname <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$wqp_monitoringlocationname))==as.character(as.numeric("NA"))),NA,dt4$wqp_monitoringlocationname))
dt4$wqp_providername <- ifelse((trimws(as.character(dt4$wqp_providername))==trimws("NA")),NA,dt4$wqp_providername)
suppressWarnings(dt4$wqp_providername <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$wqp_providername))==as.character(as.numeric("NA"))),NA,dt4$wqp_providername))
dt4$nhdplusv2_comid <- ifelse((trimws(as.character(dt4$nhdplusv2_comid))==trimws("NA")),NA,dt4$nhdplusv2_comid)
suppressWarnings(dt4$nhdplusv2_comid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdplusv2_comid))==as.character(as.numeric("NA"))),NA,dt4$nhdplusv2_comid))
dt4$nhdplusv2_reachcode <- ifelse((trimws(as.character(dt4$nhdplusv2_reachcode))==trimws("NA")),NA,dt4$nhdplusv2_reachcode)
suppressWarnings(dt4$nhdplusv2_reachcode <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdplusv2_reachcode))==as.character(as.numeric("NA"))),NA,dt4$nhdplusv2_reachcode))
dt4$nhdplusv2_area_sqkm <- ifelse((trimws(as.character(dt4$nhdplusv2_area_sqkm))==trimws("NA")),NA,dt4$nhdplusv2_area_sqkm)
suppressWarnings(dt4$nhdplusv2_area_sqkm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdplusv2_area_sqkm))==as.character(as.numeric("NA"))),NA,dt4$nhdplusv2_area_sqkm))
dt4$lagosne_lagoslakeid <- ifelse((trimws(as.character(dt4$lagosne_lagoslakeid))==trimws("NA")),NA,dt4$lagosne_lagoslakeid)
suppressWarnings(dt4$lagosne_lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosne_lagoslakeid))==as.character(as.numeric("NA"))),NA,dt4$lagosne_lagoslakeid))
dt4$lagosne_legacysiteid <- ifelse((trimws(as.character(dt4$lagosne_legacysiteid))==trimws("NA")),NA,dt4$lagosne_legacysiteid)
suppressWarnings(dt4$lagosne_legacysiteid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosne_legacysiteid))==as.character(as.numeric("NA"))),NA,dt4$lagosne_legacysiteid))
dt4$nla2007_siteid <- ifelse((trimws(as.character(dt4$nla2007_siteid))==trimws("NA")),NA,dt4$nla2007_siteid)
suppressWarnings(dt4$nla2007_siteid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nla2007_siteid))==as.character(as.numeric("NA"))),NA,dt4$nla2007_siteid))
dt4$nla2012_siteid <- ifelse((trimws(as.character(dt4$nla2012_siteid))==trimws("NA")),NA,dt4$nla2012_siteid)
suppressWarnings(dt4$nla2012_siteid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nla2012_siteid))==as.character(as.numeric("NA"))),NA,dt4$nla2012_siteid))
dt4$nhdplusv2_lakes_n <- ifelse((trimws(as.character(dt4$nhdplusv2_lakes_n))==trimws("NA")),NA,dt4$nhdplusv2_lakes_n)
suppressWarnings(dt4$nhdplusv2_lakes_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdplusv2_lakes_n))==as.character(as.numeric("NA"))),NA,dt4$nhdplusv2_lakes_n))
dt4$lagosne_lakes_n <- ifelse((trimws(as.character(dt4$lagosne_lakes_n))==trimws("NA")),NA,dt4$lagosne_lakes_n)
suppressWarnings(dt4$lagosne_lakes_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosne_lakes_n))==as.character(as.numeric("NA"))),NA,dt4$lagosne_lakes_n))
dt4$wqp_sites_n <- ifelse((trimws(as.character(dt4$wqp_sites_n))==trimws("NA")),NA,dt4$wqp_sites_n)
suppressWarnings(dt4$wqp_sites_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$wqp_sites_n))==as.character(as.numeric("NA"))),NA,dt4$wqp_sites_n))
dt4$lagosus_legacyids_n <- ifelse((trimws(as.character(dt4$lagosus_legacyids_n))==trimws("NA")),NA,dt4$lagosus_legacyids_n)
suppressWarnings(dt4$lagosus_legacyids_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosus_legacyids_n))==as.character(as.numeric("NA"))),NA,dt4$lagosus_legacyids_n))


# Observed issues when reading the data. An empty list is good!
problems(dt4)
# Here is the structure of the input data tibble:
glimpse(dt4)
# And some statistical summaries of the data
summary(dt4)
# Get more details on character variables

summary(as.factor(dt4$lake_nhdid))
summary(as.factor(dt4$lake_reachcode))
summary(as.factor(dt4$lake_namegnis))
summary(as.factor(dt4$lake_namelagos))
summary(as.factor(dt4$lake_county))
summary(as.factor(dt4$lake_countyfips))
summary(as.factor(dt4$lake_centroidstate))
summary(as.factor(dt4$nhdhr_fdate))
summary(as.factor(dt4$nhdhr_gnisid))
summary(as.factor(dt4$lagosus_legacysiteid))
summary(as.factor(dt4$lagosus_legacysitelabel))
summary(as.factor(dt4$lagosus_legacyprogram))
summary(as.factor(dt4$wqp_monitoringlocationidentifier))
summary(as.factor(dt4$wqp_monitoringlocationname))
summary(as.factor(dt4$wqp_providername))
summary(as.factor(dt4$nhdplusv2_comid))
summary(as.factor(dt4$nhdplusv2_reachcode))
summary(as.factor(dt4$lagosne_legacysiteid))
summary(as.factor(dt4$nla2007_siteid))
summary(as.factor(dt4$nla2012_siteid))


# in situ -----------------------------------------------------------------

chemicalphysical<-read.csv("C:/Users/cbarbosa/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US/LIMNO_v2.1/site_chemicalphysical_epi.csv") %>%
  mutate(event_date=lubridate::ymd(event_date),
         year=lubridate::year(event_date),
         year=factor(year),
         lagoslakeid=factor(lagoslakeid))

nutrientsalgae<-read.csv("C:/Users/cbarbosa/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US/LIMNO_v2.1/site_nutrientsalgae_epi.csv")%>%
  mutate(event_date=lubridate::ymd(event_date),
         year=lubridate::year(event_date),
         year=factor(year),
         lagoslakeid=factor(lagoslakeid))

depth<-read.csv("C:/Users/cbarbosa/Dropbox/CL_LAGOSUS_exports/LAGOSUS_DEPTH/DEPTH_v0/lake_depth.csv") %>%
  mutate(lagoslakeid=factor(lagoslakeid))


lakeinformation <- lakeinformation %>%
  filter(lake_centroidstate %in% c("CA", "UT", "NV",
                                   "WA", "OR", "ID",
                                   "MT", "WY", "CO",
                                   "NM", "AZ", "SD", "NE", "KS")) %>%
  mutate(lagoslakeid=factor(lagoslakeid))

#write.csv(lakeinformation, "lakes_WE_lagos.csv")

lakewatersheds <- lakewatersheds %>%
  mutate(lagoslakeid=factor(lagoslakeid))

lakecharacteristics <- lakecharacteristics %>%
  mutate(lagoslakeid=factor(lagoslakeid))


##SUMMARIZE IN SITU DATA

# names(chemicalphysical)
chemicalphysical <- chemicalphysical %>%
  group_by(lagoslakeid, year) %>%
  summarise_at(c("ca_mgl","alk_ueql","do_mgl","ph_eq",
                 "so4_mgl","temp_degc","salinity_mgl"), list(median = function(x) median(x,na.rm=T),
                                               max = function(x) max(x,na.rm=T),
                                               n=length))

# names(nutrientsalgae)
nutrientsalgae <- nutrientsalgae %>%
  group_by(lagoslakeid, year) %>%
  summarize_at(c("chla_ugl","no2no3n_ugl","nh4n_ugl",
                 "tkn_ugl","tn_ugl","ton_ugl","tp_ugl",
                 "srpp_ugl","microcystin_ugl"), list(median = function(x) median(x,na.rm=T),
                                                     max = function(x) max(x,na.rm=T),
                                                     n=length))

#Joining each data.table to lakeinformation which only has western states
chemicalphysical<-merge(lakeinformation,chemicalphysical, no.dups=TRUE, by="lagoslakeid") 
nutrientsalgae<-merge(lakeinformation,nutrientsalgae,  no.dups=TRUE, by="lagoslakeid")
colnames<-(intersect( colnames(lakeinformation),  colnames(depth))) #identify common columns between data.tables
depth<-merge(lakeinformation,depth, all.x=TRUE,  no.dups=TRUE, by=colnames)
colnames<-(intersect( colnames(lakeinformation),  colnames(lakewatersheds))) #identify common columns between data.tables
lakewatersheds<-merge(lakeinformation,lakewatersheds,  no.dups=TRUE, by=colnames)
colnames<-(intersect( colnames(lakeinformation),  colnames(lakecharacteristics))) #identify common columns between data.tables
lakecharacteristics<-merge(lakeinformation,lakecharacteristics,  no.dups=TRUE, by=colnames)


#Make one big dataframe, and join by all of the common columns ("colnames")
colnames<-(intersect( colnames(chemicalphysical),  colnames(nutrientsalgae)))
dt_limno<- merge(chemicalphysical,nutrientsalgae, all=TRUE,by=colnames) 

colnames<-(intersect( colnames(dt_limno),  colnames(depth)))
dt_limno<- merge(dt_limno,depth,by=colnames) 

colnames<-(intersect( colnames(dt_limno),  colnames(lakewatersheds)))
dt_limno<- merge(dt_limno,lakewatersheds,all=TRUE,by=colnames) 

colnames<-(intersect( colnames(dt_limno), colnames(lakecharacteristics)))
dt_limno<- merge(dt_limno,lakecharacteristics,all=TRUE,by=colnames) 

dt_limno_ids <-  length(unique(dt_limno$lagoslakeid))

# EDI connectivity --------------------------------------------------------


# Package ID: edi.879.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US NETWORKS v1.0: Data module of surface water networks characterizing connections among lakes, streams, and rivers in the conterminous U.S.
# Data set creator:  Katelyn King - Michigan State University 
# Data set creator:  Qi Wang - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Maggie Haite - Michigan State University 
# Data set creator:  Laura Danila - Michigan State University 
# Data set creator:  Pang-Ning Tan - Michigan State University 
# Data set creator:  Jiayu Zhou - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Contact:  Katelyn King -  Michigan State University  - kingka21@msu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/879/1/eb1c4a78df8140b89cbf053b0cef3976") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
dt1 <-read_delim(infile1  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "lagoslakeid",   
                   "lake_nets_upstreamlake_km",   
                   "lake_nets_downstreamlake_km",   
                   "lake_nets_bidirectionallake_km",   
                   "lake_nets_upstreamlake_n",   
                   "lake_nets_downstreamlake_n",   
                   "lake_nets_lakeorder",   
                   "lake_nets_lnn",   
                   "net_id",   
                   "net_lakes_n",   
                   "net_averagelakedistance_km",   
                   "net_averagelakearea_ha",   
                   "lake_nets_nearestdamdown_km",   
                   "lake_nets_nearestdamdown_id",   
                   "lake_nets_totaldamdown_n",   
                   "lake_nets_nearestdamup_km",   
                   "lake_nets_nearestdamup_id",   
                   "lake_nets_totaldamup_n",   
                   "lake_nets_damonlake_flag",   
                   "lake_nets_multidam_flag",   
                   "net_dams_n",   
                   "nhdplusv2_comid"   ), 
                 col_types=list(
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(), 
                   col_number() , 
                   col_number() ,  
                   col_character(), 
                   col_number() ,  
                   col_character(),  
                   col_character(), 
                   col_number() ,  
                   col_character()), 
                 na=c(" ",".","NA")  )


# Convert Missing Values to NA for individual vectors 
dt1$lagoslakeid <- ifelse((trimws(as.character(dt1$lagoslakeid))==trimws("NA")),NA,dt1$lagoslakeid)               
suppressWarnings(dt1$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lagoslakeid))==as.character(as.numeric("NA"))),NA,dt1$lagoslakeid))
dt1$lake_nets_upstreamlake_km <- ifelse((trimws(as.character(dt1$lake_nets_upstreamlake_km))==trimws("NA")),NA,dt1$lake_nets_upstreamlake_km)               
suppressWarnings(dt1$lake_nets_upstreamlake_km <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_upstreamlake_km))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_upstreamlake_km))
dt1$lake_nets_downstreamlake_km <- ifelse((trimws(as.character(dt1$lake_nets_downstreamlake_km))==trimws("NA")),NA,dt1$lake_nets_downstreamlake_km)               
suppressWarnings(dt1$lake_nets_downstreamlake_km <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_downstreamlake_km))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_downstreamlake_km))
dt1$lake_nets_bidirectionallake_km <- ifelse((trimws(as.character(dt1$lake_nets_bidirectionallake_km))==trimws("NA")),NA,dt1$lake_nets_bidirectionallake_km)               
suppressWarnings(dt1$lake_nets_bidirectionallake_km <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_bidirectionallake_km))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_bidirectionallake_km))
dt1$lake_nets_upstreamlake_n <- ifelse((trimws(as.character(dt1$lake_nets_upstreamlake_n))==trimws("NA")),NA,dt1$lake_nets_upstreamlake_n)               
suppressWarnings(dt1$lake_nets_upstreamlake_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_upstreamlake_n))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_upstreamlake_n))
dt1$lake_nets_downstreamlake_n <- ifelse((trimws(as.character(dt1$lake_nets_downstreamlake_n))==trimws("NA")),NA,dt1$lake_nets_downstreamlake_n)               
suppressWarnings(dt1$lake_nets_downstreamlake_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_downstreamlake_n))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_downstreamlake_n))
dt1$lake_nets_lakeorder <- ifelse((trimws(as.character(dt1$lake_nets_lakeorder))==trimws("NA")),NA,dt1$lake_nets_lakeorder)               
suppressWarnings(dt1$lake_nets_lakeorder <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_lakeorder))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_lakeorder))
dt1$lake_nets_lnn <- ifelse((trimws(as.character(dt1$lake_nets_lnn))==trimws("NA")),NA,dt1$lake_nets_lnn)               
suppressWarnings(dt1$lake_nets_lnn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_lnn))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_lnn))
dt1$net_lakes_n <- ifelse((trimws(as.character(dt1$net_lakes_n))==trimws("NA")),NA,dt1$net_lakes_n)               
suppressWarnings(dt1$net_lakes_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$net_lakes_n))==as.character(as.numeric("NA"))),NA,dt1$net_lakes_n))
dt1$net_averagelakedistance_km <- ifelse((trimws(as.character(dt1$net_averagelakedistance_km))==trimws("NA")),NA,dt1$net_averagelakedistance_km)               
suppressWarnings(dt1$net_averagelakedistance_km <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$net_averagelakedistance_km))==as.character(as.numeric("NA"))),NA,dt1$net_averagelakedistance_km))
dt1$net_averagelakearea_ha <- ifelse((trimws(as.character(dt1$net_averagelakearea_ha))==trimws("NA")),NA,dt1$net_averagelakearea_ha)               
suppressWarnings(dt1$net_averagelakearea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$net_averagelakearea_ha))==as.character(as.numeric("NA"))),NA,dt1$net_averagelakearea_ha))
dt1$lake_nets_nearestdamdown_km <- ifelse((trimws(as.character(dt1$lake_nets_nearestdamdown_km))==trimws("NA")),NA,dt1$lake_nets_nearestdamdown_km)               
suppressWarnings(dt1$lake_nets_nearestdamdown_km <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_nearestdamdown_km))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_nearestdamdown_km))
dt1$lake_nets_totaldamdown_n <- ifelse((trimws(as.character(dt1$lake_nets_totaldamdown_n))==trimws("NA")),NA,dt1$lake_nets_totaldamdown_n)               
suppressWarnings(dt1$lake_nets_totaldamdown_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_totaldamdown_n))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_totaldamdown_n))
dt1$lake_nets_nearestdamup_km <- ifelse((trimws(as.character(dt1$lake_nets_nearestdamup_km))==trimws("NA")),NA,dt1$lake_nets_nearestdamup_km)               
suppressWarnings(dt1$lake_nets_nearestdamup_km <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_nearestdamup_km))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_nearestdamup_km))
dt1$lake_nets_totaldamup_n <- ifelse((trimws(as.character(dt1$lake_nets_totaldamup_n))==trimws("NA")),NA,dt1$lake_nets_totaldamup_n)               
suppressWarnings(dt1$lake_nets_totaldamup_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_totaldamup_n))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_totaldamup_n))
dt1$lake_nets_damonlake_flag <- ifelse((trimws(as.character(dt1$lake_nets_damonlake_flag))==trimws("NA")),NA,dt1$lake_nets_damonlake_flag)               
suppressWarnings(dt1$lake_nets_damonlake_flag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_damonlake_flag))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_damonlake_flag))
dt1$lake_nets_multidam_flag <- ifelse((trimws(as.character(dt1$lake_nets_multidam_flag))==trimws("NA")),NA,dt1$lake_nets_multidam_flag)               
suppressWarnings(dt1$lake_nets_multidam_flag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_nets_multidam_flag))==as.character(as.numeric("NA"))),NA,dt1$lake_nets_multidam_flag))
dt1$net_dams_n <- ifelse((trimws(as.character(dt1$net_dams_n))==trimws("NA")),NA,dt1$net_dams_n)               
suppressWarnings(dt1$net_dams_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$net_dams_n))==as.character(as.numeric("NA"))),NA,dt1$net_dams_n))
dt1$nhdplusv2_comid <- ifelse((trimws(as.character(dt1$nhdplusv2_comid))==trimws("NA")),NA,dt1$nhdplusv2_comid)               
suppressWarnings(dt1$nhdplusv2_comid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$nhdplusv2_comid))==as.character(as.numeric("NA"))),NA,dt1$nhdplusv2_comid))


# Observed issues when reading the data. An empty list is good!
problems(dt1) 
# Here is the structure of the input data tibble: 
glimpse(dt1) 
# And some statistical summaries of the data 
summary(dt1) 
# Get more details on character variables

summary(as.factor(dt1$net_id)) 
summary(as.factor(dt1$lake_nets_nearestdamdown_id)) 
summary(as.factor(dt1$lake_nets_nearestdamup_id)) 
summary(as.factor(dt1$lake_nets_damonlake_flag)) 
summary(as.factor(dt1$lake_nets_multidam_flag)) 
summary(as.factor(dt1$nhdplusv2_comid)) 


dt1$lagoslakeid<-factor(dt1$lagoslakeid)

#Where are these lakes and what network are they in?
#Joining  to lakeinformation which only has western states
colnames<-(intersect( colnames(lakeinformation),  colnames(dt1)))
dt1_western<-merge(lakeinformation,dt1, all.x=TRUE, no.dups=TRUE, by="lagoslakeid") %>%
  mutate(connectivity_type = case_when(net_id >= 1 ~ 'yes network',
                                       TRUE ~ 'no network')) 

colnames<-(intersect( colnames(dt1_western),  colnames(dt_limno))) #Find common column names
dt1_western<-left_join(dt1_western,dt_limno, by=colnames)
sum(is.na(dt_limno$year))
sum(is.na(dt1_western$year))

# Add reservoir data ------------------------------------------------------

## Reservoir data
# Package ID: edi.1016.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US RESERVOIR: Data module classifying conterminous U.S. lakes 4 hectares and larger as natural lakes or reservoirs.
# Data set creator:  Sam Polus - Michigan State University 
# Data set creator:  Patrick Hanly - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Qi Wang - Michigan State University 
# Data set creator:  Jessica DÃ­az VÃ¡zquez - Michigan State University 
# Data set creator:  Katherine Webster - Michigan State University 
# Data set creator:  Pang-Ning Tan - Michigan State University 
# Data set creator:  Jiayu Zhou - Michigan State University 
# Data set creator:  Laura Danila - Michigan State University 
# Data set creator:  Patricia Soranno - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Contact:  Patrick Hanly - Academic Specialist Michigan State University  - hanlypat@msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1016/2/dac3a0d7e34070639f4894ccc316cbd1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lagoslakeid",     
                 "lake_rsvr_nidid",     
                 "lake_nhdid",     
                 "neon_zoneid",     
                 "lake_rsvr_model_class",     
                 "lake_lat_decdeg",     
                 "lake_lon_decdeg",     
                 "lake_connectivity_class",     
                 "lake_rsvr_probnl",     
                 "lake_rsvr_probrsvr",     
                 "lake_rsvr_probdiff",     
                 "lake_rsvr_model",     
                 "lake_rsvr_nlneardam_flag",     
                 "lake_rsvr_rsvrisolated_flag",     
                 "lake_rsvr_classmethod",     
                 "lake_centroidstate",     
                 "lake_namelagos",     
                 "lake_shorelinedevfactor",     
                 "lake_rsvr_nidlat_decdeg",     
                 "lake_rsvr_nidlon_decdeg",     
                 "lake_shape",     
                 "FType",     
                 "lake_rsvr_class"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lagoslakeid)=="factor") dt1$lagoslakeid <-as.numeric(levels(dt1$lagoslakeid))[as.integer(dt1$lagoslakeid) ]               
if (class(dt1$lagoslakeid)=="character") dt1$lagoslakeid <-as.numeric(dt1$lagoslakeid)
if (class(dt1$lake_rsvr_nidid)!="factor") dt1$lake_rsvr_nidid<- as.factor(dt1$lake_rsvr_nidid)
if (class(dt1$lake_nhdid)!="factor") dt1$lake_nhdid<- as.factor(dt1$lake_nhdid)
if (class(dt1$neon_zoneid)!="factor") dt1$neon_zoneid<- as.factor(dt1$neon_zoneid)
if (class(dt1$lake_rsvr_model_class)!="factor") dt1$lake_rsvr_model_class<- as.factor(dt1$lake_rsvr_model_class)
if (class(dt1$lake_lat_decdeg)=="factor") dt1$lake_lat_decdeg <-as.numeric(levels(dt1$lake_lat_decdeg))[as.integer(dt1$lake_lat_decdeg) ]               
if (class(dt1$lake_lat_decdeg)=="character") dt1$lake_lat_decdeg <-as.numeric(dt1$lake_lat_decdeg)
if (class(dt1$lake_lon_decdeg)=="factor") dt1$lake_lon_decdeg <-as.numeric(levels(dt1$lake_lon_decdeg))[as.integer(dt1$lake_lon_decdeg) ]               
if (class(dt1$lake_lon_decdeg)=="character") dt1$lake_lon_decdeg <-as.numeric(dt1$lake_lon_decdeg)
if (class(dt1$lake_connectivity_class)!="factor") dt1$lake_connectivity_class<- as.factor(dt1$lake_connectivity_class)
if (class(dt1$lake_rsvr_probnl)=="factor") dt1$lake_rsvr_probnl <-as.numeric(levels(dt1$lake_rsvr_probnl))[as.integer(dt1$lake_rsvr_probnl) ]               
if (class(dt1$lake_rsvr_probnl)=="character") dt1$lake_rsvr_probnl <-as.numeric(dt1$lake_rsvr_probnl)
if (class(dt1$lake_rsvr_probrsvr)=="factor") dt1$lake_rsvr_probrsvr <-as.numeric(levels(dt1$lake_rsvr_probrsvr))[as.integer(dt1$lake_rsvr_probrsvr) ]               
if (class(dt1$lake_rsvr_probrsvr)=="character") dt1$lake_rsvr_probrsvr <-as.numeric(dt1$lake_rsvr_probrsvr)
if (class(dt1$lake_rsvr_probdiff)=="factor") dt1$lake_rsvr_probdiff <-as.numeric(levels(dt1$lake_rsvr_probdiff))[as.integer(dt1$lake_rsvr_probdiff) ]               
if (class(dt1$lake_rsvr_probdiff)=="character") dt1$lake_rsvr_probdiff <-as.numeric(dt1$lake_rsvr_probdiff)
if (class(dt1$lake_rsvr_model)!="factor") dt1$lake_rsvr_model<- as.factor(dt1$lake_rsvr_model)
if (class(dt1$lake_rsvr_nlneardam_flag)!="factor") dt1$lake_rsvr_nlneardam_flag<- as.factor(dt1$lake_rsvr_nlneardam_flag)
if (class(dt1$lake_rsvr_rsvrisolated_flag)!="factor") dt1$lake_rsvr_rsvrisolated_flag<- as.factor(dt1$lake_rsvr_rsvrisolated_flag)
if (class(dt1$lake_rsvr_classmethod)!="factor") dt1$lake_rsvr_classmethod<- as.factor(dt1$lake_rsvr_classmethod)
if (class(dt1$lake_centroidstate)!="factor") dt1$lake_centroidstate<- as.factor(dt1$lake_centroidstate)
if (class(dt1$lake_namelagos)!="factor") dt1$lake_namelagos<- as.factor(dt1$lake_namelagos)
if (class(dt1$lake_shorelinedevfactor)=="factor") dt1$lake_shorelinedevfactor <-as.numeric(levels(dt1$lake_shorelinedevfactor))[as.integer(dt1$lake_shorelinedevfactor) ]               
if (class(dt1$lake_shorelinedevfactor)=="character") dt1$lake_shorelinedevfactor <-as.numeric(dt1$lake_shorelinedevfactor)
if (class(dt1$lake_rsvr_nidlat_decdeg)=="factor") dt1$lake_rsvr_nidlat_decdeg <-as.numeric(levels(dt1$lake_rsvr_nidlat_decdeg))[as.integer(dt1$lake_rsvr_nidlat_decdeg) ]               
if (class(dt1$lake_rsvr_nidlat_decdeg)=="character") dt1$lake_rsvr_nidlat_decdeg <-as.numeric(dt1$lake_rsvr_nidlat_decdeg)
if (class(dt1$lake_rsvr_nidlon_decdeg)=="factor") dt1$lake_rsvr_nidlon_decdeg <-as.numeric(levels(dt1$lake_rsvr_nidlon_decdeg))[as.integer(dt1$lake_rsvr_nidlon_decdeg) ]               
if (class(dt1$lake_rsvr_nidlon_decdeg)=="character") dt1$lake_rsvr_nidlon_decdeg <-as.numeric(dt1$lake_rsvr_nidlon_decdeg)
if (class(dt1$lake_shape)!="factor") dt1$lake_shape<- as.factor(dt1$lake_shape)
if (class(dt1$FType)!="factor") dt1$FType<- as.factor(dt1$FType)
if (class(dt1$lake_rsvr_class)!="factor") dt1$lake_rsvr_class<- as.factor(dt1$lake_rsvr_class)

# Convert Missing Values to NA for non-dates

dt1$lake_rsvr_nidid <- as.factor(ifelse((trimws(as.character(dt1$lake_rsvr_nidid))==trimws("NULL")),NA,as.character(dt1$lake_rsvr_nidid)))
dt1$lake_connectivity_class <- as.factor(ifelse((trimws(as.character(dt1$lake_connectivity_class))==trimws("NULL")),NA,as.character(dt1$lake_connectivity_class)))
dt1$lake_rsvr_model <- as.factor(ifelse((trimws(as.character(dt1$lake_rsvr_model))==trimws("NULL")),NA,as.character(dt1$lake_rsvr_model)))
dt1$lake_namelagos <- as.factor(ifelse((trimws(as.character(dt1$lake_namelagos))==trimws("NULL")),NA,as.character(dt1$lake_namelagos)))
dt1$lake_rsvr_nidlat_decdeg <- ifelse((trimws(as.character(dt1$lake_rsvr_nidlat_decdeg))==trimws("NULL")),NA,dt1$lake_rsvr_nidlat_decdeg)               
suppressWarnings(dt1$lake_rsvr_nidlat_decdeg <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$lake_rsvr_nidlat_decdeg))==as.character(as.numeric("NULL"))),NA,dt1$lake_rsvr_nidlat_decdeg))
dt1$lake_rsvr_nidlon_decdeg <- ifelse((trimws(as.character(dt1$lake_rsvr_nidlon_decdeg))==trimws("NULL")),NA,dt1$lake_rsvr_nidlon_decdeg)               
suppressWarnings(dt1$lake_rsvr_nidlon_decdeg <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$lake_rsvr_nidlon_decdeg))==as.character(as.numeric("NULL"))),NA,dt1$lake_rsvr_nidlon_decdeg))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lagoslakeid)
summary(lake_rsvr_nidid)
summary(lake_nhdid)
summary(neon_zoneid)
summary(lake_rsvr_model_class)
summary(lake_lat_decdeg)
summary(lake_lon_decdeg)
summary(lake_connectivity_class)
summary(lake_rsvr_probnl)
summary(lake_rsvr_probrsvr)
summary(lake_rsvr_probdiff)
summary(lake_rsvr_model)
summary(lake_rsvr_nlneardam_flag)
summary(lake_rsvr_rsvrisolated_flag)
summary(lake_rsvr_classmethod)
summary(lake_centroidstate)
summary(lake_namelagos)
summary(lake_shorelinedevfactor)
summary(lake_rsvr_nidlat_decdeg)
summary(lake_rsvr_nidlon_decdeg)
summary(lake_shape)
summary(FType)
summary(lake_rsvr_class) 
# Get more details on character variables

summary(as.factor(dt1$lake_rsvr_nidid)) 
summary(as.factor(dt1$lake_nhdid)) 
summary(as.factor(dt1$neon_zoneid)) 
summary(as.factor(dt1$lake_rsvr_model_class)) 
summary(as.factor(dt1$lake_connectivity_class)) 
summary(as.factor(dt1$lake_rsvr_model)) 
summary(as.factor(dt1$lake_rsvr_nlneardam_flag)) 
summary(as.factor(dt1$lake_rsvr_rsvrisolated_flag)) 
summary(as.factor(dt1$lake_rsvr_classmethod)) 
summary(as.factor(dt1$lake_centroidstate)) 
summary(as.factor(dt1$lake_namelagos)) 
summary(as.factor(dt1$lake_shape)) 
summary(as.factor(dt1$FType)) 
summary(as.factor(dt1$lake_rsvr_class))
detach(dt1)               


reservoir<-dt1 %>%
  dplyr::select(lagoslakeid, lake_rsvr_class, lake_rsvr_probnl, lake_rsvr_probrsvr,
                lake_rsvr_model,lake_rsvr_probdiff,lake_rsvr_classmethod,
                lake_rsvr_rsvrisolated_flag,lake_rsvr_nlneardam_flag) %>%
  mutate(lagoslakeid=factor(lagoslakeid))


colnames<-(intersect( colnames(dt1_western), colnames(reservoir)))
dt1_western<- left_join(dt1_western,reservoir,by=colnames) 


# summarize spatial DF ----------------------------------------------------


#median of the medians
#But prior to transformation I want to keep year as a number because I am curious when most of these lakes were sampled
dt1_western_yearasNUM<-dt1_western %>%
  mutate(year=as.numeric(as.character(year))) %>%
  dplyr::select(lagoslakeid, year, contains(c("_median")))
#Data.frame method
setDT(dt1_western_yearasNUM)
dt1_western_summary<-dt1_western_yearasNUM[, lapply(.SD, median, na.rm=TRUE), .SDcols = is.numeric, by=c("lagoslakeid")  ] 

dt1_western_summary <- dt1_western_summary %>%
  mutate(year=round(year,0))%>%
  mutate(decade_sampled=
           ifelse(year >1970 & year <=1979, "1970s", 
                  ifelse(year >=1980 & year <=1989, "1980s", 
                         ifelse(year >=1990 & year <=1999, "1990s", 
                                ifelse(year >=2000 & year <=2009, "2000s",
                                       ifelse(year >=2010 & year <=2020, "2010s",
                                              ifelse(year >=2020,  "> 2020", "error"))))))) %>%
  mutate(decade_sampled = factor(decade_sampled,
                                 levels = c("1970s", "1980s", "1990s",
                                            "2000s","2010s"," > 2020"))) #Reorder factors for plotting


#Using merge, we end up losing a lot of observations starting with "reservoir"
colnames<-(intersect( colnames(dt1_western_summary),  colnames(lakewatersheds))) #identify common columns between data.tables
dt1_western_summary<-left_join(dt1_western_summary,lakewatersheds, by=colnames)
colnames<-(intersect( colnames(dt1_western_summary),  colnames(dt1))) #identify common columns between data.tables
dt1_western_summary<-left_join(dt1_western_summary,dt1,   by=colnames)
colnames<-(intersect( colnames(dt1_western_summary),  colnames(depth))) #identify common columns between data.tables
dt1_western_summary<-left_join(dt1_western_summary,depth, by=colnames)
colnames<-(intersect( colnames(dt1_western_summary),  colnames(reservoir))) #identify common columns between data.tables
dt1_western_summary<-left_join(dt1_western_summary,reservoir, by=colnames)
colnames<-(intersect( colnames(dt1_western_summary),  colnames(lakecharacteristics))) #identify common columns between data.tables
dt1_western_summary<-left_join(dt1_western_summary,lakecharacteristics,  by=colnames)


#double checks
dt1_western_summary$year


#since we are going to focus on TP and TN, filter out NAs now
 dt1_western_summary<-dt1_western_summary%>%
   filter(!(is.na(tp_ugl_median) &
            is.na(tn_ugl_median))) 
# 
dt1_western_summary_TP <- dt1_western_summary %>%
   filter(!is.na(tp_ugl_median)) 
# 
 dt1_western_summary_TN <- dt1_western_summary %>%
   filter(!is.na(tn_ugl_median)) 


## Add levels and labels for EPA zones 
dt1_western_summary<- dt1_western_summary %>%
  mutate(epanutr_zoneid = factor(epanutr_zoneid,
                                 levels=c("epanutr_3","epanutr_5",
                                                   "epanutr_8","epanutr_9"),
                                 labels=c("N. Plains","S. Plains","Western Mtns","Xeric"))) 
              
#write.csv(dt1_western_summary, "data/limno_summary.csv")

# summarize spatial df (2000-present) -------------------------------------

dt1_western_yearasNUM2000<-dt1_western %>%
  mutate(year=as.numeric(as.character(year))) %>%
  dplyr::select(lagoslakeid, year, contains(c("_median"))) %>%
  filter(year>=2000)

#Data.frame method
setDT(dt1_western_yearasNUM2000)
dt1_western_summary_2000s<-dt1_western_yearasNUM2000[, lapply(.SD, median, na.rm=TRUE), .SDcols = is.numeric, by=c("lagoslakeid")  ] 


#We lost some variables in the summary, so adding them back in here
colnames<-(intersect( colnames(dt1_western_summary_2000s),  colnames(lakewatersheds))) #identify common columns between data.tables
dt1_western_summary_2000s<-merge(dt1_western_summary_2000s,lakewatersheds,  no.dups=TRUE, by=colnames)
#colnames<-(intersect( colnames(dt1_western_summary_2000s),  colnames(dt1))) #identify common columns between data.tables
#dt1_western_summary_2000s<-merge(dt1_western_summary_2000s,dt1,  no.dups=TRUE, by=colnames)
colnames<-(intersect( colnames(dt1_western_summary_2000s),  colnames(depth))) #identify common columns between data.tables
dt1_western_summary_2000s<-merge(dt1_western_summary_2000s,depth,  no.dups=TRUE, by=colnames)
colnames<-(intersect( colnames(dt1_western_summary_2000s),  colnames(reservoir))) #identify common columns between data.tables
dt1_western_summary_2000s<-merge(dt1_western_summary_2000s,reservoir,  no.dups=TRUE, by=colnames)


lakecharacteristics_trim <- lakecharacteristics %>%
  dplyr::select(lagoslakeid, lake_perimeter_m, lake_totalarea_ha) #,
               # lake_connectivity_class, lake_connectivity_fluctuates, lake_connectivity_permanent,
              #  lake_lakes4ha_upstream_ha, lake_lakes4ha_upstream_n, lake_lakes1ha_upstream_ha,
              #  lake_lakes1ha_upstream_n, lake_lakes10ha_upstream_n, lake_lakes10ha_upstream_ha, 
             #   lake_glaciatedlatewisc)
dt1_western_summary_2000s<-merge(dt1_western_summary_2000s,lakecharacteristics_trim,  no.dups=TRUE, by="lagoslakeid")%>%
  #mutate(year= as.character.Date(year))%>%
  select(lagoslakeid, year,ca_mgl_median, alk_ueql_median, do_mgl_median,ph_eq_median,so4_mgl_median, salinity_mgl_median,
         chla_ugl_median, no2no3n_ugl_median, nh4n_ugl_median,tkn_ugl_median, tn_ugl_median, ton_ugl_median, tp_ugl_median, 
         srpp_ugl_median, microcystin_ugl_median,ws_subtype,ws_area_ha, ws_perimeter_m,ws_lake_arearatio, ws_mbgconhull_length_m, 
        ws_mbgconhull_width_m, ws_meanwidth_m, nws_area_ha,nws_perimeter_m, nws_lake_arearatio,nws_mbgconhull_length_m,lake_rsvr_class,               
         lake_rsvr_probnl, lake_rsvr_probrsvr)%>%
  mutate(year = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))%>%
  mutate(year = format(year, format = "%Y"))



#write.csv(dt1_western_summary_2000s, "data/limno_summary_2000s.csv")
