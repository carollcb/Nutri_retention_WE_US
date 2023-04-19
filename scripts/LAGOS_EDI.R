# Package ID: edi.854.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical characteristics of lakes and their watersheds in the conterminous U.S..
# Data set creator:  Nicole Smith - Michigan State University 
# Data set creator:  Katherine Webster - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Data set creator:  Patricia Soranno - Michigan State University 
# Contact:  Kendra Cheruvelil -  Michigan State University  - ksc@msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/854/1/007ca4f5ec02bb5809fc661dcfa7a903" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
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
                 "neon_zoneid"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lagoslakeid)=="factor") dt1$lagoslakeid <-as.numeric(levels(dt1$lagoslakeid))[as.integer(dt1$lagoslakeid) ]               
if (class(dt1$lagoslakeid)=="character") dt1$lagoslakeid <-as.numeric(dt1$lagoslakeid)
if (class(dt1$lake_nhdid)!="factor") dt1$lake_nhdid<- as.factor(dt1$lake_nhdid)
if (class(dt1$lake_nhdfcode)!="factor") dt1$lake_nhdfcode<- as.factor(dt1$lake_nhdfcode)
if (class(dt1$lake_nhdftype)!="factor") dt1$lake_nhdftype<- as.factor(dt1$lake_nhdftype)
if (class(dt1$lake_reachcode)!="factor") dt1$lake_reachcode<- as.factor(dt1$lake_reachcode)
if (class(dt1$lake_namegnis)!="factor") dt1$lake_namegnis<- as.factor(dt1$lake_namegnis)
if (class(dt1$lake_namelagos)!="factor") dt1$lake_namelagos<- as.factor(dt1$lake_namelagos)
if (class(dt1$lake_onlandborder)!="factor") dt1$lake_onlandborder<- as.factor(dt1$lake_onlandborder)
if (class(dt1$lake_ismultipart)!="factor") dt1$lake_ismultipart<- as.factor(dt1$lake_ismultipart)
if (class(dt1$lake_missingws)!="factor") dt1$lake_missingws<- as.factor(dt1$lake_missingws)
if (class(dt1$lake_shapeflag)!="factor") dt1$lake_shapeflag<- as.factor(dt1$lake_shapeflag)
if (class(dt1$lake_lat_decdeg)=="factor") dt1$lake_lat_decdeg <-as.numeric(levels(dt1$lake_lat_decdeg))[as.integer(dt1$lake_lat_decdeg) ]               
if (class(dt1$lake_lat_decdeg)=="character") dt1$lake_lat_decdeg <-as.numeric(dt1$lake_lat_decdeg)
if (class(dt1$lake_lon_decdeg)=="factor") dt1$lake_lon_decdeg <-as.numeric(levels(dt1$lake_lon_decdeg))[as.integer(dt1$lake_lon_decdeg) ]               
if (class(dt1$lake_lon_decdeg)=="character") dt1$lake_lon_decdeg <-as.numeric(dt1$lake_lon_decdeg)
if (class(dt1$lake_elevation_m)=="factor") dt1$lake_elevation_m <-as.numeric(levels(dt1$lake_elevation_m))[as.integer(dt1$lake_elevation_m) ]               
if (class(dt1$lake_elevation_m)=="character") dt1$lake_elevation_m <-as.numeric(dt1$lake_elevation_m)
if (class(dt1$lake_centroidstate)!="factor") dt1$lake_centroidstate<- as.factor(dt1$lake_centroidstate)
if (class(dt1$lake_states)!="factor") dt1$lake_states<- as.factor(dt1$lake_states)
if (class(dt1$lake_county)!="factor") dt1$lake_county<- as.factor(dt1$lake_county)
if (class(dt1$lake_countyfips)!="factor") dt1$lake_countyfips<- as.factor(dt1$lake_countyfips)
if (class(dt1$lake_huc12)!="factor") dt1$lake_huc12<- as.factor(dt1$lake_huc12)
if (class(dt1$buff100_zoneid)!="factor") dt1$buff100_zoneid<- as.factor(dt1$buff100_zoneid)
if (class(dt1$buff500_zoneid)!="factor") dt1$buff500_zoneid<- as.factor(dt1$buff500_zoneid)
if (class(dt1$ws_zoneid)!="factor") dt1$ws_zoneid<- as.factor(dt1$ws_zoneid)
if (class(dt1$nws_zoneid)!="factor") dt1$nws_zoneid<- as.factor(dt1$nws_zoneid)
if (class(dt1$hu12_zoneid)!="factor") dt1$hu12_zoneid<- as.factor(dt1$hu12_zoneid)
if (class(dt1$hu8_zoneid)!="factor") dt1$hu8_zoneid<- as.factor(dt1$hu8_zoneid)
if (class(dt1$hu4_zoneid)!="factor") dt1$hu4_zoneid<- as.factor(dt1$hu4_zoneid)
if (class(dt1$county_zoneid)!="factor") dt1$county_zoneid<- as.factor(dt1$county_zoneid)
if (class(dt1$state_zoneid)!="factor") dt1$state_zoneid<- as.factor(dt1$state_zoneid)
if (class(dt1$epanutr_zoneid)!="factor") dt1$epanutr_zoneid<- as.factor(dt1$epanutr_zoneid)
if (class(dt1$omernik3_zoneid)!="factor") dt1$omernik3_zoneid<- as.factor(dt1$omernik3_zoneid)
if (class(dt1$wwf_zoneid)!="factor") dt1$wwf_zoneid<- as.factor(dt1$wwf_zoneid)
if (class(dt1$mlra_zoneid)!="factor") dt1$mlra_zoneid<- as.factor(dt1$mlra_zoneid)
if (class(dt1$bailey_zoneid)!="factor") dt1$bailey_zoneid<- as.factor(dt1$bailey_zoneid)
if (class(dt1$neon_zoneid)!="factor") dt1$neon_zoneid<- as.factor(dt1$neon_zoneid)

# Convert Missing Values to NA for non-dates

dt1$lagoslakeid <- ifelse((trimws(as.character(dt1$lagoslakeid))==trimws("NA")),NA,dt1$lagoslakeid)               
suppressWarnings(dt1$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lagoslakeid))==as.character(as.numeric("NA"))),NA,dt1$lagoslakeid))
dt1$lake_nhdid <- as.factor(ifelse((trimws(as.character(dt1$lake_nhdid))==trimws("NA")),NA,as.character(dt1$lake_nhdid)))
dt1$lake_nhdfcode <- as.factor(ifelse((trimws(as.character(dt1$lake_nhdfcode))==trimws("NA")),NA,as.character(dt1$lake_nhdfcode)))
dt1$lake_nhdftype <- as.factor(ifelse((trimws(as.character(dt1$lake_nhdftype))==trimws("NA")),NA,as.character(dt1$lake_nhdftype)))
dt1$lake_reachcode <- as.factor(ifelse((trimws(as.character(dt1$lake_reachcode))==trimws("NA")),NA,as.character(dt1$lake_reachcode)))
dt1$lake_namegnis <- as.factor(ifelse((trimws(as.character(dt1$lake_namegnis))==trimws("NA")),NA,as.character(dt1$lake_namegnis)))
dt1$lake_namelagos <- as.factor(ifelse((trimws(as.character(dt1$lake_namelagos))==trimws("NA")),NA,as.character(dt1$lake_namelagos)))
dt1$lake_onlandborder <- as.factor(ifelse((trimws(as.character(dt1$lake_onlandborder))==trimws("NA")),NA,as.character(dt1$lake_onlandborder)))
dt1$lake_ismultipart <- as.factor(ifelse((trimws(as.character(dt1$lake_ismultipart))==trimws("NA")),NA,as.character(dt1$lake_ismultipart)))
dt1$lake_missingws <- as.factor(ifelse((trimws(as.character(dt1$lake_missingws))==trimws("NA")),NA,as.character(dt1$lake_missingws)))
dt1$lake_shapeflag <- as.factor(ifelse((trimws(as.character(dt1$lake_shapeflag))==trimws("NA")),NA,as.character(dt1$lake_shapeflag)))
dt1$lake_lat_decdeg <- ifelse((trimws(as.character(dt1$lake_lat_decdeg))==trimws("NA")),NA,dt1$lake_lat_decdeg)               
suppressWarnings(dt1$lake_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_lat_decdeg))==as.character(as.numeric("NA"))),NA,dt1$lake_lat_decdeg))
dt1$lake_lon_decdeg <- ifelse((trimws(as.character(dt1$lake_lon_decdeg))==trimws("NA")),NA,dt1$lake_lon_decdeg)               
suppressWarnings(dt1$lake_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_lon_decdeg))==as.character(as.numeric("NA"))),NA,dt1$lake_lon_decdeg))
dt1$lake_elevation_m <- ifelse((trimws(as.character(dt1$lake_elevation_m))==trimws("NA")),NA,dt1$lake_elevation_m)               
suppressWarnings(dt1$lake_elevation_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lake_elevation_m))==as.character(as.numeric("NA"))),NA,dt1$lake_elevation_m))
dt1$lake_centroidstate <- as.factor(ifelse((trimws(as.character(dt1$lake_centroidstate))==trimws("NA")),NA,as.character(dt1$lake_centroidstate)))
dt1$lake_states <- as.factor(ifelse((trimws(as.character(dt1$lake_states))==trimws("NA")),NA,as.character(dt1$lake_states)))
dt1$lake_county <- as.factor(ifelse((trimws(as.character(dt1$lake_county))==trimws("NA")),NA,as.character(dt1$lake_county)))
dt1$lake_countyfips <- as.factor(ifelse((trimws(as.character(dt1$lake_countyfips))==trimws("NA")),NA,as.character(dt1$lake_countyfips)))
dt1$lake_huc12 <- as.factor(ifelse((trimws(as.character(dt1$lake_huc12))==trimws("NA")),NA,as.character(dt1$lake_huc12)))
dt1$buff100_zoneid <- as.factor(ifelse((trimws(as.character(dt1$buff100_zoneid))==trimws("NA")),NA,as.character(dt1$buff100_zoneid)))
dt1$buff500_zoneid <- as.factor(ifelse((trimws(as.character(dt1$buff500_zoneid))==trimws("NA")),NA,as.character(dt1$buff500_zoneid)))
dt1$ws_zoneid <- as.factor(ifelse((trimws(as.character(dt1$ws_zoneid))==trimws("NA")),NA,as.character(dt1$ws_zoneid)))
dt1$nws_zoneid <- as.factor(ifelse((trimws(as.character(dt1$nws_zoneid))==trimws("NA")),NA,as.character(dt1$nws_zoneid)))
dt1$hu12_zoneid <- as.factor(ifelse((trimws(as.character(dt1$hu12_zoneid))==trimws("NA")),NA,as.character(dt1$hu12_zoneid)))
dt1$hu8_zoneid <- as.factor(ifelse((trimws(as.character(dt1$hu8_zoneid))==trimws("NA")),NA,as.character(dt1$hu8_zoneid)))
dt1$hu4_zoneid <- as.factor(ifelse((trimws(as.character(dt1$hu4_zoneid))==trimws("NA")),NA,as.character(dt1$hu4_zoneid)))
dt1$county_zoneid <- as.factor(ifelse((trimws(as.character(dt1$county_zoneid))==trimws("NA")),NA,as.character(dt1$county_zoneid)))
dt1$state_zoneid <- as.factor(ifelse((trimws(as.character(dt1$state_zoneid))==trimws("NA")),NA,as.character(dt1$state_zoneid)))
dt1$epanutr_zoneid <- as.factor(ifelse((trimws(as.character(dt1$epanutr_zoneid))==trimws("NA")),NA,as.character(dt1$epanutr_zoneid)))
dt1$omernik3_zoneid <- as.factor(ifelse((trimws(as.character(dt1$omernik3_zoneid))==trimws("NA")),NA,as.character(dt1$omernik3_zoneid)))
dt1$wwf_zoneid <- as.factor(ifelse((trimws(as.character(dt1$wwf_zoneid))==trimws("NA")),NA,as.character(dt1$wwf_zoneid)))
dt1$mlra_zoneid <- as.factor(ifelse((trimws(as.character(dt1$mlra_zoneid))==trimws("NA")),NA,as.character(dt1$mlra_zoneid)))
dt1$bailey_zoneid <- as.factor(ifelse((trimws(as.character(dt1$bailey_zoneid))==trimws("NA")),NA,as.character(dt1$bailey_zoneid)))
dt1$neon_zoneid <- as.factor(ifelse((trimws(as.character(dt1$neon_zoneid))==trimws("NA")),NA,as.character(dt1$neon_zoneid)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lagoslakeid)
summary(lake_nhdid)
summary(lake_nhdfcode)
summary(lake_nhdftype)
summary(lake_reachcode)
summary(lake_namegnis)
summary(lake_namelagos)
summary(lake_onlandborder)
summary(lake_ismultipart)
summary(lake_missingws)
summary(lake_shapeflag)
summary(lake_lat_decdeg)
summary(lake_lon_decdeg)
summary(lake_elevation_m)
summary(lake_centroidstate)
summary(lake_states)
summary(lake_county)
summary(lake_countyfips)
summary(lake_huc12)
summary(buff100_zoneid)
summary(buff500_zoneid)
summary(ws_zoneid)
summary(nws_zoneid)
summary(hu12_zoneid)
summary(hu8_zoneid)
summary(hu4_zoneid)
summary(county_zoneid)
summary(state_zoneid)
summary(epanutr_zoneid)
summary(omernik3_zoneid)
summary(wwf_zoneid)
summary(mlra_zoneid)
summary(bailey_zoneid)
summary(neon_zoneid) 
# Get more details on character variables

summary(as.factor(dt1$lake_nhdid)) 
summary(as.factor(dt1$lake_nhdfcode)) 
summary(as.factor(dt1$lake_nhdftype)) 
summary(as.factor(dt1$lake_reachcode)) 
summary(as.factor(dt1$lake_namegnis)) 
summary(as.factor(dt1$lake_namelagos)) 
summary(as.factor(dt1$lake_onlandborder)) 
summary(as.factor(dt1$lake_ismultipart)) 
summary(as.factor(dt1$lake_missingws)) 
summary(as.factor(dt1$lake_shapeflag)) 
summary(as.factor(dt1$lake_centroidstate)) 
summary(as.factor(dt1$lake_states)) 
summary(as.factor(dt1$lake_county)) 
summary(as.factor(dt1$lake_countyfips)) 
summary(as.factor(dt1$lake_huc12)) 
summary(as.factor(dt1$buff100_zoneid)) 
summary(as.factor(dt1$buff500_zoneid)) 
summary(as.factor(dt1$ws_zoneid)) 
summary(as.factor(dt1$nws_zoneid)) 
summary(as.factor(dt1$hu12_zoneid)) 
summary(as.factor(dt1$hu8_zoneid)) 
summary(as.factor(dt1$hu4_zoneid)) 
summary(as.factor(dt1$county_zoneid)) 
summary(as.factor(dt1$state_zoneid)) 
summary(as.factor(dt1$epanutr_zoneid)) 
summary(as.factor(dt1$omernik3_zoneid)) 
summary(as.factor(dt1$wwf_zoneid)) 
summary(as.factor(dt1$mlra_zoneid)) 
summary(as.factor(dt1$bailey_zoneid)) 
summary(as.factor(dt1$neon_zoneid))
detach(dt1)               

