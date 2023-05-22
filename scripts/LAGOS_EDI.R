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


d_mm <-read.csv(infile1,header=F 
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

if (class(d_mm$lagoslakeid)=="factor") d_mm$lagoslakeid <-as.numeric(levels(d_mm$lagoslakeid))[as.integer(d_mm$lagoslakeid) ]               
if (class(d_mm$lagoslakeid)=="character") d_mm$lagoslakeid <-as.numeric(d_mm$lagoslakeid)
if (class(d_mm$lake_nhdid)!="factor") d_mm$lake_nhdid<- as.factor(d_mm$lake_nhdid)
if (class(d_mm$lake_nhdfcode)!="factor") d_mm$lake_nhdfcode<- as.factor(d_mm$lake_nhdfcode)
if (class(d_mm$lake_nhdftype)!="factor") d_mm$lake_nhdftype<- as.factor(d_mm$lake_nhdftype)
if (class(d_mm$lake_reachcode)!="factor") d_mm$lake_reachcode<- as.factor(d_mm$lake_reachcode)
if (class(d_mm$lake_namegnis)!="factor") d_mm$lake_namegnis<- as.factor(d_mm$lake_namegnis)
if (class(d_mm$lake_namelagos)!="factor") d_mm$lake_namelagos<- as.factor(d_mm$lake_namelagos)
if (class(d_mm$lake_onlandborder)!="factor") d_mm$lake_onlandborder<- as.factor(d_mm$lake_onlandborder)
if (class(d_mm$lake_ismultipart)!="factor") d_mm$lake_ismultipart<- as.factor(d_mm$lake_ismultipart)
if (class(d_mm$lake_missingws)!="factor") d_mm$lake_missingws<- as.factor(d_mm$lake_missingws)
if (class(d_mm$lake_shapeflag)!="factor") d_mm$lake_shapeflag<- as.factor(d_mm$lake_shapeflag)
if (class(d_mm$lake_lat_decdeg)=="factor") d_mm$lake_lat_decdeg <-as.numeric(levels(d_mm$lake_lat_decdeg))[as.integer(d_mm$lake_lat_decdeg) ]               
if (class(d_mm$lake_lat_decdeg)=="character") d_mm$lake_lat_decdeg <-as.numeric(d_mm$lake_lat_decdeg)
if (class(d_mm$lake_lon_decdeg)=="factor") d_mm$lake_lon_decdeg <-as.numeric(levels(d_mm$lake_lon_decdeg))[as.integer(d_mm$lake_lon_decdeg) ]               
if (class(d_mm$lake_lon_decdeg)=="character") d_mm$lake_lon_decdeg <-as.numeric(d_mm$lake_lon_decdeg)
if (class(d_mm$lake_elevation_m)=="factor") d_mm$lake_elevation_m <-as.numeric(levels(d_mm$lake_elevation_m))[as.integer(d_mm$lake_elevation_m) ]               
if (class(d_mm$lake_elevation_m)=="character") d_mm$lake_elevation_m <-as.numeric(d_mm$lake_elevation_m)
if (class(d_mm$lake_centroidstate)!="factor") d_mm$lake_centroidstate<- as.factor(d_mm$lake_centroidstate)
if (class(d_mm$lake_states)!="factor") d_mm$lake_states<- as.factor(d_mm$lake_states)
if (class(d_mm$lake_county)!="factor") d_mm$lake_county<- as.factor(d_mm$lake_county)
if (class(d_mm$lake_countyfips)!="factor") d_mm$lake_countyfips<- as.factor(d_mm$lake_countyfips)
if (class(d_mm$lake_huc12)!="factor") d_mm$lake_huc12<- as.factor(d_mm$lake_huc12)
if (class(d_mm$buff100_zoneid)!="factor") d_mm$buff100_zoneid<- as.factor(d_mm$buff100_zoneid)
if (class(d_mm$buff500_zoneid)!="factor") d_mm$buff500_zoneid<- as.factor(d_mm$buff500_zoneid)
if (class(d_mm$ws_zoneid)!="factor") d_mm$ws_zoneid<- as.factor(d_mm$ws_zoneid)
if (class(d_mm$nws_zoneid)!="factor") d_mm$nws_zoneid<- as.factor(d_mm$nws_zoneid)
if (class(d_mm$hu12_zoneid)!="factor") d_mm$hu12_zoneid<- as.factor(d_mm$hu12_zoneid)
if (class(d_mm$hu8_zoneid)!="factor") d_mm$hu8_zoneid<- as.factor(d_mm$hu8_zoneid)
if (class(d_mm$hu4_zoneid)!="factor") d_mm$hu4_zoneid<- as.factor(d_mm$hu4_zoneid)
if (class(d_mm$county_zoneid)!="factor") d_mm$county_zoneid<- as.factor(d_mm$county_zoneid)
if (class(d_mm$state_zoneid)!="factor") d_mm$state_zoneid<- as.factor(d_mm$state_zoneid)
if (class(d_mm$epanutr_zoneid)!="factor") d_mm$epanutr_zoneid<- as.factor(d_mm$epanutr_zoneid)
if (class(d_mm$omernik3_zoneid)!="factor") d_mm$omernik3_zoneid<- as.factor(d_mm$omernik3_zoneid)
if (class(d_mm$wwf_zoneid)!="factor") d_mm$wwf_zoneid<- as.factor(d_mm$wwf_zoneid)
if (class(d_mm$mlra_zoneid)!="factor") d_mm$mlra_zoneid<- as.factor(d_mm$mlra_zoneid)
if (class(d_mm$bailey_zoneid)!="factor") d_mm$bailey_zoneid<- as.factor(d_mm$bailey_zoneid)
if (class(d_mm$neon_zoneid)!="factor") d_mm$neon_zoneid<- as.factor(d_mm$neon_zoneid)

# Convert Missing Values to NA for non-dates

d_mm$lagoslakeid <- ifelse((trimws(as.character(d_mm$lagoslakeid))==trimws("NA")),NA,d_mm$lagoslakeid)               
suppressWarnings(d_mm$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(d_mm$lagoslakeid))==as.character(as.numeric("NA"))),NA,d_mm$lagoslakeid))
d_mm$lake_nhdid <- as.factor(ifelse((trimws(as.character(d_mm$lake_nhdid))==trimws("NA")),NA,as.character(d_mm$lake_nhdid)))
d_mm$lake_nhdfcode <- as.factor(ifelse((trimws(as.character(d_mm$lake_nhdfcode))==trimws("NA")),NA,as.character(d_mm$lake_nhdfcode)))
d_mm$lake_nhdftype <- as.factor(ifelse((trimws(as.character(d_mm$lake_nhdftype))==trimws("NA")),NA,as.character(d_mm$lake_nhdftype)))
d_mm$lake_reachcode <- as.factor(ifelse((trimws(as.character(d_mm$lake_reachcode))==trimws("NA")),NA,as.character(d_mm$lake_reachcode)))
d_mm$lake_namegnis <- as.factor(ifelse((trimws(as.character(d_mm$lake_namegnis))==trimws("NA")),NA,as.character(d_mm$lake_namegnis)))
d_mm$lake_namelagos <- as.factor(ifelse((trimws(as.character(d_mm$lake_namelagos))==trimws("NA")),NA,as.character(d_mm$lake_namelagos)))
d_mm$lake_onlandborder <- as.factor(ifelse((trimws(as.character(d_mm$lake_onlandborder))==trimws("NA")),NA,as.character(d_mm$lake_onlandborder)))
d_mm$lake_ismultipart <- as.factor(ifelse((trimws(as.character(d_mm$lake_ismultipart))==trimws("NA")),NA,as.character(d_mm$lake_ismultipart)))
d_mm$lake_missingws <- as.factor(ifelse((trimws(as.character(d_mm$lake_missingws))==trimws("NA")),NA,as.character(d_mm$lake_missingws)))
d_mm$lake_shapeflag <- as.factor(ifelse((trimws(as.character(d_mm$lake_shapeflag))==trimws("NA")),NA,as.character(d_mm$lake_shapeflag)))
d_mm$lake_lat_decdeg <- ifelse((trimws(as.character(d_mm$lake_lat_decdeg))==trimws("NA")),NA,d_mm$lake_lat_decdeg)               
suppressWarnings(d_mm$lake_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(d_mm$lake_lat_decdeg))==as.character(as.numeric("NA"))),NA,d_mm$lake_lat_decdeg))
d_mm$lake_lon_decdeg <- ifelse((trimws(as.character(d_mm$lake_lon_decdeg))==trimws("NA")),NA,d_mm$lake_lon_decdeg)               
suppressWarnings(d_mm$lake_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(d_mm$lake_lon_decdeg))==as.character(as.numeric("NA"))),NA,d_mm$lake_lon_decdeg))
d_mm$lake_elevation_m <- ifelse((trimws(as.character(d_mm$lake_elevation_m))==trimws("NA")),NA,d_mm$lake_elevation_m)               
suppressWarnings(d_mm$lake_elevation_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(d_mm$lake_elevation_m))==as.character(as.numeric("NA"))),NA,d_mm$lake_elevation_m))
d_mm$lake_centroidstate <- as.factor(ifelse((trimws(as.character(d_mm$lake_centroidstate))==trimws("NA")),NA,as.character(d_mm$lake_centroidstate)))
d_mm$lake_states <- as.factor(ifelse((trimws(as.character(d_mm$lake_states))==trimws("NA")),NA,as.character(d_mm$lake_states)))
d_mm$lake_county <- as.factor(ifelse((trimws(as.character(d_mm$lake_county))==trimws("NA")),NA,as.character(d_mm$lake_county)))
d_mm$lake_countyfips <- as.factor(ifelse((trimws(as.character(d_mm$lake_countyfips))==trimws("NA")),NA,as.character(d_mm$lake_countyfips)))
d_mm$lake_huc12 <- as.factor(ifelse((trimws(as.character(d_mm$lake_huc12))==trimws("NA")),NA,as.character(d_mm$lake_huc12)))
d_mm$buff100_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$buff100_zoneid))==trimws("NA")),NA,as.character(d_mm$buff100_zoneid)))
d_mm$buff500_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$buff500_zoneid))==trimws("NA")),NA,as.character(d_mm$buff500_zoneid)))
d_mm$ws_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$ws_zoneid))==trimws("NA")),NA,as.character(d_mm$ws_zoneid)))
d_mm$nws_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$nws_zoneid))==trimws("NA")),NA,as.character(d_mm$nws_zoneid)))
d_mm$hu12_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$hu12_zoneid))==trimws("NA")),NA,as.character(d_mm$hu12_zoneid)))
d_mm$hu8_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$hu8_zoneid))==trimws("NA")),NA,as.character(d_mm$hu8_zoneid)))
d_mm$hu4_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$hu4_zoneid))==trimws("NA")),NA,as.character(d_mm$hu4_zoneid)))
d_mm$county_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$county_zoneid))==trimws("NA")),NA,as.character(d_mm$county_zoneid)))
d_mm$state_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$state_zoneid))==trimws("NA")),NA,as.character(d_mm$state_zoneid)))
d_mm$epanutr_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$epanutr_zoneid))==trimws("NA")),NA,as.character(d_mm$epanutr_zoneid)))
d_mm$omernik3_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$omernik3_zoneid))==trimws("NA")),NA,as.character(d_mm$omernik3_zoneid)))
d_mm$wwf_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$wwf_zoneid))==trimws("NA")),NA,as.character(d_mm$wwf_zoneid)))
d_mm$mlra_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$mlra_zoneid))==trimws("NA")),NA,as.character(d_mm$mlra_zoneid)))
d_mm$bailey_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$bailey_zoneid))==trimws("NA")),NA,as.character(d_mm$bailey_zoneid)))
d_mm$neon_zoneid <- as.factor(ifelse((trimws(as.character(d_mm$neon_zoneid))==trimws("NA")),NA,as.character(d_mm$neon_zoneid)))


# Here is the structure of the input data frame:
str(d_mm)                            
attach(d_mm)                            
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

summary(as.factor(d_mm$lake_nhdid)) 
summary(as.factor(d_mm$lake_nhdfcode)) 
summary(as.factor(d_mm$lake_nhdftype)) 
summary(as.factor(d_mm$lake_reachcode)) 
summary(as.factor(d_mm$lake_namegnis)) 
summary(as.factor(d_mm$lake_namelagos)) 
summary(as.factor(d_mm$lake_onlandborder)) 
summary(as.factor(d_mm$lake_ismultipart)) 
summary(as.factor(d_mm$lake_missingws)) 
summary(as.factor(d_mm$lake_shapeflag)) 
summary(as.factor(d_mm$lake_centroidstate)) 
summary(as.factor(d_mm$lake_states)) 
summary(as.factor(d_mm$lake_county)) 
summary(as.factor(d_mm$lake_countyfips)) 
summary(as.factor(d_mm$lake_huc12)) 
summary(as.factor(d_mm$buff100_zoneid)) 
summary(as.factor(d_mm$buff500_zoneid)) 
summary(as.factor(d_mm$ws_zoneid)) 
summary(as.factor(d_mm$nws_zoneid)) 
summary(as.factor(d_mm$hu12_zoneid)) 
summary(as.factor(d_mm$hu8_zoneid)) 
summary(as.factor(d_mm$hu4_zoneid)) 
summary(as.factor(d_mm$county_zoneid)) 
summary(as.factor(d_mm$state_zoneid)) 
summary(as.factor(d_mm$epanutr_zoneid)) 
summary(as.factor(d_mm$omernik3_zoneid)) 
summary(as.factor(d_mm$wwf_zoneid)) 
summary(as.factor(d_mm$mlra_zoneid)) 
summary(as.factor(d_mm$bailey_zoneid)) 
summary(as.factor(d_mm$neon_zoneid))
detach(d_mm)               

