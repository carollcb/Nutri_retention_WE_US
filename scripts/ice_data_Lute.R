library(tidyverse)
library(sf)
library(ncdf4)
library(raster)
library(sp)

study_sites <- read.csv("data/Nretention_df.csv")%>%
  distinct(lagoslakeid, .keep_all = TRUE)   %>%
  dplyr::select(lagoslakeid, lake_lon_decdeg,lake_lat_decdeg)%>%
  #st_as_sf(coords=c("lake_lon_decdeg", "lake_lat_decdeg"), crs=4326)
  rename(lon=lake_lon_decdeg, lat = lake_lat_decdeg)

######Lute et al 2022 dataset: The suffix '_CTRL' indicates that the data represents conditions under the control scenario (aka historical climate).
##historical climate data was developed downscaling raw WRF data (Rasmussen & Liu, 2017) consisting of 4 km spatial resolution hourly simulations for 1 October 2000 to 30 September 2013.

###Snow duration --------------------------
#Number of days between the start and end of snow cover, averaged across years. 
#The start of snow cover is defined as the first day of the first period of 5 consecutive days with snow depth greater 
#than 10 mm, and day of snow cover end is defined as the last day of the last period of 5 consecutive days with snow depth greater than 10 mm.
snow_duration <- nc_open("D:/Datasets/Datasets/Lute_dataset/snow_duration_CTRL.nc")

{
  sink('data/snow_duration_CTRL.txt')
  print(snow_duration)
  sink()
}


attributes(snow_duration$dim)

#attributes are lat and lon; I'm going to want both to add to the snow duration matrix

lat <- ncvar_get(snow_duration, "lat")

lon <- ncvar_get(snow_duration, "lon")

snow_dur.array <- ncvar_get(snow_duration, "snow_duration")

#missing data info
fillvalue <- ncatt_get(snow_duration, "snow_duration", "_FillValue")
fillvalue #The fill value is -9999

nc_close(snow_duration) #close netcdf file

snow_dur.array[snow_dur.array == fillvalue$value] <- NA

r1 <- raster(t(snow_dur.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r1 <- flip(r1, direction='y')

points_sp <- SpatialPoints(study_sites[, c("lon", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))

extracted_values <- extract(r1, points_sp)

extracted_df <- data.frame(snowdur = extracted_values)

snow_dur <- cbind(study_sites, extracted_df)%>%
  dplyr::select(lagoslakeid, snowdur)

rm(r1)
rm(snow_dur.array)

###Snow free days -----------------------------------
# Annual number of days with snow depth less than 10 mm between the start and end of snow cover, averaged across years. The start of 
#snow cover is defined as the first day of the first period of 5 consecutive days with snow depth greater than 10 mm, and day of 
#snow cover end is defined as the last day of the last period of 5 consecutive days with snow depth greater than 10 mm.(snow_free_days_CTRL.nc)
snow_free_days <- nc_open("D:/Datasets/Datasets/Lute_dataset/snow_free_days_CTRL.nc")

{
  sink('data/snow_free_days_CTRL.txt')
  print(snow_free_days)
  sink()
}

attributes(snow_free_days$dim) #attributes are lat and lon; I'm going to want both to add to the snow free days matrix

lat <- ncvar_get(snow_free_days, "lat")

lon <- ncvar_get(snow_free_days, "lon")

snow_free_days.array <- ncvar_get(snow_free_days, "snow_free_days")

#missing data info
fillvalue <- ncatt_get(snow_free_days, "snow_free_days", "_FillValue")
fillvalue #The fill value is -9999

nc_close(snow_free_days) #close netcdf file

snow_free_days.array[snow_free_days.array == fillvalue$value] <- NA

r2 <- raster(t(snow_free_days.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r2 <- flip(r2, direction='y')

points_sp <- SpatialPoints(study_sites[, c("lon", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))

extracted_values <- extract(r2, points_sp)

extracted_df <- data.frame(snow_free_days = extracted_values)

snow_free <- cbind(study_sites, extracted_df)%>%
  dplyr::select(lagoslakeid, snow_free_days)

rm(r2)
rm(snow_free_days.array)

###Annual maximum SWE ---------------------
#Annual maximum snow water equivalent (m), averaged across years (annual_maximum_swe_CTRL.nc)
max_swe <- nc_open("D:/Datasets/Datasets/Lute_dataset/annual_maximum_swe_CTRL.nc")

{
  sink('data/swe.txt')
  print(max_swe)
  sink()
}

attributes(max_swe$dim)

#attributes are lat and lon; I'm going to want both to add to the snow max_depth matrix

lat <- ncvar_get(max_swe, "lat")

lon <- ncvar_get(max_swe, "lon")

annual_maximum_swe.array <- ncvar_get(max_swe, "annual_maximum_swe")

#missing data info
fillvalue <- ncatt_get(max_swe, "annual_maximum_swe", "_FillValue")
fillvalue

#The fill value is -9999

nc_close(max_swe) #close netcdf file

annual_maximum_swe.array[annual_maximum_swe.array == fillvalue$value] <- NA

r3 <- raster(t(annual_maximum_swe.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r3 <- flip(r3, direction='y')

points_sp <- SpatialPoints(study_sites[, c("lon", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))

extracted_values <- extract(r3, points_sp)

extracted_df <- data.frame(ann_max_swe = extracted_values)

snow_max_swe <- cbind(study_sites, extracted_df)%>%
  dplyr::select(lagoslakeid, ann_max_swe)

rm(r3)
rm(annual_maximum_swe.array)

##Joining

snow_data <- inner_join(snow_dur, snow_free, by="lagoslakeid")%>%
  inner_join(snow_max_swe, by="lagoslakeid")%>%
  mutate(lagoslakeid = as.character(lagoslakeid))
