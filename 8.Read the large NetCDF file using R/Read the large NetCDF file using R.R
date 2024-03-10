# Name: Read the large NetCDF file(for R)
# Purpose: Read the large NetCDF file using R 
# Author:      Dai shaoqing
#
# Created:     03/10/2024
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2024
#------------------------------------------------------------
#install.packages('sf')
#install.packages('raster')
#install.packages('terra')
#install.packages('ncdf4')
#Load library
library(sf)
library(ncdf4)
library(raster)
library(terra)
library(rasterVis)

#Read shapefile
fjcities <- read_sf('city.shp')
fjcitieswgs <- st_transform(fjcities, crs = '+proj=longlat +datum=WGS84 +no_defs')

#Read netcdf file
nc_pre <- nc_open('pre_2021.nc')

#Display the information of NetCDF
print(nc_pre)

fjcitieswgs

#To obtain the longitude, latitude, time, and name of variable
long <- ncvar_get(nc_pre, 'lon')
lat <- ncvar_get(nc_pre, 'lat')

#Calculate numbers of rows and columns of the specific study area
LonIdx <- which(long[] > 115 & long[] < 121)
LatIdx <- which(lat[] > 23 & lat[] < 29)

pre <- ncvar_get(nc_pre, 'pre', 
                 start = c(LonIdx[1], LatIdx[1],1),
                 count = c(length(LonIdx), length(LatIdx),1))
fillvalue <- ncatt_get(nc_pre, "pre", "missing_value")
pre[pre==fillvalue$value] <- NA

#To generate raster
r <- raster(t(pre), xmn=min(long[LonIdx]), xmx=max(long[LonIdx]), ymn=min(lat[LatIdx]), ymx=max(lat[LatIdx]), 
            crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Display the raster
plot(r)

#Clip the raster using the shapefile
r <- crop(r, fjcitieswgs)
r <- mask(r, fjcitieswgs)

#Display the raster
plot(r)

#Using different r package to plot raster
levelplot(r)

#Use terra pacakge to generate raster
rn <- rast(t(pre), crs= '+proj=longlat +datum=WGS84 +no_defs')

#Display the raster
plot(rn)
