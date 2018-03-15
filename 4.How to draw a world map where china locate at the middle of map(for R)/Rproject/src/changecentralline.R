# Name: Central China(for R)
# Purpose: How to draw a world map which china locate at the middle of map
# Author:      Dai shaoqing
#
# Created:     11/14/2017
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2017
#------------------------------------------------------------
#Load packages
library(sp)
library(rgdal)

#Read spatial data
a<-readOGR("../data/World_region.shp")

#Construct Projection Coordinate System
centralchina<-spTransform(a,CRS=CRS("+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=150 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#Plot map
jpeg(filename="../centralchina",width=600,height=600,units="in",res=300)
plot(centralchina)
dev.off()

#Output map
writeOGR(centralchina,"../data/Centralchina.shp",layer="centralchina",driver="ESRI Shapefile")