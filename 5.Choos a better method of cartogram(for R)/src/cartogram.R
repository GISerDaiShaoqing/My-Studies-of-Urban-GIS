# Name: Cartogram(for R)
# Purpose: Choose a better method of cartogram
# Author:      Dai shaoqing
#
# Created:     10/08/2018
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2018
#------------------------------------------------------------
#Load packages
library(cartogram)
library(rgeos)
library(maptools)
library(sp)
library(geofacet)
library(ggplot2)

nmgau <- readShapePoly("F:/R/demo/cartogram/mongoliaurbanmigration.shp")
spplot(nmgau, zcol = "ALabor2000")

nmgauca <- cartogram_cont(nmgau, weight = "ALabor2000", 10)
spplot(nmgauca, zcol = "ALabor2000")

writePolyShape(nmgauca, "F:/R/demo/cartogram/Rcartogram.shp")