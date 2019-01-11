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