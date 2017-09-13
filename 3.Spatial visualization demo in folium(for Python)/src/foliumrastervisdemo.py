# -*- coding: utf-8 -*-
"""
# Name: Visualization of raster in folium 
# Author:      Dai shaoqing
#
# Created:     09/13/2017
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2017
#------------------------------------------------------------
"""
#Load library
import gdal
import folium
from folium import plugins

#Open raster file
driver=gdal.GetDriverByName('GTiff')
driver.Register() 
ds = gdal.Open('..../wd.tif') 
if ds is None:
    print('Could not open')

#Get coordinates, cols and rows
geotransform = ds.GetGeoTransform()
cols = ds.RasterXSize 
rows = ds.RasterYSize 

#Get extent
xmin=geotransform[0]
ymax=geotransform[3]
xmax=xmin+cols*geotransform[1]
ymin=ymax+rows*geotransform[5]

#Get Central point
centerx=(xmin+xmax)/2
centery=(ymin+ymax)/2

#Raster convert to array in numpy
bands = ds.RasterCount
band=ds.GetRasterBand(1)
dataset= band.ReadAsArray(0,0,cols,rows)
dataimage=dataset
dataimage[dataimage[:,:]==-340282346638528859811704183484516925440.000]=0


#Visualization in folium
map= folium.Map(location=[centery, centerx], zoom_start=7,tiles='Stamen Terrain')
plugins.ImageOverlay(
    image=dataimage,
    bounds=[[ymin, xmin], [ymax, xmax]],
    colormap=lambda x: (1, 0, x, x),#R,G,B,alpha
).add_to(map)

#Save html
map.save('wd.html')