# Name: The visualization of OLS in ArcGIS(for R)
# Purpose: 
# Author:      Dai shaoqing
#
# Created:     04/25/2017
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2017
#------------------------------------------------------------
#载入包
#如果没安装，请先安装，如果已安装，请注释
#install.packages(".../arcgisbinding_1.0.0.125.zip", repos = NULL, type = "win.binary")…表示arcgisbinding离线包的路径
#install.packages("car")
#install.packages("GGally")
#install.packages("ggplot2")
#ArcGIS OLS visualization
library(arcgisbinding)
library(car)
library(GGally)
library(ggplot2)

#设置工作路径
setwd("F:/R/demo/readdata")

#检查ArcGIS产品许可
arc.check_product()

#读取数据
olsdata<-arc.open("china.gdb/olstest")
olsdata
olsdataframe<-arc.select(olsdata,fields = c("gdp","Index_2000","Pop_Urban","POPU","PRODUCT","Estimated","Residual","StdResid"))

#把因变量和自变量单独分离出来并用car包里的spm函数绘图
variableframe<-olsdataframe[,c(1:5)]
spm(variableframe,diagonal="hist")

#利用GGally的ggpairs函数画图
ggpairs(variableframe,upper = list(continuous="cor"),lower = list(continuous="smooth"),diag = list(continuous="barDiag"))

#绘制标准残差的分布，用ggplot2画图
a<-ggplot(olsdataframe,aes(x=StdResid))+
  geom_histogram(aes(y=..density..),binwidth = 0.5,colour="white",fill="grey")+
  geom_line(stat='density',colour="#FF6666")
a

#绘制标准残差和观测值的散点图
#ggplot2版
hist1<-ggplot(olsdataframe,aes(x=StdResid))+
  geom_histogram(aes(y=..density..),binwidth = 0.5,colour="white",fill="grey")

scater<-ggplot(olsdataframe)+geom_point(mapping =aes(x=gdp,y=StdResid),colour="grey",fill="grey")
  
hist2<-ggplot(olsdataframe,aes(x=gdp))+
  geom_histogram(aes(y=..density..),binwidth = 500,colour="white",fill="grey")

source("F:/R/multiplot.R")
multiplot(hist1,scater,hist2,cols=2)

#普通版
opar<-par(no.readonly = T)

par(fig=c(0,0.8,0,0.8))
plot(olsdataframe$gdp,olsdataframe$StdResid,col="grey",pch=16)

par(fig=c(0,0.8,0.7,1),new=T)
hist(olsdataframe$gdp,col="grey")

par(fig=c(0.75,1,0,0.8),new=T)
hist(olsdataframe$StdResid,col="grey")