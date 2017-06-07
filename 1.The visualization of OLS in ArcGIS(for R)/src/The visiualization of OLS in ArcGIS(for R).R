# Name: The visualization of OLS in ArcGIS(for R)
# Purpose: The function to visualize results of OLS in ArcGIS
# Author:      Dai shaoqing
#
# Created:     04/25/2017
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2017
#------------------------------------------------------------
#�����
#���û��װ�����Ȱ�װ������Ѱ�װ����ע��
#install.packages(".../arcgisbinding_1.0.0.125.zip", repos = NULL, type = "win.binary")����ʾarcgisbinding���߰���·��
#install.packages("car")
#install.packages("GGally")
#install.packages("ggplot2")
#ArcGIS OLS visualization
library(arcgisbinding)
library(car)
library(GGally)
library(ggplot2)

#���ù���·��
setwd("F:/R/demo/readdata")

#���ArcGIS��Ʒ����
arc.check_product()

#��ȡ����
olsdata<-arc.open("china.gdb/olstest")
olsdata
olsdataframe<-arc.select(olsdata,fields = c("gdp","Index_2000","Pop_Urban","POPU","PRODUCT","Estimated","Residual","StdResid"))

#����������Ա������������������car�����spm������ͼ
variableframe<-olsdataframe[,c(1:5)]
spm(variableframe,diagonal="hist")

#����GGally��ggpairs������ͼ
ggpairs(variableframe,upper = list(continuous="cor"),lower = list(continuous="smooth"),diag = list(continuous="barDiag"))

#���Ʊ�׼�в�ķֲ�����ggplot2��ͼ
a<-ggplot(olsdataframe,aes(x=StdResid))+
  geom_histogram(aes(y=..density..),binwidth = 0.5,colour="white",fill="grey")+
  geom_line(stat='density',colour="#FF6666")
a

#���Ʊ�׼�в�͹۲�ֵ��ɢ��ͼ
#ggplot2��
hist1<-ggplot(olsdataframe,aes(x=StdResid))+
  geom_histogram(aes(y=..density..),binwidth = 0.5,colour="white",fill="grey")

scater<-ggplot(olsdataframe)+geom_point(mapping =aes(x=gdp,y=StdResid),colour="grey",fill="grey")
  
hist2<-ggplot(olsdataframe,aes(x=gdp))+
  geom_histogram(aes(y=..density..),binwidth = 500,colour="white",fill="grey")

source("F:/R/multiplot.R")
multiplot(hist1,scater,hist2,cols=2)

#��ͨ��
opar<-par(no.readonly = T)

par(fig=c(0,0.8,0,0.8))
plot(olsdataframe$gdp,olsdataframe$StdResid,col="grey",pch=16)

par(fig=c(0,0.8,0.7,1),new=T)
hist(olsdataframe$gdp,col="grey")

par(fig=c(0.75,1,0,0.8),new=T)
hist(olsdataframe$StdResid,col="grey")