# Name: The visualization of voronoi treemap(for R)
# Purpose: Visualize the COVID-19 confirmed cases by using the voronoi treemap 
# Author:      Dai shaoqing
#
# Created:     03/31/2020
# Copyright:   (c) Dai shaoqing <dsq1993qingge@163.com> 2020
#------------------------------------------------------------
#install.packages('voronoiTreemap')
#Load library
library(voronoiTreemap)

#Explore the shiny app of voronoiTreemap
vt_app()

#Read the data of JHU(https://github.com/CSSEGISandData/COVID-19)
covid19cn <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv")

#Data preprocessing
covid19cn <- subset(covid19cn, covid19cn$Country_Region == "China" & covid19cn$Province_State != "Macau" 
                    & covid19cn$Province_State != "Hong Kong")

#Choose the 30 provinces of the mainland China except Hubei
covid19cn <- subset(covid19cn, covid19cn$Country_Region == "China" & covid19cn$Province_State != "Macau" 
                    & covid19cn$Province_State != "Hong Kong" & covid19cn$Province_State != "Hubei")


#Classify the provinces into three regions
covid19cn$Zone <- ifelse(covid19cn$Province_State == "Beijing" | 
                                     covid19cn$Province_State == "Tianjin" |
                                     covid19cn$Province_State == "Hebei" |
                                     covid19cn$Province_State == "Liaoning" |
                                     covid19cn$Province_State == "Shanghai" |
                                     covid19cn$Province_State == "Jiangsu" |
                                     covid19cn$Province_State == "Zhejiang" |
                                     covid19cn$Province_State == "Fujian" |
                                     covid19cn$Province_State == "Shandong" |
                                     covid19cn$Province_State == "Guangdong" |
                                     covid19cn$Province_State == "Hainan", "Eastern", 
                                   ifelse(covid19cn$Province_State == "Heilongjiang" | 
                                            covid19cn$Province_State == "Jilin" |
                                            covid19cn$Province_State == "Shanxi" |
                                            covid19cn$Province_State == "Anhui" |
                                            covid19cn$Province_State == "Jiangxi" |
                                            covid19cn$Province_State == "Henan" |
                                            covid19cn$Province_State == "Hubei" |
                                            covid19cn$Province_State == "Hunan", "Central", "Western"))


#Set the colors of the different regions
covid19cn$color <- ifelse(covid19cn$Zone == "Eastern", "#eaffd0", 
                          ifelse(covid19cn$Zone == "Central", "#fce38a", "#f38181"))

#Calculate the weight of different provinces
covid19cn$wc <- covid19cn$Confirmed/sum(covid19cn$Confirmed)*100

#Prepare the specifit data frame for voronoi treemap 
covid19vtmu <- covid19cn
covid19vtmp <- data.frame(h1 = "Mainland China", h2 = covid19vtmu$Zone, 
                          h3 = covid19vtmu$Province_State, color = covid19vtmu$color, 
                          weight = covid19vtmu$wc, codes = covid19vtmu$Province_State)

#The function of conversion between character variables and non-character variables
convertchac <- function(x) {
  x <- as.character(x)
  return(x)
}

#Apply the function
covid19vtmp[, c(1:4, 6)] <- apply(covid19vtmp[, c(1:4, 6)], 2, FUN = convertchac)

#Transform the data frame into the json
covid19cnjson <- vt_export_json(vt_input_from_df(covid19vtmp))
vt_d3(covid19cnjson)

