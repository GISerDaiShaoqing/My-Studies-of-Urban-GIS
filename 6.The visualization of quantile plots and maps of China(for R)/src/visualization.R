# load packages
library(rgdal)
library(classInt)
library(sp)

#read spatial data
china <- readOGR("D:/Documents/GitHub/My-Studies-of-Urban-GIS/6.The visualization of quantile plots and maps of China(for R)/data/shp/china_simple.shp")
jdx <- readOGR("D:/Documents/GitHub/My-Studies-of-Urban-GIS/6.The visualization of quantile plots and maps of China(for R)/data/shp/南中国海.shp")
whsr <- read.csv("D:/Documents/GitHub/My-Studies-of-Urban-GIS/6.The visualization of quantile plots and maps of China(for R)/data/旅游收入un.csv")

#merge table and spatial data
chinau <- merge(china, whsr, by.x = "Code", by.y = "code")

#quantile range calculation
##use ClassInt
q5 <- classIntervals(chinau$y2008, 5, style = 'quantile')

##self calculation
q25 <- quantile(whsr$y2008, probs = c(0.25))
q50 <- quantile(whsr$y2008, probs = c(0.5))
q75 <- quantile(whsr$y2008, probs = c(0.75))
iqr <- q75 - q25
u <- q75 + 1.5 * iqr
d <- q25 - 1.5 * iqr
u <- ifelse(u > max(whsr$y2008), max(whsr$y2008), u)
d <- ifelse(d < min(whsr$y2008), min(whsr$y2008), d)
box <- c(d, q25, q50, q75, u)

#classification based on quantile
chinau$plot <- ifelse(chinau$y2008 < box[1], 1, 
                      ifelse(chinau$y2008 < box[2], 2, 
                             ifelse(chinau$y2008 < box[3], 3, 
                                    ifelse(chinau$y2008 < box[4], 4, 
                                           ifelse(chinau$y2008 < box[5], 5, 6)))))

#set color
colspic <- colorRampPalette(c("#f38181", "#fce38a", "#eaffd0", "#95e1d3"))(length(box)+1)

chinau$col <- ifelse(chinau$plot == 1, colspic[6], 
                     ifelse(chinau$plot == 2, colspic[5], 
                            ifelse(chinau$plot == 3, colspic[4], 
                                   ifelse(chinau$plot == 4, colspic[3], 
                                          ifelse(chinau$plot == 5, colspic[2], colspic[1])))))

chinau$boxu <- 1


#spplot map
spplot(chinau, zcol = "plot", scales = list(draw = T), 
       xlim = c(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])), max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))),
       ylim = c(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])), max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))), 
       main = "2008年中国各省国际旅游外汇收入分位数专题图\n数据来源:国家统计局", col.regions = colspic,
       at = seq(1, 7, 1), 
       legendEntries = c("缺失数据", "低异常值", "< 25%", "25-50%", "50-75%", "> 75%", "高异常值"))


#spplot map using ClassInt
colspic <- colorRampPalette(c("#f38181", "#fce38a", "#eaffd0", "#95e1d3"))(length(q5$brks)+1)
spplot(chinau, zcol = "y2008", scales = list(draw = T), 
       main = "2008年中国各省国际旅游外汇收入分位数专题图\n数据来源:国家统计局", col.regions = colspic,
       at = q5$brks)



#plot map using 
par(fig = c(0.3, 1, 0, 1))
plot(jdx, 
     xlim = c(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])), max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))),
     ylim = c(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])), max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))), 
     main = '2008年中国各省国际旅游外汇收入分位数专题图\n数据来源:国家统计局',
     axes = F)
axis(1, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     tck = 1, col = 'grey')
axis(2, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5), 
     tck = 1, col = 'grey')
axis(1, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(2, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5), 
     label = F, tck = 0)
axis(3, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(4, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5),
     label = F, tck = 0)
plot(chinau, col = chinau$col, add = T)
polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))), 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+3, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+3, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+2.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))), 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))), 7)), 
        border = 'black',
        col = 'white')
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 3)/2, "无数据")

polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+3, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+6, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+5.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+3, 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+3, 7)), 
        border = 'black',
        col = colspic[6])
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 3 + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 6)/2, "低异常值")

polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+6, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+9, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+9, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+8.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+6, 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+6, 7)), 
        border = 'black',
        col = colspic[5])
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 6 + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 9)/2, "< 25%")

polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+9, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+12, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+12, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+11.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+9, 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+9, 7)), 
        border = 'black',
        col = colspic[4])
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 9 + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 12)/2, "25-50%")

polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+12, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+15, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+15, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+14.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+12, 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+12, 7)), 
        border = 'black',
        col = colspic[3])
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 12 + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 15)/2, "50-75%")

polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+15, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+18, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+18, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+17.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+15, 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+15, 7)), 
        border = 'black',
        col = colspic[2])
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 15 + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 18)/2, "> 75%")

polygon(x = c(rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 7), 
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 
                  1),
              rep(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+6, 7),
              seq(round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+5, 
                  round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))), 
                  -1)),
        y = c(seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+18, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+21, 
                  0.5), 
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+21, 7),
              seq(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+20.5, 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+18, 
                  -0.5),
              rep(round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])))+18, 7)), 
        border = 'black',
        col = colspic[1])
text(x = round(1.1*min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])))+10, 
     y = (round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 18 + 
                  round(1.1*min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))) + 21)/2, "高异常值")

par(fig = c(0, 0.3, 0, 1), new = T)
boxplot(y2008 ~ boxu, data = as.data.frame(chinau))
points(factor(chinau$boxu), chinau$y2008, col = chinau$col, pch = 16)

#par(fig = c(0.7, 1, 0, 0.5), new = T)
par(fig = c(0.8, 1, 0, 0.6), new = T)
plot(jdx,
     axe = F)
axis(1, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(2, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5), 
     label = F, tck = 0)
axis(3, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(4, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5),
     label = F, tck = 0)
plot(chinau, col = chinau$col, add = T)

####---------------------------------------------
##use legend
par(fig = c(0.3, 1, 0, 1))
plot(jdx, 
     xlim = c(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])), max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))),
     ylim = c(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])), max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))), 
     main = '2008年中国各省国际旅游外汇收入分位数专题图\n数据来源:国家统计局',
     axes = F)
axis(1, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     tck = 1, col = 'grey')
axis(2, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5), 
     tck = 1, col = 'grey')
axis(1, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(2, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5), 
     label = F, tck = 0)
axis(3, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(4, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5),
     label = F, tck = 0)
plot(chinau, col = chinau$col, add = T)
legend("bottomleft", title = "外汇收入", 
       c("高异常值", "> 75%", "50-75%", "25-50%", "< 25%", "低异常值", "无数据"),
       lty = 1.2, border = "black", pch = 15, col = c(colspic, "white"))

par(fig = c(0, 0.3, 0, 1), new = T)
boxplot(y2008 ~ boxu, data = as.data.frame(chinau))
points(factor(chinau$boxu), chinau$y2008, col = chinau$col, pch = 16)

#par(fig = c(0.7, 1, 0, 0.5), new = T)
par(fig = c(0.8, 1, 0, 0.6), new = T)
plot(jdx,
     axe = F)
axis(1, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(2, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5), 
     label = F, tck = 0)
axis(3, at = seq(round(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1]))-10), 
                 round(max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))+10),
                 10), 
     label = F, tck = 0)
axis(4, at = seq(round(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1]))-5), 
                 round(max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))+5),
                 5),
     label = F, tck = 0)
plot(chinau, col = chinau$col, add = T)

### spplot
borderpic <- list("sp.polygons", jdx)

piclayout <- list(borderpic)

spplot(chinau, zcol = "plot", scales = list(draw = T), 
       xlim = c(min(c(bbox(chinau)[1,1], bbox(jdx)[1,1])), max(c(bbox(chinau)[1,2], bbox(jdx)[1,2]))),
       ylim = c(min(c(bbox(chinau)[2,1], bbox(jdx)[2,1])), max(c(bbox(chinau)[2,2], bbox(jdx)[2,2]))), 
       main = "2008年中国各省国际旅游外汇收入分位数专题图\n数据来源:国家统计局", col.regions = colspic,
       at = seq(1, 7, 1), sp.layout = piclayout,
       legendEntries = c("缺失数据", "低异常值", "< 25%", "25-50%", "50-75%", "> 75%", "高异常值"))
