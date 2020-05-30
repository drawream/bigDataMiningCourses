
# 大数据分析期中 2020.4.22 段晶晶

# 加载包
# install.packages("ggplot2")
library(ggplot2) 
# install.packages("rgdal")
library(rgdal) # 用于读取地图数据
# install.packages("sp")
library(sp)
install.packages("plyr")
library(plyr) # 用于数据处理

options(stringsAsFactors = FALSE)

# 设置字体
windowsFonts("Times New Roman" = windowsFont("Times New Roman")) 

# 读取文件，设置工作路径
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\qz") 

# install.packages("readOGR")
vData <- readOGR("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\qz\\bou2_4p.shp")

# 数据处理
vTemp <- vData@data;  # S4类数据,用@取子集,得到的为描述层

# 为描述层添加id，方便后续内联数据
vDescribeData <- data.frame(vTemp, id = seq(0:923) - 1) 
vFormatData <- fortify(vData) # 得倒的为几何层数据

# 合并描述层和几何层数据（普通做图这一步并不是必须的，通常只需要几何层就可以
# 做出地图，合并起来更直观一些）
vMapData<-join(vFormatData, vDescribeData, type = "full")

# 读取各省份的发病人数
vNum <- read.csv("num.csv", stringsAsFactors = FALSE)
vProvName <- vNum$vProvName # 省份名称
vNum <- vNum$vNum # 发病人数
vProvCol = vNum # 将发病人数传给颜色，用于表示颜色的深浅

# 颜色函数，\\用处是给每一个描述层生成一个颜色
# （描述层有924条数据）
fGetColor <- function(mapdata, vProvName, vProvCol, othercol){
  f = function(x, y) ifelse(x %in% y, which(y == x), 0)
  colIndex = sapply(mapdata@data$NAME,  f,  vProvName)
  fg = c(othercol, vProvCol)[colIndex + 1]
  return(fg)
}

# 使用自定义的颜色函数，为描述层生成颜色
vCol <- fGetColor(vData, vProvName, vProvCol,0)

# 为描述层颜色添加id
vRealCol <- data.frame(vCol, id = seq(0:923) - 1)

# 通过id将描述层的颜色数据和之前的几何层地图数据内联，简单的说就是将924条
# 颜色数据根据id映射到9万多条几何层数据
vMapData <- join(vMapData, vRealCol, type = "full")

# ggplot2常规做图，通过scale_fill_continuous函数产生渐变色
#install.packages("mapproj")
ggplot(vMapData, 
       aes(x = long, y = lat, group = group, fill = as.numeric(vCol)))+
  geom_polygon() +
  scale_fill_continuous(low = "white", high = "red") +
  geom_path(colour = "black") +
  ggtitle("新型肺炎确诊病例分布示意图") +
  theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 20)) +
  theme(axis.text = element_text(size = 15, 
                                 colour = "black", 
                                 family = "Times New Roman"),
        axis.title =  element_blank()) +
  # specify tick marks
  coord_map(xlim = c(73, 136), ylim = c(5, 54)) +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  scale_x_continuous(breaks = seq(70, 135, 10)) +
  # remove guide
  guides(fill = FALSE)



