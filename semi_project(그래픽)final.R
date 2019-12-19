install.packages("plotly")
install.packages("ggmap")
install.packages("gglplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("tidyverse")
install.packages("sf")
install.packages("viridis")
install.packages("readxl")

library(plotly)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(stringr)
library(tidyverse)
library(sf)
library(viridis)
library(xlsx)
library(readxl)
sessionInfo()

setwd("C:/Users/student/Desktop/semi/semi-project")
getwd()
.libPaths()
.libPaths("C:/R_Lecture/lib")

################################################################################

raw <- "C:/Users/student/Desktop/semi/semi-project/data/CCTV11.csv" #CCTV11 빈도데이터 필첨 


df <- read.csv(file = raw,
               header = T,
               fileEncoding = "UTF-8")
#View(df)

map <- shapefile("C:/Users/student/Desktop/semi/semi-project/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))
korea_map <- fortify(map,region ="SIG_CD")

#서울시만 뽑아내기 
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]

# CCTV DF와 서울시 맵 조인
map_merge <- merge(seoul_map, df, by="id")

# 서울시 관서별CCTV 개수를 그래픽으로 표현 
cameras <- ggplot() + geom_polygon(data = map_merge,
                        aes(x=long, y = lat, group = group, fill=X2018),
                        color = "gold") 
cameras 

#########################################################################

###서울시 구별 치안기구 분포비교 시각화 
raw <- "C:/Users/student/Desktop/semi/semi-project/data/Seoul_Police_Stations(2018).xls"


df <- read_excel(path = raw, 
                 col_names = T)
View(df)

map <- shapefile("C:/Users/student/Desktop/semi/semi-project/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))
korea_map <- fortify(map,region ="SIG_CD")

#서울시만 뽑아내기 
#행정구별 치안시설 인구대비비율을 그림으로 도출((치안시설/구 인구)*100)
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, df, by="id")
View(map_merge)

table(map_merge$치안시설비율)

Patrols <- ggplot() + geom_polygon(data = map_merge,
                                   aes(x=long, y = lat, group = group, 
                                       fill=치안시설비율),
                                   color = "white") +
  scale_fill_gradient(low='white', high='red') +
  scale_fill_viridis(direction=-1) + theme_void()

Patrols

#########################################################################

###인구 10만명당 치안센터 비율로 구하기 
raw <- "C:/Users/student/Desktop/semi/semi-project/data/Seoul_Police_Stations(2018).xls"


df <- read_excel(path = raw, 
                 col_names = T)
#View(df)
#치안시설 비율 순서 
df2 <- arrange(df,
        desc(치안시설비율2))

View(df2)

map <- shapefile("C:/Users/student/Desktop/semi/semi-project/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))

korea_map <- fortify(map,region ="SIG_CD")


#서울시만 뽑아내기 
#행정구별 치안시설 인구대비비율을 그림으로 도출((치안시설/구 인구)*100)
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, df, by="id")
View(map_merge)

patrols2 <- ggplot() + geom_polygon(data = map_merge,
                                   aes(x=long, y = lat, group = group, 
                                       fill=치안시설비율2),
                                   color = "white") +  
  scale_fill_gradient(low='white', high='#004ea2') +
  scale_fill_viridis(direction=-1) + theme_void()

patrols2
############################################################################