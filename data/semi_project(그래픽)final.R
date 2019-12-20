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
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
############################################################################

###인구 10만명당 구별 범죄 발생 건수 시각화하기

# 구별 인구 수 -> pop
#population_file = "C:/Users/LHJ/Desktop/안심귀가 project/구별 인구 수.xls"
#population_file = "C:/Users/student/Desktop/semi-project/data/구별 인구 수.xls"
population_file = "C:/Users/student/Desktop/local_reposit/semi-project/data/구별 인구 수_id추가.xls"
raw_population <- read_excel(path = population_file)
pop <- as.data.frame(raw_population)
pop <- rename(pop,gu=...1)
View(pop);str(pop)

# 구별 연도별 범죄 발생 건 수 ->crime
#crime_file = "C:/Users/LHJ/Desktop/안심귀가 project/구별 연도별 범죄 발생 건수.xls"
#crime_file = "C:/Users/student/Desktop/semi-project/data/구별 연도별 범죄 발생 건수.xls"
crime_file = "C:/Users/student/Desktop/local_reposit/semi-project/data/구별 연도별 범죄 발생 건수_id추가.xls"
raw_crime <- read_excel(path = crime_file)
crime <- as.data.frame(raw_crime)
crime <- rename(crime,
                gu=...1,
                c_14=`2014`,
                c_15=`2015`,
                c_16=`2016`,
                c_17=`2017`,
                c_18=`2018`)
View(crime);str(crime)


# 인구 10만당 범죄 발생 건수 -> gu_crime
gu_crime <- left_join(pop,crime)%>%
  select("gu","population","c_18","id")%>%
  mutate(gu_crime=c_18/(population/100000))%>%
  arrange(desc(gu_crime))
str(gu_crime);View(gu_crime)



#그래픽 도출하기 
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, gu_crime, by="id")
View(map_merge)

crime_per10 <- ggplot() + geom_polygon(data = map_merge,
                                    aes(x=long, y = lat, group = group, 
                                        fill=gu_crime),
                                    color = "white") +  
  scale_fill_gradient(low='white', high='#004ea2') +
  scale_fill_viridis(direction=-1) + theme_void()

crime_per10


############################################################################

###CCTV와 범죄률의 상관계수 구별로 시각화하기
#cctv개수 파일 불러오기 
cctv_file ="C:/Users/student/Desktop/local_reposit/semi-project/data/서울시 자치구 년도별 CCTV 설치 현황(2011년 이전_2018년).xlsx"
raw_cctv <- read_excel(path = cctv_file)
cctv <- as.data.frame(raw_cctv)
cctv <- rename(cctv,
               gu=...1,
               cam_14=`2014`,
               cam_15=`2015`,
               cam_16=`2016`,
               cam_17=`2017`,
               cam_18=`2018`)%>%
  select("gu","cam_14","cam_15","cam_16","cam_17","cam_18")
View(cctv);str(cctv)

# 구 이름 통일해주기
for (i in 1:length(cctv$gu)){
  cctv$gu[i]<-str_replace_all(cctv$gu[i]," ","")
}
# 구 이름 순서대로 맞춰주기
cctv<-cctv%>%arrange(gu)
crime<-crime%>%arrange(gu)
View(cctv);View(crime)

# cor를 하기 위해 data 형식 바꿔주기
cctv_f<-as.data.frame(t(cctv),stringsAsFactors = F)
crime_f<-as.data.frame(t(crime),stringsAsFactors = F)

gu = cctv$gu
id = crime$id

cor_v = c()
for (i in 1:length(gu)){
  cor_v[i] <- cor(as.numeric(cctv_f[2:6,i]),as.numeric(crime_f[2:6,i]))
}
cor_v
cctv_cor<-as.data.frame(cbind(gu,cor_v,id), stringsAsFactors = F)
cctv_cor<- cctv_cor%>%
  arrange(cor_v)
View(cctv_cor)
str(cctv_cor)
cctv_cor$cor_v <- as.numeric(cctv_cor$cor_v)

#그래픽 도출하기(상관관계)
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, cctv_cor, by="id")
View(map_merge)

correlation <- ggplot() + geom_polygon(data = map_merge,
                                       aes(x=long, y = lat, group = group, 
                                           fill=cor_v),
                                       color = "white")  +  
  #scale_fill_gradient(low='white', high='#004ea2') +
  scale_fill_viridis(direction=-1) + theme_void()

correlation
=======
############################################################################
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############################################################################
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############################################################################
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############################################################################
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############################################################################
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
