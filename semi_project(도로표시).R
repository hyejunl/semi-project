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
install.packages("leaflet")

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
library(leaflet)
sessionInfo()

###업데이트 내용 : 한적한 도로좌표 추가 

yongsan_smpop = "C:/Users/student/Desktop/local_reposit/semi-project/data/yongsan_smpop.xlsx" #raw데이터 불러오기 

smpop_df <- read_excel(path = yongsan_smpop)
smpop_df <- as.data.frame(smpop_df)
View(smpop_df)

#csv형태로 쓰고 다시 불러오기(출력이안되네...)
#write.csv(smpop_df,"C:/Users/student/Desktop/local_reposit/semi-project/data/temp_df.csv" )
smpop <- read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/temp_df.csv")
smpop <- as.data.frame(smpop, stringsAsFactor=F)
class(smpop)
View(smpop)



#smpop_visidf <- seodaemapmap +
#  geom_point(data = smpop_df, aes(x=lon, y = lan))

#mypopulMap <- ggmap(gg_seoul)+
#  geom_point(data = smpop_df, aes(x=lon, y = lan))

#ggplotly(mypopulMap)

#################################################################

###시각화하기 
## 동에 대한 좌표 지도 구하기 
##읍면동 단위로 나눈 지도 데이터 가져와 


small_map <-shapefile("C:/R_Lecture/data/EMD_201703/TL_SCCO_EMD.shp")
small_map <- spTransform(small_map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGs84 +no_defs"))

small_map <- fortify(small_map,region ="EMD_CD")
View(small_map)

##. 용산구에 치안유지장치 찍어보기 
#shp 파일에서 용산구만 뽑아내기
yongsan_map <- small_map[(11170100 < small_map$id) & (small_map$id <= 11170136),]

#일단 merge안한 지도부터 
ggplot() + geom_polygon(data = yongsan_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="gold")


#여성 안전지킴이집 좌표 
read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") -> female_safety
arrange(female_safety,관할경찰서명)-> female_safety
View(female_safety)
table(female_safety$관할경찰서명)

female_safety_1 <- female_safety %>%
  select("위도","경도","소재지지번주소","관할경찰서명")
View(female_safety_1)


#CCTV 위치(*원본이 위도 경도만 추려짐)
read.csv(file="C:/Users/student/Desktop/local_reposit/semi-project/data/서울특별시_용산구_CCTV위치.csv") -> yongsan_cctv
View(yongsan_cctv)


#안전 비상벨 위치 
read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv") -> safety_bell
View(safety_bell)
arrange(safety_bell,소재지지번주소) -> safety_bell 
safety_bell_1 <- safety_bell %>% 
  select(위도, 경도)
View(safety_bell_1)


#경찰서, 파출소, 지구대위치(좌표로) 
#read.xlsx2(file = "C:/Users/student/Desktop/local_reposit/semi_project/data/서울시 지구대 파출소 치안센터 정보.xlsx", sheetIndex=2) -> yongsan_patrol
read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울시 지구대 파출소 치안센터 정보_용산.csv", sep = ",") -> yongsan_patrol

View(yongsan_patrol)
class(yongsan_patrol)
yongsan_patrol<- as.data.frame(yongsan_patrol1, stringAsFactor = F)



# 구글 맵 투명도로 덧씌우기 
#googleAPIkey = "AIzaSyD_kdESG6jCzU3SxGl8FWIxp_MkAYeynRw"
googleAPIkey = "AIzaSyDb8Oqv9AqTVBFWUKyOZh1SkSv_9SeEtKI"

register_google(googleAPIkey)

cen <- c(126.981825,37.529563)
gg_seoul <- get_googlemap(center = cen,
                          maptype = "roadmap",
                          zoom = 13)



myMap <- ggmap(gg_seoul) + stat_density_2d(data=female_safety_1, aes(x=경도, y=위도)) +
  #geom_point(data=yongsan_cctv,aes(x=경도, y=위도), size=0.6,color="yellow") +
  geom_point(data=female_safety_1,aes(x=경도,y=위도),size=0.6,color="red") +
  geom_point(data=safety_bell_1,aes(x=경도, y=위도), size=0.6,color="green") +
  #geom_point(data=yongsan_patrol1,aes(x=경도, y=위도), size=0.6,color="blue") +
  geom_point(data=police_loc,aes(x=경도,y=위도),size=1,color="blue")+
  geom_polygon(data = yongsan_map,
               aes(x=long,
                   y=lat,
                   group=group,
                   colour=phyla, 
                   alpha = 0.0),
               color="black",
               col = adjustcolor("gray",alpha = 0.5),
               fill=NA)


#geom_polygon(data = yongsan_map,
#             aes(x=long,
#                 y=lat,
#                 group=group),
#             fill = NA,
#             color= "black",
#             size = 0.5) +

ggplotly(myMap)

#help(geom_polygon)
# 구글 맵 투명도로 덧씌우기 
#googleAPIkey = "AIzaSyD_kdESG6jCzU3SxGl8FWIxp_MkAYeynRw"
googleAPIkey = "AIzaSyDb8Oqv9AqTVBFWUKyOZh1SkSv_9SeEtKI"

register_google(googleAPIkey)

cen <- c(126.981825,37.529563)
gg_seoul <- get_googlemap(center = cen,
                          maptype = "roadmap",
                          zoom = 13)



myMap <- ggmap(gg_seoul) +
  #geom_point(data=yongsan_cctv,aes(x=경도, y=위도), size=0.6,color="yellow") +
  geom_point(data=female_safety_1,aes(x=경도,y=위도),size=0.6,color="red") +
  geom_point(data=safety_bell_1,aes(x=경도, y=위도), size=0.6,color="green") +
  geom_point(data=yongsan_patrol,aes(x=경도, y=위도), size=0.6,color="blue") +
  geom_point(data=smpop,aes(x=lon, y=lan),size=1,color="purple")+ #길 추가 
  geom_line(data=smpop,aes(x=lon, y=lan, group=id),color="purple")
  geom_polygon(data = yongsan_map,
               aes(x=long,
                   y=lat,
                   group=group,
                   colour=phyla, 
                   alpha = 0.0),
               color="black",
               col = adjustcolor("gray",alpha = 0.5),
               fill=NA)


#geom_polygon(data = yongsan_map,
#             aes(x=long,
#                 y=lat,
#                 group=group),
#             fill = NA,
#             color= "black",
#             size = 0.5) +

ggplotly(myMap)

#help(geom_polygon)
str(smpop)

