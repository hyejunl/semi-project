install.packages("readxl")
install.packages("xlsx")
install.packages("dplyr")
install.packages("stringr")
# map 만들때 필요한 packages
install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("viridis")
install.packages("ggplot2")
install.packages("devtools")
install.packages("plotly")
install.packages("dygraphs")

library(readxl)
library(xlsx)
library(dplyr)
library(stringr)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(viridis)
library(ggplot2)
library(devtools)
library(plotly)
library(dygraphs)
############################### 파일 불러오기

# 구별 인구 수 -> pop
#population_file = "C:/Users/LHJ/Desktop/안심귀가 project/구별 인구 수.xls"
population_file = "C:/Users/student/Desktop/local_reposit/semi_project/data/구별 인구 수.xls"
raw_population <- read_excel(path = population_file)
pop <- as.data.frame(raw_population)
pop <- rename(pop,gu=...1)
View(pop);str(pop)

# 구별 연도별 범죄 발생 건 수 ->crime
#crime_file = "C:/Users/LHJ/Desktop/안심귀가 project/구별 연도별 범죄 발생 건수.xls"
crime_file = "C:/Users/student/Desktop/local_reposit/semi_project/data/구별 연도별 범죄 발생 건수.xls"
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

# 구별 경찰서 개소 수 -> police
#police_file = "C:/Users/LHJ/Desktop/안심귀가 project/서울시 구별 경찰관서 개소 수.xls"
police_file = "C:/Users/student/Desktop/local_reposit/semi_project/data/서울시 구별 경찰관서 개소 수.xls"
raw_police <- read_excel(path = police_file)
police <- as.data.frame(raw_police)
police <- rename(police,
                 gu=...1,
                 p_station=경찰서,
                 p_substation=지구대파출소치안센터)
View(police);str(police)

# 구별 연도별 cctv 개수 -> cctv
#cctv_file = "C:/Users/LHJ/Desktop/안심귀가 project/서울시 자치구 년도별 CCTV 설치 현황(2011년 이전_2018년).xlsx"
cctv_file ="C:/Users/student/Desktop/local_reposit/semi_project/data/서울시 자치구 년도별 CCTV 설치 현황(2011년 이전_2018년).xlsx"
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



#######################1. 10만명 당 범죄 건수가 많은 구(pop,crime 사용)->gu_crime
gu_crime <- left_join(pop,crime)%>%
  select("gu","population","c_18")%>%
  mutate(gu_crime=c_18/(population/100000))%>%
  arrange(desc(gu_crime))
str(gu_crime);View(gu_crime)


#######################2. CCTV로도 범죄가 줄어들지 않는 구(crime,cctv사용)->cctv_cor
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
cor_v = c()
for (i in 1:length(gu)){
  cor_v[i] <- cor(as.numeric(cctv_f[2:6,i]),as.numeric(crime_f[2:6,i]))
}
cor_v
cctv_cor<-as.data.frame(cbind(gu,cor_v))
cctv_cor<- cctv_cor%>%
  arrange(cor_v)



#######################3. 10만명 당 지구대, 경찰서 수가 적은 구(pop,police 사용)->gu_police
gu_police <- left_join(pop,police)%>%
  mutate(gu_police=(p_station+p_substation)/(population/100000))%>%
  arrange(gu_police)
View(gu_police)

###########################################gu_crime 시각화
???


####################### 어떤 지역 선택??
head(gu_crime)
gu_police
head(cctv_cor)
write.csv(gu_crime, "C:/Users/student/Desktop/local_reposit/semi_project/data/gu_crime.csv")


gu_mapid = read.csv(file.choose(), stringsAsFactors = T )
gu_mapid = as.data.frame(gu_mapid)
gu_mapid <- rename(gu_mapid,
                   gu = "시군구명") 
gu_mapid
View(gu_crime)
crimemap_df <- left_join(gu_mapid,gu_crime, by="gu", stringsAsFactors = T)
class(crimemap_df)
View(crimemap_df)
# cctv와 범죄건수의 상관계수가 -0.5이상이며,
# 구별 범죄건수 4위
# ??? 10만명 당 지구대, 경찰서 수가 21위인데 어떻게 연관지을지... 

########################서울시 행정구별 범죄률 지도를 작성해보자 
map <- shapefile("C:/R_Lecture/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))
korea_map <- fortify(map,region ="SIG_CD")

class(korea_map)
View(korea_map)


#서울시만 뽑아내기 
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, crimemap_df, by="id")
View(map_merge)

crime_map <- ggplot() + geom_polygon(data = map_merge,
                                   aes(x=long, y = lat, group = group, fill=gu_crime),
                                   color = "gold") 
crime_map


