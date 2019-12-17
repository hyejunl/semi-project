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




## 동에 대한 좌표 지도 구하기 
##읍면동 단위로 나눈 지도 데이터 가져와 


small_map <-shapefile("C:/R_Lecture/data/EMD_201703/TL_SCCO_EMD.shp")
small_map <- spTransform(small_map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGs84 +no_defs"))

small_map <- fortify(small_map,region ="EMD_CD")
View(small_map)




#1. 종로구 
#shp 파일에서 용산구만 뽑아내기(뭐야 개넓어)
jongro_map <- small_map[(11110100 < small_map$id) & (small_map$id <= 11110290),]


#일단 merge안한 지도부터 
ggplot() + geom_polygon(data = jongro_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#2. 중구 
#shp 파일에서 중구만 뽑아내기
junggu_map <- small_map[(11140100 < small_map$id) & (small_map$id <= 11140174),]

#일단 merge안한 지도부터 
ggplot() + geom_polygon(data = junggu_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#3. 용산구
#shp 파일에서 용산구만 뽑아내기
yongsan_map <- small_map[(11170100 < small_map$id) & (small_map$id <= 11170136),]

#일단 merge안한 지도부터 
ggplot() + geom_polygon(data = yongsan_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#4. 성동구
sungdong_map <- small_map[(11200100 < small_map$id) & (small_map$id <= 11200122),]
ggplot() + geom_polygon(data = sungdong_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")



#5.광진구  
gwangjin_map <- small_map[(11215100 < small_map$id) & (small_map$id <= 11215109),]
ggplot() + geom_polygon(data = gwangjin_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#6.동대문구  
dongdaemoon_map <- small_map[(11230100 < small_map$id) & (small_map$id <= 11230110),]
ggplot() + geom_polygon(data = dongdaemoon_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#7.중랑구  
jungrang_map <- small_map[(11260100 < small_map$id) & (small_map$id <= 11260106),]
ggplot() + geom_polygon(data = jungrang_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")



#8.성북구  
sungbuk_map <- small_map[(11290100 < small_map$id) & (small_map$id <= 11305100),]
ggplot() + geom_polygon(data = sungbuk_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#9.강북구  
gangbuk_map <- small_map[(11305100 < small_map$id) & (small_map$id <= 11320100),]
ggplot() + geom_polygon(data = gangbuk_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#10.도봉구  
dobong_map <- small_map[(11320100 < small_map$id) & (small_map$id <= 11350100),]
ggplot() + geom_polygon(data = dobong_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#11.노원구  
nowon_map <- small_map[(11350100 < small_map$id) & (small_map$id <= 11380100),]
ggplot() + geom_polygon(data = nowon_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#12.은평구  
eunpyung_map <- small_map[(11380100 < small_map$id) & (small_map$id <= 11410100),]
ggplot() + geom_polygon(data = eunpyung_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#13.서대문구  
seodaemun_map <- small_map[(11410100 < small_map$id) & (small_map$id <= 11440100),]
ggplot() + geom_polygon(data = seodaemun_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#14.마포구  
mapo_map <- small_map[(11440100 < small_map$id) & (small_map$id <= 11470100),]
ggplot() + geom_polygon(data = mapo_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#15.양천구
yangchun_map <- small_map[(11470100 < small_map$id) & (small_map$id <= 11500100),]
ggplot() + geom_polygon(data = yangchun_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#16.강서구
gangseo_map <- small_map[(11500100 < small_map$id) & (small_map$id <= 11530100),]
ggplot() + geom_polygon(data = gangseo_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#17.구로구
guro_map <- small_map[(11530100 < small_map$id) & (small_map$id <= 11545100),]
ggplot() + geom_polygon(data = guro_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#18.금천구
geumcheon_map <- small_map[(11545100 < small_map$id) & (small_map$id <= 11560100),]
ggplot() + geom_polygon(data = geumcheon_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#19.영등포구
yeongdungpo_map <- small_map[(11560100 < small_map$id) & (small_map$id <= 11590100),]
ggplot() + geom_polygon(data = yeongdungpo_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#20.동작구
dongjak_map <- small_map[(11590100 < small_map$id) & (small_map$id <= 11620100),]
ggplot() + geom_polygon(data = dongjak_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#21.관악구
gwanak_map <- small_map[(11620100 < small_map$id) & (small_map$id <= 11650100),]
ggplot() + geom_polygon(data = gwanak_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#22.서초구
seocho_map <- small_map[(11650100 < small_map$id) & (small_map$id <= 11680100),]
ggplot() + geom_polygon(data = seocho_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#23.강남구
gangnam_map <- small_map[(11680100 < small_map$id) & (small_map$id <= 11710100),]
ggplot() + geom_polygon(data = gangnam_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


#24.송파구
songpa_map <- small_map[(11710100 < small_map$id) & (small_map$id <= 11740100),]
ggplot() + geom_polygon(data = songpa_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

#25. 강동구
gangdong_map <- small_map[(11740100 < small_map$id) & (small_map$id <= 11740110),]
ggplot() + geom_polygon(data = gangdong_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")

###################################################

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
read.csv(file = "C:/Users/student/Desktop/local_reposit/semi_project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") -> female_safety
arrange(female_safety,관할경찰서명)-> female_safety
View(female_safety)
table(female_safety$관할경찰서명)

female_safety_1 <- female_safety %>%
  select("위도","경도","소재지지번주소","관할경찰서명")
View(female_safety_1)



#안전 비상벨 위치 
read.csv(file = "C:/Users/student/Desktop/local_reposit/semi_project/data/서울특별시_용산구_안전비상벨위치_20190725.csv") -> safety_bell
View(safety_bell)
arrange(safety_bell,소재지지번주소) -> safety_bell 
safety_bell_1 <- safety_bell %>% 
  select(위도, 경도)
View(safety_bell_1)

# merge한 지도 (여기서부터는 진행중) 
ggplot() + geom_polygon(data = yongsan_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="gold") +
  geom_point(data=female_safety_1,aes(x=경도,y=위도),size=1,color="red")+
  geom_point(data=safety_bell_1,aes(x=경도, y=위도), size=1,color="green")



##########원효동 
#wonhyo1_map <- small_map[(11170100 < small_map$id) & (small_map$id <= 11170120) & ( 11170123 < small_map$id) & (small_map$id <= 11170136),]

##원효로 1가, 원효로 2가 (12,13)
wonhyo1_1map <- small_map[(11170111 < small_map$id) & (small_map$id <= 11170113) ,]

ggplot() + geom_polygon(data = wonhyo1_1map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="gold")

##문배동, 신계동  (23,22)
wonhyo1_2map <- small_map[(11170123 == small_map$id) | (small_map$id == 11170122) ,]

ggplot() + geom_polygon(data = wonhyo1_2map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="gold")

##원효로 1동 
wonhyo1_map <- small_map[(11170112 == small_map$id) | (small_map$id == 11170113) | (11170123 == small_map$id) | (small_map$id == 11170122) ,]

ggplot() + geom_polygon(data = wonhyo1_map,
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
read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울시_지구대_파출소_치안센터_정보.csv", sep = " ") -> yongsan_patrol

View(yongsan_patrol)
yongsan_patrol1 <- yongsan_patrol %>%
  select(위도, 경도)
View(yongsan_patrol1)
yongsan_patrol1 <- as.data.frame(yongsan_patrol1, stringAsFactor = F)




#용산구내 경찰서 좌표(안심벨 관할 포함) 
female_safety_1 <- female_safety_1 %>%
  mutate(id = ifelse(관할경찰서명 == "보광파출소", "1",
                           ifelse(관할경찰서명 == "용산역파출소","2",
                                        ifelse(관할경찰서명 == "용중지구대","3",
                                                     ifelse(관할경찰서명 == "원효지구대","4",
                                                                  ifelse(관할경찰서명 == "이촌파출소","5",
                                                                               ifelse(관할경찰서명 == "이태원파출소","6",
                                                                                            ifelse(관할경찰서명 == "한강로파출소","7","8"))))))))
View(female_safety_1)



# 용산구 경찰서 좌표
geo_code = geocode(enc2utf8("보광파출소"))
geo_code1 = geocode(enc2utf8("용산역파출소"))
geo_code2 = geocode(enc2utf8("용중지구대"))
geo_code3 = geocode(enc2utf8("원효지구대"))
geo_code4 = geocode(enc2utf8("이촌파출소"))
geo_code5 = geocode(enc2utf8("이태원파출소"))
geo_code6 = geocode(enc2utf8("한강로파출소"))
geo_code7 = geocode(enc2utf8("한남파출소"))

police_loc = rbind(geo_code,
                   geo_code1,
                   geo_code2,
                   geo_code3,
                   geo_code4,
                   geo_code5,
                   geo_code6,
                   geo_code7)
police_loc <- as.data.frame(police_loc)
police_loc <- rename(police_loc,"위도"="lat","경도"="lon") 














# 구글 맵 투명도로 덧씌우기 
#googleAPIkey = "AIzaSyD_kdESG6jCzU3SxGl8FWIxp_MkAYeynRw"
googleAPIkey = "AIzaSyDb8Oqv9AqTVBFWUKyOZh1SkSv_9SeEtKI"

register_google(googleAPIkey)

cen <- c(126.965807,37.537746)  
gg_wonhyo1 <- get_googlemap(center = cen,
                          maptype = "satellite",
                          zoom = 16)



myMap <- ggmap(gg_wonhyo1) + 
  #geom_point(data=yongsan_cctv,aes(x=경도, y=위도), size=0.6,color="yellow") +
  geom_point(data=female_safety_1,aes(x=경도,y=위도),size=0.6,color="red") +
  geom_point(data=safety_bell_1,aes(x=경도, y=위도), size=0.6,color="green") +
  #geom_point(data=yongsan_patrol1,aes(x=경도, y=위도), size=0.6,color="blue") +
  #geom_point(data=police_loc,aes(x=경도,y=위도),size=1,color="blue")+
  geom_polygon(data = wonhyo1_map,
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
  
