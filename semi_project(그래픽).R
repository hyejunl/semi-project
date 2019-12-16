install.packages("ggmap")
install.packages("gglplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("tidyverse")
install.packages("sf")
install.packages("viridis")
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
sessionInfo()




raw <- "C:/R_Lecture/data/SIG_201905/CCTV11.csv"


df <- read.csv(file = raw,
               header = T,
               fileEncoding = "UTF-8")
View(df)

map <- shapefile("C:/R_Lecture/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))
korea_map <- fortify(map,region ="SIG_CD")

class(korea_map)
View(korea_map)


#서울시만 뽑아내기 
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, df, by="id")
View(map_merge)

cameras <- ggplot() + geom_polygon(data = map_merge,
                        aes(x=long, y = lat, group = group, fill=X2018),
                        color = "gold") 
cameras



#########################################################################

install.packages("readxl")
library(readxl)

raw <- "C:/R_Lecture/data/Seoul_Police_Stations(2018).xls"


df <- read_excel(path = raw, 
                 col_names = T)
View(df)

map <- shapefile("C:/R_Lecture/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))
korea_map <- fortify(map,region ="SIG_CD")

class(korea_map)
View(korea_map)


#서울시만 뽑아내기 
#행정구별 치안시설 인구대비비율을 그림으로 도출((치안시설/구 인구)*100)
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, df, by="id")
View(map_merge)

cameras <- ggplot() + geom_polygon(data = map_merge,
                                   aes(x=long, y = lat, group = group, 
                                       fill=치안시설비율),
                                   color = "white") 
cameras + scale_fill_gradient(low='white', high='#004ea2') +
  scale_fill_viridis(direction=-1) + theme_void()



#########################################################################

#인구 10만명당 치안센터 비율로 구하기 
install.packages("readxl")
library(readxl)

raw <- "C:/R_Lecture/data/Seoul_Police_Stations(2018).xls"


df <- read_excel(path = raw, 
                 col_names = T)
#View(df)
#치안시설 비율 순서 
df2 <- arrange(df,
        desc(치안시설비율2))

View(df2)

map <- shapefile("C:/R_Lecture/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))

korea_map <- fortify(map,region ="SIG_CD")

class(korea_map)
View(korea_map)


#서울시만 뽑아내기 
#행정구별 치안시설 인구대비비율을 그림으로 도출((치안시설/구 인구)*100)
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
View(seoul_map)
map_merge <- merge(seoul_map, df, by="id")
View(map_merge)

cameras <- ggplot() + geom_polygon(data = map_merge,
                                   aes(x=long, y = lat, group = group, 
                                       fill=치안시설비율2),
                                   color = "white") 
cameras + scale_fill_gradient(low='white', high='#004ea2') +
  scale_fill_viridis(direction=-1) + theme_void()

############################################################################

## 읍에 대한 좌표 지도 구하기 
##읍면동 단위로 나눈 지도 데이터 가져와 


small_map <-shapefile("C:/R_Lecture/data/EMD_201703/TL_SCCO_EMD.shp")
small_map <- spTransform(small_map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGs84 +no_defs"))

small_map <- fortify(small_map,region ="EMD_CD")
View(small_map)

# 서울시 치안시설 통계 데이터(읍단위로 나눠져 있진 않지만 그냥 해보자)
# 걍 때려치고 출력안되는 이유를 찾기위해 CCTV 분포로 가자 
#raw <- "C:/R_Lecture/data/SIG_201905/CCTV11.csv"
#df <- read_excel(path = raw, 
#                 col_names = T)

df <- read.csv(file = raw,
               header = T,
               fileEncoding = "UTF-8")

df <- as.data.frame(df)
View(df)

#25. 강동구
#shp 파일에서 용산구만 뽑아내기(근데왜 강동구가 나오냐 빡치네)
small_map$id <- as.numeric(small_map$id)
yongsan_map <- small_map[(11740100 < small_map$id) & (small_map$id <= 11740110),]
View(yongsan_map) 
View(small_map)
str(yongsan_map)
yongsan_merge <- merge(yongsan_map, df, by="id")
View(yongsan_merge)


#일단 merge안한 지도부터 
ggplot() + geom_polygon(data = yongsan_map,
                        aes(x=long,
                            y=lat,
                            group=group),
                        color="black")


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
#shp 파일에서 강동구만 뽑아내기
small_map$id <- as.numeric(small_map$id)
gangdong_map <- small_map[(11740100 < small_map$id) & (small_map$id <= 11740110),]
View(gangdong_map) 
View(small_map)
str(gangdong_map)
gangdong_merge <- merge(gangdong_map, df, by="id")
View(yongsan_merge)
#####일단 여기까지 


# merge한 지도 (여기서부터는 진행중) 
yongsan_merge <- ggplot() + geom_polygon(data = yongsan_merge,
                                         aes(x=long, 
                                             y=lat,
                                             group = group, 
                                             fill =X2018),
                                         color = "black")

yongsan_merge

