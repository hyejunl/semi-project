##################################################################################
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
install.packages("leaflet")
install.packages("tidyverse")
install.packages("sf")


library(rgdal)
library(sf)
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
library(leaflet)
################### 구별 인구수, 범죄발생건수, 경찰서 수,########################

# # 구별 인구 수 -> pop
#population_file = "C:/Users/LHJ/Desktop/안심귀가 project/구별 인구 수.xls"
population_file = "C:/Users/student/Desktop/semi/semi-project/data/구별 인구 수.xls"
raw_population <- read_excel(path = population_file)
pop <- as.data.frame(raw_population)
pop <- rename(pop,gu=...1)
#View(pop);str(pop)

# 구별 연도별 범죄 발생 건 수 ->crime
#crime_file = "C:/Users/LHJ/Desktop/안심귀가 project/구별 연도별 범죄 발생 건수.xls"
crime_file = "C:/Users/student/Desktop/semi/semi-project/data/구별 연도별 범죄 발생 건수.xls"
raw_crime <- read_excel(path = crime_file)
crime <- as.data.frame(raw_crime)
crime <- rename(crime,
                gu=...1,
                c_14=`2014`,
                c_15=`2015`,
                c_16=`2016`,
                c_17=`2017`,
                c_18=`2018`)
#View(crime);str(crime)

# 구별 경찰서 개소 수 -> police
#police_file = "C:/Users/LHJ/Desktop/안심귀가 project/서울시 구별 경찰관서 개소 수.xls"
police_file = "C:/Users/student/Desktop/semi/semi-project/data/서울시 구별 경찰관서 개소 수.xls"
raw_police <- read_excel(path = police_file)
police <- as.data.frame(raw_police)
police <- rename(police,
                 gu=...1,
                 p_station=경찰서,
                 p_substation=지구대파출소치안센터)
#View(police);str(police)

################### 서울시 구별 치안기구 분포비교 시각화 ######################

raw <- "C:/Users/student/Desktop/semi/semi-project/data/Seoul_Police_Stations(2018).xls"


df <- read_excel(path = raw, 
                 col_names = T)
#View(df)

map <- shapefile("C:/Users/student/Desktop/semi/semi-project/data/SIG_201703/TL_SCCO_SIG.shp") #좌표계 설정 

map <- spTransform(map,CRSobj = CRS("+proj=longlat +ellps=WGS84 +datumWGS84 +no_defs"))
korea_map <- fortify(map,region ="SIG_CD")

#서울시만 뽑아내기 
#행정구별 치안시설 인구대비비율을 그림으로 도출((치안시설/구 인구)*100)
korea_map$id <- as.numeric(korea_map$id)
seoul_map <- korea_map[korea_map$id <= 11740,]
#View(seoul_map)
map_merge <- merge(seoul_map, df, by="id")
#View(map_merge)

table(map_merge$치안시설비율)

Patrols <- ggplot() + geom_polygon(data = map_merge,
                                   aes(x=long, y = lat, group = group, 
                                       fill=치안시설비율),
                                   color = "white") +
    scale_fill_gradient(low='white', high='red') +
    scale_fill_viridis(direction=-1) + theme_void()

Patrols
############################### 구별 CCTV 설치 추세 #############################

# 구별 연도별 cctv 개수 -> cctv
#cctv_file = "C:/Users/LHJ/Desktop/안심귀가 project/서울시 자치구 년도별 CCTV 설치 현황(2011년 이전_2018년).xlsx"
cctv_file ="C:/Users/student/Desktop/semi/semi-project/data/서울시 자치구 년도별 CCTV 설치 현황(2011년 이전_2018년).xlsx"
raw_cctv <- read_excel(path = cctv_file)
cctv <- as.data.frame(raw_cctv)

cctv <- rename(cctv,
               cam_14=`2014`,
               cam_15=`2015`,
               cam_16=`2016`,
               cam_17=`2017`,
               cam_18=`2018`)

cctv <- cctv %>% select(gu,cam_14,cam_15,cam_16,cam_17,cam_18)
#View(cctv);str(cctv)



#######################1. 10만명 당 범죄 건수가 많은 구(pop,crime 사용)->gu_crime
gu_crime <- left_join(pop,crime)%>%
    select("gu","population","c_18")%>%
    mutate(crime=c_18/(population/100000))%>%
    arrange(desc(crime))
str(gu_crime)
#View(gu_crime)



#######################2. CCTV로도 범죄가 줄어들지 않는 구(crime,cctv사용)->cctv_cor
# 구 이름 통일해주기
for (i in 1:length(cctv$gu)){
    cctv$gu[i]<-str_replace_all(cctv$gu[i]," ","")
}
# 구 이름 순서대로 맞춰주기
cctv<-cctv%>%arrange(gu)
crime<-crime%>%arrange(gu)
#View(cctv);View(crime)

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
cctv_cor

#######################3. 10만명 당 지구대, 경찰서 수가 적은 구(pop,police 사용)->gu_police
gu_police <- left_join(pop,police)%>%
    mutate(gu_police=(p_station+p_substation)/(population/100000))%>%
    arrange(gu_police)
#View(gu_police)

######################### 서울시 관서별  CCTV 설치 정도 그래픽 ##################
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

###################### 동별 안심벨 안전지킴이집 개수, ##########################

########################## 안심벨 동별 개수 
#안전 비상벨 위치 
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
#safety_bell <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
safety_bell <-read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
=======
safety_bell <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
safety_bell <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
safety_bell <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
safety_bell <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
safety_bell <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_안전비상벨위치_20190725.csv")
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
#View(safety_bell)
safety_bell_r <- select(safety_bell,"소재지지번주소")

dong_v = c()
for (i in 1:nrow(safety_bell_r)){
    a =as.character(safety_bell_r[i,])
    dong_v[i]<-str_split(a,' ')[[1]][3]
}
dong_df <- as.data.frame(table(dong_v))
dong_df <-dong_df%>%
    arrange(Freq)
dong_df <-subset(dong_df,str_length(dong_v)<7)

########## 안전지킴이집 동별 개수
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
#female_safety <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv")
female_safety <-read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv")
=======
female_safety <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
female_safety <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
female_safety <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
female_safety <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
female_safety <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
#View(female_safety)
female_safety_r <- select(female_safety,"소재지지번주소")
fdong_v = c()
for (i in 1:nrow(female_safety_r)){
    a =as.character(female_safety_r[i,])
    fdong_v[i]<-str_split(a,' ')[[1]][3]
}
fdong_df <- as.data.frame(table(fdong_v))
fdong_df <-fdong_df%>%
    arrange(Freq)
fdong_df <-subset(fdong_df,str_length(fdong_v)<7)


########### 안심벨 동별 갯수로변환
bell_dong_df<-dong_df%>%
    mutate(dong = ifelse(dong_v=="후암동","후암동",
                         ifelse(dong_v=="갈월동"|dong_v=="남영동"|dong_v=="동자동"|dong_v=="용산동1가","남영동",
                                ifelse(dong_v=="용산동2가"|dong_v=="용산동4가"|dong_v=="용산2가동","용산2가동",
                                       ifelse(dong_v=="용산동3가"|dong_v=="용산동5가"|dong_v=="한강로1가"|dong_v=="한강로2가"|dong_v=="한강로3가"|dong_v=="한강로동","한강로동",
                                              ifelse(dong_v=="동빙고동"|dong_v=="서빙고동"|dong_v=="주성동"|dong_v=="용산동6가"|dong_v=="서빙고동","서빙고동",
                                                     ifelse(dong_v=="서계동"|dong_v=="청파동1가"|dong_v=="청파동2가"|dong_v=="청파동3가"|dong_v=="청파동","청파동",
                                                            ifelse(dong_v=="문배동"|dong_v=="신계동"|dong_v=="원효로1가"|dong_v=="원효로2가"|dong_v=="원효로1동","원효로1동",
                                                                   ifelse(dong_v=="원효로3가"|dong_v=="원효로4가"|dong_v=="신창동"|dong_v=="산천동"|dong_v=="청암동"|dong_v=="원효로2동","원효로2동",
                                                                          ifelse(dong_v=="효창동","효창동",
                                                                                 ifelse(dong_v=="도원동"|dong_v=="용문동","용문동",
                                                                                        ifelse(dong_v=="이촌1동"|dong_v=="이촌2동"|dong_v=="이촌동","이촌동",
                                                                                               ifelse(dong_v=="이태원1동"|dong_v=="이태원2동"|dong_v=="이태원동","이태원동",
                                                                                                      ifelse(dong_v=="한남동","한남동",
                                                                                                             ifelse(dong_v=="보광동","보광동","NA")))))))))))))))%>%
    group_by(dong)%>%
    summarise(n=sum(Freq))

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

bell_dong_df

############### 용산구 동별 인구수
#yongpop_file ="C:/Users/student/Desktop/semi/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
yongpop_file ="C:/Users/student/Desktop/local_reposit/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
=======
############### 용산구 동별 인구수
yongpop_file ="C:/Users/student/Desktop/semi/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############### 용산구 동별 인구수
yongpop_file ="C:/Users/student/Desktop/semi/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############### 용산구 동별 인구수
yongpop_file ="C:/Users/student/Desktop/semi/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############### 용산구 동별 인구수
yongpop_file ="C:/Users/student/Desktop/semi/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
############### 용산구 동별 인구수
yongpop_file ="C:/Users/student/Desktop/semi/semi-project/data/용산구 동별 인구현황(2019.11월말).xlsx"
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
raw_yongpop <- read_excel(path = yongpop_file)
youngpop <- as.data.frame(raw_yongpop)

str(youngpop)
bell_dong_df <- as.data.frame(bell_dong_df)
str(bell_dong_df)
#View(bell_dong_df)

# 1만명당 동별 벨 갯수
youngpop<-youngpop%>%
    rename(dong=행정동)
left_join(youngpop,bell_dong_df)%>%
    mutate(bell_pop = n/(인구수/10000))%>%
    arrange(bell_pop)

#### 원효로1동 길뽑기
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
movepop_raw <-read.csv(file = "C:/Users/student/Desktop/local_reposit/semi-project/data/서울시_추정유동인구.csv") 
=======
movepop_raw <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울시_추정유동인구.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
movepop_raw <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울시_추정유동인구.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
movepop_raw <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울시_추정유동인구.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
movepop_raw <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울시_추정유동인구.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
=======
movepop_raw <-read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울시_추정유동인구.csv") 
>>>>>>> 1723d04dba5c390823cfa6b7309723f2b3d1366d
#View(movepop)


movepop<- movepop_raw%>%
    select("기준_년_코드","상권_코드_명","시간대_1_유동인구_수","시간대_6_유동인구_수","월요일_유동인구_수","화요일_유동인구_수","수요일_유동인구_수","목요일_유동인구_수","금요일_유동인구_수","토요일_유동인구_수","일요일_유동인구_수")


############################ 동에서 길뽑기 ####################################

#### 원효로1동 길뽑기
won1<-movepop%>% ##
    filter(상권_코드_명 == "원효로53길")
won2<-movepop%>% ##
    filter(상권_코드_명 == "원효로41길")
won3<-movepop%>%##
    filter(상권_코드_명 == "원효로89길")
won4<-movepop%>%##
    filter(상권_코드_명 == "백범로90길")
won5<-movepop%>%##
    filter(상권_코드_명 == "백범로79길")

#### 용문동 길뽑기
yong1<-movepop%>% ##
    filter(상권_코드_명 == "원효로41길") #won2랑 같다
yong2<-movepop%>% ##
    filter(상권_코드_명 == "원효로53길") #won1랑 같다
yong3<-movepop%>% ##
    filter(상권_코드_명 == "효창원로39길") 

#### 이촌동 길뽑기
lee1<-movepop%>% ##
    filter(상권_코드_명 == "이촌로22길") 

### 원효로2동 길뽑기
wone1<-movepop%>% ##
    filter(상권_코드_명 == "원효로19길")
wone2<-movepop%>% ##
    filter(상권_코드_명 == "원효로41길")#won2와 같다

#### 한강로동 길뽑기
han1<-movepop%>% ##
    filter(상권_코드_명 == "한강대로52길")
han2<-movepop%>% ##
    filter(상권_코드_명 == "한강대로62길")
han3<-movepop%>% ##
    filter(상권_코드_명 == "한강대로43길")
han4<-movepop%>% ##
    filter(상권_코드_명 == "한강대로15길")
han5<-movepop%>% ##
    filter(상권_코드_명 == "한강대로7길")


gil_df <- as.data.frame(rbind(won1[22,],won2[22,],won3[22,],won4[22,],won5[22,],yong3[22,],lee1[22,],wone1[22,],han1[22,],han2[22,],han3[22,],han4[22,],han5[22,]),stringsAsFactors=F)
df_result<- gil_df%>%arrange(시간대_6_유동인구_수)
#View(df_result)

##################################

#### 용산구 치안센터
yongp_file = "C:/Users/student/Desktop/semi/semi-project/data/서울시 지구대 파출소 치안센터 정보.xlsx"
raw_yongp <- read_excel(path = yongp_file)
yongp <- as.data.frame(raw_yongp)
yongp<- yongp %>% 
    select("최하위기관명","행정 동","위도","경도")%>%
    filter(최하위기관명 != "서울용산경찰서")
yongp



#### 여성 안전지킴이집 좌표 
read.csv(file = "C:/Users/student/Desktop/semi/semi-project/data/서울특별시_용산구_여성안심지킴이집_20190731.csv") -> female_safety
arrange(female_safety,관할경찰서명)-> female_safety
#View(female_safety)
table(female_safety$관할경찰서명)

female_safety_1 <- female_safety %>%
    select("위도","경도","소재지지번주소","관할경찰서명")
#View(female_safety_1)


#### 원으로 맵그리기
map_background <- leaflet()%>%
    addTiles()%>%
    setView(lng = yongp$경도[1],lat = yongp$위도[1],zoom=13)%>%
    addCircles(lng = yongp$경도[1],lat = yongp$위도[1],radius=465, color = "blue")%>%
    addCircles(lng = yongp$경도[1],lat = yongp$위도[1],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[1],lat = female_safety$위도[1],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[2],lat = female_safety$위도[2],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[2],lat = yongp$위도[2],radius=550, color = "blue")%>%
    addCircles(lng = yongp$경도[2],lat = yongp$위도[2],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[3],lat = female_safety$위도[3],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[4],lat = female_safety$위도[4],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[5],lat = female_safety$위도[5],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[6],lat = female_safety$위도[6],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[3],lat = yongp$위도[3],radius=1145, color = "blue")%>%
    addCircles(lng = yongp$경도[3],lat = yongp$위도[3],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[7],lat = female_safety$위도[7],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[8],lat = female_safety$위도[8],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[9],lat = female_safety$위도[9],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[4],lat = yongp$위도[4],radius=1644, color = "blue")%>%
    addCircles(lng = yongp$경도[4],lat = yongp$위도[4],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[10],lat = female_safety$위도[10],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[11],lat = female_safety$위도[11],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[12],lat = female_safety$위도[12],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[13],lat = female_safety$위도[13],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[14],lat = female_safety$위도[14],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[15],lat = female_safety$위도[15],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[16],lat = female_safety$위도[16],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[17],lat = female_safety$위도[17],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[18],lat = female_safety$위도[18],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[19],lat = female_safety$위도[19],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[20],lat = female_safety$위도[20],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[21],lat = female_safety$위도[21],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[22],lat = female_safety$위도[22],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[5],lat = yongp$위도[5],radius=1355, color = "blue")%>%
    addCircles(lng = yongp$경도[5],lat = yongp$위도[5],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[23],lat = female_safety$위도[23],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[24],lat = female_safety$위도[24],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[25],lat = female_safety$위도[25],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[26],lat = female_safety$위도[2],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[6],lat = yongp$위도[6],radius=743, color = "blue")%>%
    addCircles(lng = yongp$경도[6],lat = yongp$위도[6],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[27],lat = female_safety$위도[27],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[28],lat = female_safety$위도[28],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[7],lat = yongp$위도[7],radius=800, color = "blue")%>%
    addCircles(lng = yongp$경도[7],lat = yongp$위도[7],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[29],lat = female_safety$위도[29],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[30],lat = female_safety$위도[30],radius=5, color = "red")%>%
    
    addCircles(lng = yongp$경도[8],lat = yongp$위도[8],radius=1305, color = "blue")%>%
    addCircles(lng = yongp$경도[8],lat = yongp$위도[8],radius=1, color = "darkblue")%>%
    addCircles(lng = female_safety_1$경도[31],lat = female_safety$위도[31],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[32],lat = female_safety$위도[32],radius=5, color = "red")%>%
    addCircles(lng = female_safety_1$경도[33],lat = female_safety$위도[33],radius=5, color = "red")



map_background
