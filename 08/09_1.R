#한국환경공단_에어코리아_대기오염통계 현황
#시도별 실시간 평균정보 조회 상세기능명세
#최근 한달간 지역별 일평균 대기오염 정보
#기준초과인경우 
#https://www.airkorea.or.kr/web/contents/contentView/?pMENU_NO=132&cntnts_no=6

#json처리
install.packages("jsonlite") 
library(jsonlite)

#자료처리
library(dplyr)

# url <-"http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?itemCode=O3&dataGubun=DAILY&searchCondition=Month&pageNo=1&numOfRows=100&returnType=json&serviceKey=kL00C7wEpGgpo2n%2FErweSgWemnixWZxI2N%2BTzlkyWrugWrGVQTQGO0lpetrDfzp88yDNXbcJJDyN2mTGdowblw%3D%3D"
# 
# mv <- fromJSON(url)
# 
# data<-mv$response$body$items

#데이터 가져오기함수
getData <- function(item, gubun) {

  url <-paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?",
  "itemCode=",item,
  "&dataGubun=",gubun,
  "&searchCondition=Month&pageNo=1&numOfRows=100&returnType=json&serviceKey=kL00C7wEpGgpo2n%2FErweSgWemnixWZxI2N%2BTzlkyWrugWrGVQTQGO0lpetrDfzp88yDNXbcJJDyN2mTGdowblw%3D%3D",sep="")
  
  mv <-fromJSON(url)
  
  data<-mv$response$body$items
  return (data)
}
#1. pm10, O3 데이터 추출하여 합치기
 
pm<-getData("PM10","DAILY")
o3<-getData("O3","DAILY")
names(pm)
names(o3) 
df<-rbind(pm,o3)

#2. 지역명 벡터
area <- c("seoul", "busan", "daegu","incheon","gwangju",
          "daejeon", "ulsan", "gyeonggi", "gangwon",
          "chungbuk", "chungnam", "jeonbuk", "jeonnam",
          "gyeongbuk", "gyeongnam", "jeju", "sejong")

areaname <- c("서울","부산","대구","인천","광주","대전",
              "울산","경기","강원","충북","충남","전북",
              "전남","경북","경남","제주","세종")


#3. 통합데이터프레임 만들기
dft <- data.frame()
for (i in 1:length(area)){
  temp <-c()
  temp$dataTime <- unlist(df["dataTime"])
  temp$itemCode <- unlist(df["itemCode"])
  temp$area <- areaname[i]
  temp$item <- unlist(area[i])
  
  dft <-bind_rows(dft,temp)
}
dft <-data.frame()
for( i in 1:length(area)){
  t <-df[c("dataTime","itemCode",area[i])]
  t$area <-areaname[i]
  names(t)<-c("dataTime","itemCode","item","area")
  dft <- bind_rows(dft, t)
}





#4.주의보
#https://www.airkorea.or.kr/web/dustForecast?pMENU_NO=113

# 왼쪽정렬 되어 있으면 chr , 오른쪽 정렬이 되어 있어야 수치데이터로 바뀜
dft$item<-as.numeric(dft$item)
str(dft)
dft$기준 <-ifelse(dft$itemCode =="PM10",
            ifelse(dft$item <= 30, "좋음", 
                   ifelse(dft$item<=80, "보통", 
                          ifelse(dft$item <=150,"나쁨","매우나쁨"))),
            ifelse(dft$item <=0.03, "좋음",
                   ifelse(dft$item <=0.09,"보통", 
                          ifelse(dft$item <=0.15, "나쁨","매우나쁨"))))



#5.일자별 주의보정보
library(dplyr)
library(ggplot2)
install.packages('patchwork')
library('patchwork')
names(dft)

# 빈도표 사용시에 무조건 table 만들어서
dfn <-table(dft$dataTime, dft$기준)
dfn2 <-data.frame(dfn)
  
names(dfn2) <- c("일자", "기준","기준수")
ggplot(dfn2, aes(x=일자, y =기준수, group=기준, color=기준)) +
  geom_line() +
  geom_point() +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#5.지역별 PM10이 좋음인 날 수 
names(dft)

dfPM10 <-dft %>% filter(dft$itemCode == "PM10" & dft$기준 =="좋음")


ggplot(dfPM10, aes(x=area, y =item, fill=area)) +
  geom_bar(stat="identity") +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(dfPM10, aes(x=area, y =item, fill=area)) +
  geom_bar(stat="identity") +
  ggtitle("지역별 PM10이 좋음인 날 수")
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
