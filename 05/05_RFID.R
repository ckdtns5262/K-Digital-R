library(jsonlite)
library(dplyr)
# JSON URL 가져오기
getData <- function(year,month){
  service <-"kL00C7wEpGgpo2n%2FErweSgWemnixWZxI2N%2BTzlkyWrugWrGVQTQGO0lpetrDfzp88yDNXbcJJDyN2mTGdowblw%3D%3D&type=json&page=1&rowNum=10&"
  
  url <-paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?",
              "ServiceKey=",service,
              "disYear=",year,"&disMonth=",month, sep="")
  
  mv <- fromJSON(url)
  
  RFID <- mv$data$list
  
  return(RFID)

}




str(RFID)
str(data1)
# 데이터 분류
data1 <-getData("2020","06")
data2 <-getData("2021", "07")
data3 <-getData("2022","08")
data4 <-getData("2022","09")
# 데이터 합치기
dft<-rbind(data1,data2,data3,data4)

dft <- data.frame()
for(i in 1:12) {
  if(i < 10){ m = paste("0", i , sep = "")}
  else m = as.character(i)
  temp <-getData("2021", m)
  dft <- rbind(dft, temp)
  print(m)
  
}




# 이름 바꾸기
names(dft)<-c("배출년","배출월","배출요일","요일갯수","배출량","일평균배출량","배출횟수","일평균배출횟수")

# 수치 변환
dft$배출량 <-dft$배출량 * 0.01
dft$일평균배출량 <- dft$일평균배출량 * 0.01

# 데이터 열 생성
dft$배출요일<-
    ifelse(dft$배출요일 ==1 , "일",
     ifelse(dft$배출요일 ==2 , "월",
      ifelse(dft$배출요일 ==3 , "화",
       ifelse(dft$배출요일 ==4 , "수",
        ifelse(dft$배출요일 ==5 , "목",
           ifelse(dft$배출요일 ==6 , "금","토"))))))

library(dplyr)
library(ggplot2)
df<-dft %>% select("배출년","배출월","배출요일","배출량")


ggplot(mapping =aes(x=배출요일, y=배출량, fill=배출월), data=df) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("2020년 월별 일평균배출량")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))




