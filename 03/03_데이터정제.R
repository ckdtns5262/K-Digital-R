df1<-read.csv("C:/Rwork/03_치매환자현황.csv", fileEncoding = "euc-kr")

df1
## 거주지역별 치매환자 빈도표-1
table(df1$거주지역)
table(df1$성별)
tb1<-table(df1$거주지역, df1$성별)
tb1<-as.data.frame(tb1)
class(tb1)
tb1

names(tb)

library(ggplot2)
qplot(거주지역, data=df1, fill=거주지역) + 
  ggtitle("거주지역별 치매환자 빈도표")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

## 거주지역별 치매환자 빈도표-2
dflo<-df1[c("번호","거주지역")]
dflo
dflo2<-table(dflo)
qplot(거주지역, data=dflo,fill=거주지역)

# 날짜처리 패키지
install.packages("lubridate")
library("lubridate")

# 날짜 형식 변환
class(df1$진단일자)
df1$진단일자<-as.Date(df1$진단일자)
df1$데이터기준일자<-as.Date(df1$데이터기준일자)

## abs 절대값 적용 계산시
df1$진단일수 <-abs(difftime(df2$데이터기준일자,df2$진단일자,units = 'days')) 
# numeric으로 바꿔줌
df1$진단일수<-as.numeric(df1$진단일수)
mean(df1$진단일수)

mode(df1$진단일수)


df1$연령<-(2022-df1$출생년도)
df1$연령

df1$연령대 <-df1$연령 %/% 10 * 10 # %/% 몫 연산자
df1$연령대 <-paste(df1$연령대,"대",sep="")
df1$연령대

subset(df1, df1$연령대 == "100대")
df1$연령대 <- ifelse(df1$연령대 == "100대", "90대",df1$연령대)


qplot(연령대, data=df1, fill=연령대) + 
  ggtitle("연령별 치매환자")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

## 암 발생자수
df2<-read.csv("C:/Rwork/03_암발생자수_.csv", fileEncoding = "euc-kr")

# 열명 변경
names(df2) = c("암종별","성별","연령별","2019년","2019년_1")

# 모든 암

df2 <- subset(df2,df2$암종별 =="모든 암(C00-C96)")

df22<-df2 %>% filter(!(연령별 %in% c("계","연령미상")))

df22<-df22 %>% filter(암종별=="모든 암(C00-C96)" & !(연령별 %in% c("계","연령미상"))&
                        !(성별 %in% c("계")))
df22
unique(df22)
df22$연령대 <- ifelse(df22$연령별 %in% c("0-4세","5-9세","10-14세","15-19세", 
                                   "20-24세","25-29세","30-34세","35-39세"),"30대이하",
                   ifelse(df2$연령별 %in% c("40-44세","45-49세","50-54세","55-59세"),"40대~50대",
                        ifelse(df2$연령별 %in% c("60-64세","65-69세","70-74세","75-79세"),"60대~70대",
                               "80대이상"
                               )))
              
unique(df22$연령대)

names(df22)<-c("암종별","성별","연령별","y2019년","y2019년_1","연령대")

class(df22$y2019년)
df22$y2019년<-as.numeric(df22$y2019년)

df22g<-df22 %>%
        group_by(연령대, 성별) %>%
        summarise(계 = sum(y2019년))


ggplot(mapping =aes(x=연령대, y=계, fill=성별), data=df22g) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("연령대별 성별 분석")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))







