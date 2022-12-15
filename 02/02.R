install.packages("readxl")
library(readxl)
dfxl = read_excel(path = "C:/Rwork/02_역주행사고.xlsx")


# 3년 평균 사고건수,사망자수,치명률(사망지수/사고건수)구하기

# 평균사고건수

# 테이블 분리
df1 <- subset(dfxl, dfxl$구분 == '전체')
df1

df2 <- dfxl[dfxl$구분 =="역주행",]
df2

# 일반 교통사고
df3 <- df1
df3$구분 <-"일반"
df3

df3[c("사고","사망")]<-df1[c("사고","사망")]-df2[c("사고","사망")]
df3

# 치명률 계산
df1$치명률 <- round(df1$사망 / df1$사고 * 100,2)
df2$치명률 <- round(df2$사망 / df2$사고 * 100,2)
df3$치명률 <- round(df3$사망 / df3$사고 * 100,2)

# 기초통계값
summary(df2)

mean(df2$치명률)
mean(df3$치명률)


cat("최근 3년간 역주행 교통사고의 치명률이", 
    round(mean(df2$치명률),1),
    "%로 일반 교통사고(",
    round(mean(df3$치명률),1),
    "%)보다",
    round(mean(df2$치명률),1) / round(mean(df3$치명률),1),
    "배 높은 것으로 나타났다.")

install.packages("ggplot2")
library("ggplot2")
ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=dfxl) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))



# 부산시 체납현황 분석
dftax = read.csv("C:/Rwork/02_부산광역시_지방세 체납현황.csv", fileEncoding = "euc-kr")

dftax <-dftax[c("과세년도","세목명", "체납액구간", "누적체납금액")]

# 세목명
cols = unique(dftax$세목명)
cols


# 과세년도 범주형
dftax$과세년도 <-as.factor(dftax$과세년도)

subset(dftax,dftax$세목명 == "지방소득세")


# 함수 만들기
makedf <- function(item) {
  temp <- subset(dftax,dftax$세목명 == item)
  ggplot(mapping =aes(x=과세년도, y=누적체납금액, fill=체납액구간), data=temp) + 
    geom_bar(stat="identity", position=position_dodge())  + 
    ggtitle(item)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}

makedf("지방소득세")
## 연간자료 제외
dfwe<-dfwe[2:13,]

## 기상개황 자료를 분석하여 월별 불쾌지수와 단계
dfwe = read.csv("C:/Rwork/02_기상개황.csv", fileEncoding = "euc-kr")

names(dfwe)

#필요할 열 추출

dfwe<-dfwe[c("월별.1.","평균기온....","평균상대습도....")]

names(dfwe)<-c("월별","평균기온","평균상대습도")

dfDI<- 0.81*(dfwe$평균기온)+0.01 *dfwe$평균상대습도*(0.99*dfwe$평균기온-14.3) +46.3

dfwe$불쾌지수 <- dfDI
dfwe

dfwe$판별 <-ifelse(dfwe$불쾌지수 >= 80, "매우높음",
            ifelse(dfwe$불쾌지수 >= 75, "높음",
            ifelse(dfwe$불쾌지수 >= 68, "보통","낮음"
                 )))

ggplot(mapping =aes(x=월별, y=판별, fill=불쾌지수), data=dfwe) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("불쾌지수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))



dfwe<-table(dfwe$불쾌지수)
class(dfwe)
barplot(dfwe)

dfwe2<-as.data.frame(dfwe)
class(dfwe2)

ggplot(mapping =aes(x=var1, y=Freq, fill=var1), data=dfwe2) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("불쾌지수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))






