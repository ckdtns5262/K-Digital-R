#인구 동향
library(dplyr)

#데이터 불러오기(인구동향)
df = read.csv("C:/Rwork/04_인구동향.csv",  fileEncoding = "euc-kr")
# 열명 확인
names(df)
# "행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수"
names(df)<-c("행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수") 

head(df)
# =>시점은 수치형으로 표현되어 있지만 범주형 자료로 보는게 효율적 , 범주형자료 : 행정구역별, 시점 / 수치형자료 : 나머지


#EDA 
#데이터 구조 확인
str(df)
df
#범주형 변경 : 시점 int => category
df$시점 <- as.factor(df$시점)
mode(df$시점)
class(df$시점)

#결측치 확인 
summary(df)
is.na(df$출생아수)


#결측치 내용 확인 

# is.na 사용
df1 <- df[is.na(df$출생아수),]

# filter 사용
df2 <- df %>% filter(is.na(출생아수))

unique(df2$행정구역별)
unique(df2$시점)
df2 
#결측치 행 제거
df3 <- df %>%filter(!is.na(df$출생아수) & !is.na(df$사망자수) & 
                      !is.na(df$혼인건수) & !is.na(df$이혼건수)) 

df3 <-na.omit(df)
summary(df3)
summary(df)

#결측치 값 대체
df4 <- df
df4$출생아수 <- ifelse(is.na(df4$출생아수) , 0, df4$출생아수)
summary(df4)

#출생아수, 사망지수 , 혼인건수, 이혼건수에 대해
# na 값 0으로 변경 : 반복문
col <- names(df)[3:6]

for(c in col) {
  temp <-df[,c]
  temp <- ifelse(is.na(temp),0, temp)
  df[,c] <-temp
}
summary(df)

df4<-df4 %>% replace(is.na(df4), 0)
summary(df4)

# 자연증가수 
df$자연증가수 <- df$출생아수-df$사망자수
df$자연증가수

df6<-df %>% filter(df$자연증가수 < 0)
df6

# case 1 
df[df$행정구역별 =='전국' & df$자연증가수 <0,]

# case 2 
df[which(df$행정구역별 == '전국' & df$자연증가수 < 0),]

# case 3 
df %>% 
  filter(df$행정구역별 =='전국' & df$자연증가수 <0)%>%
  select('시점')






# 데이터 분석
# 기술통계분석- 범주형자료-빈도분석
table(df$행정구역별)
table(df$시점)
# 기술통계분석- 범주형자료-빈도분석-barplot 그래프


# 기술통계분석- 연속형자료-요약 통계량
summary(df$출생아수)

# 기술통계분석- 연속형자료-산점도 그래프
plot(df$출생아수, df$혼인건수)

# 전국 자료와 지역자료 분리
df_all<-df %>% filter(df$행정구역별 =="전국")
df_loc<-df %>% filter(df$행정구역별 !="전국")

# 전국데이터 자연 증가수 그래프
 

  ggplot(mapping =aes(x=자연증가수 ,y=행정구역별, fill=시점 ), data=df_all) + 
  geom_bar(stat="identity", position=position_dodge())  + 
    coord_flip() +
  ggtitle("전국데이터 자연 증가수 그래프")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))



library(ggplot2)
  


