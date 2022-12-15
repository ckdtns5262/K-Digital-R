#암종류별 성별 분석
library(dplyr)

#데이터 불러오기(암발생자수)
df = read.csv("C:/Rwork/03_암발생자수.csv", fileEncoding = "euc-kr") 

df 

# 열명 변경
# "암종별", "성별", "연령별", "발생자수", "조발생률"
names(df)<-c("암종별", "성별","연령별","발생자수","조발생률") 


# 데이터셋 조회
# 1) 특정 변수 조회
t1 <- df$암종별
mode(t1)
class(t1)
is.vector(t1)
# 2) 특정 열명을 사용하여 조회
t2 <- df['암종별']
mode(t2)
class(t2)
# 3) 특정 행 조회 :1행 조회
df[1,]
df[2,] # 2행 조회
df[c(2,4),] # 2행, 4행 조회

# 4)특정행 제거 : 1행제거
df1 <- df[-1,] # -를 붙이면 행을 제거
head(df1)


# 열 데이터 타입 확인
str(df)


# 값 변경 : - => 0
df$발생자수 <- ifelse(df$발생자수=="-", 0, df$발생자수)
df$조발생률 <- ifelse(df$조발생률=="-", 0, df$조발생률)


# 열 데이터타입 변경
df1$발생자수 <- as.numeric(df1$발생자수)
df1$조발생률 <- as.numeric(df1$조발생률) 

# 모든암 제거하고 연령별이 계인 데이터 


# 암종류 확인
df$암종별
unique(df$암종별)

df2 <-df1 %>%
  filter(암종별 != "모든 암(C00-C96)") %>%
  filter(연령별 == "계")

df21 <-df2 %>%
  filter(성별== "계")

df22  <-df2 %>%
  filter(성별 != "계")


unique(df2$암종별)
# 5) 특정행 열 조회
df21 <- df21[,c('암종별','발생자수')]

df22 <- df2[,c('암종별','성별','발생자수')]

df2m <- df22 %>%
  filter(성별 =='남자')

df2w <- df22 %>%
  filter(성별 =='여자')

df1[1,'암종별']
df1[1:3,c('암종별','발생자수')]


df22<-df$암종별



 
# 그래프 
install.packages("ggthemes")
library(ggthemes)
library(ggplot2)

ggplot(mapping =aes(x=암종별, y=발생자수, fill=성별), data=df22) + 
  geom_bar(stat="identity", position=position_dodge())  +
  coord_flip()+

  theme(axis.text.x=element_text(angle=90, hjust=1, size=10)) +
  labs(
    title = "남여별 암종별 환자수",
    subtitle = "그래프실험",
    caption = "환자 파악용",
    x = "암종별",
    y = "발생자수"
  )
  
  
  
str(df22)

 