# 데이터 가져오기
df<-read.csv("C:/Rwork/06_지상관측.csv",  header = T, 
             stringsAsFactors = F, 
             fileEncoding = 'euc-kr')
# 열명변경
names(df)<-c("지점","지점명","일시","기온","풍속","상대습도")

# 체감온도 구하기
df$체감온도 <-13.12 + (0.6215 *df$기온) -(11.37*df$풍속*3.6*0.16) + (0.3965*df$풍속*3.6*0.16*df$기온)

# 지점명 부산자료 추출
df2<- subset(df,df$지점명=="부산")
df2

library(dplyr)

# 겨울철 체감온도 자료 추출
df2<-df2 %>% filter(기온 <= 10)
df2 <-df2 %>% filter(풍속 >=1.3)

# 그래프 그리기
library(ggplot2)
ggplot(mapping =aes(x=일시 ,y=체감온도, fill=체감온도 ), data=df2) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("부산지역 겨울철 체감온도")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

# 열지수 구하기
df$열지수 <- 0.5 * {df$기온+61.0 +(df$기온-68.0)*1.2 + (df$상대습도*0.094)}

# 지점명 서울,부산,제주 추출
df2<-df %>%filter(지점명 %in% c("서울", "부산","제주"))

# 평균기온
df2$평균기온 <- mean(df2$기온)

# 일자별 기온그래프
ggplot(mapping =aes(x=일시 ,y=기온, fill=지점명 ), data=df2) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("기온현황")+ geom_hline(yintercept =12.27)
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

# 일자별 열지수 그래프
  ggplot(data=df2)+
    geom_line(mapping=aes(x=일시,
                          y=열지수,
                          group=지점명,
                          color=지점명))+geom_hline(yintercept =5)

  # 일자별 그룹핑  
df22 <-df2 %>% group_by(일시)%>% 
  filter(열지수 <=5)
  

  
  

  
  
  