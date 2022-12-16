# 기상개황 자료를 이용하여 월별 불쾌지수를 계산하고 불쾌지수가 높음이상인 월을 구하시오.
# 
# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE
df = read.csv("C:/Rwork/04_기상개황.csv",  fileEncoding = "euc-kr")
#열명확인
df<-df[,1:8]
names(df)
head(df)
names(df)<-c("월별","평균기온","평균최고기온","최고기온","평균최저기온","최저극값기온",
             "강수량","상대습도")
names(df)
df<-df[2:13,]
df<-df[,2:8]
# 마이너스이용해서 제거
df<-df[-c(1:2)]
df
# 불쾌지수 공식
df$불쾌지수<- 0.81 *df$평균기온 +0.01 * df$상대습도 *(0.99 *df$평균기온 - 14.3) + 46.3

df$불쾌지수

# DI = 0.81 * Ta + 0.01 * RH * (0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)
# 불쾌지수 단계
df$불쾌지수단계 <- ifelse(df$불쾌지수 >= 80, "매우높음",
                    ifelse(df$불쾌지수 >=75, "높음",
                         ifelse(df$불쾌지수 >=68, "보통","낮음"))) 
df$불쾌지수단계
# 매우높음: 80이상
# 높음: 75이상 80미만
# 보통: 68이상 75미만
# 낮음: 68미만



ggplot(mapping =aes(x=불쾌지수 ,y=월별, fill=불쾌지수단계 ), data=df) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  coord_flip() +
  ggtitle("월별 불쾌지수 단계")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))



install.packages('patchwork')





