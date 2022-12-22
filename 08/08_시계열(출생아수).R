#데이터 불러오기
df <- read.csv("C:/Rwork/07_출생아수.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr') 
df
str(df)
names(df)<-c("시점","출생아수")
library(dplyr)
train <- df %>% filter(시점 < 2016 & 시점 >= 2000)
test <- df %>% filter(시점 >= 2016)

temp<- train$출생아수

#2. 시계열 데이터 생성 시계열데이터 만들어주는 함수 (ts) 
temp_ts <- ts(temp, frequency =1 , start = c(2000, 1))

library(forecast)

arima <- auto.arima(temp_ts)
arima

model <- arima(temp_ts, order=c(0,1,0))
model

tsdiag(model)
#box-Ljungn잔차항 모형 진단
#p-value >= 0.05 통계적으로 적절한지  
Box.test(model$residuals, lag=1, type="Ljung")

#7.예측 => 후에 11개월을 예측
fore <- forecast(model, h=6)
fore
plot(fore) 

test$출생아수
class(fore$mean)
pred = as.vector(fore$mean)

result <- data.frame(test = test$출생아수, pred=pred)
result

plot(result$test, type="o", col="red")
par(new=T)
plot(result$pred, type="o", col="blue")


