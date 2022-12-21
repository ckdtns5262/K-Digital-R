df<-read.csv("C:/Rwork/06_국민건강보험공단500.csv")
names(df)
df1<-df %>% select("성별코드","연령대.코드.5세단위.","신장.5Cm단위.","체중.5Kg.단위.")

names(df1)<-c("성별","연령","키","몸무게")

df1<-na.omit(df1)
df1$BMI <- df1$몸무게/(df1$키*0.01)^2 
df1$BMI <-round(df1$BMI)

df1$판별 <- ifelse(df1$BMI >=30 , "비만",
              ifelse(df1$BMI >=25, "과체중",
                 ifelse(df1$BMI >=20, "정상", "저체중")))



df1$판별 <-as.factor(df1$판별)

df1$판별수 <-ifelse(df1$판별=='저체중',0,
                 ifelse(df1$판별=='정상',1,
                    ifelse(df1$판별=='과체중',2,3)))

x <- sample(1:nrow(df1),0.7 * nrow(df1))
train <- df1[x,]
test <- df1[-x,]

nrow(train)
nrow(test)

library(party)
model <- ctree(판별 ~ 성별 + 연령 +키 + 몸무게, data = train)

pred <-predict(model, test) 

cm <- table(pred, test$판별)
cm

head(test)
test$예측 <-pred
test$예측확인 <- ifelse(test$판별 == test$예측,1,0)
sum(test$예측확인) / nrow(test)



