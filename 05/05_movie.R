install.packages("jsonlite")
library(jsonlite)

# 일일박스 오피스 자료 가져오기
# url <- "http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=f5eef3421c602c6cb7ea224104795888&targetDt=20120101"
box <-function(dt){
  apikey <-"f5eef3421c602c6cb7ea224104795888"
  url <- paste("http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
                 "key=", apikey, 
               "&targetDt=",dt, sep="")
  
  mv <-fromJSON(url)
  
  # 박스오피스 목록 추출
  BoxOfficeList <-mv$boxOfficeResult$dailyBoxOfficeList
 return (BoxOfficeList)
}


# 데이터형변환 : 수치데이터 변환
names(BoxOfficeList)
str(BoxOfficeList)

col <- c("rnum","rank","rankInten","salesAmt","salesShare","salesInten","salesChange","salesAcc",
         "audiCnt","audiInten","audiChange","audiAcc",
         "scrnCnt","showCnt" )

# unlist 쓰는 이유가 리스트를 벡터화시키게 하기 위해서

for(c in col){
  BoxOfficeList[c]<-as.numeric(unlist(BoxOfficeList[c]))
}

library(dplyr)
BoxOfficeList %>%
  filter(salesAmt > mean(salesAmt)) %>%
  select(rank, movieNm,salesAmt)

box("20211220")







