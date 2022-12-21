library(jsonlite)


box <- function(year,month){
service <-"kL00C7wEpGgpo2n%2FErweSgWemnixWZxI2N%2BTzlkyWrugWrGVQTQGO0lpetrDfzp88yDNXbcJJDyN2mTGdowblw%3D%3D&type=json&page=1&rowNum=2&"  
  
url <-paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?",
            "ServiceKey=",service,
            "disYear=",year,"&disMonth=",month, sep="")

mv <- fromJSON(url)

RFID <- mv$data$list
return(RFID)
}

data1 <- box("2020","06")
data2 <-box("2020", "07")
data3 <-box("2020","08")

