install.packages("gsubfn")
install.packages("formattable")
setwd("D:/wooseok/0.Quantitative Applied Economics/2.경제데이터분석 (최재성 교수님)")

library(dplyr)
library(stringr)
library(httr)
library(XML)
library(jsonlite)
library(readxl)
library(writexl)
library(RSelenium)
library(rvest)
library(gsubfn)
library(readr)
library(formattable)

library(lubridate)
library(knitr)
library(kableExtra)
library(dplyr)
library(httr)
library(rvest)
library(xml2)
library(stringr)

# Table 1 (펀드매니져별 핵심 정보)
summary <- res %>% 
  html_table() %>% 
  .[[1]]
summary2<-summary[,2:6]

#Could be sovled this way!
#for (fundname in hedgefund.list) {
#  tab <- fundinfo(fundname)
#  Stack <- rbind(Stack, tab)
#  Sys.sleep(3)
#}


#for (i in 1:length(hedgefund.list)) {
#  tab <- fundinfo(hedgefund.list[i])
#  Stack <- rbind(Stack, tab)
#  Sys.sleep(3)
}

#TOP PEF 이름 및 핵심운용역 이름
fundmanager.list <- c("Warren Buffett", "Carl Icahn", "Michael Burry","George Soros","Bill Ackman","John Paulson","David Tepper", "Daniel Loeb")
hedgefund.list <- c("Berkshire+Hathaway", "Icahn+Enterprises", "Scion+Asset+Management","Soros+Fund+Management","Pershing+Square+Capital+Management","Paulson","Appaloosa", "Third+Point")


rD <- rsDriver(browser="firefox", port=4800L, chromever=NULL)
remDr <- rD$client




fundinfo <- function(hf) {
  
  URL <- str_c("https://hedgefollow.com/funds/", hf)
  remDr$navigate(URL)
  remDr$refresh()
  
    # Get HTML source
    txt <- remDr$getPageSource()[[1]]
    res <- read_html(txt)
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(1)"
  ticker<- res %>% 
      html_nodes(pattern) %>% 
      html_text()
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(2)"
  companyname<- res %>% 
    html_nodes(pattern) %>% 
    html_text() 
  

  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(3)"
  portfoliopercentage <- res %>% 
    html_nodes(pattern) %>% 
    html_text() 
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(4)"
  sharesowned <- res %>% 
    html_nodes(pattern) %>% 
    html_text()

  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(5)"
  valueowned <- res %>% 
    html_nodes(pattern) %>% 
    html_text()

  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(6)"
  changeinshare <- res %>% 
    html_nodes(pattern) %>% 
    html_text()

  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(8)"
  averagebuyprice <- res %>% 
    html_nodes(pattern) %>% 
    html_text()

  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(10)"
  valuedate <- res %>% 
    html_nodes(pattern) %>% 
    html_text()
    
  rawsummary <- cbind(ticker, companyname, portfoliopercentage, sharesowned , valueowned, changeinshare, averagebuyprice, valuedate) %>% as.data.frame()
   
   return(rawsummary)
 
  }


Stack2 <- NULL
for (j in 1:8) {
  print(fundmanager.list[j])
  tab <- fundinfo(hedgefund.list[j])
  tab2 <- cbind(fundmanager.list[j], tab)
  Stack2 <- rbind(Stack2, tab2)
  Sys.sleep(3)}


Stack2$portfoliopercentage <- parse_number(Stack2$portfoliopercentage)
raw$portfoliopercentage  <- as.numeric(gsubfn("[^0-9.-]", "", raw$portfoliopercentage )) <-error 뜸

#펀드매니져별 중복 행 삭제 (수기작업 필요)
Stack3 <- Stack2[c(-43,-44,-45,-46,-47,-63,-94,-95,-96,-147,-148,-149,-150,-151,-152,-161,-181,-182,-226,-227,-228,-229,-230,-268,-269,-270,-271),]


#rownames(raw2) <- seq(nrow(raw2)) 잘안됨 (42~48등 이음새)
#Stack3$seq <- row.names(Stack3) 잘안됨 (42~48등 이음새)
#raw3 %>% filter(,-2) 잘안됨 (42~48등 이음새)

#펀드매니져별 중복 행 삭제 후 행 숫자 공백 생기는 부분 보정
Stack3$consecutive_numbers<-1:244
Stack4<- Stack3 %>% select(consecutive_numbers, everything())

#표 중 ticker 정보 다운받으면서 순서 오류 발생(삭제)
Stack4_filtered <- Stack4 %>% select(-3)


#Valueowned부분 $~B 및 $~M character 형태를 numerical value로 전환하기 위해서 아래와 같이 함수를 활용해봤지만 coersion 발생, stackoverflow상에서 다양한 방법 활용해봤으나. 효과 미비, 따라서, 데이터를 엑셀로 옮긴 후 million usd 단위로 수기로 열을 만듬 (sharedownedinMIl)
## Sample data with a column containing dollar amounts denominated in B and M
#df <- data.frame(valueowned = c("$100.25M", "$2.5B", "$75.8M", "$500K", "$1.2B", "$135.36B"))

# 아래 함수 활용하니 잘 풀림
convert_denominated_value <- function(x) {
ifelse(str_detect(x, "B"),
        as.numeric(str_extract(x, "[0-9.]+")) * 1e9,
         ifelse(str_detect(x, "M"),
                as.numeric(str_extract(x, "[0-9.]+")) * 1e6,
                as.numeric(str_extract(x, "[0-9.]+"))
         )
  )
}

options(scipen = 999) 
#Scientific e number 제거 (Stack4_filtered$valueownedinmil열)
options(scipen = 0) 
#Scientific e number 복원 (Stack4_filtered$valueownedinmil열)

Stack4_filtered$valueownedinmil <- convert_denominated_value(Stack4_filtered$valueowned)



#전체 펀드메니저 정보 뽑기  (테이블)
kable(Stack4_filtered) %>%
  kable_styling("striped", full_width = F) %>%
  scroll_box(width = "100%", height = "200px")

------------------------------------------------------------------------
#펀드매니져별 정부 정리 및 다운로드 

Warrenonly<- Stack4_filtered[grepl("Warren Buffett", Stack4_filtered$fundmanager.list),]

Carlonly<- Stack4_filtered[grepl("Carl Icahn", Stack4_filtered$fundmanager.list),]

Michaelonly<- Stack4_filtered[grepl("Michael Burry", Stack4_filtered$fundmanager.list),]

Georgeonly<- Stack4_filtered[grepl("George Soros", Stack4_filtered$fundmanager.list),]

Billonly<- Stack4_filtered[grepl("Bill Ackman", Stack4_filtered$fundmanager.list),]

Johnonly<- Stack4_filtered[grepl("John Paulson", Stack4_filtered$fundmanager.list),]

Davidonly<- Stack4_filtered[grepl("David Tepper", Stack4_filtered$fundmanager.list),]

Danielonly<- Stack4_filtered[grepl("Daniel Loeb", Stack4_filtered$fundmanager.list),]
  
write_xlsx(Stack4_filtered, "Hedgefollow(Masterfile).xlsx")
write_xlsx(Warrenonly, "Warrenonly.xlsx")
write_xlsx(Carlonly, "Carlonly.xlsx")
write_xlsx(Michaelonly, "Michaelonly.xlsx")
write_xlsx(Georgeonly, "Georgeonly.xlsx")
write_xlsx(Billonly, "Billonly.xlsx")
write_xlsx(Johnonly, "Johnonly.xlsx")
write_xlsx(Davidonly, "Davidonly.xlsx")
write_xlsx(Danielonly, "Danielonly.xlsx")

# 펀드매니져별 보유 종목 및 보유 금액 

Warrenvalue <- Warrenonly[,10]
Warrencompany <- Warrenonly[,3]
  
Carlvalue <- Carlonly[,10]
Carlcompany <- Carlonly[,3]
  
Michaelvalue <- Michaelonly[,10]
Michaelcompany <- Michaelonly[,3]
  
Georgevalue <- Georgeonly[,10]
Georgecompany <- Georgeonly[,3]

Billvalue <- Billonly[,10]
Billcompany <- Billonly[,3]

Johnvalue<- Johnonly[,10]
Johncompany <- Johnonly[,3]

Davidvalue<- Davidonly[,10]
Davidcompany <- Davidonly[,3]

Danielvalue<- Danielonly[,10]
Danielcompany <-Danielonly[,3] 


#펀드매니져 별로 파이차트 그리기

pie(Warrenvalue ,main="Warren Buffet Top Holding Stocks", col=rainbow(length(Warrenvalue)), labels=Warrencompany) 

pie(Carlvalue ,main="Carl Icahn Top Holding Stocks",col=rainbow(length(Carlvalue )), labels=Carlcompany)  

pie(Michaelvalue ,main="Michael Burry Top Holding Stocks",col=rainbow(length(Michaelvalue)), labels=Michaelcompany) 

pie(Georgevalue ,main="George Soros Top Holding Stocks",col=rainbow(length(Georgevalue)), labels=Georgecompany) 

pie(Billvalue,main="Bill Ackman Top Holding Stocks",col=rainbow(length(Billvalue)), labels=Billcompany) 

pie(Johnvalue,main="John Paulson Top Holding Stocks",col=rainbow(length(Johnvalue)), labels=Johncompany) 

pie(Davidvalue,main="David Tepper Top Holding Stocks",col=rainbow(length(Davidvalue)), labels=Davidcompany)

pie(Danielvalue,main="Daniel Loeb Top Holding Stocks",col=rainbow(length(Danielvalue)), labels=Danielcompany)


remDr$close()
rD$server$stop()
