

#------------------------------------------------------------------
#                      라이브러리 불러오기
#------------------------------------------------------------------
library(dplyr)
library(ggplot2)



#------------------------------------------------------------------
#                        데이터 불러오기
#------------------------------------------------------------------
news <- read.csv("science_IT_data/2007/2007_science_IT.csv", stringsAsFactors=FALSE)



#------------------------------------------------------------------
#                      월별로 데이터 분리
#------------------------------------------------------------------
news_01 <- grep(".-01-.", news$postTime)
news_01 <- news[news_01,]
news_01[4] <- "2017-01"

news_02 <- grep(".-02-.", news$postTime)
news_02 <- news[news_02,]
news_02[4] <- "2017-02"

news_03 <- grep(".-03-.", news$postTime)
news_03 <- news[news_03,]
news_03[4] <- "2017-03"

news_04 <- grep(".-04-.", news$postTime)
news_04 <- news[news_04,]
news_04[4] <- "2017-04"

news_05 <- grep(".-05-.", news$postTime)
news_05 <- news[news_05,]
news_05[4] <- "2017-05"

news_06 <- grep(".-06-.", news$postTime)
news_06 <- news[news_06,]
news_06[4] <- "2017-06"

news_07 <- grep(".-07-.", news$postTime)
news_07 <- news[news_07,]
news_07[4] <- "2017-07"

news_08 <- grep(".-08-.", news$postTime)
news_08 <- news[news_08,]
news_08[4] <- "2017-08"

news_09 <- grep(".-09-.", news$postTime)
news_09 <- news[news_09,]
news_09[4] <- "2017-09"

news_10 <- grep(".-10-.", news$postTime)
news_10 <- news[news_10,]
news_10[4] <- "2017-10"

news_11 <- grep(".-11-.", news$postTime)
news_11 <- news[news_11,]
news_11[4] <- "2017-11"

news_12 <- grep(".-12-.", news$postTime)
news_12 <- news[news_12,]
news_12[4] <- "2017-12"



#------------------------------------------------------------------
#                         월별 기사수
#------------------------------------------------------------------
news1 <- rbind(news_01, news_02)
news1 <- rbind(news1, news_03)
news1 <- rbind(news1, news_04)
news1 <- rbind(news1, news_05)
news1 <- rbind(news1, news_06)
news1 <- rbind(news1, news_07)
news1 <- rbind(news1, news_08)
news1 <- rbind(news1, news_09)
news1 <- rbind(news1, news_10)
news1 <- rbind(news1, news_11)
news1 <- rbind(news1, news_12)

ggplot(data=news1, aes(x=factor(postTime))) + 
  geom_bar(stat='count', fill='#BDD7EE') +
  geom_text(stat='count', aes(label=..count..), vjust=1.5, color="#1F4E79") +
  xlab('년-월') +
  ylab('기사수')



#------------------------------------------------------------------
#                        분기별 뉴스 데이터
#------------------------------------------------------------------
news1 <- rbind(news_01, news_02)
news1 <- rbind(news1, news_03)

news2 <- rbind(news_04, news_05)
news2 <- rbind(news2, news_06)

news3 <- rbind(news_07, news_08)
news3 <- rbind(news3, news_09)

news4 <- rbind(news_10, news_11)
news4 <- rbind(news4, news_12)



#------------------------------------------------------------------
#                        데이터 전처리 1차
#------------------------------------------------------------------
Contents <- news4[,6]
c <- Contents


# html 코드, 홍보 문구, 신문사명 등 제거
Contents <- gsub("// flash 오류를 우회하기 위한 함수 추가", "", Contents, perl=F)
Contents <- gsub("function _flash_removeCallback\\(\\) \\{\\}", "", Contents, perl=F)
Contents <- gsub("<.{1,100}>", "", Contents, perl=F)
Contents <- gsub("<.{1,250}>", "", Contents, perl=F)
Contents <- gsub("&.{1,5};", "", Contents, perl=F)
Contents <- gsub("\\[.{1,50}\\]", "", Contents, perl=F)
Contents <- gsub("\\..{1,50}@.{0,100}$", "\\.", Contents, perl=F)
Contents <- gsub("\\..{1,50}▶.*", "\\.", Contents, perl=F)
Contents <- gsub("◀.{1,10}▶", "\\.", Contents, perl=F)
Contents <- gsub("▶ 전자.*", "\\.", Contents, perl=F)
Contents <- gsub("\\.by.*", "\\.", Contents, perl=F)
Contents <- gsub("/.{1,50}by.*", "", Contents, perl=F)
Contents <- gsub("@.{1,200} 금지", "", Contents, perl=F)
Contents <- gsub("서경C.*", "", Contents, perl=F)
Contents <- gsub("\\(.{1,5}=.{1,5}\\)", "", Contents, perl=F)
Contents <- gsub(".{1,5}기자", "", Contents, perl=F)
Contents <- gsub("\\..{1,30}com", "", Contents, perl=F)
Contents <- gsub("최신 유행 트렌드 총집결 #흥\\(클릭!\\)", "", Contents, perl=F)
Contents <- gsub("최신 유행 트렌드 총집결 \\(클릭!\\)", "", Contents, perl=F)


# 특수문자 제거
Contents <- gsub("[^\uAC00-\uD7A3xfe 0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣\\s]",
                 " ", Contents, perl=F)

# 공간 제거
for (i in 1:15) {
  Contents <- gsub("  "," ", Contents, perl=F)
}



#------------------------------------------------------------------
#                          데이터 저장
#------------------------------------------------------------------
data <- data.frame(title=news4[2], contents=Contents)
write.csv(data, "science_IT_data/2007_4Q.csv", row.names=F)



#------------------------------------------------------------------
#                             참고
#------------------------------------------------------------------
# 정규표현식 : https://statkclee.github.io/ds-authoring/regex-r.html



rm(list=ls())
rm(news)
rm(news_01)
rm(news_02)
rm(news_03)
rm(news_04)
rm(news_05)
rm(news_06)
rm(news_07)
rm(news_08)
rm(news_09)
rm(news_10)
rm(news_11)
rm(news_12)
