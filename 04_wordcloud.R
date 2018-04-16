

#------------------------------------------------------------------
#                      라이브러리 불러오기
#------------------------------------------------------------------
library(stringr)
library(dplyr)
library(wordcloud2)


#------------------------------------------------------------------
#                        데이터 불러오기
#------------------------------------------------------------------
Q1 <- read.table("science_IT_data/2017/2017_1Q_words_NC.txt", header=T)
Q2 <- read.table("science_IT_data/2017/2017_2Q_words_NC.txt", header=T)
Q3 <- read.table("science_IT_data/2017/2017_3Q_words_NC.txt", header=T)
Q4 <- read.table("science_IT_data/2017/2017_4Q_words_NC.txt", header=T)


#------------------------------------------------------------------
#                         데이터 합치기
#------------------------------------------------------------------
words <- rbind(Q1,Q2)
words <- rbind(words, Q3)
words <- rbind(words, Q4)
words <- unlist(words)


#------------------------------------------------------------------
#                          빈도수 확인
#------------------------------------------------------------------
words_freq <- table(words)
words_freq <- sort(words_freq, decreasing=T)
words_freq <- words_freq[words_freq >= 2000]
words_list <- names(words_freq)
words_list



#------------------------------------------------------------------
#                          데이터 저장
#------------------------------------------------------------------
write.table(words, "science_IT_data/2017/2017_contents_words_NC.txt", row.names=F)
write.table(words_freq, "science_IT_data/2017/2017_contents_freq.txt", row.names=F)



#------------------------------------------------------------------
#                        데이터 불러오기
#------------------------------------------------------------------
freq <- read.table("science_IT_data/2017/2017_contents_freq.txt", header=T)


#------------------------------------------------------------------
#                         워드클라우드
#------------------------------------------------------------------
# 색 지정
c <- c('#E6B0AA','#F5B7B1','#D7BDE2',
       '#D2B4DE','#A9CCE3','#AED6F1',
       '#A3E4D7','#A2D9CE','#A9DFBF',
       '#ABEBC6','#F9E79F','#FAD7A0',
       '#F5CBA7','#EDBB99','#F2D7D5',
       '#FADBD8','#E8DAEF','#D4E6F1',
       '#D6EAF8','#D1F2EB','#D0ECE7',
       '#D4EFDF','#D5F5E3','#FAE5D3',
       '#F6DDCC','#D7DBDD','#BFC9CA',
       '#CCD1D1','#B3B6B7','#A6ACAF')
c <- rep(c, times=6)


# 기본 시각화
wordcloud2(freq, color=c)


# 글자모양으로 시각화
letterCloud(f, "R", color=c)



#------------------------------------------------------------------
#                             참고
#------------------------------------------------------------------
# 색 : http://htmlcolorcodes.com/color-chart/



# devtools::install_github("lchiffon/wordcloud2")

rm(list=ls())
rm(Q1)
rm(Q2)
rm(Q3)
rm(Q4)