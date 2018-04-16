

#------------------------------------------------------------------
#                      라이브러리 불러오기
#------------------------------------------------------------------
library(stringr)
library(dplyr)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)



#------------------------------------------------------------------
#                        데이터 불러오기
#------------------------------------------------------------------
load("science_IT_data/2017/2017_1Q_words_NC2.RData")
load("science_IT_data/2017/2017_2Q_words_NC2.RData")
load("science_IT_data/2017/2017_3Q_words_NC2.RData")
load("science_IT_data/2017/2017_4Q_words_NC2.RData")



#------------------------------------------------------------------
#                         데이터 합치기
#------------------------------------------------------------------
words_NC <- c(Q1_words_NC,Q2_words_NC,Q3_words_NC,Q4_words_NC)



#------------------------------------------------------------------
#                           연관 분석
#------------------------------------------------------------------
# 데이터형 변환
words_tran <- as(words_NC, "transactions")


# 연관규칙 분석
words_net <- apriori(words_tran, 
                     parameter=list(support=0.01, confidence=0.20, minlen=2))


# 연관규칙 조회
words_net2 <- sort(words_net)
inspect(words_net2)


# 신뢰도 0.9이상인것만 보기
w <- subset(words_net2, confidence > 0.9)
inspect(w)


# 만약 신뢰도 0.9이상인 데이터가 의미없는 데이터면 제거
words_net2 <- subset(words_net2, confidence < 0.9)



#------------------------------------------------------------------
#                          데이터 저장
#------------------------------------------------------------------
net2017 <- words_net2
capture.output(inspect(net2017), file="science_IT_data/2017/2017_net2.txt")
save(net2017, file="science_IT_data/2017/2017_net2.RData")



#------------------------------------------------------------------
#                        데이터 불러오기
#------------------------------------------------------------------
load("science_IT_data/2007/2007_net.RData")
load("science_IT_data/2012/2012_net.RData")
load("science_IT_data/2017/2017_net.RData")



#------------------------------------------------------------------
#                        연관규칙 시각화
#------------------------------------------------------------------
plot(net2007)
plot(sort(net2017, by="confidence"), method="grouped")


ig <- plot(net2007, method="graph", control=list(type="items",max=500))
ig_df <- get.data.frame(ig, what="both")

visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name,
    value = ig_df$vertices$support,
    title = ifelse(ig_df$vertices$label == "",
                   ig_df$vertices$name, 
                   ig_df$vertices$label),
    ig_df$vertices
  ), 
  edges = ig_df$edges
) %>%
  visEdges(ig_df$edges) %>%
  visOptions(highlightNearest = T)



#------------------------------------------------------------------
#                             참고
#------------------------------------------------------------------
# http://blog.naver.com/PostView.nhn?blogId=leedk1110&logNo=220788082381
# http://rfriend.tistory.com/193
# http://gigle.tistory.com/126
# http://kateto.net/networks-r-igraph





rm(list=ls())
rm(news)
rm(Contents)
rm(words)
rm(words_pre)
rm(words_net)
rm(words_net2)
rm(words_tran)

options(max.print=99999)
