

#------------------------------------------------------------------
#                      ∂Û¿Ã∫Í∑Ø∏Æ ∫“∑Øø¿±‚
#------------------------------------------------------------------
library(stringr)
library(dplyr)



#------------------------------------------------------------------
#                        µ•¿Ã≈Õ ∫“∑Øø¿±‚
#------------------------------------------------------------------
load("science_IT_data/2017_4Q_words.RData")



#------------------------------------------------------------------
#               list «¸≈¬∑Œ NC, F «¸≈¬∏∏ √ﬂ√‚
#------------------------------------------------------------------
# NC, F «¸≈¬∏∏ √ﬂ√‚ 
words_pre <- sapply(Q4_words, function(x) str_match(x, "([0-9a-zA-Z∞°-∆R]+)/(NC|F)"))


# «¸≈¬ ∫–ºÆ ∏¯«— «‡ ∞…∑Ø≥ª±‚
c <- c()
for (i in 1:length(words_pre)) {
  words <- words_pre[[i]]
  tryCatch(w <- words[,2],
           error=function(e) {
             c[length(c)+1] <<- i
             message(e)
           })
}
words_pre <- words_pre[-c]


# NC, F ø‹¿« «¸≈¬ ¡¶∞≈
words_NC <- sapply(words_pre, function(x) Filter(function(y) !is.na(y), x[,2]))



#------------------------------------------------------------------
#                     ∞∞¿∫ ¿«πÃ¿« ¥‹æÓ ≈Î¿œ
#------------------------------------------------------------------
words_NC <- sapply(words_NC, toupper)
words_NC <- sapply(words_NC, function(x) gsub("∞∂S","∞∂∑∞Ω√S", x))
words_NC <- sapply(words_NC, function(x) gsub("(∞∂∑∞Ω√S[0-9]).*","\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("^(S[0-9]).*","∞∂∑∞Ω√\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("∞∂≥Î∆Æ", "∞∂∑∞Ω√≥Î∆Æ", x))
words_NC <- sapply(words_NC, function(x) gsub("(∞∂∑∞Ω√≥Î∆Æ[0-9]).*","\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("^(≥Î∆Æ[0-9]).*","∞∂∑∞Ω√\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("(G[0-9]{1,2}).", "\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("(V[0-9]{1,2}).", "\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("(æ∆¿Ã∆˘[0-9]).", "\\1", x))
words_NC <- sapply(words_NC, function(x) gsub("∏πŸ¿œø˘µÂƒ·±◊∑πΩ∫", "MWC", x))
words_NC <- sapply(words_NC, function(x) gsub("≥™ªÁ", "NASA", x))
words_NC <- sapply(words_NC, function(x) gsub("¿Ø¿¸¿⁄", "DNA", x))
words_NC <- sapply(words_NC, function(x) gsub("»ﬁ¥Î¿¸»≠", "»ﬁ¥Î∆˘", x))
words_NC <- sapply(words_NC, function(x) gsub("¡§∫∏≈ÎΩ≈±‚º˙", "ICT", x))
words_NC <- sapply(words_NC, function(x) gsub("DRONE", "µÂ∑–", x))
words_NC <- sapply(words_NC, function(x) gsub("∑’≈“ø°∫º∑Áº«", "LTE", x))
words_NC <- sapply(words_NC, function(x) gsub("ø§∆º¿Ã", "LTE", x))
words_NC <- sapply(words_NC, function(x) gsub("4¬˜ªÍæ˜«ı∏Ì.", "4¬˜ªÍæ˜«ı∏Ì", x))
words_NC <- sapply(words_NC, function(x) gsub("æÀ∆ƒ∞Ì.", "æÀ∆ƒ∞Ì", x))
words_NC <- sapply(words_NC, function(x) gsub("πﬂªÁ∞°", "πﬂªÁ", x))
words_NC <- sapply(words_NC, function(x) gsub("MP3«√∑π¿ÃæÓ", "MP3", x))
words_NC <- sapply(words_NC, function(x) gsub("¡ˆªÛ∆ƒDMB", "DMB", x))
words_NC <- sapply(words_NC, function(x) gsub("¿¸¿⁄Ω≈πÆ¿Œ≈Õ≥›", "¿¸¿⁄Ω≈πÆ", x))
words_NC <- sapply(words_NC, function(x) gsub("ªÁπ∞¿Œ≈Õ≥›", "IOT", x))
words_NC <- sapply(words_NC, function(x) gsub("∞°ªÛ«ˆΩ«", "VR", x))
words_NC <- sapply(words_NC, function(x) gsub("¡ı∞≠«ˆΩ«", "AR", x))
words_NC <- sapply(words_NC, function(x) gsub("¿Ø±‚πﬂ±§¥Ÿ¿Ãø¿µÂ", "OLED", x))
words_NC <- sapply(words_NC, function(x) gsub("AI", "¿Œ∞¯¡ˆ¥…", x))


#------------------------------------------------------------------
#                 ∫“øÎæÓ ∏ÆΩ∫∆Æø° ¿÷¥¬ ¥‹æÓ ¡¶∞≈
#------------------------------------------------------------------
# ∫“øÎæÓ ∏ÆΩ∫∆Æ ∫“∑Øø¿±‚
remove_list <- read.table("science_IT_data/remove_list.txt", header=T)
remove_list <- unlist(remove_list)
remove_list <- as.character(remove_list)


# ∫“øÎæÓ ªË¡¶
remove_list <- sort(remove_list, decreasing=T)
r <- length(remove_list)

for(i in 1:r){
  word <- remove_list[i]
  
  words_NC <- sapply(words_NC, function(x) gsub(word,"", x))
  words_NC <- sapply(words_NC, 
                     function(x) Filter(function(y) {nchar(y) >= 1}, x))
  
  cat(i,".",word,"remove.\n")
}



#------------------------------------------------------------------
#                        µ•¿Ã≈Õ ¿˙¿Â
#------------------------------------------------------------------
Q4_words_NC <- words_NC
save(Q4_words_NC, file="science_IT_data/2017/2017_4Q_words_NC.RData")



#------------------------------------------------------------------
#                ¿”Ω√¿˙¿Â«— µ•¿Ã≈Õ ∫“∑Øø¿±‚
#------------------------------------------------------------------
load("science_IT_data/2017/2017_4Q_words_NC.RData")
words_NC <- Q4_words_NC



rm(Q1_words)
rm(Q1_words_NC)
rm(list=ls())
