install.packages("wordcloud")
#install.packages("KoNLP")
install.packages("RColorBrewer")

#an package for Chiense text analysis
install.packages("chinese.misc")
install.packages("quanteda")


#speech
library(Rwordseg)
#library(KoNLP)
library(RColorBrewer)
library(wordcloud)

library(quanteda)

#prepare brewwer
pal2<-brewer.pal(8,"Dark2")

text<-readLines(file.choose(),encoding = "UTF-8")

temp<-segmentCN(text)

#stopwords in Chinese
ch_stop<-stopwords("zh",source="misc")

#removeStopWords

removeStopWords<-function(x, stopwords){
  temp <- character(0)  
  index <- 1  
  xLen <- length(x)  
  while (index <= xLen) {  
    if (length(stopwords[stopwords==x[index]]) <1)  
      temp<- c(temp,x[index])  
    index <- index +1  
  }  
  temp  
}


noun<-lapply(temp, removeStopWords, ch_stop)

noun2<-unlist(noun)


#count word frequnce
word_count<-table(noun2)
word_count

wordsXi<-head(sort(word_count,decreasing = T),100)
#plot wordcloud
wordcloud(names(word_count),freq = word_count,scale = c(3,0.3),min.freq = 3,random.order = F,rot.per = .1,colors = pal2)


