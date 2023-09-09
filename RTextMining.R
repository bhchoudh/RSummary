#Sentiment Analysis
library(syuzhet)
library(dplyr)

##overall Sentiment by  by lines from document  
textline <- readLines("C:/BhaskarCode/Demonetisation_Sentiment.txt")
senti <- get_sentiment(textline)
senti <- data.frame(Lines=c(1:length(textline)),Sentiment=senti)
senti
plot(senti)

## Granular Sentiments by category by lines from document
SentiNRC <- get_nrc_sentiment(textline)
SentiNRC
Sentiment <-cbind(Index=c(1:9),SentiNRC)
Sentiment
plot(x=Sentiment$Index,y=Sentiment$negative,type = "s",col="red")

#count the sentiment words by types of sentiment
TotalSenti <- data.frame(SentiName=colnames(SentiNRC),SentiCount = colSums(SentiNRC))
TotalSenti
plot(x=TotalSenti$SentiName,y=TotalSenti$SentiCount,type="l")

#total sentiment score of all texts
##ggplot(data = TotalSenti, aes(x = SentiName, y = SentiCount)) +   geom_bar(aes(fill = SentiName), stat = "identity") 



###############################################

library(tm)
##library(SnowballC)
##library(RColorBrewer)
##library(ggplot2)
##library(wordcloud)
##library(biclust)
##library(igraph)
##library(fpc)



filenames <-list.files("C:/BhaskarAcademic/EFPM-Term2/BA/Session 19&20-TA/texts")
corp <- c()
for (fname in filenames){
  fname <-paste("C:/BhaskarAcademic/EFPM-Term2/BA/Session 19&20-TA/texts/",fname, sep = "")
  fread <- readLines(fname)
  fread <- paste(fread, collapse = " ")
  ##writeLines(fread)
  corp <- c(corp,fread)
}

corp
docs <-VCorpus(VectorSource(corp))
##cname <- file.path("C:/BhaskarAcademic/EFPM-Term2/BA/Session 19&20-TA", "texts")
##dir(cname)
##docs <- VCorpus(DirSource(cname))
summary(docs)
writeLines(as.character(docs))

#Preprocessing
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("English"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, stemDocument)

#Documet Term Matrix and Term Document Matrix
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
dim(m)
freq <- colSums(m)
length(freq)
ord <- order(freq)



write.csv(m, "C:/BhaskarAcademic/EFPM-Term2/BA/Session 19&20/DocumentTermMatrix.csv")
dtms <- removeSparseTerms(dtm, 0.30)
ms <- as.matrix(dtms)
freq_new <- colSums(ms)
ord_new <- order(freq_new)
freq_new

#Plot the Bart Chart
wf <- data.frame(word = names(freq_new), freq = freq_new)
p <- ggplot(wf, aes(word, freq_new))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.title.x = element_text(angle = 45, hjust = 1))
p

#Plot the wordcloud
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq_new), freq_new, max.words = 100, rot.per = 0.20, colors = dark2)

#9/9/23
#Cosine using textTinyR package 
install.packages("textTinyR")
library(textTinyR)
text1 <- "i like to write code in python"
text2 <- "java is a better language than python"
COS_TEXT(text_vector1 = text1, text_vector2 = text2, separator = " ")

#Cosine using lsa package
installed.packages("lsa")
library(lsa)

# Cosine Option 1 with single matrix
c1 <- c("i like to write code in java")
c2 <- c("i like to write code in python")
vec <- cbind(c1, c2)
veccorp <-VCorpus(VectorSource(vec))
dtm <- DocumentTermMatrix(veccorp)
vecmat <- as.matrix(dtm)
cosine(vecmat)

## Cosine option2 with 2 vector - not working
l1 <-strsplit("i like to write code in java", " ")
l2 <-strsplit("i like to write code in python", " ")
dtm <- DocumentTermMatrix(veccorp)
dtm1 <- DocumentTermMatrix(VCorpus(VectorSource(l1)))
dtm2 <- DocumentTermMatrix(VCorpus(VectorSource(l2)))
cosine(dtm1, dtm2)
