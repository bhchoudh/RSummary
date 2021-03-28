### Apriori & Market basket analysis
library(arules)
library(arulesViz)
## Load file as transaction not dataframe
ts <- read.transactions("c:/BhaskarCode/groceries.csv")
rules <- apriori(ts,parameter = list(conf =.5, supp=.01))
rules<-sort(rules,by = "lift")
inspect(rules)
summary(rules)
plot(rules)
## rules involving a specific items in basket
rules <- apriori(ts,parameter = list(conf =.5, supp=.001), appearance = list(lhs="citrus"))
rules<-sort(rules,by = "lift")
inspect(rules)
##################################


###### Clustering 
library(stats)
library(cluster)
## K-means Clustering
df <- read.csv("c:/BhaskarCode/admission.csv")
df <- read.csv("c:/BhaskarCode/winequalitydecision.csv")
df <- read.csv("c:/BhaskarCode/Universities_Clustering.csv")
## Example 1 - creating new DF with specific columns
df1 <- data.frame(df$gre,df$gpa)
km <- kmeans(df1, 5)
plot(df.gpa~df.gre,df1, col=km$cluster)

## Example 2 - removing redundant column from existing DF
df$Univ <- NULL
df$quality <- NULL   
head(df)
df <- scale(df)           
km <- kmeans(df, 5)
km$cluster
plot(density~alcohol,df, col=km$cluster)
plot(GradRate~Accept,df, col=km$cluster)
plot(df,col=km$cluster)
clusplot(df, km$cluster, frame=TRUE , color = TRUE, shade = TRUE)

## Agglomerate Hierarchial Clustering
df <- read.csv("c:/BhaskarCode/iris.csv")
df$Species <- NULL      
df$Id <- NULL
df <- scale(df) 
distance <- dist(df, method = 'euclidian')
clust <- hclust(distance,method = "complete")
plot(clust)
clustgroup <-cutree (clust,5)
plot(clustgroup)


## Time Series 
install.packages("forecast")
library(forecast)
df <- read.csv("c:/BhaskarCode/GDPTS.csv")
## Make the data as Time Series with time frequency & start
tsdata <-ts(df$GDP, frequency = 12, start = c(1984,1))
plot (tsdata)
arimafit <- auto.arima(tsdata)
arimafit
fcast <- forecast(arimafit, h=20)
plot(fcast)
plot(fcast$residuals)
accuracy(arimafit)
acf(fcast$residuals)
pacf(fcast$residuals)
qqnorm(fcast$residuals)

