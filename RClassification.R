##File read & Split data in test & Train
df <- read.csv("c:/BhaskarCode/winequalitydecision.csv")
df <- read.csv("c:/BhaskarCode/admission.csv")
df <- read.csv("c:/BhaskarCode/iris.csv")
df<-read.csv(file.choose(),T)
df <- scale(df)
head(df)
str(df)
df$rank <- as.factor(df$rank)
df$admit <- as.factor(df$admit)

##library(caTools)
set.seed(88)
split <- sample.split(df, SplitRatio = 0.99)
traindata <- subset(df, split == TRUE)
testdata <- subset(df, split == FALSE)
str(testdata)
str(traindata)

##Neural Network 
install.packages("neuralnet")
library(neuralnet)
nnmodel <- neuralnet(admit ~.,data = traindata, hidden = 3,linear.output = FALSE)
plot(nnmodel)
nnpred <- compute(nnmodel, testdata[,-1])   ## remove y variable from test
nnpred$net.result
str(nnpred$net.result)
head (nnpred$net.result)                    ##array of array 
head(testdata)

dfcomp <- data.frame(testdata$admit ,round(nnpred$net.result))
head(dfcomp)
table(testdata$admit,round(nnpred$net.result))
table(testdata$quality,ifelse(nnpred$net.result > .5, 1,0))


## Decision tree / RPART, CART, Random Forest
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
treefit<- rpart(quality ~ chlorides+pH+sulphates+alcohol,data = traindata, method = "class", minbucket = 10)
prp(treefit)
rpart.plot(treefit,extra=3)
predTree<-predict(treefit,testdata,type="class")
predTree
str(predTree)
summary(predTree)
table(testdata$quality,predTree)
dfcomp <- data.frame(testdata$quality ,predTree)
head(dfcomp)
plot(predTree)


## Decision tree using random foest 
install.packages("randomForest")
library(randomForest)
traindata$quality <- as.factor(traindata$quality)
testdata$quality <- as.factor(testdata$quality)
myForest <- randomForest(quality ~ chlorides+pH+sulphates+alcohol,data = traindata,ndesize =20, ntree =20)
predForest<-predict(myForest,testdata)
predForest
str(predForest)
summary(predForest)
table(testdata$quality,predForest)
dfcomp <- data.frame(testdata$quality ,predTree)
head(dfcomp)

## Decision tree using party package (not preferred)
##install.packages("party")
##library(party)
##mytree<- ctree(quality ~ chlorides+pH+sulphates+alcohol,data = traindata)
##plot(mytree)
##pred <- predict(mytree,testdata,type="class")
##pred
##table(testdata$quality,pred) ## compare observed vs predicted


## Logistic Regression for 2 class classification
##testdata <- data.frame(gre=790,gpa=3.8,rank=as.factor(1))
logit <- glm(admit ~ .,data=df,family="binomial")
summary(logit)
predictdata <- predict(logit,testdata)
predictdata
summary (predictdata)
head(predictdata)
dfcomp <- data.frame(testdata$admit,predictdata)
head(dfcomp)
dfcomp
table(testdata$admit,ifelse(predictdata > .4, 1,0))

## Discriminant analysis - multi class classification
library("MASS")
ldamodel <- lda(admit ~ .,data=df)
ldamodel
summary(ldamodel)
predictdata <- predict(ldamodel,testdata)
predictdata
dfcomp <- data.frame(testdata$admit ,predictdata$class)
head(dfcomp)
dfcomp
table(testdata$admit,predictdata$class)

## Simple regression 
dftrain <- read.csv("c:/BhaskarCode/RegBMITrain.csv",header = T)
dftrain$id <-NULL
cor(dftrain)
regmodel <- lm(dftrain$BMI ~., data = dftrain)
summary(regmodel)
dftest <- read.csv("c:/BhaskarCode/RegBMITest.csv",header = T)
dftest$id = NULL
regpredict <- predict(regmodel,dftest )
regpredict

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

##plot residuals to check autocorelation
plot(fcast$residuals)
accuracy(arimafit)
acf(fcast$residuals)
pacf(fcast$residuals)
qqnorm(fcast$residuals)
