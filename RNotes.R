
############## Basics
## Handling Vector, list, Array & dataframe 
vct1 <- c(10, 14, 15, 17, 18, 20)
vct2 <- c("Adi","Bunty","Chan", "Dhir", "Eddy", "Frans")
lst1 = list(vct1, vct2)
vct3 = merge(vct1, vct2)
aray1 = array(c(vct1, vct2), dim = c(6,2))
mtr = matrix(c(vct1,vct2),6,2)

df = data.frame(vct1, vct2)
colnames(df) <- c("Age", "Name")
df1 <- data.frame("Age"= c(7,22),"Name"=c("Gini","Hilda"))
##colnames(df1) <- c("Age", "Name")
df3 <- rbind(df, df1)
df3$Country <-c ("IN", "IN","SG","IN","US","US","SG","US")
str(df3)
summary(df3)
summary(df3$Age)

tapply(df3$Age, df3$Country, mean)
plot(df3$Name,df3$Age)
hist(df3$Age)
boxplot(df3$Age ~ df3$Country)

df4 <- subset(df3, Age > 10 & Country =="IN")
write.csv(df3, "c:/BhaskarCode/df3.csv")
pie (vct1, vct2)

## While & for loop & If/Else
i = 1
while (i < 6) {
  print (vct[i])
  i=i+1
}

i <- 1
for (i in  1:6) {
  print(vct2[i])
  i = i+1
}

n = 3 
if (n < 5) {
  print ("IF")
}else {
  print ("ELSE")
}

df= read.csv("Employee.csv",TRUE,",")
df1<-read.csv(file.choose(),T)
df
df[,"salary"]
df["salary"]
df[2,3]
df[2,]
df[,5]
