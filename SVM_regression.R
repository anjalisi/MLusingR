df <- read.csv("E:/CompleteMLinR/SupportVectorMachines/Movie_regression.csv", header = TRUE)

#We have to find the total box office collection

#DATA PREPROCESSING
summary(df)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm=TRUE)
summary(df)

#TEST-TRAIN SPLIT
set.seed(0)
split=sample.split(df, SplitRatio = 0.8)
train = subset(df,split==TRUE)
test = subset(df,split==FALSE)

#importing the relevant libraries
svmfit=svm(Collection~.,data=train, kernel="linear",cost=0.01,scale=TRUE)
summary(svmfit)

#now test
ypred= predict(svmfit, test)
MSE <- mean((ypred-test$Collection)^2)

#FOR RADIAL
svmfitR=svm(Collection~.,data=train, kernel="radial",cost=0.01, gamma=1, scale=TRUE)
summary(svmfitR)

#now test
ypredR= predict(svmfitR, test)
MSE <- mean((ypredR-test$Collection)^2) #Comes out to be more than radial


