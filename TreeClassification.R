rm(list = ls())
movie1 <- read.csv("E:/MLUsingR/Movie_classification.csv",header = TRUE)

View(movie1)

#DATA PREPROCESSING
summary(movie1)
#time taken has NA values, remove that
mean(movie1$Time_taken, na.rm=TRUE)
which(is.na(movie1$Time_taken))
movie1$Time_taken[is.na(movie1$Time_taken)] <- mean(movie1$Time_taken, na.rm=TRUE)

#Train-test split
set.seed(0)
split=sample.split(movie1, SplitRatio = 0.8)
trainc=subset(movie1, split==TRUE)
testc=subset(movie1, split==FALSE)

#Bulding a Regression tree
install.packages('rpart')
install.packages('rpart.plot')
library('rpart.plot')

#Run regressionn tree model on training set
classtree<- rpart(formula = Start_Tech_Oscar~.,data = trainc, method = 'class',control = rpart.control(maxdepth = 3))

#Plot the decision tree
#rpart.plot(classtree,box.palette = "RdBu",digits = 0)#gives in scientific format
rpart.plot(classtree,box.palette = "RdBu",digits = -3)

#Predict value at any point(use test set)
testc$predict<-predict(classtree,testc,type = "vector")

#Checking table
table(testc$Start_Tech_Oscar,testc$predict)
accuracy=68/108
#Calc MSE
mse<-mean((testc$predict-testc$Start_Tech_Oscar)^2)
mse


#ADA BOOSTING
trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar) #changing numeric to factor type, as we cant work on numeric type (0,1)

adaboost <- boosting(Start_Tech_Oscar~., data= trainc, boos=TRUE)#add mfinal=1000

#Predicting value on the test data
predada <- predict(adaboost, testc)
table(predada$class, testc$Start_Tech_Oscar)
74/113

#Plotting the first tree
t1<- adaboost$trees[[1]]
plot(t1)
text(t1, pretty = 100) #adding test to this tree


#XGBOOST
install.packages("xgboost")

#here, we have to make multiple test and training set
#the value removed should be boolean
trainY= trainc$Start_Tech_Oscar=="1"

#We need to convert all the categorical variables to dummy var(numeric type)
trainX <- model.matrix(Start_Tech_Oscar~., -1, data = trainc)
View(trainX)

trainX
