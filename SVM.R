#SVM FOR CLASSIFICATION


#IMPORTING OUR DATA SET

movie <- read.csv("E:/CompleteMLinR/SupportVectorMachines/Movie_classification.csv", header = TRUE)

#Data preprocessing
summary(movie)
#We see that time has null values
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken, na.rm=TRUE)
#Filling all the null vlaues with the mean values.

#TEST-TRAIN SPLIT
set.seed(0)
split= sample.split(movie, SplitRatio = 0.8)

trainc = subset(movie, split==TRUE)
testc = subset(movie, split==FALSE)

#CLASSIFICATION OF SVM
#We need to change the bool/numeric to factor values so our model can do classification
trainc$Start_Tech_Oscar = as.factor(trainc$Start_Tech_Oscar)
testc$Start_Tech_Oscar = as.factor(testc$Start_Tech_Oscar)

#Importing the packages
install.packages('e1071')

#making the training model(LINEAR KERNEL)
svmfit= svm(Start_Tech_Oscar~., data= trainc, kernel= "linear", cost=1, scale= TRUE)
summary(svmfit)

#Prediction on the test set
ypred= predict(svmfit, testc)
table(predict= ypred, truth= testc$Start_Tech_Oscar)
66/107 #It is 52% accurate

#To check the support vectors
svmfit$index

#Initially we took C=1, but the best value of C might be different. WE need to find that out.
set.seed(0)
tune.out = tune(svm,Start_Tech_Oscar~., data = trainc, kernel="linear", ranges = list(cost= c(0.001, 0.01, 0.1, 1, 10, 100)))

#now we need to fetch the best model
bestmod=tune.out$best.model
summary(bestmod) #Turns out to be the same where cost=1
#So, it is the same as svmfit.



#POLYNOMIAL KERNEL
svmfitp= svm(Start_Tech_Oscar~., data= trainc, kernel= "polynomial", cost=1, degree=2)

#Prediction on test set

ypred1= predict(svmfitp, testc)
table(predict= ypred1, truth= testc$Start_Tech_Oscar)
#The accuracy is:
64/107 #approx 60%

#Finding the best value of Hyperparam, degree
tune.out= tune(svm, Start_Tech_Oscar~., data = trainc, cross=5, kernel="polynomial", ranges = list(cost=c(0.001, 0.1, 1,2,5,10), degree=c(0.5,1,2,3,4,5)))
bestmodp=tune.out$best.model
summary(bestmodp) #Turns out to be the same where cost=1
ypred2= predict(bestmodp, testc)
table(predict= ypred2, truth= testc$Start_Tech_Oscar) #Gives 62%accuracy



#RADIAL KERNEL
svmfitR= svm(Start_Tech_Oscar~., data= trainc, kernel= "radial", gamma=1, cost=1)
tune.out=tune(svm, Start_Tech_Oscar~., data = trainc, kernel="radial", ranges = list(cost=c(0.001,0.01, 0.1, 1,10,100,1000), gamma=c(0.01,0.1,0.5,1,3,10,50)), cross=5)
bestmodr=tune.out$best.model
summary(bestmodr) #Turns out to be the same where cost=1
ypredr= predict(bestmodr, testc)
table(predict= ypredr, truth= testc$Start_Tech_Oscar) #Gives 54%accuracy

