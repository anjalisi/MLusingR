movie <- read.csv("E:/MLUsingR/Movie_regression.csv",header = TRUE)

#DATA PREPROCESSING
summary(movie)
#time taken has NA values, remove that
mean(movie$Time_taken, na.rm=TRUE)
which(is.na(movie$Time_taken))
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken, na.rm=TRUE)

#Train-test split
set.seed(0)
split=sample.split(movie, SplitRatio = 0.8)
train=subset(movie, split==TRUE)
test=subset(movie, split==FALSE)

#Bulding a Regression tree
install.packages('rpart')
install.packages('rpart.plot')
library('rpart.plot')

#Run regressionn tree model on training set
regtree<- rpart(formula = Collection~.,data = train, control = rpart.control(maxdepth = 3))

#Plot the decision tree
rpart.plot(regtree,box.palette = "RdBu",digits = 0)#gives in scientific format
rpart.plot(regtree,box.palette = "RdBu",digits = -3)

#Predict value at any point(use test set)
test$predict<-predict(regtree,test,type = "vector")

#Calc MSE
mse<-mean((test$predict-test$Collection)^2)
mse


#TREE PRUNING 
fulltree <- rpart(formula = Collection~.,data = train, control = rpart.control(cp=0))
rpart.plot(fulltree, box.palette = "RdBu",digits = -3)
printcp(fulltree)
plotcp(regtree) #We need to find where relative error is min

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(regtree,cp=mincp)
rpart.plot(regtree, box.palette = "RdBu",digits = -3)

#Now testing and checking if fulltree was better or prunedtree
test$fulltree <- predict(fulltree, test, type = "vector")
mse2<-mean((test$fulltree-test$Collection)^2)

#Now checking for pruned tree
test$pruned <- predict(fulltree, test, type = "vector")
mse3<-mean((test$pruned-test$Collection)^2)


#BAGGING
install.packages('randomForest')
set.seed(0)
bagging=randomForest(formula=Collection~.,data=train,mtry=17)
test$bagging <- predict(bagging, test)

#Check mse
mseBag <- mean((test$bagging- test$Collection)^2)




#RANDOMFOREST
randomFor <- randomForest(formula=Collection~.,data=train,ntree=500)
test$randomfor <- predict(randomFor, test)

#Check mse
mseForest <- mean((test$randomfor- test$Collection)^2)



#BOOSTING
install.packages('gbm')
grad_boosting= gbm(Collection~. data = train, distribution = "gaussian", n.trees = 5000,interaction.depth = 4, shrinkage = 0.2, verbose = F )
boosting = gbm(Collection~Budget+Trailer_views, data = train, distribution="gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)

test$boost = predict (boosting, test,n.trees =5000)
MSE2boost <- mean((test$boost - test$Collection)^2)

#ADABOOST(only on classification trees)
install.packages('adabag')
