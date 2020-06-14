df <- read.csv("E:/House-Price.csv",header = TRUE)

summary(df)
boxplot(df$n_hot_rooms)
pairs(~df$Sold+df$rainfall)

barplot(table(df$airport))
barplot(table(df$bus_ter))

#1. 2 vars have outliers: n_hot_rooms, rainfall
#2. bus_term is useless
#3. n_hos_beds has missing values

#REMOVING OUTLIERS
quantile(df$n_hot_rooms, 0.99)
uv = 3*quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms> uv] <- uv

summary(df$n_hot_rooms)

lv = 0.3*quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall<lv] <-lv
summary(df$rainfall)

#MISSING VALUES

mean(df$n_hos_beds) #gives NA because of NA values
mean(df$n_hos_beds,na.rm = TRUE) #we remove NA
which(is.na(df$n_hos_beds)) #Getting indeces of NA values
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE)
#Here, we add the mean values to NA

#VARIABLE TRANSFORMATION
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4
df2 <- df[,-6:-9]
df <- df2
rm(df2)

df <-df[,-13] #removed bus_term

#CREATING DUMMY VARIABLES(for categorical values)
df<- dummy.data.frame(df)
View(df)

df<- df[,-8]
df <- df[,-13]

#LOGISTIC REGRESSION USING SINGLE PREDICTOR

glm.fit= glm(Sold~price,data = df,family = binomial)
summary(glm.fit)


#LOGISTIC REGRESSION USING MULTIPLE PREDICTOR

glm.fit= glm(Sold~.,data = df,family = binomial)
summary(glm.fit)


glm.prob=predict(glm.fit,type = "response")
glm.prob[1:10]

#Now we will use boundary condition to assign classes
#1. create array of 506 with all values "NO"
glm.pred=rep("NO",506)
#2. All values with boundary value>0.5 will be changes to "YES"(assigns class)
glm.pred[glm.prob>0.5]="YES"
#3. Create the confusion matrix
table(glm.pred,df$Sold)


#LINEAR DISCRIMINANT ANALYSIS
lda.fit=lda(Sold~.,data = df)
lda.fit
lda.pred=predict(lda.fit,df)
lda.pred$posterior
lda.class=lda.pred$class #to classify
#now create confusion matrix
table(lda.class,df$Sold)
#Change the boundary condition to 0.8
sum(lda.pred$posterior[,1]>0.8)
  

#DIVIDING THE DATA INTO TRAIN-TEST SPLIT
set.seed(0)
split=sample.split(df, SplitRatio = 0.8)

#Splits into True and False
training_set=subset(df,split==TRUE) #all the true values stored in taining set
test_set= subset(df,split==FALSE)
#WE ARE USING LODGISTIC REGRESSION
train.fit=glm(Sold~.,data = training_set, family = binomial)
test_probs=predict(train.fit,test_set,type="response")

test.pred=rep('NO',120) #since test set has 120 obs
test.pred[test_probs >0.5]='YES'
#Now create a confusion matrix
table(test.pred, test_set$Sold)


#KNN CLASSIFIER
#step1: sepearte the independent data(every col except Sold) from the training set & data set.
trainX=training_set[,-16]
testX=test_set[,-16]

trainY=training_set$Sold
testY=test_set$Sold

#Definine k 
k=3
#Standardise the X val
trainX_s=scale(trainX)
testX_s=scale(testX)

set.seed(0)

#now define knn
knn_pred=knn(trainX_s,testX_s,trainY,k=k)
#Create confusion tab
table(knn_pred,testY)

#changing value of k=1
k=1
knn_pred=knn(trainX_s,testX_s,trainY,k=k)
#Create confusion tab
table(knn_pred,testY)
