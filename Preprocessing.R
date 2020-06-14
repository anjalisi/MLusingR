#Importing in datafinal
df <- read.csv("E:/MLUsingR/House_Price.csv",header = TRUE)
View(df)
#Gives the structure of dataFrame
str(df)

#Getting the EDD
summary(df)

#crime_rate,n_hos_rooms, rainfall either has outliers, NA values or skweness
hist(df$crime_rate)
#we see a huge difference.Not a correct desc, so we use scatter plots
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df)

# now check barplots for categorical value columns like airport, bus_term & water_body
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))

#ignore this categorical var, bus_ter

#OBSERVATIONS
#1. n_hot_rooms & ranifall has outliers
#2. n_hos_beds has missing values
#3. bus_term is a useless variable
#4. crime_rate has not a linear relationship with price

#HANDLING OUTLIER USING CAPING AND FLOORING
#upper: 3*99percentile & lower to 0.3*1st%ile

quantile(df$n_hot_rooms, 0.99)
upper_v= 3*quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms>upper_v]<- upper_v #all values greater get the value same as that of upper value
x<-2
summary(df$n_hot_rooms)
#mean and median are now closer

#Doing the same for rainfall. it needs a lower binding.
lower_val=0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall <lower_val] <- lower_val

summary(df$rainfall)
#WE see we handle the outliers now. 
#Now we need to handle the missing values. This can create a lot of errors.
#We can handle them, by either 
#1.removing the whole row
#2.fill the missing values with the mean or median value.
#3. Impute the values by adding 0 

#HANDLING THE MISSING VALUES IN N-HOS-BEDS
mean(df$n_hos_beds) #we need to change this

mean_calc=mean(df$n_hos_beds,na.rm = TRUE) #while calc the mean we remove NA
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean_calc
summary(df$n_hos_beds)

which(is.na(df$n_hos_beds)) #HERE WE SEE NO NA NOW

#We need to transform the crime_rate variable so it has a more linear relationship with price

#initially we can do this by two methods, 1. Pairs
pairs(~price+crime_rate,data = df)
#or 2. plots
plot(df$price, df$crime_rate)
 #the curve looks like a log func, which is translated and rotated.

#TRANSFORMING
df$crime_rate <- log(1+df$crime_rate)

plot(df$price, df$crime_rate)

#TRANSORMATION OF THE 4 DISTANCE VARIABLE AND CREATE INTO ONE AVG DIST

df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4

#remove these 4 columns using their indeces, we make these changes in a new df
df2 <- df[,-7:-10]

View(df2)
df <- df2
rm(df2)

#Removing the bus_term var as it only contains YES
df<- df[,-14]
View(df)

#PREPROCESSING IS COMPLETED NOW.

#CHANGING OF THE NON-NUMERICAL VALUES TO NUMBERS
#Use Dummy Variables.

#airport: yes and no
#waterbody: river,lake, none, both
install.packages("dummies")

df<- dummy.data.frame(df)
View(df)
#Creates airportYES, airportNO, water_bodyLake, etc.
#now we need to delete the vars waterbody_none & airportNO
df<- df[,-9]
df<-df[,-14]
View(df)

#CORRELATION MATRIX
typeof(df)
cor(df)
round(cor(df),2) #just to make it readable

#variables like waterbodies, n_hot_rooms have very less value of coeff. 
#So they, dont have any effect as such and can be deleted later on.

#We notice that independent vars like, parks & air_qual are highly correlated.
#we have to remove one to avoid multicollinearity.
#Parks v/s price < air_qual v/s price
#Hence, we will delete parks.

df<- df[,-16]

#LINEAR REGRESSION MODEL
simple_model<- lm(price~room_num,data=df) 
#here, lm is linear model (dependent var~indep var, data frame)
summary(simple_model)
plot(df$room_num,df$price)
abline(simple_model) #shows our line on the model


#MULTIPLE REGRESSION MODEL
multiple_model= lm(price~., data=df)
summary(multiple_model)

#TRAIN AND TEST SPLIT
install.packages("caTools")
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)

#Splits into True and False
training_set=subset(df,split==TRUE) #all the true values stored in taining set
test_set= subset(df,split==FALSE)

#Now run a linear model on the training data set
lm_a=lm(price~.,data = training_set)

#Now we will calculate the MSE 
#a. First predict the value of price(use predict() function)
train_predict=predict(lm_a,training_set)
test_predict=predict(lm_a,test_set)

mean((training_set$price- train_predict)^2) #true value-predicted val
mean((test_set$price- test_predict)^2) #true value-predicted val
  
#NOW WE WILL USE OTHER TRAINING TECHNIQUES


#1. SUBSET SELECTION TECHNIQUE
install.packages("leaps")

#a. best subset selection technique
lm_best= regsubsets(price~., data=df)
#it will only run upto 8 variables, not more than that

lm_best= regsubsets(price~., data=df,nvmax = 15)
summary(lm_best)

summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
coef(lm_best,8) #to see the 8th val

#b. forward selection
lm_forward=regsubsets(price~., data=df,nvmax = 15,method = "forward")
summary(lm_forward)$adjr2
which.max(summary(lm_forward)$adjr2)
coef(lm_forward,8)
coef(lm_best,8) #for both forward and best, the values are coming out to be the same 

#c. backward selection
lm_back=regsubsets(price~., data=df,nvmax = 15,method = "backward")
which.max(summary(lm_back)$adjr2)
coef(lm_back,8)

#SHRINKAGE METHOD
install.packages("glmnet")

#segregating the dependent and independent var
independent= model.matrix(price~.,data = df)[,-1] #-1 removes the price col as it is dependant
dependent=df$price

#now we have to find out lambda
grid=10^seq(10,-2,length=100)
grid

#Now put dep, indep and lambda in model
lm_ridge=glmnet(independent,dependent,alpha = 0,lambda = grid)
summary(lm_ridge)

#now find the best lambda
cv_fit=cv.glmnet(independent,dependent,alpha = 0,lambda = grid)
plot(cv_fit)

#now getting the min labda
optimum_lamb=cv_fit$lambda.min
tss= sum((dependent-mean(dependent))^2)

#Find predicted val of price to get RSS later
price_pred= predict(lm_ridge,s = optimum_lamb,newx = independent)

#Now calculate rss
rss=sum((price_pred-dependent)^2)

#Now calc R^2
rsq=1-(rss/tss)
#Approx 72%

#Now for Lasso Shrinkage method
lm_lasso=glmnet(independent,dependent,alpha = 1,lambda = grid)
#Other steps remain same
#Compare both the rsq to compare.