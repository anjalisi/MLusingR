x<-2
x<-3
x
y<-c(1,2,3,4,5)
z<-1:10
x<-y<-1:10
z<-x+y
z2<-x*y
X<-10 #case sensitive

ls() #gives all the var in our workspace
rm(x) #removes the x
remove(z2)
rm(list=ls()) #removes everything from workspace

iris
data("iris")
x1<-1:10
x3<-seq(5,50,by=5)
x4<- scan()

#import data from txt file to workspace
Product <- read.table("C:/something/url/Product.txt",header=TRUE,sep = "\t")
str(Product) #for seeing the structure

#reading csv data
Customer <- read.csv("E:/MLUsingR/Customer.csv",header=TRUE)
View(Customer) #creates a view of the data in a new tab in tabular form

#frequency distribution for Region column
y<- table(Customer$Region)
y
#it shouws us the frequency dist
View(y)

#creating a bar/graph of frequency
barplot(y)
#Arranging the barplot in ascending order of height
barplot(y[order(y)])
#Arranging in the descending order
barplot(y[order(-y)])
#changing the orientation to horizontal
barplot(y[order(y)],horiz = TRUE)
#Changing the colour in the barplot
barplot(y[order(y)],horiz = TRUE,col="red")
barplot(y[order(y)],horiz = TRUE,col=c("red","yellow","blue","green"))

#Removing the border
barplot(y[order(y)],horiz = TRUE,col=c("red","yellow","blue","green"),border = NA)

#Adding a title to the chart
barplot(y[order(y)],horiz = TRUE,col=c("red","yellow","blue","green"),border = NA,main="Frequency Distribution \n along Region")

#Adding a title to the x-axis
barplot(y[order(y)],horiz = TRUE,col=c("red","yellow","blue","green"),border = NA,main="Frequency Distribution \n along Region",xlab = "Number of Customers")


#CREATING A HISTOGRAM
hist(Customer$Age)
#selecting or suggesting 5 bars
hist(Customer$Age,breaks = 5)
#changing scale at X-axis
hist(Customer$Age,breaks = c(0,40,60,100)) #it provides density, we want freq
hist(Customer$Age,breaks = c(0,40,60,100),freq = TRUE)

colors()

#changing the color of the histograph
hist(Customer$Age,breaks = c(0,40,60,100),freq = TRUE,col = c("violetred","turquoise","salmon","orange"))

#Adding a title
hist(Customer$Age,breaks = c(0,40,60,100),freq = TRUE,col = c("violetred","turquoise","salmon","orange"),main = "Histogram of Age",xlab = "Age")
