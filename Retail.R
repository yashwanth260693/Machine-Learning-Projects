#load packages
library(dplyr)
library(VIM)
library(ggplot2)
library(corrplot)
library(GGally)
library(randomForest)
library(ROCR)

#import data
Features.data.set <- read.csv("C:/Users/Dell/Desktop/New folder/Kaggle/retail-data-analytics/Features data set.csv")
sales.data.set <- read.csv("C:/Users/Dell/Desktop/New folder/Kaggle/retail-data-analytics/sales data-set.csv")
stores.data.set <- read.csv("C:/Users/Dell/Desktop/New folder/Kaggle/retail-data-analytics/stores data-set.csv")

#merge datasets (ignored the year 2013 data as CPI and unemployment data is incomplete)
x<-merge(Features.data.set,sales.data.set)
final<-merge(x,stores.data.set)

#see the summary
str(final)
summary(final)

#checking missing values
aggr(final, col=c("blue","red"),
     numbers=TRUE, combined=TRUE, varheight=FALSE, border="green",
     sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern"),
     labels=names(final), cex.axis=.7)


#removing the markdown data as majority of them are NA's
final[,6:10]<-NULL

str(final)
#converting date format
final$Date<-as.character(final$Date)
final$Date<-as.Date(final$Date,"%d/%m/%Y")

#splitting date into three features for model understanding
final$year = as.numeric(format(final$Date, format = "%Y"))
final$month = as.numeric(format(final$Date, format = "%m"))
final$day = as.numeric(format(final$Date, format = "%d"))
str(final)

#replacing T or F in IS Holiday wih 0 or 1
final[final[,3]==FALSE,3]<-0
final[final[,3]==TRUE,3]<-1


#correlation
corrplot(cor(final[,4:9]),method = "number")


#EDA
ggplot(data = final,aes(x=final$Date,y=final$Weekly_Sales))+geom_line()

ggplot(data=final, aes(x=as.factor(Store), y=Weekly_Sales,fill=as.factor(Dept))) + 
  stat_summary(fun.y=mean, geom="bar", position="stack") +
  ggtitle("North America Total Sales by Platform")

ggplot(data=final, aes(x=as.factor(Store), y=Weekly_Sales,fill=Type)) + 
  stat_summary(fun.y=mean, geom="bar", position="stack") +
  ggtitle("North America Total Sales by Platform")


ggplot(data=final, aes(x=Type, y=Weekly_Sales)) + 
  stat_summary(fun.y=mean, geom="bar", position="stack") +
  ggtitle("North America Total Sales by Platform")

ggplot(data=final, aes(x=as.factor(month), y=Weekly_Sales)) + 
  stat_summary(fun.y=sum, geom="bar", position="stack") +
  ggtitle("North America Total Sales by Platform")


ggplot(data=final, aes(x=as.factor(year), y=Weekly_Sales)) + 
  stat_summary(fun.y=sum, geom="bar", position="stack") +
  coord_flip() + 
  ggtitle("North America Total Sales by Platform")


ggplot(data=final, aes(x=as.factor(day), y=Weekly_Sales)) + 
  stat_summary(fun.y=sum, geom="bar", position="stack") +
  ggtitle("North America Total Sales by Platform")


#Partition data according to the year
training<-final[final$Date<as.Date("2012-04-01","%Y-%m-%d"),]
testing<-final[final$Date>=as.Date("2012-04-01","%Y-%m-%d"),]


#Linear regression Model
hist(final$Weekly_Sales)
model<-lm(Weekly_Sales~.,training[,1:11])
summary(model)
plot(model)


#Randomforest
set.seed(465)
mydata<-sample_frac(training[,1:11],0.05)
rfmodel<-randomForest(Weekly_Sales~.,mydata,ntree=300)
print(rfmodel)
attributes(rfmodel)

#importance
importance(rfmodel)
varImpPlot(rfmodel)


#performance measures


#best mtry
#mtry <- tuneRF(mydata[-1],mydata$Weekly_Sales, ntreeTry=500,
#               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
#best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
#print(mtry)
#print(best.m)






