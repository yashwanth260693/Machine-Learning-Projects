#loading packages
library(dplyr)
library(psych)
library(corrplot)

#loading data
x<-read.csv("C:/Users/Dell/Downloads/Studentdataset.csv")

#summary of data
glimpse(x)
summary(x)

#Checking for missing values
sum(is.na(x))              #No missing values

#partitioning data in to numeric and non numeric types
x_num<-select_if(x,is.numeric)
x_char<-select_if(x,is.factor)

#visualizing correlations
corrplot(cor(x_num),method = "number")

#Scatter Plots, Histograms
pairs(x_num) #pairwaise scatterplot of numerical variables
multi.hist(x_num) #Histogram of all numerical variables to identify skewness and do transformations

#Box plot to identify outliers
par(mfrow=c(4,4))
for (i in 1:length(x_num))
{
  boxplot(x_num[,i],main=names(x_num[i]), type="l")
}

#Cooks distance to detect outliers
fit <- lm(G1 ~ ., data=x)
distance <- cooks.distance(fit)

par(mfrow=c(1,1))
plot(distance, pch="*", cex=2, main="Outliers by Cooks distance")  # plot cook's distance
abline(h = 4*mean(distance, na.rm=T), col="red")  # add cutoff line
text(x=1:length(distance)+1, y=distance, labels=ifelse(distance>4*mean(distance, na.rm=T),names(distance),""), col="blue")  # add labels

outliers <- as.numeric(names(distance)[(distance > 4*mean(distance, na.rm=T))])  # influential row numbers
head(x[outliers, ])  # influential observations.

#Removing outliers
x_new<-x[-outliers,]

#Linear regression
fit <- lm(G1 ~ ., data=x_new)
summary(fit)
plot(fit) #to see residual plots















