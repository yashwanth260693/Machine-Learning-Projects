#Loading packages
library(dplyr)
library(plyr)
library(readr)
library(corrplot)
library(ggplot2)
library(VIM)
library(mice)
library(ROCR)
library(ROSE)
library(rpart)
library(caret)

#Importing or reading data
train<-read_csv("C:/Users/Dell/Downloads/train_ZoGVYWq.csv")
test<-read_csv("C:/Users/Dell/Downloads/test_66516Ee.csv")
Full<-rbind.fill(train,test)

#Cleaning data

glimpse(Full)                               #Checking the structure of data
sort(colSums(is.na(Full)),decreasing = T)   #Missingvalues by columns
nrow(Full)-nrow(unique(Full))               #Checking for duplicate rows   
Full[1]<-NULL                               #Removing ID variable

Full_numr<-Full[,sapply(Full,is.numeric)]
corrplot(cor(Full_numr),method = "number")

Full$age_in_days<-Full$age_in_days/365.25   #Converting age from days to years considering leapyear effect
#To visualise distribution of age wrt residence type
ggplot(train,aes(x=(age_in_days/365.25),color=residence_area_type))+geom_histogram(fill="white",breaks=c(10,20,30,40,50,60,70,80))
ggplot(train,aes(x=(age_in_days/365.25),color=as.factor(renewal)))+geom_histogram(fill="white",breaks=c(10,20,30,40,50,60,70,80))
#I can use the inference from above graph in binning the age in to adult(<30),middle(30-60),old(60-80)

#Preparing data with out missing values for EDA
Full_notblank<-Full[complete.cases(Full[,1:11]),]

#Box plot for underwriting score wrt scourcing channel and residenc type
ggplot(Full_notblank,aes(x=as.factor(sourcing_channel),y=application_underwriting_score,fill=residence_area_type))+
geom_boxplot()                  #no pattern observed

#As no clear pattern is observed in underwriting score i will use imputing to fill the blanks

ggplot(Full_notblank,aes(x=as.factor(residence_area_type),y=Income))+
geom_boxplot()

Full$sourcing_channel<-as.factor(Full$sourcing_channel)
Full$residence_area_type<-as.factor(Full$residence_area_type)

#Dealing with missing values using imputation technique
imputed_Data <- mice(Full[1:11], m=5, maxit = 2, method = 'pmm', seed = 101)
completeData <- complete(imputed_Data,2)

completeData$renewal<-Full$renewal

#Analyzing correlations
corrplot(cor(completeData[1:79853,sapply(completeData,function(x) {is.numeric(x)})]),method = "number")
colnames(completeData)<-c("perc_cashcred","age","Income","Count_3-6","Count_6-12","Count_12plus","UWscore",
                  "Prems_paid","Sourcechannel","residtype","premium")
#from the correaltion plot it is clear that target variable has relatively more correlation with delayed premiums

ggplot(completeData[1:79853,],aes(x=Sourcechannel,y=renewal,fill=residtype))+stat_summary(fun.y = mean,geom="bar",position = "stack")

ggplot(completeData[1:79853,],aes(x=as.factor(renewal),fill=Sourcechannel))+geom_bar(position = "dodge")
#from the above graph one can infer that data is unbalanced

#Running base logistic model to set the benchmark accuracy
completeData_train<-completeData[1:79853,]
completeData_train$renewal<-as.factor(completeData_train$renewal)
completeData_test<-completeData[79854:114077,1:11]

# Partition completedata_train - train (80%) & test (20%)
set.seed(101)
ind <- sample(2, nrow(completeData_train), replace = T, prob = c(0.8, 0.2))
training <- completeData_train[ind==1,]
testing <- completeData_train[ind==2,]
glimpse(completeData_train)


# Logistic regression model
mymodel <- glm(renewal~., data = training, family = 'binomial')
summary(mymodel)

# Prediction
p <- predict(mymodel,testing, type = 'response')
head(p)
pred<-ifelse(p>0.5, 1, 0)
head(pred)
x<-table(Predicted = pred, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
#Overall 94.4% accuracy is observed but accuracy in predicting 0's is seen as 57.2% which infers that result is clearly biased towards 1 due to imbalanced data

#Stepwise
reg<-step(mymodel,data=training,direction = "backward")
p1 <- predict(reg,testing, type = 'response')
head(p1)
pred1<-ifelse(p1>0.5, 1, 0)
head(pred1)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
performance(pred_test,"auc")
#auc of 0.825

#Feature engineering
#Age variable is already converted to years from days
#Now i will add new feature with weighted average of number of premiums delayed by 3/ 6/ 12 months
completeData_train$Weighted_no_of_prem<-4.5*completeData_train$`Count_3-6`+9*completeData_train$`Count_6-12`+

train1<-completeData_train
names(train1)<-names(train)[2:13]

test1<-completeData_test
names(test1)<-names(test)[2:12]

mymodel <- glm(renewal~., data = train1, family = 'binomial')
reg<-step(mymodel,data=train1,direction = "backward")
p <- predict(reg,test1, type = 'response')
p
test1$renewal<-p

y<-data.frame(test[1],test1[12])

#Balancing data
#1 Oversampling
train2<-train1
colnames(train2)<-c("perc_cashcred","age","Income","x1","x2","x3","UWscore",
                    "Prems_paid","Sourcechannel","residtype","premium","renewal")
set.seed(101)
ind <- sample(2, nrow(train2), replace = T, prob = c(0.8, 0.2))
training <- train2[ind==1,]
testing <- train2[ind==2,]

table(training$renewal)
data_balanced_over <- ovun.sample(renewal ~ ., data = training, method = "over",N = 119632)$data
table(data_balanced_over$renewal)

mymodel <- glm(renewal~., data = data_balanced_over, family = 'binomial')
reg<-step(mymodel,data=data_balanced_over,direction = "backward")
p1 <- predict(mymodel,testing,type = 'response')
head(p1)
pred1 <- ifelse(p1>0.5, 1, 0)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
prf <- performance(pred_test, measure = "tpr", x.measure = "fpr")
plot(prf)
performance(pred_test,"auc")@y.values[[1]]
#auc of 0.16

#undersampling
data_balanced_under <- ovun.sample(renewal ~ ., data = training, method = "under",N = 7980,seed = 1)$data
table(data_balanced_under$renewal)

mymodel <- glm(renewal~., data = data_balanced_under, family = 'binomial')
reg<-step(mymodel,data=data_balanced_under,direction = "backward")
p1 <- predict(reg,testing, type = 'response')
head(p1)
pred1 <- ifelse(p1>0.5, 1, 0)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
prf <- performance(pred_test, measure = "tpr", x.measure = "fpr")
plot(prf)
performance(pred_test,"auc")@y.values[[1]]
#auc of 0.16

#both under and over
data_balanced_both <- ovun.sample(renewal ~ ., data = training, method = "both", p=0.5,N=79853, seed = 1)$data
table(data_balanced_both$renewal)

mymodel <- glm(renewal~., data = data_balanced_both, family = 'binomial')
reg<-step(mymodel,data=data_balanced_both,direction = "backward")
p1 <- predict(reg,testing, type = 'response')
head(p1)
pred1 <- ifelse(p1>0.5, 1, 0)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
prf <- performance(pred_test, measure = "tpr", x.measure = "fpr")
plot(prf)
performance(pred_test,"auc")@y.values[[1]]

#ROSE
data.rose <- ROSE(renewal ~ ., data = training, seed = 1)$data

mymodel <- glm(renewal~., data = data.rose, family = 'binomial')
reg<-step(mymodel,data=data.rose,direction = "backward")
p1 <- predict(reg,testing, type = 'response')
head(p1)
pred1 <- ifelse(p1>0.5, 1, 0)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
prf <- performance(pred_test, measure = "tpr", x.measure = "fpr")
plot(prf)
performance(pred_test,"auc")@y.values[[1]]
#auc of 0.17

#Decision tree
mtree <- rpart(renewal~., data = data_balanced_over, method="class")
library(rattle)
plot(mtree)
prp(mtree, faclen = 0, cex = 0.8, extra = 1)
fancyRpartPlot(mtree)
printcp(mtree)
pred=predict(mtree,testing,type = "prob")
roc.curve(testing$renewal, pred[,1], plotit = T)
pred1 <- ifelse(pred[,1]>0.5, 1, 0)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)

pred_val <-prediction(pred[,2] ,testing$renewal)
prf <- performance(pred_val, measure = "tpr", x.measure = "fpr")
plot(prf)
performance(pred_val,"auc")@y.values[[1]]

#auc of over and under is good but to not lose data i will use over

#both under and over
data_balanced_over <- ovun.sample(renewal ~ ., data = train2, method = "over",N = 149710)$data
table(data_balanced_over$renewal)

table(train2$renewal)
varImp(mtree)

testlast<-test[2:12]
names(testlast)<-c("perc_cashcred","age","Income","x1","x2","x3","UWscore",
                   "Prems_paid","Sourcechannel","residtype","premium")
pred=predict(mtree,testlast,type = "prob")

final<-data.frame(test$id,pred[,1])
write.csv(final,"comp.csv")





##########################################################################  
  
# Logistic regression model
mymodel <- glm(renewal~., data = training, family = 'binomial')

#Stepwise
reg<-step(mymodel,data=training,direction = "backward")
p1 <- predict(reg,testing, type = 'response')
head(p1)
pred1<-ifelse(p1>0.5, 1, 0)
head(pred1)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
performance(pred_test,"auc")
#auc of 0.825

summary(reg)


data_feat<-completeData
data_feat$age<-ifelse(data_feat$age<=30,"Adult",ifelse(data_feat$age<=60,"Middle","Old"))
glimpse(data_feat)
data_feat$age<-as.factor(data_feat$age)
data_feat$prem_aspercof_income<-data_feat$premium/data_feat$Income
data_feat$premdelayed_aspercof_total<-(data_feat$`Count_3-6`+data_feat$`Count_6-12`+data_feat$Count_12plus)/data_feat$Prems_paid
data_feat_train<-data_feat[1:79853,]
data_feat_test<-data_feat[79854:114077,]


set.seed(101)
ind <- sample(2, nrow(data_feat_train), replace = T, prob = c(0.8, 0.2))
training <- data_feat_train[ind==1,]
testing <- data_feat_train[ind==2,]
summary(reg)


#Feature engineering
ggplot(data_feat,aes(x=renewal,fill=Sourcechannel))+geom_bar(position ="dodge")

ggplot(data_feat[1:79853,],aes(x=UWscore,color=as.factor(renewal)))+geom_histogram(fill="white",breaks=c(91,92,93,94))


training=data_feat_train

#final
mymodel <- glm(renewal~., data = training, family = 'binomial')
reg<-step(mymodel,data=training,direction = "backward")

testing=data_feat_test[,-12]
p1 <- predict(reg,testing, type = 'response')

final<-data.frame(test$id,p1)
write.csv(final,"final.csv")


head(p1)
pred1<-ifelse(p1>0.5, 1, 0)
head(pred1)
x<-table(Predicted = pred1, Actual = testing$renewal)
1 - sum(diag(x))/sum(x)
pred_test<-prediction(p1,testing$renewal)
performance(pred_test,"auc")


write.csv()
