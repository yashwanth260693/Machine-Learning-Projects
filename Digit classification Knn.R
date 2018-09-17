#Pakages
install.packages("ElemStatLearn")
library(ElemStatLearn)
library(caret)
library(dplyr)
library(ggplot2)

#data
x<-as.data.frame(zip.train)
y<-as.data.frame(zip.test)


training<-x[x$V1==2 | x$V1==3,]
testing<-y[y$V1==2 | y$V1==3,]

str(training)

training$V1<-as.factor(training$V1)
testing$V1<-as.factor(testing$V1)


#model
levels(training$V1) <- make.names(levels(factor(training$V1)))
levels(testing$V1) <- make.names(levels(factor(testing$V1)))

trcr = trainControl(method = "repeatedcv",
                 number = 10,
                 repeats = 3,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

tune_grid <- expand.grid(k = c(1,3,5,7,9,11,13,15))

model_knn <- train(V1~. , data = training, method = "knn",
                preProcess = c("center","scale"),
                trControl = trcr,
                metric = "ROC",
                tuneGrid = tune_grid)

#k=15 has the optimal auc

pred<-predict(model_knn,testing)

#confusion matrix and accuracy
confusionMatrix(table(pred,testing$V1))

#additional auc scores
library(ROCR)
pred <-prediction(predict(model_knn,testing,type="prob")[,2],testing$V1)

# Calculating Area under Curve (AUC)
perf <- performance(pred,"auc")
perf

# Plot AUC
perf <- performance(pred, "tpr", "fpr")
plot(perf, col = "blue", lwd = 1.5)


