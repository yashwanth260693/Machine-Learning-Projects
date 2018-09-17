install.packages("RcppArmadillo")
install.packages("quanteda")
install.packages("RColorBrewer")

#load packages
library(quanteda)
library(RColorBrewer)

#import data
raw.data <- readxl::read_excel("C:/Users/Dell/Downloads/Compressed/Assignment5/train.xlsx")


#begin with spam
sms.corpus <- corpus(raw.data$Message)
docvars(sms.corpus) <- raw.data$Target
spam.plot <- corpus_subset(sms.corpus, docvar1 == "yes")
spam.plot <- dfm(spam.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("SMART"))
spam.col <- brewer.pal(10, "BrBG")
spam.cloud <- textplot_wordcloud(spam.plot, min.freq = 16, color = spam.col)  
title("Spam Wordcloud", col.main = "grey14")
textstat_frequency(spam.plot)


#ham
ham.plot <- corpus_subset(sms.corpus, docvar1 == "no")
ham.plot <- dfm(ham.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("SMART"))
ham.col <- brewer.pal(10, "BrBG")
ham.cloud <- textplot_wordcloud(ham.plot, min.freq = 16, color = ham.col)  
title("ham Wordcloud", col.main = "grey14")
textstat_frequency(ham.plot)


#model
sms.dfm <- dfm(sms.corpus, tolower = TRUE)  
sms.dfm <- dfm_trim(sms.dfm, min_count = 5, min_docfreq = 3)  
sms.dfm <- dfm_tfidf(sms.dfm)  

sms.raw.train <- raw.data[1:3883,]
sms.raw.test <- raw.data[3884:nrow(raw.data),]


sms.dfm.train <- sms.dfm[1:3883,]
sms.dfm.test <- sms.dfm[3884:nrow(raw.data),]
sms.classifier <- textmodel_nb(sms.dfm.train, sms.raw.train$Target)

sms.predictions <- predict(sms.classifier, newdata = sms.dfm.test,"probability")  

length(sms.predictions)

table(as.vector(sms.predictions), sms.raw.test$Target)

x<-as.data.frame(sms.predictions)$yes

x

library(pROC)

roc_obj <- roc(sms.raw.test$Target, x)
auc(roc_obj)

table(as.vector(sms.predictions), sms.raw.test$Target)


roc_obj <- roc(sms.raw.test$Target, y$MY)
auc(roc_obj)

y<-readxl::read_excel("C:/Users/Dell/Downloads/score_final_pred_latest.xlsx")

z<-readxl::read_excel("C:/Users/Dell/Desktop/Finalpred.xlsx")


for(i in 2:6)
{
auc[i]<-auc(roc(sms.raw.test$Target, as.vector(as.matrix((z[,i])))))}
auc
z[,2]

table(z$`11`, sms.raw.test$Target)

roc_obj <- roc(sms.raw.test$Target, as.vector(as.matrix((z[,3]))))
auc(roc_obj)
class(as.vector(as.matrix((z[,2]))))


class(sms.raw.test$Target)


