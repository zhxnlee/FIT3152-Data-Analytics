setwd("C:/Monash/FIT3152/Assignment2")
install.packages("dplyr")
library(dplyr)
install.packages("tree")
library(tree)
install.packages("e1071")
library(e1071)
install.packages(("ROCR"))
library(ROCR)
install.packages("randomForest")
library(randomForest)
install.packages("adabag")
library(adabag)
install.packages("rpart")
library(rpart)
install.packages("caret")
library(caret)


rm(list = ls())
WAUS <- read.csv("HumidPredict2023D.csv")
L <- as.data.frame(c(1:49))
set.seed(31860532) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

str(WAUS)
dim(WAUS) # 22 columns from the dataset  
ncol(WAUS) #2000 rows from the dataset
nrow(WAUS)
sapply(WAUS, function(x) sum(is.na(x)))
table(WAUS['MHT'])
#999 0s
#931 1s


#preprocessing
WAUS = WAUS[complete.cases(WAUS),]
WAUS$MHT <- as.numeric(WAUS$MHT)
dim(WAUS)  
ncol(WAUS) 
nrow(WAUS)
#after ommiting NA values, there are 385 rows 
table(WAUS['MHT'])
#177 0s
#208 1s

set.seed(31860532) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]
WAUS.train
WAUS.test

# Calculate a decision tree
library(tree)

str(WAUS.train)
WAUS.fit=tree(as.factor(MHT) ~ ., data=WAUS.train)
summary(WAUS.fit)
plot(WAUS.fit)
text(WAUS.fit, pretty = 0)



WAUS.predict = predict(WAUS.fit, WAUS.test, type = "class")

#confusion matrix of desicion tree
CF_DT = table(actual = WAUS.test$MHT, predicted = WAUS.predict)
CF_DT

#calculating accuracy
TP = CF_DT[1,1]
FP = CF_DT[1,2]
FN = CF_DT[2,1]
TN = CF_DT[2,2]
acc_DT = (TP + TN) / (TP+TN+FP+FN) 
acc_DT

#confidence level for each case of predicting "more humid tomorrow"
WAUS.pred.tree = predict(WAUS.fit, WAUS.test, type = "vector")
WAUS.pred.tree

# do predictions as probabilities and draw ROC
WAUSpred <- prediction( WAUS.pred.tree[,2], WAUS.test$MHT)
WAUSpred
WAUSperf <- performance(WAUSpred,"tpr","fpr")
plot(WAUSperf)
abline(0,1)
auc_DT <- performance(WAUSpred, "auc")@y.values[[1]]
auc_DT



# Calculate naive bayes
WAUS.bayes = naiveBayes(MHT ~. , data = WAUS.train)
WAUS.bayes

#confidence level for each case of predicting "more humid tomorrow"
WAUS.predbayes = predict(WAUS.bayes, WAUS.test)
WAUS.predbayes

#confusion matrix of naive bayes
CF_NB =table(Predicted_Class = WAUS.predbayes, Actual_Class = WAUS.test$MHT)

#calculating accuracy
TP = CF_NB[1,1]
FP = CF_NB[1,2]
FN = CF_NB[2,1]
TN = CF_NB[2,2]
acc_NB = (TP + TN) / (TP+TN+FP+FN) 
acc_NB

# outputs as confidence levels
WAUSpred.bayes = predict(WAUS.bayes, WAUS.test, type = 'raw')

# do predictions as probabilities and draw ROC
WAUSBpred <- prediction( WAUSpred.bayes[,2], WAUS.test$MHT)
WAUSBpred
WAUSBperf <- performance(WAUSBpred,"tpr","fpr")
WAUSBperf
plot(WAUSBperf, add=TRUE, col = "blueviolet")
auc_NB <- performance(WAUSBpred, "auc")@y.values[[1]]
auc_NB



# Bagging
WAUS.train$MHT <- as.factor(WAUS.train$MHT)
WAUS.bag <- bagging(MHT ~. , data = WAUS.train, mfinal=5)


#important variables
BAG_imp = WAUS.bag$importance
BAG_imp[order(BAG_imp, decreasing = TRUE)]

#confidence level for each case of predicting "more humid tomorrow"
WAUSpred.bag <- predict.bagging(WAUS.bag, WAUS.test)
WAUSpred.bag

#confusion matrix of bagging
CF_BG = WAUSpred.bag$confusion

#calculating accuracy
TP = CF_BG[1,1]
FP = CF_BG[1,2]
FN = CF_BG[2,1]
TN = CF_BG[2,2]
acc_BG = (TP + TN) / (TP+TN+FP+FN) 
acc_BG

# do predictions as probabilities and draw ROC
WAUSBagpred <- prediction( WAUSpred.bag$prob[,2], WAUS.test$MHT)
WAUSBagperf <- performance(WAUSBagpred,"tpr","fpr")
plot(WAUSBagperf, add=TRUE, col = "blue")
auc_BG <- performance(WAUSBagpred, "auc")@y.values[[1]]
auc_BG





#Boosting
WAUS.Boost <- boosting(MHT ~. , data = WAUS.train, mfinal=10)

#important variables
BOS_imp = WAUS.Boost$importance

#confidence level for each case of predicting "more humid tomorrow"
WAUSpred.boost <- predict.boosting(WAUS.Boost, newdata=WAUS.test)
# WAUSpred.boost

#confusion matrix of boosting
CF_BOS = WAUSpred.boost$confusion

#calculating accuracy
TP = CF_BOS[1,1]
FP = CF_BOS[1,2]
FN = CF_BOS[2,1]
TN = CF_BOS[2,2]
acc_BOS = (TP + TN) / (TP+TN+FP+FN) 
acc_BOS

# do predictions as probabilities and draw ROC
WAUSBoostpred <- prediction( WAUSpred.boost$prob[,2], WAUS.test$MHT)
WAUSBoostperf <- performance(WAUSBoostpred,"tpr","fpr")
plot(WAUSBoostperf, add=TRUE, col = "red")
auc_BOS <- performance(WAUSBoostpred, "auc")@y.values[[1]]
auc_BOS




# Random Forest
WAUS.rf <- randomForest(MHT ~. , data = WAUS.train, na.action = na.exclude)

#important variables
RF_imp = WAUS.rf$importance

WAUSpredrf <- predict(WAUS.rf, WAUS.test)

#confusion matrix of random forest
CF_RF = table(Predicted_Class = WAUSpredrf, Actual_Class = WAUS.test$MHT)
#calculating accuracy
TP = CF_RF[1,1]
FP = CF_RF[1,2]
FN = CF_RF[2,1]
TN = CF_RF[2,2]
acc_RF = (TP + TN) / (TP+TN+FP+FN) 
acc_RF

#confidence level for each case of predicting "more humid tomorrow"
WAUSpred.rf <- predict(WAUS.rf, WAUS.test, type="prob")
WAUSpred.rf

# do predictions as probabilities and draw ROC
WAUSFpred <- prediction( WAUSpred.rf[,2], WAUS.test$MHT)
WAUSFperf <- performance(WAUSFpred,"tpr","fpr")
plot(WAUSFperf, add=TRUE, col = "darkgreen")
auc_RF <- performance(WAUSFpred, "auc")@y.values[[1]]
auc_RF

#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(WAUS.pred.tree))
cat("\n#Baging Attribute Importance\n")
print(WAUS.bag$importance)
cat("\n#Boosting Attribute Importance\n")
print(WAUS.Boost$importance)
cat("\n#Random Forest Attribute Importance\n")
print(WAUS.rf$importance)


accuracy_table <- data.frame("Decision Tree" = acc_DT, "Naive Bayes" = acc_NB, "Bagging" = acc_BG, "Boosting" = acc_BOS, "Random Forest" = acc_RF)
classifiers <- colnames(accuracy_table)
accuracy_values <- unlist(accuracy_table)
barplot(accuracy_values, names.arg = classifiers, ylim = c(0, 1), ylab = "Accuracy", xlab = "Classifier", las = 2, main = "Classifiers and Accuracy")


auc_table <- data.frame("Decision Tree" = auc_DT, "Naive Bayes" = auc_NB, "Bagging" = auc_BG, "Boosting" = auc_BOS, "Random Forest" = auc_RF)
classifiers_auc <- colnames(accuracy_table)
auc_values <- unlist(accuracy_table)
barplot(auc_values, names.arg = classifiers_auc, ylim = c(0, 1), ylab = "Accuracy", xlab = "Classifier", las = 2, main = "Classifiers and Area of the curve")





