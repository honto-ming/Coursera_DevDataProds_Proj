library(dplyr)
library(ggplot2)
library(caret)
library(doMC)

# Read in the data
heart1 <- read.csv("data/framingham.csv")

# remove the NA's
heart1 <- heart1[complete.cases(heart1),]

# change education & label to factor variables, and then create dummy variables
# heart1 <- mutate(heart1, male=as.logical(male), education=as.factor(education), currentSmoker=as.logical(currentSmoker),
#                  BPMeds = as.logical(BPMeds), prevalentStroke=as.logical(prevalentStroke),
#                  prevalentHyp=as.logical(prevalentHyp), diabetes=as.logical(diabetes),
#                  TenYearCHD=as.factor(TenYearCHD))
heart1 <- mutate(heart1, male=as.factor(male), education=as.factor(education), currentSmoker=as.factor(currentSmoker),
                 BPMeds = as.factor(BPMeds), prevalentStroke=as.factor(prevalentStroke),
                 prevalentHyp=as.factor(prevalentHyp), diabetes=as.factor(diabetes),
                 TenYearCHD=as.factor(TenYearCHD))
heart1.labels <-select(heart1, TenYearCHD)
heart.dummyVars <- dummyVars(TenYearCHD~., data=heart1)
heart1 <- as.data.frame(predict(heart.dummyVars, heart1))
heart1 <- cbind(heart1, heart1.labels)

# split into training and testing set 80/20 split
set.seed(1234)
trainIdx <- createDataPartition(heart1$TenYearCHD, p=0.8, list=FALSE, times=1)
heart.train <- heart1[trainIdx,]
heart.test <- heart1[-trainIdx,]

# scale & center the numeric variables
heart.train.num <- heart.train[,c("age","cigsPerDay", "totChol", "sysBP", "diaBP", 
                                  "BMI", "heartRate", "glucose")]
heart.test.num <- heart.test[,c("age","cigsPerDay", "totChol", "sysBP", "diaBP", 
                                  "BMI", "heartRate", "glucose")]
preProcValues <- preProcess(heart.train.num, method=c("center", "scale"))
heart.train.num.pre.proc <- predict(preProcValues, heart.train.num)
heart.test.num.pre.proc <- predict(preProcValues, heart.test.num)
heart.train <- select(heart.train,-age,-cigsPerDay,-totChol,-sysBP,-diaBP,-BMI,
                      -heartRate,-glucose)
heart.train <- cbind(heart.train.num.pre.proc,heart.train)
heart.test <- select(heart.test,-age,-cigsPerDay,-totChol,-sysBP,-diaBP,-BMI,
                      -heartRate,-glucose)
heart.test <- cbind(heart.test.num.pre.proc,heart.test)

logitBoostGrid <- expand.grid(nIter=c(10,50,100,150,200,1000))
rfGrid <- expand.grid(mtry=c(1,3,5,10,15))
svmGrid <- expand.grid(C=c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
                       sigma=c(0.25, 0.5, 1.0, 1.25, 1.5))

# Perform parameter tuning with 10-fold cross validation
cross.val <- trainControl(method="repeatedcv", number=10)
# utilize multicores & TRAIN
registerDoMC(cores=3)
set.seed(2345)
logitBoostModel <- train(TenYearCHD~., data=heart.train, method="LogitBoost",
                         trControl=cross.val, tuneGrid=logitBoostGrid)
rfModel <- train(TenYearCHD~., data=heart.train, method="rf",
                 trControl=cross.val, tuneGrid=rfGrid)
svmModel <- train(TenYearCHD~., data=heart.train, method="svmRadial",
                   trControl=cross.val, tuneGrid=svmGrid)

# Test with tuned models
logitBoost.pred.test <- predict(logitBoostModel, newdata=heart.test)
confusionMatrix(data=logitBoost.pred.test, reference=heart.test$TenYearCHD)
rf.pred.test <- predict(rfModel, newdata=heart.test)
confusionMatrix(data=rf.pred.test, reference=heart.test$TenYearCHD)
svm.pred.test <- predict(svmModel, newdata=heart.test)
confusionMatrix(data=svm.pred.test, reference=heart.test$TenYearCHD)



# let's predict with the test set
test.labels <- heart.test$TenYearCHD
heart.test <- select(heart.test, -TenYearCHD)
pred.test <- predict(svmModel1, newdata=heart.test, type="raw")
confusionMatrix(pred.test, test.labels)
table(test.labels, pred.test)



# Logistic Regression Model with ability to tune threshold
# we are unable to do this with LogitBoost
logit.model <- glm(TenYearCHD ~ ., data=heart.train, family=binomial)
summary(logit.model)
# plot a ROC curve to chose threshold
predict.train <- predict(logit.model, type="response")
ROCRpred <- prediction(predict.train, heart.train$TenYearCHD)
# build the ROC curve based on the predicito object, true pos rate, false pos rate (y & x)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
# plot with colorize, and set cutoffs from 0-1 at each 0.1. Text labels are 0.2 to the right, and 1.7 down
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7),
     main="ROC Plot for Logistic Regression Model")
# looks like a 0.4 threshold will work. Let us test it with our test data
predict.test <- predict(logit.model, type="response", newdata=heart.test)
table(heart.test$TenYearCHD, predict.test > 0.4)
# sensitivity (~19%):
snstvty <- 21/(21+90)
# specitivity (~95.6%)
spctvty <- 593/(593+27)
# accuracy is at ~84%
acc <- (593+21) / (593+21+90+27)
# compare baseline - most frequent is baseline so we'd predict all no TenYearCHD
(593+27)/(593+21+90+27)
# not better than baseline. try to change threshold? Try AUC first
ROCRpred <- prediction(predict.test, heart.test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Our model can differentiate low and high risk values 71% of the time, not bad


