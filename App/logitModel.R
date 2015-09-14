library(dplyr)
library(caret)
library(ROCR)

# Read in the data
heart1 <- read.csv("data/framingham.csv")

# remove the NA's
heart1 <- heart1[complete.cases(heart1),]

# change education & label to factor variables, and then create dummy variables
heart1 <- mutate(heart1, male=as.factor(male), education=as.factor(education), currentSmoker=as.factor(currentSmoker),
                 BPMeds = as.factor(BPMeds), prevalentStroke=as.factor(prevalentStroke),
                 prevalentHyp=as.factor(prevalentHyp), diabetes=as.factor(diabetes),
                 TenYearCHD=as.factor(TenYearCHD))

# split into training and testing set 80/20 split
set.seed(1234)
trainIdx <- createDataPartition(heart1$TenYearCHD, p=0.8, list=FALSE, times=1)
heart.train <- heart1[trainIdx,]
heart.test <- heart1[-trainIdx,]

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
