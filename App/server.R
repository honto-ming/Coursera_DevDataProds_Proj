library(shiny)
library(dplyr)
library(caret)
library(ggplot2)
library(ROCR)
library(e1071)

####################
# DO AT APP LAUNCH #
####################
# Read in the data
heart1 <- read.csv("data/framingham.csv")

# remove the NA's
heart1 <- heart1[complete.cases(heart1),]

# change education & label to factor variables, and then create dummy variables
heart1 <- mutate(heart1, male=as.factor(male), education=as.factor(education), currentSmoker=as.factor(currentSmoker),
                 BPMeds = as.factor(BPMeds), prevalentStroke=as.factor(prevalentStroke),
                 prevalentHyp=as.factor(prevalentHyp), diabetes=as.factor(diabetes),
                 TenYearCHD=as.factor(TenYearCHD))

# Build model as simplified for deployment. Use only the significant variables
heart2 <- select(heart1, male, age, cigsPerDay, prevalentHyp, sysBP, glucose, TenYearCHD)
# split into training and testing set 80/20 split
set.seed(1234)
trainIdx <- createDataPartition(heart2$TenYearCHD, p=0.8, list=FALSE, times=1)
heart.train <- heart2[trainIdx,]
heart.test <- heart2[-trainIdx,]


shinyServer(function(input, output) {
    ################################
    # DO AT EACH USER VISIT TO APP #
    ################################
    # Build Logistic Regression Model 
    logit.model <- glm(TenYearCHD ~ ., data=heart.train, family=binomial)
    #summary(logit.model)
    # plot a ROC curve to chose threshold
    predict.train <- predict(logit.model, type="response")
    ROCRpred <- prediction(predict.train, heart.train$TenYearCHD)
    # build the ROC curve based on the predicito object, true pos rate, false pos rate (y & x)
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    # plot with colorize, and set cutoffs from 0-1 at each 0.1. Text labels are 0.2 to the right, and 1.7 down
    output$rocPlot <- renderPlot(plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7),
         main="ROC Plot for Logistic Regression Model"))
    # get predictions from Test Set
    predict.test <- predict(logit.model, type="response", newdata=heart.test)
    # AUC
    ROCRpred <- prediction(predict.test, heart.test$TenYearCHD)
    output$auc <- renderText(paste("Model AUC:",
                                   performance(ROCRpred, "auc")@y.values[[1]][1]))
    
    ##########################################################
    # REACTIVE FUNCTION ON CHANGING SLIDEY_BAR FOR THRESHOLD #
    ##########################################################
    model.metrics <- reactive({
        threshold <- input$threshold
        # Update estimated out-of-sample accuracy, sensitivy, and specificity
        pred.test.class <- as.factor(predict.test > threshold)
        levels(pred.test.class) <- c("FALSE", "TRUE")
        label.test.class <- heart.test$TenYearCHD
        levels(label.test.class) <- c("FALSE", "TRUE")
        confusionMatrix(data=pred.test.class, reference=label.test.class, 
                                             positive="TRUE")
    })
    #################################################################
    # Use the reactive function above to update Model Metrics based #
    # on Threshold.
    #################################################################
    output$accuracy <- renderText(paste("Model Accuracy:",
                                        model.metrics()[["overall"]]["Accuracy"]))
    output$sensitivity <- renderText(paste("Model Sensitivity:",
                                           model.metrics()[["byClass"]]["Sensitivity"]))
    output$specificity <- renderText(paste("Model Specificity:",
                                           model.metrics()[["byClass"]]["Specificity"]))
    
    ######################################
    # REACTIVE ON HITTING PREDICT BUTTON #
    ######################################
    pred.plot.data <- reactive({ 
        input$predBtn
        isolate({
            # predict from input
            input.df <- data.frame(male=factor(x=ifelse(input$male=="male",1,0), 
                                               levels=c("0","1")), 
                                   age=input$age, cigsPerDay=input$cigsPerDay, 
                                   prevalentHyp=factor(x=ifelse(input$male=="True",1,0), 
                                                       levels=c("0","1")),
                                   sysBP=input$sysBP, 
                                   glucose=input$glucose)
            # predict
            pred.input <- predict(logit.model, type="response", newdata=input.df)
            
            classLabel <- ifelse(pred.input > input$threshold, "AT RISK", "NOT AT RISK")
            output$classByThreshold <- renderText(paste("Based on threshold of",
                                                        input$threshold, ",", 
                                                        "this patient with the characteristics given would be classified as", classLabel,
                                                        "of diagnosed with Coronary Heart Disease in the next 10 years."))
            # plot 
            probabilities <- data.frame(TenYearCHD=c("TRUE","FALSE"),
                                        Probability=c(pred.input, 1-pred.input),
                                        dummyX=c("10 Yr CHD", "10 Yr CHD"))
            mutate(probabilities, midpts=cumsum(Probability)-0.5*Probability)
        })
        
    })
    
    # plot                        
    output$predPlot <- renderPlot(ggplot(data=pred.plot.data(), 
                                         aes(x=dummyX, y=Probability, fill=TenYearCHD)) +
                                      geom_bar(stat="identity", width=0.5) + 
                                      coord_flip() + 
                                      scale_fill_manual(values=c("darkolivegreen3","darkorange")) +
                                      theme(axis.text=element_blank(), axis.ticks=element_blank(), 
                                                axis.title.y=element_blank()) +
                                      geom_text(aes(label=format(Probability, digits=3), y=midpts), hjust=0.5) + 
                                      ggtitle(paste0("Predicted Probability of Coronary Heart Disease (CHD)\n",
                                                         "from Logistic Regression Model of Framingham Heart Study Patients"))
                                  )

    
})








