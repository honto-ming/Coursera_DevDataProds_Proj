library(shiny)

instructions.sidebar.1 <- paste("This Shiny app will classify a patient as being AT RISK or NOT AT RISK of ", 
                      "being diagnosed with Coronary Heart Disease (CHD) within the next 10 years.")
instructions.sidebar.2 <- paste("To obtain a prediction, provide the patient's characteristics through the", 
                      "inputs on this sidebar and click the 'Predict' button.")
instructions.predict.1 <- paste("The figure below shows the predicted probability a patient with the", 
                              "given characteristics will be diagnosed with CHD in the next 10 years.")
instructions.predict.2 <- paste("Based on the provided threshold, a statement below the figure will classify",
                              "the patient as AT RISK or NOT AT RISK of CHD.")
instructions.model.1 <- paste("The figure below shows the ROC curve for the logistic regression model",
                            "used for predictions. Numbers below shows the performance metrics for the model.")
instructions.model.2 <- paste("Adjust the threshold through the slide-bar on the side panel on the left",
                            "to see the impact to accuracy, sensitivity, and specificity.")

shinyUI(fluidPage(
    titlePanel("Predicting Coranary Heart Disease"),
    sidebarLayout(
        sidebarPanel(
            helpText(p(instructions.sidebar.1), p(instructions.sidebar.2)),
            sliderInput("threshold",
                        label="Threshold:",
                        min=0.0, 
                        max=1.0, 
                        value=0.4,
                        step=0.1),
            selectInput("male",
                        label="Sex:",
                        choices=c("male", "female"),
                        selected="male",
                        multiple=FALSE),
            numericInput("age",
                         label="Age (integers only between 0-100):",
                         value=30,
                         min=0,
                         max=100, 
                         step=1),
            numericInput("cigsPerDay",
                         label="Cigarettes Per Day (integers only > 0):",
                         value=10,
                         min=0,
                         step=1),
            selectInput("prevalentHyp",
                        label="Prevalent to Hyptertension?",
                        choices=c("True", "False"),
                        selected="Fals",
                        multiple=FALSE),
            numericInput("sysBP",
                         label="Systolic Blood Pressure:",
                         value=120,
                         min=0),
            numericInput("glucose",
                         label="Glucose levels (integers only > 0):",
                         value=80,
                         min=0,
                         step=1),
            actionButton("predBtn", "Predict")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Prediction",
                         h3("Probability of Coronary Heart Disease in the Next 10 Years:"),
                         p(instructions.predict.1), 
                         p(instructions.predict.2),
                         plotOutput("predPlot"),
                         p(textOutput("classByThreshold"))
                         ),
                tabPanel("Model Info", 
                         h3("ROC Curve for Logistic Regression Model"),
                         p(instructions.model.1), 
                         p(instructions.model.2),
                         plotOutput("rocPlot"),
                         p("Adjust Threshold slider input on the left Sidebar to obtain desired accuracy, sensitivity, and specificity"),
                         p(textOutput("accuracy")),
                         p(textOutput("sensitivity")),
                         p(textOutput("specificity")),
                         p(textOutput("auc"))
                )
            )
        )
    )
))