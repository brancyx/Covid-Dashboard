## R Shiny Dashboard of “Covid Prediction” ##

#Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(caTools)
library(DMwR)
library(dplyr)
library(randomForest)
library(Metrics)
library(data.table)
library(neuralnet)
library(car)
library(rpart)
library(rpart.plot)
library(UBL)
library(stringr)

#Importing datasets
covid.dt <- fread("final_CP.csv", stringsAsFactors = TRUE)
summary(covid.dt)


# Train-Test Split (Ratio of Trainset:Testset is 70:30)
set.seed(2407)
mod1_train <- sample.split(Y = covid.dt$Covid_Results, SplitRatio = 0.7)
mod1_trainset <- subset(covid.dt, mod1_train == T)
mod1_testset <- subset(covid.dt, mod1_train == F)
summary(mod1_trainset$Covid_Results) # Ensure trainset has the same proportion as initial dataset
summary(mod1_testset$Covid_Results) # Ensure testset has the same proportion as initial dataset

# Verify that the proportion of Exited is the about the same in Trainset and Testset  
prop.table(table(mod1_trainset$Covid_Results))
prop.table(table(mod1_testset$Covid_Results))

# SMOTE dataset
trainSet_SMOTE_a <- SmoteClassif(Covid_Results~., mod1_trainset, C.perc = "balance", dist = "HEOM") 
prop.table(table(trainSet_SMOTE_a$Covid_Results))

# Random Forest model
m.RF <- randomForest(Covid_Results ~ ., data=trainSet_SMOTE_a, na.action=na.omit, importance=T)
Predicted_Covid_RF <- predict(m.RF, newdata = mod1_testset,type="class")
summary(Predicted_Covid_RF)
summary(trainSet_SMOTE_a)

#### R Shiny ui #### 
ui <- dashboardPage(
    
    #Dashboard title
    dashboardHeader(title = 'COVID EXPLORER', titleWidth = 290),
    
    #Sidebar layout
    dashboardSidebar(width = 290,
                     sidebarMenu(menuItem("Prediction", tabName = 'pred', icon = icon('search')),
                                 menuItem("Plots", tabName = "plots", icon = icon('poll'))
                                 # menuItem("Dashboard", tabName = "dash", icon = icon('tachometer-alt')),
                                 )),
    
    #Tabs layout
    dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                  #Plots tab content
                  tabItems(tabItem('plots', 
                                   #Histogram filter
                                   box(status = 'primary', title = 'Filter for the histogram plot',
                                       selectInput('num', "Numerical variables:", c('Mean_platelet_volume','Platelets','Red_blood_Cells')),
                                       footer = 'Histogram plot for numerical variables'),
                                   #Frequency plot filter
                                   box(status = 'primary', title = 'Filter for the frequency plot',
                                       selectInput('cat', 'Categorical variables:', c('age_60_and_above','gender')),
                                       footer = 'Frequency plot for categorical variables'),
                                   #Boxes to display the plots
                                   box(plotOutput('histPlot')),
                                   box(plotOutput('freqPlot'))),
                           
                           #Prediction tab content
                           tabItem('pred',
                                   #Filters for categorical variables
                                   box(title = 'Categorical variables', status = 'primary', width = 12,
                                       splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                   cellWidths = c('0%', '19%', '4%', '10%', '4%', '10%', '4%'),
                                                   radioButtons('p_age', 'Aged 60 and above?', c('Yes', 'No')),
                                                   div(),
                                                   radioButtons('p_gender', 'Gender', c('male', 'female')),
                                                   div(),
                                                   radioButtons('p_cough', 'Cough?', c(0, 1)),
                                                   div(),
                                                   radioButtons('p_fever', 'Fever?', c(0, 1)),
                                                   div(),
                                                   radioButtons('p_soret', 'Sore Throat?', c(0, 1)),
                                                   div()
                                                   
                                                   ),
                                       splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                   cellWidths = c('0%', '19%', '4%', '10%', '4%', '10%', '4%'),
                                                   radioButtons('p_sob', 'Breath Shortness?', c(0, 1)),
                                                   div(),
                                                   radioButtons('p_headache', 'Headache', c(0, 1))
                                       )
                                       
                                       ),
                                   
                                   #Filters for numeric variables
                                   box(title = 'Numerical variables', status = 'primary', width = 12,
                                       splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                                   numericInput('p_platelets', 'Platelets', 0),
                                                   div(),
                                                   numericInput('p_meanpv', 'Mean Platelet Volume', 0),
                                                   div(),
                                                   numericInput('p_rbc', 'Red Blood Cells', 0),
                                                   div(),
                                                   numericInput('p_leu', 'Leukocytes', 0)),
                                        splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                                   numericInput('p_ser', 'Serum_Glucose', 0),
                                                   div(),
                                                   numericInput('p_urea', 'Urea', 0),
                                                   div(),
                                                   numericInput('p_proteina', 'Proteina_C_reativa', 0),
                                                   div(),
                                                   numericInput('p_creatinine', 'Creatinine', 0)),
                                       splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                                   numericInput('p_pot', 'Potassium', 0),
                                                   div(),
                                                   numericInput('p_sod', 'Sodium', 0))
                                       ),
                                   
                                   # box to upload csv
                                   # box(status = 'primary', width = 10,
                                   #     splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                   #                 cellWidths = c('0%', '19%', '4%', '10%', '4%', '10%', '4%'),
                                   #                 fileInput("file1", "Choose CSV File",
                                   #                           multiple = FALSE,
                                   #                           accept = c("text/csv",
                                   #                                      "text/comma-separated-values,text/plain",
                                   #                                      ".csv"))
                                   #                 
                                   #     )
                                   #     
                                   # ),
                                   
                                   #Box to display the prediction results
                                   box(title = 'Prediction result', status = 'success', solidHeader = TRUE, width = 4, height = 260,
                                       div(h5('Covid:')),
                                       verbatimTextOutput("value", placeholder = TRUE),
                                       actionButton('cal','Calculate', icon = icon('calculator'))),
                                   #Box to display information about the model
                                   box(title = 'Model explanation', status = 'success', width = 8, height = 260,
                                       helpText('The following model will predict whether a patient has Covid or not based on Platelets count, Mean Platelet Volume and Red blood Cells.'),
                                       helpText('The name of the dataset used to train the model is "final_cp", taken from BC2407 Team 1 website. The data contains 500 observations and 16 attributes related to lab.'),
                                       helpText('The prediction is based on a random forest supervised machine learning model and has an accuracy of 88%.')))
                  )
    )
)

# R Shiny server
server <- shinyServer(function(input, output) {
    
    #Univariate analysis
    output$histPlot <- renderPlot({
        
        #Column name variable
        num_val = ifelse(input$num == 'Platelets', 'Platelets',
                         ifelse(input$num == 'Mean_platelet_volume', 'Mean_platelet_volume',
                                ifelse(input$num == 'Red_blood_Cells', 'Red_blood_Cells',
                                       )
                                )
                         )
        
        #Histogram plot
        ggplot(data = covid.dt, aes(x = covid.dt[[num_val]]))+ 
            geom_histogram(stat = "bin", fill = 'steelblue3', color = 'lightgrey')+
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face = 'bold'))+
            labs(title = sprintf('Histogram plot of the variable %s', num_val),
                 x = sprintf('%s', input$num),y = 'Frequency')+
            stat_bin(geom = 'text', 
                     aes(label = ifelse(..count.. == max(..count..), as.character(max(..count..)), '')),
                     vjust = -0.6)
    })
    
    output$freqPlot <- renderPlot({
        
        #Column name variable
        cat_val = ifelse(input$cat == 'age_60_and_above', 'age_60_and_above',
                         ifelse(input$cat == 'gender', 'gender',
                                ))
        
        #Frecuency plot
        ggplot(data = covid.dt, aes(x = covid.dt[[cat_val]]))+
            geom_bar(stat = 'count', fill = 'mediumseagreen', 
                     width = 0.5)+
            stat_count(geom = 'text', size = 4,
                       aes(label = ..count..),
                       position = position_stack(vjust = 1.03))+
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face="bold"))+
            labs(title = sprintf('Frecuency plot of the variable %s', cat_val),
                 x = sprintf('%s', input$cat), y = 'Count')
        
    })

    #Prediction model
    #React value when using the action button
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$cal, {
        #Copy of the test data without the dependent variable
        test_pred <- mod1_testset

        values = data.frame(
            Platelets = input$p_platelets,
            Mean_platelet_volume = input$p_meanpv,
            Red_blood_Cells = input$p_rbc,
            Leukocytes = input$p_leu,
            Serum_Glucose = input$p_ser,
            Urea = input$p_urea,
            Proteina_C_reativa = input$p_proteina,
            Potassium = input$p_pot,
            Creatinine = input$p_creatinine,
            Sodium = input$p_sod,
            cough = input$p_cough,
            fever = input$p_fever,
            sore_throat = input$p_soret,
            shortness_of_breath = input$p_sob,
            head_ache = input$p_headache,
            age_60_and_above = input$p_age,
            gender = input$p_gender,
            V1 = mean(mod1_testset$V1)
        )
        test_pred <- rbind(test_pred,values, fill=TRUE)
        test_pred$gender[nrow(test_pred)] <- as.factor(input$p_gender)
        test_pred$age_60_and_above[nrow(test_pred)] <- as.factor(input$p_age)
        
        #Single prediction using the randomforest model
        prediction <- predict(m.RF, newdata = test_pred,type="class")
        if (prediction[length(prediction)] == "negative") {
            a$result <- "Negative"
        } else {
            a$result <- "Positive"
        }
    })
    
    output$value <- renderText({
        #Display the prediction value
        input$cal
        paste(a$result[length(a$result)])
    })
    
    
})

shinyApp(ui, server) 