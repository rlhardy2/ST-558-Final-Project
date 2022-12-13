#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)
library(caret)
library(mathjaxr)

###########################################################################################################

setwd("C:/Users/Rachel Hardy/OneDrive/Documents/NCSU/ST 558/Projects/Final Project/ST-558-Final-Project/FinalProject")

#Reading in the data and combining the red and white data sets into a single data set called "wine"
red <- read_csv2("winequality-red.csv") %>% mutate(type = "red") %>% select(type, everything())
white <- read_csv2("winequality-white.csv") %>% mutate(type = "white") %>% select(type, everything())
wine <- rbind(red, white)

#Renaming some variables so they are easier to deal with
wine <- wine %>% rename(fixed_acidity = `fixed acidity`,
                        volatile_acidity = `volatile acidity`,
                        citric_acid = `citric acid`,
                        residual_sugar = `residual sugar`,
                        free_sulfur_dioxide = `free sulfur dioxide`,
                        total_sulfur_dioxide = `total sulfur dioxide`)

#Not all of the variables were numeric, so I am fixing that here
wine$volatile_acidity <- as.numeric(wine$volatile_acidity)
wine$citric_acid <- as.numeric(wine$citric_acid)
wine$residual_sugar <- as.numeric(wine$residual_sugar)
wine$chlorides <- as.numeric(wine$chlorides)
wine$density <- as.numeric(wine$density)
wine$sulphates <- as.numeric(wine$sulphates)

#Creating a new categorical variable called quality_level
wine <- wine %>% mutate(quality_level = if_else((quality == 3 | quality == 4), "low", 
                                                if_else((quality == 5 | quality == 6), "medium",
                                                        if_else((quality == 7 | quality == 8), "high",
                                                                if_else((quality == 9), "very high", " ")))))

#Converting the new categorical variable quality_level into a factor and ordering the levels
wine$quality_level <- factor(wine$quality_level, levels = c("low", "medium", "high", "very high"))

#The alcohol variables is acting really strange and not being read in correctly
#I'm going to remove the crazy large outliers with the filtering code below
#Also removing observations with missing values

wine <- wine %>% filter(alcohol < 1000) %>% na.omit()

##########################################################################################################

#Define server
shinyServer(function(input, output, session) {
    
    #Code for rendering the image
    output$wine_picture <- renderImage({
      list(src = "winepicture.jpg",
           width = 450,
           height = 300)
      }, deleteFile = FALSE) 
    
    
    #MathJax code
    output$math_jax <- renderUI({
      withMathJax(helpText('$$Y = a + b_1X_1 + b_2X_2 + ... + b_nX_n$$'))
    })
    
    
    #Subset the data based on user selection of columns and rows (type and quality level)
    data_subset <- reactive({
      cols <- input$cols
      type <- input$wine_type
      level <- input$level
      
      if(type == "all"){
        if(level == "all"){
          data <- wine[ ,cols]
          data
        }
        else if(level == "low"){
          data <- wine[wine$quality_level == "low", cols]
          data
        }
        else if(level == "medium"){
          data <- wine[wine$quality_level == "medium", cols]
          data
        }
        else if(level == "high"){
          data <- wine[wine$quality_level == "high", cols]
          data
        }
        else if(level == "very high"){
          data <- wine[wine$quality_level == "very high", cols]
          data
        }
      }
      else if(type == "red"){
        if(level == "all"){
          data <- wine[wine$type == "red" ,cols]
          data
        }
        else if(level == "low"){
          data <- wine[(wine$quality_level == "low" & wine$type == "red"), cols]
          data
        }
        else if(level == "medium"){
          data <- wine[(wine$quality_level == "medium" & wine$type == "red"), cols]
          data
        }
        else if(level == "high"){
          data <- wine[(wine$quality_level == "high" & wine$type == "red"), cols]
          data
        }
        else if(level == "very high"){
          data <- wine[(wine$quality_level == "very high" & wine$type == "red"), cols]
          data
        }
      }
      else if(type == "white"){
        if(level == "all"){
          data <- wine[wine$type == "white" ,cols]
          data
        }
        else if(level == "low"){
          data <- wine[(wine$quality_level == "low" & wine$type == "white"), cols]
          data
        }
        else if(level == "medium"){
          data <- wine[(wine$quality_level == "medium" & wine$type == "white"), cols]
          data
        }
        else if(level == "high"){
          data <- wine[(wine$quality_level == "high" & wine$type == "white"), cols]
          data
        }
        else if(level == "very high"){
          data <- wine[(wine$quality_level == "very high" & wine$type == "white"), cols]
          data
        }
      }
    })
    
    
    #Rendering the above sub-setted data into a table
    output$data_table <- DT::renderDataTable({
      data_subset()
    })
    
    
    #Downloading the data as a .csv file
    output$download <- downloadHandler(
      filename = "wine.csv",
      content = function(file) {
        write.csv(data_subset(), file, row.names = FALSE)
      }
    )
    
    
    #Create numerical summaries (mean and SD) for the user selected variable with grouping option
    summaries <- reactive({
      var <- input$summary
      group <- input$grouping
      
      if(group == "none"){
        tab <- wine %>% select(var) %>% 
          summarize(mean = round(mean(get(var)), 2), sd = round(sd(get(var)), 2))
      }
      else if(group == "type"){
        tab <- wine %>% 
          select("type", var) %>% 
          group_by(type) %>%
          summarize(mean = round(mean(get(var)), 2), sd = round(sd(get(var)), 2))
      }
      else if(group == "quality_level"){
        tab <- wine %>% 
          select("quality_level", var) %>% 
          group_by(quality_level) %>%
          summarize(mean = round(mean(get(var)), 2), sd = round(sd(get(var)), 2))
      }
      else if(group == "type and quality_level"){
        tab <- wine %>% 
          select("type", "quality_level", var) %>%
          group_by(type, quality_level) %>%
          summarize(mean = round(mean(get(var)), 2), sd = round(sd(get(var)), 2))
      }
    })
    
    
    #Render numerical summary above
    output$summary <- DT::renderDataTable({
      summaries()
    })
    
    
    #Create summary statistics
    summary_stats <- reactive({
      var <- input$summary
      sum <- as.data.frame(wine[[var]])
      colnames(sum) <- var
      summary(sum)
    })
    
    
    #Renders summary statistics above
    output$stats <- renderPrint({
      summary_stats()
    })
    
    
    #Create some contingency tables
    contingency <- reactive({
      table <- input$contingency
      
      if(table == "type"){
        tab <- data.frame(table(wine$type))
        colnames(tab) <- c("type", "freq")
        tab
      }
      else if(table == "quality_level"){
        tab <- data.frame(table(wine$quality_level))
        colnames(tab) <- c("quality_level", "freq")
        tab
      }
      else if(table == "quality"){
        tab <- data.frame(table(wine$quality))
        colnames(tab) <- c("quality", "freq")
        tab
      }
      else if(table == "type vs quality_level"){
        tab <- data.frame(table(wine$type, wine$quality_level))
        colnames(tab) <- c("type", "quality_level", "freq")
        tab
      }
      else if(table == "type vs quality"){
        tab <- data.frame(table(wine$type, wine$quality))
        colnames(tab) <- c("type", "quality", "freq")
        tab
      }
    })
    
    
    #Render the contingency tables from above
    output$contingency <- renderPrint({
      contingency()
    })
    
    
    #Create graphical summaries - histogram or box plot
    graph_summary <- reactive({
      graph_type <- input$graph_choice
      var <- input$quan_var
      wine_type <- input$graph_wine
      
      if(graph_type == "Histogram"){
        
        if(wine_type == "all"){
          ggplot(wine, aes_string(x = var)) + geom_histogram(fill = "steelblue")
        }
        else if(wine_type == "red"){
          temp_red <- wine[wine$type == "red", ]
          ggplot(temp_red, aes_string(x = var)) + geom_histogram(fill = "steelblue")
        }
        else if(wine_type == "white"){
          temp_white <- wine[wine$type == "white", ]
          ggplot(temp_white, aes_string(x = var)) + geom_histogram(fill = "steelblue")
        }
      }
      else if(graph_type == "Box Plot"){
        
        if(wine_type == "all"){
          ggplot(wine, aes_string(x = var)) + geom_boxplot(color = "steelblue")
        }
        else if(wine_type == "red"){
          temp_red <- wine[wine$type == "red", ]
          ggplot(temp_red, aes_string(x = var)) + geom_boxplot(color = "steelblue")
        }
        else if(wine_type == "white"){
          temp_white <- wine[wine$type == "white", ]
          ggplot(temp_white, aes_string(x = var)) + geom_boxplot(color = "steelblue")
        }
      }
    })
    
    
    #Renders the graph created above
    output$graph_summary <- renderPlot({
      graph_summary()
    })
    
    
    #Create graphical summaries - bar plot
    bar_plot <- reactive({
      var <- input$cate_var
      group <- input$bar_group
      
      if(group == "No"){
        ggplot(wine, aes_string(x = var)) + geom_bar() 
      }
      else if(group == "Yes"){
        ggplot(wine, aes_string(x = var)) + geom_bar(aes(fill = type), position = "dodge")
      }
    })
    
    
    #Renders the graph created above
    output$bar_plot <- renderPlot({
      bar_plot()
    })
    
    
    #Splitting the data into testing and training sets based on user-selected proportion value and predictor variables
    data_split <-reactive({
      predictors <- input$predictors
      subset <- wine[ , predictors]
      subset$quality <- wine$quality
      subset$qulity_level <- wine$quality_level
      
      train_index <- createDataPartition(subset$quality, p = (input$proportion), list = FALSE)
      
      train <- subset[train_index, ]
      test <- subset[-train_index, ]
      
      list(train = train, test = test)
    })
    
    
    #Multiple linear regression model - training
    MLR_train <- reactive({
      MLR <- train(quality ~., data = data_split()$train,
                   method = "lm",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = input$cv))
      MLR
    })
    
    
    #Regression tree model - training
    RT_train <- reactive({
      RT <- train(quality ~ ., data = data_split()$train,
                  method = "rpart",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = input$cv))
      RT
    })
    
    
    #Random forest model - training
    RF_train <- reactive({
      RF <- train(quality ~ ., data = data_split()$train,
                  method = "rf",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = input$cv))
      RF
    })
    
    
    #Outputting chosen model on training data
    output$training_model <- renderPrint({
      
      if(input$train == "Multiple Linear Regression"){
        MLR_train()
      }
      else if(input$train == "Regression Tree"){
        RT_train()
      }
      else if(input$train == "Random Forest"){
        RF_train()
      }
    })
    
    
    #Multiple linear regression prediction / performance
    MLR_test <- reactive({
      pred <- predict(MLR_train(), newdata = data_split()$test)
      perf <- postResample(pred, obs = data_split()$test$quality)
      
      list(prediction = pred, performance = perf)
    })
    
    
    #Regression tree prediction / performance
    RT_test <- reactive({
      pred <- predict(RT_train(), newdata = data_split()$test)
      perf <- postResample(pred, obs = data_split()$test$quality)
      
      list(prediction = pred, performance = perf)
    })
    
    
    #Random forest prediction / performance
    RF_test <- reactive({
      pred <- predict(RF_train(), newdata = data_split()$test)
      perf <- postResample(pred, obs = data_split()$test$quality)
      
      list(prediction = pred, performance = perf)
    })
    
    
    #Print out the performance results from above
    output$performance <- renderPrint({
      
      if(input$model == "Multiple Linear Regression"){
        MLR_test()$performance
      }
      else if(input$model == "Regression Tree"){
        RT_test()$performance
      }
      else if(input$model == "Random Forest"){
        RF_test()$performance
      }
    })
    
    
    #Print out the prediction values from above
    output$pred_values <- renderPrint({
      
      if(input$model == "Multiple Linear Regression"){
        as_tibble(MLR_test()$prediction)
      }
      else if(input$model == "Regression Tree"){
        as_tibble(RT_test()$prediction)
      }
      else if(input$model == "Random Forest"){
        as_tibble(RF_test()$prediction)
      }
    })
    
    
    #Creating new data frame with user inputted predictor values
    new_data <- reactive({
      data <- data.frame(
        type = input$pred_wine,
        residual_sugar = input$pred_sugar,
        pH = input$pred_pH,
        alcohol = input$pred_alcohol,
        fixed_acidity = input$pred_fixed_acid,
        volatile_acidity = input$pred_volatile_acid,
        citric_acid = input$pred_citric_acid,
        chlorides = input$pred_chlorides,
        sulphates = input$pred_sulphates,
        density = input$pred_density,
        free_sulfur_dioxide = input$pred_free_sulfur,
        total_sulfur_dioxide = input$pred_total_sulfur)
      data
    })
    
    
    #Calculating prediction
    model_predict <- reactive ({
      model <- input$model_predict
      
      if(model == "Multiple Linear Regression"){
        
      }
      else if(model == "Regression Tree"){
        pred <- predict(RT_train(), newdata = new_data()$data)
        pred
      }
      else if(model == "Random Forest"){
        pred <- predict(RF_train(), newdata = new_data()$data)
        pred
      }
    })
    
    
    #Printing the prediction result
    final_predict <- renderPrint({
      model_predict()
    })
    
})

