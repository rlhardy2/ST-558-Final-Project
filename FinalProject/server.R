#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)
library(caret)

#Define server
shinyServer(function(input, output, session) {
    
    #Code for rendering the image
    output$wine_picture <- renderImage({
      list(src = "winepicture.jpg",
           width = 450,
           height = 300)
      }, deleteFile = FALSE)
    
    
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
          ggplot(wine, aes_string(x = var)) + geom_histogram(fill = "blue")
        }
        else if(wine_type == "red"){
          temp_red <- wine[wine$type == "red", ]
          ggplot(temp_red, aes_string(x = var)) + geom_histogram(fill = "blue")
        }
        else if(wine_type == "white"){
          temp_white <- wine[wine$type == "white", ]
          ggplot(temp_white, aes_string(x = var)) + geom_histogram(fill = "blue")
        }
      }
      else if(graph_type == "Box Plot"){
        
        if(wine_type == "all"){
          ggplot(wine, aes_string(x = var)) + geom_boxplot()
        }
        else if(wine_type == "red"){
          temp_red <- wine[wine$type == "red", ]
          ggplot(temp_red, aes_string(x = var)) + geom_boxplot()
        }
        else if(wine_type == "white"){
          temp_white <- wine[wine$type == "white", ]
          ggplot(temp_white, aes_string(x = var)) + geom_boxplot()
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
    
    
    #Print out the results from above
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
    
    
    #Prediction tab
    pred_tab <- reactice({
      
    })
    
    
    #Rendering the output for the prediction tab
    output$prediction_tab <- renderPrint({
      
    })
    
    
})

