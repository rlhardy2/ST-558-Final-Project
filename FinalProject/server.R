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
    
    
    #Subset the data based on user selection of columns
    data_subset <- reactive({
      cols <- input$cols
      rows <- input$wine_type
      
      if(rows == "red and white"){
        data_subset <- wine[ ,cols]
        data_subset
      }
      else if(rows == "red"){
        data_subset <- wine[wine$type == "red", cols]
        data_subset
      }
      else if(rows == "white"){
        data_subset <- wine[wine$type == "white", cols]
        data_subset
      }
    })
    
    
    #Rendering the above sub-setted data into a table
    output$data_table <- DT::renderDataTable({
      data_subset()
    })
    
    
    #This if for downloading the data
    output$download <- downloadHandler(
      filename = "wine.csv",
      content = function(file) {
        write.csv(data_subset(), file, row.names = FALSE)
      }
    )
    
    
    #Create numerical summaries for the user selected variable
    summaries <- reactive({
      var <- input$summary
      tab <- wine %>% select("type", "quality_level", var) %>%
        group_by(type, quality_level) %>%
        summarize(mean = round(mean(get(var)), 2))
    })
    
    
    #Numerical summary generation code
    output$summary <- DT::renderDataTable({
      summaries()
    })
    
    
    #Create some contingency tables
    contingency <- reactive({
      table <- input$contingency
      
      if(table == "type"){
        tab <- data.frame(table(wine$type))
        tab
      }
      else if(table == "quality_level"){
        tab <- data.frame(table(wine$quality_level))
        tab
      }
      else if(table == "type vs quality_level"){
        tab <- data.frame(table(wine$type, wine$quality_level))
        tab
      }
    })
    
    
    #Renders the contingency table from above
    output$contingency <- renderPrint({
      contingency()
    })
  
    
    #Create summary statistics
    summary_stats <- reactive({
      var <- input$stats
      sum <- as.data.frame(wine[[var]])
      colnames(sum) <- var
      summary(sum)
    })
    
    
    #Renders the summary statistics above
    output$stats <- renderPrint({
      summary_stats()
    })
    
    
    #Create graphical summaries - histogram or box plot
    graph_summary <- reactive({
      graph_type <- input$graph_choice
      var <- input$quan_var
      wine_type <- input$graph_wine
      
      if(graph_type == "Histogram"){
        
        if(wine_type == "red and white"){
          ggplot(wine, aes_string(x=var)) + geom_histogram(fill = "blue")
        }
        else if(wine_type == "red"){
          temp_red <- wine[wine$type == "red", ]
          ggplot(temp_red, aes_string(x=var)) + geom_histogram(fill = "blue")
        }
        else if(wine_type == "white"){
          temp_white <- wine[wine$type == "white", ]
          ggplot(temp_white, aes_string(x=var)) + geom_histogram(fill = "blue")
        }
      }
      else if(graph_type == "Box Plot"){
        
        if(wine_type == "red and white"){
          ggplot(wine, aes_string(x=var)) + geom_boxplot()
        }
        else if(wine_type == "red"){
          temp_red <- wine[wine$type == "red", ]
          ggplot(temp_red, aes_string(x=var)) + geom_boxplot()
        }
        else if(wine_type == "white"){
          temp_white <- wine[wine$type == "white", ]
          ggplot(temp_white, aes_string(x=var)) + geom_boxplot()
        }
      }
    })
    
    
    #Renders the graph created above
    output$graph_summary <- renderPlot({
      graph_summary()
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
      
})

