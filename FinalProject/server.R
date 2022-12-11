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
      data_subset <- wine[ ,cols]
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
      
})

