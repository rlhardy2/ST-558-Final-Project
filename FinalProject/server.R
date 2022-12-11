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
           width = 550,
           height = 400)
      }, deleteFile = FALSE)
    
    #Subset the data based on user selection
    data_subset <- reactive({
      cols <- input$cols
      data_subset <- wine[ ,cols]
    })
    
    #Rendering the above sub-setted data into a table
    output$data_table <- DT::renderDataTable({
      data_subset()
    })
    
    #Data frame for "Data" tab
    output$table <- DT::renderDataTable(wine)
    
    #This if for downloading the data
    output$download <- downloadHandler(
      filename = "wine.csv",
      content = function(file) {
        write.csv(data_subset(), file, row.names = FALSE)
      }
    )
    
    #Mean summary for "Data Exploration" tab
    output$summary <- DT::renderDataTable({
      var <- input$summary
      tab <- wine %>% 
        select("type", "quality_level", var) %>%
        group_by(type, quality_level) %>%
        summarize(mean = round(mean(get(var)), 2), sd = round(sd(get(var)), 2))
      tab
    })
    
})
