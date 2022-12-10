#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #Data frame for "Data" tab
    output$table <- DT::renderDataTable(wine)
    
    #Mean summary for "Data Exploration" tab
    output$summary <- DT::renderDataTable({
      var <- input$summary
      tab <- wine %>% 
        select("type", "quality", var) %>%
        group_by(type, quality) %>%
        summarize(mean = round(mean(get(var)), 2), sd = round(sd(get(var)), 2))
      tab
    })

})
