#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$picture <- renderImage({
        
    })

})
