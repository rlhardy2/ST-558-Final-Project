#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)

red <- read_csv2("winequality-red.csv")
white <- read_csv2("winequality-white.csv")

dashboardPage(skin = "blue",
    
    #Add a title
    dashboardHeader(title = "Wine Quality"),
    
    #Create sidebar tabs with icons
    dashboardSidebar(sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("archive")),
        menuItem("Data", tabName = "data", icon = icon("folder-open")),
        menuItem("Data Exploration", tabName = "exploration", icon = icon("calculator")),
        menuItem("Modeling", tabName = "modeling", icon = icon("line-chart"))
    )),
    
    #Create the body of the app
    dashboardBody(
        tabItems(
            
            #First tab content
            tabItem(tabName = "about",
                    h2("Wine Quality App - Information"),
                    br(),
                    h4("About the app..."),
                    br(),
                    h4("More about the app..."),
                    br(),
                    h4("Put image here somehow.")),
            
            tabItem(tabName = "data",
                    h2("Wine Quality App - Data"))
            
        )
    )
)















