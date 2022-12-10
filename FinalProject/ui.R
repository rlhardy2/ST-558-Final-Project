#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)

red <- read_csv2("winequality-red.csv") %>% mutate(type = "red") %>% select(type, everything())
white <- read_csv2("winequality-white.csv") %>% mutate(type = "white") %>% select(type, everything())
wine <- rbind(red, white)

wine <- wine %>% rename(fixed_acidity = `fixed acidity`,
                        volatile_acidity = `volatile acidity`,
                        citric_acid = `citric acid`,
                        residual_sugar = `residual sugar`,
                        free_sulfur_dioxide = `free sulfur dioxide`,
                        total_sulfur_dioxide = `total sulfur dioxide`)

wine$volatile_acidity <- as.numeric(wine$volatile_acidity)
wine$citric_acid <- as.numeric(wine$citric_acid)
wine$residual_sugar <- as.numeric(wine$residual_sugar)
wine$chlorides <- as.numeric(wine$chlorides)
wine$density <- as.numeric(wine$density)
wine$sulphates <- as.numeric(wine$sulphates)

dashboardPage(skin = "blue",
    
    #Add a title
    dashboardHeader(title = "Wine Quality App"),
    
    #Create sidebar tabs with icons
    dashboardSidebar(sidebarMenu(id = "sidebar",
        menuItem("About", tabName = "about", icon = icon("archive")),
        menuItem("Data", tabName = "data", icon = icon("folder-open")),
        menuItem("Data Exploration", tabName = "exploration", icon = icon("calculator")),
            menuSubItem("Numerical Summaries", tabName = "summaries"),
            menuSubItem("Contingency Tables", tabName = "tables"),
            menuSubItem("Graphical Summaries", tabName = "graphs"),
        menuItem("Modeling", tabName = "modeling", icon = icon("line-chart")),
            menuSubItem("Modeling Information", tabName = "info"),
            menuSubItem("Model Fitting", tabName = "fitting"),
            menuSubItem("Prediction", tabName = "prediction")
    )),
    
    #Create the body of the app
    dashboardBody(
        tabItems(
            
            #First tab content - About
            tabItem(tabName = "about",
                    h2(strong("Wine Quality App - Information")),
                    br(),
                    h4("About the app..."),
                    br(),
                    h4("More about the app..."),
                    br(),
                    h4("Put image here somehow.")
                    ),
            
            #Second tab content - Data
            tabItem(tabName = "data",
                    h2(strong("Wine Quality App - Data")),
                    br(),
                    fluidRow(
                        column(width = 6,
                               box(width = 12,
                                   title = strong("Subsetting rows"),
                                   background = "light-blue",
                                   selectInput(inputId = "wine_type", label = "Wine Type",
                                               choices = c("red", "white"), selected = "red"))),
                        column(width = 6,
                               box(width = 12,
                                   title = strong("Subsetting columns"),
                                   background = "light-blue"))
                    ),
                    DT::dataTableOutput("table")
                    ),
            
            #Third tab content - Data Exploration
            tabItem(tabName = "exploration",
                    h2(strong("Wine Quality App - Data Exploration")),
                    br()
                    ),
            
            #First sub tab content - Numerical Summaries
            tabItem(tabName = "summaries",
                    h3(strong("Wine Quality App - Numerical Summaries")),
                    fluidRow(
                      column(width = 12,
                             box(width = 12,
                                 title = strong("Summarizing variables (mean and standard deviation)"),
                                 background = "light-blue",
                                 selectInput(inputId = "summary", label = "Variables to Summarize",
                                             choices = c("fixed_acidity", "volatile_acidity", "citric_acid",
                                                         "residual_sugar", "chlorides", "density",
                                                         "pH", "sulphates", "alcohol", "free_sulfur_dioxide",
                                                         "total_sulfur_dioxide"), 
                                             selected = "fixed_acidity")))),
                    DT::dataTableOutput("summary")
                    ),
            
            #Second sub tab content - Contingency Tables
            tabItem(tabName = "tables",
                    h3(strong("Wine Quality App - Contingency Tables"))
                    #Code goes here
                    ),
            
            #Third sub tab content - Graphical Summaries
            tabItem(tabName = "graphs",
                    h3(strong("Wine Quality App - Graphical Summaries"))
                    #Code goes here
                    ),
            
            #Fourth tab content - Modeling
            tabItem(tabName = "modeling",
                    h2(strong("WIne Quality App - Modeling")),
                    br()
                    )
        )
    )
)
















