#Rachel Hardy
#Final Project
#12/12/2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)

###########################################################################################################

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

##########################################################################################################

#Creating the dashboard page with blue skin
dashboardPage(skin = "blue",
    
    #Adding a title for the app
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
                    fluidPage(
                        h2(strong("Wine Quality App - Information")),
                        br(),
                        p("Write stuff here!"),
                        imageOutput("wine_picture")
                    )
            ),
            
            
            #Second tab content - Data
            tabItem(tabName = "data",
                    fluidPage(
                        h2(strong("Wine Quality App - View the Data")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4("Subset the data by columns and wine type"),
                                selectInput("cols", "Select columns", choices = c("type", "quality", "quality_level", 
                                            "fixed_acidity", "volatile_acidity", "citric_acid",
                                            "residual_sugar", "chlorides", "pH", "sulphates", "density", "alcohol", 
                                            "free_sulfur_dioxide", "total_sulfur_dioxide"), 
                                            selected = c("type", "quality", "quality_level"), 
                                            multiple = TRUE),
                                selectInput("wine_type", "Select wine type", choices = c("red", "white", "red and white"),
                                            selected = "red and white"),
                                submitButton("Generate Data Set"),
                                br(),
                                p("Download a .csv file of the sub-setted data"),
                                downloadButton("download", "Download")
                            ),
                            mainPanel(
                                DT::dataTableOutput("data_table")
                            )
                        )
                    )
            ),
                    
            
            #Third tab content - Data Exploration
            tabItem(tabName = "exploration",
                    fluidPage(
                        h2(strong("Wine Quality App - Data Exploration")),
                        br(),
                        p("Write about what the tabs include.")
                    )
            ),
            
            
            #First sub tab content - Numerical Summaries
            tabItem(tabName = "summaries",
                    fluidPage(
                        h3(strong("Wine Quality App - Numerical Summaries")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4("Mean and standard deviation of chosen variable grouped by wine type and 
                                   quality level"),
                                selectInput("summary", label = "Choose which variable to summarize",
                                            choices = c("fixed_acidity", "volatile_acidity", "citric_acid",
                                                        "residual_sugar", "chlorides", "pH", "sulphates", 
                                                        "free_sulfur_dioxide", "total_sulfur_dioxide"), 
                                            selected = "pH"),
                                submitButton("Generate Grouped Summary"),
                                br(),
                                h4("Summary statistics for chosen variable"),
                                selectInput("stats", label = "Choose which variable to summarize",
                                            choices = c("fixed_acidity", "volatile_acidity", "citric_acid",
                                                        "residual_sugar", "chlorides", "pH", "sulphates", 
                                                        "free_sulfur_dioxide", "total_sulfur_dioxide"), 
                                            selected = "pH"),
                                submitButton("Generate Summary Statistics")
                            ),
                            mainPanel(
                                 DT::dataTableOutput("summary"),
                                 verbatimTextOutput("stats")
                            )
                        ) 
                    )
            ),
            
            
            #Second sub tab content - Contingency Tables
            tabItem(tabName = "tables",
                    fluidPage(
                        h3(strong("Wine Quality App - Contingency Tables")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4("Contingency table for chosen variable(s)"),
                                selectInput("contingency", "Select which contingency table to generate", 
                                            choices = list("type", "quality_level", "type vs quality_level"),
                                            selected = "type"),
                                submitButton("Generate Table")
                            ),
                            mainPanel(
                                verbatimTextOutput("contingency")
                            )
                        )
                    )
            ),
            
            
            #Third sub tab content - Graphical Summaries
            tabItem(tabName = "graphs",
                    fluidPage(
                        h3(strong("Wine Quality App - Graphical Summaries")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4("Graph for chosen quantitative variable"),
                                radioButtons("graph_choice", "Select which graph to generate",
                                             choices = c("Histogram", "Box Plot"), 
                                             selected = "Histogram"),
                                selectInput("quan_var", "Select variable", 
                                            choices = c("fixed_acidity", 
                                            "volatile_acidity",
                                            "citric_acid", "residual_sugar", "chlorides", "pH", "sulphates", "density",
                                            "free_sulfur_dioxide", "total_sulfur_dioxide"),
                                            selected = "pH"),
                                selectInput("graph_wine", "Select wine type", 
                                            choices = c("red", "white", "red and white"), 
                                            selected = "red and white"),
                                submitButton("Generate Graph"),
                                br(),
                                h4("Bar plot for chosen categorical variable"),
                                selectInput("cate_var", "Select variable",
                                            choices = c("quality", "quality_level"),
                                            selected = "quality_level"),
                                radioButtons("bar_group", "Group by wine type?",
                                             choices = c("Yes", "No"),
                                             selected = "No"),
                                submitButton("Generate Bar Plot")
                            ),
                            mainPanel(
                                plotOutput("graph_summary"),
                                plotOutput("bar_plot")
                            )
                        )
                    )
            ),
            
            
            #Fourth tab content - Modeling
            tabItem(tabName = "modeling",
                    fluidPage(
                        h2(strong("WIne Quality App - Modeling")),
                        br(),
                        p("Write about what the tabs include.")
                    )
            ),
            
            
            #First sub tab content - Modeling Information
            tabItem(tabName = "info",
                    fluidPage(
                        h3(strong("Wine Quality App - Modeling Information")),
                        br(),
                        p("Write about the different models here.")
                    )
            ),
            
            
            #Second sub tab content - Model Fitting
            tabItem(tabName = "fitting",
                    fluidPage(
                        h3(strong("Wine Quality App - Model Fitting")),
                        br()
                    )
            ),
            
            
            #Third sub tab content - Prediction
            tabItem(tabName = "prediction",
                    fluidPage(
                        h3(strong("Wine Quality App - Prediction")),
                        br()
                    )
            )
        )
    )
)
















