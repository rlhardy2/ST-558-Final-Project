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
            menuSubItem("Performance", tabName = "performance"),
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
                                h4(strong("Subset the data by columns and wine type")),
                                selectInput("cols", "Select columns", choices = c("type", "quality", "quality_level", 
                                            "fixed_acidity", "volatile_acidity", "citric_acid",
                                            "residual_sugar", "chlorides", "pH", "sulphates", "density", "alcohol", 
                                            "free_sulfur_dioxide", "total_sulfur_dioxide"), 
                                            selected = c("type", "quality", "quality_level"), 
                                            multiple = TRUE),
                                selectInput("wine_type", "Select wine type", choices = c("red", "white", "all"),
                                            selected = "all"),
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
                        h2(strong("Wine Quality App - Numerical Summaries")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4(strong("Top item is a table of mean and standard deviation of chosen variable with optional grouping 
                                        and second item is summary statistics for the chosen variable")),
                                selectInput("summary", label = "Choose which variable to summarize",
                                            choices = c("fixed_acidity", "volatile_acidity", "citric_acid",
                                                        "residual_sugar", "chlorides", "pH", "sulphates", 
                                                        "free_sulfur_dioxide", "total_sulfur_dioxide"), 
                                            selected = "pH"),
                                radioButtons("grouping", "Choose grouping option", 
                                             choices = c("none", "type", "quality_level", "type and quality_level"),
                                             selected = "none"),
                                submitButton("Generate Summary")
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
                        h2(strong("Wine Quality App - Contingency Tables")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4(strong("Contingency table for chosen variable(s)")),
                                selectInput("contingency", "Select which contingency table to generate", 
                                            choices = list("type", "quality_level", "quality", "type vs quality_level",
                                                           "type vs quality"),
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
                        h2(strong("Wine Quality App - Graphical Summaries")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4(strong("Graph for chosen quantitative variable")),
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
                                            choices = c("red", "white", "all"), 
                                            selected = "all"),
                                br(),
                                h4(strong("Bar plot for chosen categorical variable")),
                                selectInput("cate_var", "Select variable",
                                            choices = c("quality", "quality_level"),
                                            selected = "quality_level"),
                                radioButtons("bar_group", "Group by wine type?",
                                             choices = c("Yes", "No"),
                                             selected = "No"),
                                submitButton("Generate Graph")
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
                        h2(strong("Wine Quality App - Modeling Information")),
                        br(),
                        p("Write about the different models here.")
                    )
            ),
            
            
            #Second sub tab content - Model Fitting
            tabItem(tabName = "fitting",
                    fluidPage(
                        h2(strong("Wine Quality App - Model Fitting")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4(strong("Specify the parameters for model fitting and choose which model to simulate")),
                                h5("Note: the selected proportion of training data may not be less than 0.5 or greater
                                   than 0.9!"),
                                numericInput("proportion", "Select proportion of data to be used for training",
                                             value = 0.7, min = 0.5, max = 0.9, step = 0.05),
                                numericInput("cv", "Select number of folds to use for cross-validation",
                                             value = 5, min = 2, max = 10, step = 1),
                                selectInput("predictors", "Select predictor variables", choices = c("fixed_acidity", 
                                        "volatile_acidity", "citric_acid", 
                                        "residual_sugar", "chlorides","pH", "sulphates", "density", "alcohol", 
                                         "free_sulfur_dioxide"), 
                                          selected = c("residual_sugar", "alcohol", "pH"), 
                                          multiple = TRUE),
                                radioButtons("train", "Choose which model to simulate",
                                             choices = c("Multiple Linear Regression", "Regression Tree",
                                                         "Random Forest", "All Models"),
                                             selected = "Multiple Linear Regression"),
                                submitButton("Generate Model")
                            ),
                            mainPanel(
                                h3(strong("Results for Modeling on the Training Data Set")),
                                h5("Note: this page may take a few minutes to load if the Random Forest model is chosen, be patient!"),
                                verbatimTextOutput("training_model")
                            )
                        )
                    )
            ),
            
            
            #Third sub tab content - Performance
            tabItem(tabName = "performance",
                    fluidPage(
                        h2(strong("Wine Quality App - Performance")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                h4(strong("Choose which model to simulate")),
                                radioButtons("model", "Choose which model to simulate",
                                            choices = c("Multiple Linear Regression", "Regression Tree",
                                                        "Random Forest"),
                                            selected = "Multiple Linear Regression"),
                                submitButton("Generate Performance Statistics")
                            ),
                            mainPanel(
                                 h3(strong("Results for Modeling and Prediction on the Test Data Set")),
                                 h5("Note: this page may take a few minutes to load if the Random Forest model is chosen, be patient!"),
                                 verbatimTextOutput("performance")
                            )
                        )
                    )
            ),
            
            
            #Fourth sub tab content - Prediction
            tabItem(tabName = "prediction",
                    fluidPage(
                        h2(strong("Wine Quality App - Prediction")),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                            ),
                            mainPanel(
                                
                            )
                        )
                    )
            )
        )
    )
)
















