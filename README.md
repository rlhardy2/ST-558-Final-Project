# ST-558-Final-Project
GItHub repository for ST 558 Final Project.

## Wine Quality App Overview

The app that I made uses data sets found from the **UCI Machine Learning Repository**. The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. The data can be found [here](https://archive.ics.uci.edu/ml/datasets/wine+quality).

The app has several setions and allows the user to subset and download the data, render numerical and graphical summmaries of the data, and render different predictive models for the data. The predictive models used are: multiple linear regression, regression tree, and random forest. For the predictive models, the dependent/response variable is `quality`, and other variables from the data set are used to predict the quality of the wine.

**Please read:** there are a couple things I wanted to mention about the wine data for my app. First, for some odd reason, the red wine data set has values for `alcohol` that were read in as extremely large values when they really just have a lot of decimal places. Every time I tried to fix it, my data set would basically break and I would have to delete it and download a new version. I ended up just modifying the data set and excluding observations with crazy large `alcohol` values. I also omitted any and all observations that had missing values for any variable because `total_sulfur_dioxide` had some missing observations and it was messing up my modeling. This is all done at the top of either file where the data is read in and cleaned.

## Required Packages

The required packages for this app are:

* `shiny`
* `shinydashboard`
* `tidyverse`
* `ggplot2`
* `readr`
* `caret`
* `mathjaxr`

The following code can be used to install and load the above packages:

`install.packages("shiny", "shinydashboard", "tidyverse", "ggplot2", "readr", "caret", "mathjaxr")`

`library(shiny, shinydashboard, tidyverse, ggplot2, readr, caret, mathjaxr)`

## Required Code

The following code is required to run the app:

`shiny::runGitHub("ST-558-Final-Project", "rlhardy2", subdir = "/FinalProject/")`
