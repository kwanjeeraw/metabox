library(shiny)
library(shinydashboard)
library(mETABOX)
library(doParallel)
library(igraph)
library(plotly)
library(DT)
doParallel::registerDoParallel(cores = 2)
#load("mynw.RData") #for testing
