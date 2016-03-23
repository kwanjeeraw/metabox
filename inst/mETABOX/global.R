library(shiny)
library(shinydashboard)
library(mETABOX)
library(doParallel)
library(igraph)
library(plotly)
library(DT)
doParallel::registerDoParallel(cores = 2)
#load("mynw.RData") #for testing
#########################################################################
#########################################################################
#########################################################################

# load library
source('R\\libs.R', local = TRUE)
options(repos = c(CRAN = "http://cran.rstudio.com"))
# load required functions.
source("R\\general function.R", local = T)
source("R\\format data.R", local = T)
check.get.packages(libs)
