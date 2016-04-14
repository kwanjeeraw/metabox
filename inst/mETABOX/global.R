#load("mynw.RData") #for testing
#########################################################################



# load library
source('R\\libs.R', local = TRUE)
options(repos = c(CRAN = "http://cran.rstudio.com"))
# load required functions.
source("R\\general function.R", local = T)
source("R\\format data.R", local = T)
check.get.packages(libs)

# devtools::install_github("rstudio/shiny",force=TRUE)
doParallel::registerDoParallel(cores = 2)
