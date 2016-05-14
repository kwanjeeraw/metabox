#'Start GUI
#'@description start GUI as shiny app.
#'@usage runGUI()
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@export
runGUI<-function() UseMethod("runGUI")
#'@export
runGUI<-function(){
  shiny::runApp(system.file("mETABOX", package = "mETABOX"),
                display.mode = "normal",
                launch.browser = TRUE)
}
