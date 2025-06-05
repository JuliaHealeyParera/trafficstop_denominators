library(shiny)
library(bslib)
library(tidyverse)
library(sf)

ui = bslib::page_sidebar(
  ##some data input here 
  ##some column identifier dropdowns/tags 
  ##some county dropdown menu 
)

server = function(input, output, session) {
  ##make calls to data processing and denominator calculation functions
  ##necessary to make everything reactive? should only re-render if user fully resets inputs
}

shinyApp(ui = ui, server = server)
