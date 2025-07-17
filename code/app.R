library(shiny)
library(bslib)
library(tidyverse)
library(sf)

#Run once, not reactive
source("code/introduction_script.R")
source("code/census_data.R")
source("code/bg_to_policedist.R")

ui = fluidPage(
  titlePanel("Police District Population Denominators"),
  tabsetPanel(
    tabPanel("Introduction",
      uiOutput("introduction")
    ),
    tabPanel("Pre-generated district report",
      selectInput("report_city", "Select Pre-loaded City", 
                  c("Raleigh", "Charlotte", "Durham")),
      actionButton("create", "Create Report"),
      uiOutput("map")
    ),
    tabPanel("Custom report",
      fileInput("spatial_file", "Upload Spatial File", accept = c(".shp")), #add more later 
      actionButton("create", "Create Report"),
      uiOutput("map2")
    )
 )
)

server = function(input, output, session) {
  
  source("code/update_policedist_shpfiles.R")
  
  # #read_shp() <- reactive{(
  #   st_read(input$spatial_file)
  # )}
  # 
  
  output$introduction <- renderUI({
    text <- intro_txt # from introduction_script.R 
    print(text)
  })
  
  output$map <- renderUI({
    
  })
  
  output$map2 <- renderUI({
    
  })
}



# 
# ui = fluidPage(
#   titlePanel("Police District Population Denominators"),
#   mainPanel(
#     selectInput("report_type", "Select Report Type", 
#                 c("Pre-calculated district demographics", "Custom district demographics")),
#     uiOutput("report_specifics"),
#     actionButton("create", "Create Report")
#   )
# )
# 
# server = function(input, output, session) {
#   output$report_specifics <- renderUI({
#     req(input$report_type)
#     if (input$report_type == "Pre-calculated district demographics") {
#       precalc_counties <- c("Charlotte", "Durham", "Raleigh")
#       selectInput("precalc_county", "Choose County", choices = precalc_counties)
#     } else {
#       fileInput("spatial_file", "Upload Spatial File", accept = c(".csv")) #add more later 
#       county_list <- c("All", "Applicable", "NC", "Counties", "Alphabetically") #will change
#       selectInput("custom_county", "Select County", choices = county_list) #unique list pulled from Census API, for now set to NULL
#       column_selector()    
#       }
#   })
#   
#   output$column_selector <- renderUI({
#     req(input$spatial_file)
#     spatial_data()
#   })
#   
#   #read_file() <- reactive({
#   #  if (".csv" %in% tools::file_ext(input$spatial_file$datapath)) {
#   #    st_read(input$spatial_file$datapath)
#   #  }
#   #  else {
#       
#   #  }
#   ##make calls to data processing and denominator calculation functions
#   ##necessary to make everything reactive? should only re-render if user fully resets inputs
#   })
# 
shinyApp(ui = ui, server = server)
