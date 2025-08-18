library(here)

#Run once, not reactive
source(here("code", "initialize_app.R"))

ui = fluidPage(
  titlePanel("Police District Population Denominators"),
  tabsetPanel(
    tabPanel("Introduction",
      uiOutput("introduction")
    ),
    tabPanel("Pre-generated district report",
      selectInput("report_city", "Select Pre-loaded City", 
                  c("Raleigh", "Charlotte", "Durham")),
      actionButton("create_pregenerated", "Create Report"),
      uiOutput("map")
    ),
    tabPanel("Custom report",
      fileInput("spatial_file", "Upload Spatial Files (ZIP folder)", accept = c(".zip")), #add more later 
      selectInput("custom_city", "Select City", nc_city_names),
      uiOutput("district_name_var"),
      uiOutput("geometry_name_var"),
      actionButton("create_custom", "Create Report"),
      uiOutput("map2")
    )
 )
)

server = function(input, output, session) {
  police_dist <- reactiveVal()  
  
  observeEvent(input$spatial_file, {
    req(input$spatial_file)
    unzip(input$spatial_file$datapath, exdir = "../data/temp_dir")
    shp_file <- list.files("../data/temp_dir", pattern = "\\.shp$", full.names = TRUE)
    police_dist(st_read(shp_file))
  })
  
  
  output$introduction <- renderUI({
    HTML(intro_txt) # from introduction_script.R 
  })
  
  output$district_name_var <- renderUI({
    req(police_dist())
    selectInput("district_name_var", "Select the District Variable", names(police_dist()))
  })
  
  output$geometry_name_var <- renderUI({
    req(police_dist())
    selectInput("geometry_name_var", "Select the Geometry Variable", names(police_dist()))
  })
}

shinyApp(ui = ui, server = server)
