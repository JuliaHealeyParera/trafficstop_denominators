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
      uiOutput("district_name"),
      actionButton("create_pregenerated", "Create Report"),
      uiOutput("compositeOutput")
    )
    # tabPanel("Custom report",
    #   fileInput("spatial_file", "Upload Spatial Files (ZIP folder)", accept = c(".zip")), #add more later 
    #   selectInput("custom_city", "Select City", c("Burner", "Cities", "here")),
    #   uiOutput("district_name_var"),
    #   uiOutput("geometry_name_var"),
    #   actionButton("create_custom", "Create Report"),
    #   uiOutput("map2")
    # )
 )
)

server = function(input, output, session) {
  
  #### Introduction outputs ####
  output$introduction <- renderUI({
    HTML(intro_txt) # from introduction_script.R 
  })
  
  #### Pregenerated report ####
  city_calc <- reactive({
    obj <- readRDS(here("data", "district_calculations.rds")) |>
      filter(city_name == str_to_lower(input$report_city), 
             district_num > 1)
  }) |> bindEvent(input$report_city)
  
  output$district_name <- renderUI({
    req(city_calc())
    dist_list <- city_calc() |> pull(district_names) |> unlist()
    selectInput("focus_district", "Select Focus Patrol District", dist_list)
  })
  
  focusdist_calc <- reactive({
    req(input$focus_district, input$report_city)
    
    focusdist_calc <- readRDS(here("data", "district_calculations.rds")) |> 
      filter(city_name == str_to_lower(input$report_city), 
             district_num == 1,
             district_names == input$focus_district)
    if (nrow(focusdist_calc) == 0) {
      focusdist_calc <- generate_analysis(input$report_city, "district", dist_name = input$focus_district)
      append_analysis(focusdist_calc)
    }

    return(focusdist_calc)
  })
  
  output$calcdist_plot_1 <- renderPlot({ city_calc()$police_dist_ggplot })
  output$calcdist_plot_2 <- renderPlot({ city_calc()$bg_population_ggplot })
  output$calcdist_plot_3 <- renderPlot({ city_calc()$dist_bg_areraintersection_ggplot })
  output$calcdist_plot_4 <- renderPlot({ city_calc()$dist_bg_numresident_ggplot })
  output$calcdist_plot_5 <- renderPlot({ city_calc()$dist_pop_map_ggplot })
  
  output$focusdist_plot_1 <- renderPlot({ focusdist_calc()$police_dist_ggplot })
  output$focusdist_plot_2 <- renderPlot({ focusdist_calc()$bg_population_ggplot })
  output$focusdist_plot_3 <- renderPlot({ focusdist_calc()$dist_bg_areraintersection_ggplot })
  output$focusdist_plot_4 <- renderPlot({ focusdist_calc()$dist_bg_numresident_ggplot })
  output$focusdist_plot_5 <- renderPlot({ focusdist_calc()$dist_pop_map_ggplot })
  
  observeEvent(input$create_pregenerated, {
    citywide <- city_calc()
    focusdist <- focusdist_calc()
    
    ethnic_group <- "B_nH" ### THIS IS A DEFAULT and can be easily made into a selectInput and interactive
    ethnic_group_perc <- paste0(ethnic_group, "_perc")
    
    num_dist <- citywide |> pull(district_num)
    district_info <- focusdist$policedist_sf_df[[1]]
    
    dist_totalpop <- round(district_info$Total)
    dist_ethnicpop <- round(district_info |> pull(!!sym(ethnic_group)))
    dist_ethnicperc <- round(district_info |> pull(!!sym(ethnic_group_perc)) * 100)
    ethnic_label <- lab_ethnic_group(ethnic_group)
    
    output$compositeOutput <- renderUI({
      tagList(
        tags$h1("Introduction", style = "font-size:32px; margin-top:20px;"),
        tags$h2("What are Patrol Areas?", style = "font-size:24px; margin-top:20px;"),
        introduction_patrolareas_2,
        #map here? 
        tags$h2("Understanding Racial Disparities", style = "font-size:24px; margin-top:20px;"),
        introduction_racialdisparities_4,
        tags$h2("What is the U.S. Census?", style = "font-size:24px; margin-top:20px;"),
        introduction_census_5,
        br(),
        tags$h1("Citywide Calculations", style = "font-size:32px; margin-top:20px;"),
        citycalc_intro_1(input$report_city, input$focus_district),
        tags$h2("Police Districts", style = "font-size:24px; margin-top:20px;"),
        citycalc_poldist_2(input$report_city, num_dist),
        plotOutput("calcdist_plot_1"),
        tags$h2("Census Neighborhood Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_bgpop_3,
        plotOutput("calcdist_plot_2"),
        tags$h2("District Area Overlap", style = "font-size:24px; margin-top:20px;"),
        citycalc_areaoverlap_4,
        plotOutput("calcdist_plot_3"),
        tags$h2("Neighborhood-District Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_bgdistpop_5,
        plotOutput("calcdist_plot_4"),
        tags$h2("Police Patrol District Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_poldistpop_6,
        plotOutput("calcdist_plot_5"),
        br(),
        tags$h1("District-Specific Calculations", style = "font-size:32px; margin-top:20px;"),
        tags$h2("Single Police District", style = "font-size:24px; margin-top:20px;"),
        focusdist_intro_1(input$report_city, input$focus_district),
        plotOutput("focusdist_plot_1"),
        tags$h2("Census Neighborhood Populations", style = "font-size:24px; margin-top:20px;"),
        focusdist_bgpop_2,
        plotOutput("focusdist_plot_2"),
        tags$h2("District Area Overlap", style = "font-size:24px; margin-top:20px;"),
        focusdist_areaoverlap_3,
        plotOutput("focusdist_plot_3"),
        tags$h2("Neighborhood-District Populations", style = "font-size:24px; margin-top:20px;"),
        focusdist_bgdistpop_4,
        plotOutput("focusdist_plot_4"),
        tags$h2("Police Patrol District Population", style = "font-size:24px; margin-top:20px;"),
        focusdist_poldist_5(input$focus_district, dist_totalpop, dist_ethnicpop, dist_ethnicperc, ethnic_label),
        plotOutput("focusdist_plot_5"),
      )
    })
  })
  
  #### Custom geometry ####
  police_dist <- reactiveVal()  
  
  observeEvent(input$spatial_file, {
    req(input$spatial_file)
    unzip(input$spatial_file$datapath, exdir = "../data/temp_dir")
    shp_file <- list.files("../data/temp_dir", pattern = "\\.shp$", full.names = TRUE)
    police_dist(st_read(shp_file))
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
