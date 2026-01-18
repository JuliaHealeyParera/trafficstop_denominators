library(here)

#Run once, not reactive
source(here("code", "initialize_app.R"))

ui = fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal"),
  
  tags$head(
    tags$style(HTML("
    * {
      font-family: 'Times New Roman', Times, serif !important;
    }
    
    body, .container-fluid, .well, .nav, .navbar, .btn, input, select, textarea, 
    .form-control, .help-block, .control-label, h1, h2, h3, h4, h5, h6, p, div, span, 
    .shiny-input-container label, .selectize-input, .selectize-dropdown {
      font-family: 'Times New Roman', Times, serif !important;
    }
    
    .container-fluid {
      max-width: 1200px;
      margin-left: auto;
      margin-right: auto;
      background-color: white;
      padding-left: 30px; 
      padding-right: 30px;
    }

    table, th, td, .table {
      font-family: 'Times New Roman', Times, serif !important;
    }

    .nav-tabs > li > a {
      font-size: 24px !important;
    }
    
    h2 {
      padding: 10px 0 5px 0; 
      border-bottom: 1px solid #ddd;
      margin-top: 30px;
      margin-bottom: 15px;
    }
    
    .input-box {
      border: 1px solid #ccc;
      border-radius: 8px;
      padding: 25px;
      margin: 40px auto;      
      background-color: #f9f9f9;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      max-width: 500px;
      text-align: center;
    }

    .input-box .shiny-input-container {
      margin: 0 auto;       
      text-align: left; 
      padding: 10px;
      width: 80%; 
    }
    
    .input-box label {
      font-size: 18px;
      font-weight: 600;
      margin-bottom: 10px;
      display: block;
    }
    
    .custom-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 10px 20px;
      border-bottom: 1px solid #ddd;
      margin-bottom: 20px;
      background-color: #f8f9fa;
    }
    
    .logo-container {
      flex: 0 0 auto;
    }
    
    .title-container {
      flex: 1;
    }
    
    .plot-box {
      min-height: 0 !important;   
      max-width: 60% !important;
      height: auto !important;    
      margin: 10px auto;          
      display: block;
      text-align: center;         
    }
    
    .plot-box img, .plot-box .shiny-image-output {
      min-height: 0 !important;   
      max-width: 100% !important; 
      height: auto !important;    
      margin: 0 auto;             
      display: block;
      border: none;               
    }
        
    .loading-spinner {
      border: 4px solid #f3f3f3;
      border-top: 4px solid #3498db;
      border-radius: 50%;
      width: 40px;
      height: 40px;
      animation: spin 2s linear infinite;
      margin: 0 auto;
    }
    
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
  "))
  ),
  
  div(
    class = "custom-header",
    div(
      class = "title-container",
      h1("Police District Population Denominators", 
         style = "margin: 0; font-size: 34px; color: #333;")
    ),
    div(
      class = "logo-container",
      img(src = "ncemancipate_logo.png", 
          alt = "NC Emancipate Logo",
          style = "height: 60px; max-width: 200px; object-fit: contain;")
    )
  ),
  
  tabsetPanel(
    tabPanel("About this tool",
             uiOutput("introduction")
    ),
    
    tabPanel("Pre-generated district report",
             div(class = "input-box",
                 selectInput("report_city", "Select Pre-Loaded City", 
                             c("Raleigh", "Charlotte", "Durham")),
                 uiOutput("district_name"),
                 actionButton("create_pregenerated", "Create Report", class = "btn-primary")
             ),
             conditionalPanel(
               condition = "($('html').hasClass('shiny-busy'))",
               div(
                 style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; background: rgba(255,255,255,0.9); padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                 div(
                   style = "text-align: center;",
                   div(class = "loading-spinner"),
                   br(),
                   h4("Generating Report...", style = "margin: 10px 0; color: #333;")
                 )
               )
             ),
             br(),
             uiOutput("compositeOutput")
    ),
    
    tabPanel("Custom report",
             div(class = "input-box",
                 fileInput("spatial_file", "Upload Spatial Files (ZIP file with .shp or GeoJSON)", 
                           accept = c(".zip", ".geojson", ".json")),
                 uiOutput("report_city_custom"),
                 uiOutput("district_name_var"),
                 uiOutput("geometry_name_var"),
                 uiOutput("focus_district_custom"),
                 actionButton("create_custom", "Create Report", class = "btn-primary")
             ),
             conditionalPanel(
               condition = "($('html').hasClass('shiny-busy'))",
               div(
                 style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; background: rgba(255,255,255,0.9); padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                 div(
                   style = "text-align: center;",
                   div(class = "loading-spinner"),
                   br(),
                   h4("Generating Report...", style = "margin: 10px 0; color: #333;")
                 )
               )
             ),
             br(),
             uiOutput("compositeOutputCustom")
    )
  )
)
    

server = function(input, output, session) {
  
  
  
  ############################### INTRODUCTION #################################
  #----------------------------------------------------------------------------#
  
  output$introduction <- renderUI({
    tagList(
      tags$h2("Introduction"),
      introduction_general_1,
      
      tags$h2("Understanding Racial Disparities"),
      introduction_racialdisparities_2,
      introduction_racialdisparities_quote_3,
      introduction_racialdisparities_citation_4,
      
      tags$h2("What are Patrol Areas?"),
      introduction_patrol_areas_5
    )
  })
  
  
  
  ############################ PREGENERATED REPORTS ############################
  #----------------------------------------------------------------------------#
  
  city_calc <- reactive({
    obj <- readRDS(here("data","district_calculations.rds")) |>
      filter(city_name == str_to_lower(input$report_city), 
             district_num > 1)
    
    if (nrow(obj) == 0) { 
      add <- generate_analysis(input$report_city, "city") 
      append_analysis(add)
      obj <- as.tibble(add)
    }
    
    return(obj)
  }) |> bindEvent(input$report_city)
  
  output$district_name <- renderUI({
    # Hardcoded for speed, no need to dynamically read in input options for pregenerated reporst
    req(input$report_city)
    
    if (input$report_city == "Raleigh") {
      
      dist_list <- c(
        "Downtown District" = "DTD", 
        "Northeast District" = "NED", 
        "North District" = "NOD",
        "Northwest District" = "NWD", 
        "Southeast District" = "SED", 
        "Southwest District" = "SWD")
    } else if (input$report_city == "Charlotte") {
      dist_list <- c("Airport", "Eastway", "Central", "Freedom", "Hickory Grove", 
                     "Independence", "Metro", "North", "North Tryon", "Providence", 
                     "South", "Steele Creek", "University City", "Westover")
      dist_list <- map(dist_list, ~ paste(.x, "Division")) |> unlist()
    } else {
      dist_list <- c("District 0" = "0", "District 1" = "1", "District 2" = "2", 
                     "District 3" = "3", "District 4" = "4", "District 5" = "5")
    }
    
    #Use these lines for custom files:
    #req(city_calc())
    #dist_list <- city_calc() |> pull(district_names) |> unlist()
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
  
  # Citywide plot outputs
  output$calcdist_plot_1 <- renderImage({ 
    outfile <- city_calc()$police_dist_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_2 <- renderImage({
    outfile <- city_calc()$bg_population_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_3 <- renderImage({ 
    outfile <- city_calc()$dist_bg_areraintersection_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_4 <- renderImage({ 
    outfile <- city_calc()$dist_bg_numresident_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_5 <- renderImage({ 
    outfile <- city_calc()$dist_pop_map_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  
  # Focus district plot outputs
  output$focusdist_plot_1 <- renderImage({ 
    outfile <- focusdist_calc()$police_dist_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_2 <- renderImage({ 
    outfile <- focusdist_calc()$bg_population_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_3 <- renderImage({ 
    outfile <- focusdist_calc()$dist_bg_areraintersection_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_4 <- renderImage({ 
    outfile <- focusdist_calc()$dist_bg_numresident_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_5 <- renderImage({ 
    outfile <- focusdist_calc()$dist_pop_map_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  
  disttab_reactive <- reactive({
    req(input$create_pregenerated)
    
    citywide <- city_calc()
    disttab <- citywide$policedist_sf_df
    disttab <- disttab[[1]] |>
      st_drop_geometry()
    
    disttab <- disttab |>
      mutate(
        W_nH = pmap(list(W_nH, W_nH_perc), ~ reformat_num(..1, ..2)),
        B_nH = pmap(list(B_nH, B_nH_perc), ~ reformat_num(..1, ..2)),
        AmIn_nH = pmap(list(AmIn_nH, AmIn_nH_perc), ~ reformat_num(..1, ..2)),
        Asi_nH = pmap(list(Asi_nH, Asi_nH_perc), ~ reformat_num(..1, ..2)),
        HaPI_nH = pmap(list(HaPI_nH, HaPI_nH_perc), ~ reformat_num(..1, ..2)),
        Hispan = pmap(list(Hispan, Hispan_perc), ~ reformat_num(..1, ..2)), 
        Total = formatC(round(Total), format = 'd', big.mark = ',')) |> 
      select(Total, DISTRICT, W_nH, B_nH, AmIn_nH, Asi_nH, HaPI_nH, Hispan)
    
    names(disttab) <- map(names(disttab), ~ lab_ethnic_group(.x))
    disttab <- disttab |> 
      rename(
        'Total Population' = total,
        District = DISTRICT)
    
    return(disttab)
  })
  
  # Add download handler for CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("patrol_district_populations_", input$report_city, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      citywide <- city_calc()
      to_download <- citywide$policedist_sf_df
      csv_data <- to_download[[1]] |>
        st_drop_geometry() |>
        mutate(across(W_nH_perc:Hispan_perc, ~ .x * 100)) |>
        mutate(across(Total:Hispan, function(x) round(as.numeric(x))))
      
      write.csv(csv_data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$create_pregenerated, {
    citywide <- city_calc()
    focusdist <- focusdist_calc()

    ethnic_group <- "B_nH" ### THIS IS A DEFAULT and can be easily made into a selectInput and interactive
    ethnic_group_perc <- paste0(ethnic_group, "_perc")
    
    num_dist <- citywide |> pull(district_num)

    district_info <- focusdist$policedist_sf_df[[1]]
    dist_totalpop <- formatC(round(district_info$Total), format = "d", big.mark = ',')
    dist_ethnicpop <- formatC(round(district_info |> pull(!!sym(ethnic_group))), format = "d", big.mark = ',')
    dist_ethnicperc <- formatC(round(district_info |> pull(!!sym(ethnic_group_perc)) * 100), format = "d", big.mark = ',')
    ethnic_label <- lab_ethnic_group(ethnic_group)
    
    output$compositeOutput <- renderUI({
      tagList(
        tags$h1("Citywide Calculations", style = "font-size:32px; margin-top:20px;"),
        citycalc_intro_1(input$report_city, input$focus_district),
        tags$h2("Police Districts", style = "font-size:24px; margin-top:20px;"),
        citycalc_poldist_2(input$report_city, num_dist),
        div(class = "plot-box", imageOutput("calcdist_plot_1", width = "auto", height = "auto")),
        tags$h2("Census Neighborhood Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_bgpop_3,
        div(class = "plot-box", imageOutput("calcdist_plot_2", width = "auto", height = "auto")),
        tags$h2("District Area Overlap", style = "font-size:24px; margin-top:20px;"),
        citycalc_areaoverlap_4,
        div(class = "plot-box", imageOutput("calcdist_plot_3", width = "auto", height = "auto")),
        tags$h2("Neighborhood-District Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_bgdistpop_5,
        div(class = "plot-box", imageOutput("calcdist_plot_4", width = "auto", height = "auto")),
        tags$h2("Police Patrol District Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_poldistpop_6,
        div(class = "plot-box", imageOutput("calcdist_plot_5", width = "auto", height = "auto")),
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-top: 20px;",
          tags$h2("Patrol District Populations", style = "font-size:24px; margin: 0;"),
          downloadButton("downloadData", "Download CSV", 
                         class = "btn-primary")
        ),
        HTML(knitr::kable(disttab_reactive(), format = "html", digits = 0) |>
               kableExtra::kable_styling("striped", full_width = FALSE)),
        br(),
        tags$h1("District-Specific Calculations", style = "font-size:32px; margin-top:20px;"),
        tags$h2("Single Police District", style = "font-size:24px; margin-top:20px;"),
        focusdist_intro_1(input$report_city, input$focus_district),
        div(class = "plot-box",  imageOutput("focusdist_plot_1", width = "auto", height = "auto")),
        tags$h2("Census Neighborhood Populations", style = "font-size:24px; margin-top:20px;"),
        focusdist_bgpop_2,
        div(class = "plot-box",  imageOutput("focusdist_plot_2", width = "auto", height = "auto")),
        tags$h2("District Area Overlap", style = "font-size:24px; margin-top:20px;"),
        focusdist_areaoverlap_3,
        div(class = "plot-box", imageOutput("focusdist_plot_3", width = "auto", height = "auto")),
        tags$h2("Neighborhood-District Populations", style = "font-size:24px; margin-top:20px;"),
        focusdist_bgdistpop_4,
        div(class = "plot-box", imageOutput("focusdist_plot_4", width = "auto", height = "auto")),
        tags$h2("Police Patrol District Population", style = "font-size:24px; margin-top:20px;"),
        focusdist_poldist_5(input$focus_district, dist_totalpop, dist_ethnicpop, dist_ethnicperc, ethnic_label),
        div(class = "plot-box", imageOutput("focusdist_plot_5", width = "auto", height = "auto")),
      )
    })
  })
  
  
  
  ############################### CUSTOM GEOMETRY ##############################
  #----------------------------------------------------------------------------#
  
  police_dist <- reactive({
    req(input$spatial_file)
    
    ext <- tools::file_ext(input$spatial_file$datapath)
    
    if (ext == "zip") {
      # Handle Zipped Shapefile
      temp_dir <- here("temp_dir")
      if (!dir.exists(temp_dir)) dir.create(temp_dir)
      
      unzip(input$spatial_file$datapath, exdir = temp_dir)
      shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      
      if (length(shp_file) == 0) {
        showNotification("No .shp file found in ZIP", type = "error")
        return(NULL)
      }
      return(st_read(shp_file[1]))
      
    } else if (ext %in% c("geojson", "json")) {
      return(st_read(input$spatial_file$datapath))
      
    } else {
      showNotification("Please upload a .zip (Shapefile) or .geojson file", type = "error")
      return(NULL)
    }
  })
  
  output$report_city_custom <- renderUI({
    nc_city_names <- read_csv(here("data", "census_data", "nc_city_names.csv"))
    
    selectInput("report_city_custom", "Select City", nc_city_names$city)
  })

  output$district_name_var <- renderUI({
    req(police_dist())
    selectInput("district_name_var", "Select the District Variable", names(police_dist()))
  })
  
  output$geometry_name_var <- renderUI({
    req(police_dist())
    selectInput("geometry_name_var", "Select the Geometry Variable", names(police_dist()))
  })
  
  city_calc_custom <- reactive({
    req(police_dist(), input$district_name_var, input$geometry_name_var)
    obj <- as.tibble(
      generate_analysis(
        input$report_city_custom, 
        "city",
        file_obj = police_dist(), 
        dist_name_var = input$district_name_var, 
        geometry_var = input$geometry_name_var))
    return(obj)
  }) |> bindEvent(input$create_custom)
  
  output$focus_district_custom <- renderUI({
    req(input$district_name_var, police_dist())
  
    dist_list_custom <- police_dist() |> 
      pull(!!sym(input$district_name_var)) |> 
      unique()
    
    selectInput("focus_district_custom", "Select Focus Patrol District", dist_list_custom)
  })
  
  focusdist_calc_custom <- reactive({
    req(
      police_dist(), 
      input$district_name_var, 
      input$geometry_name_var, 
      input$focus_district_custom
      )
    
    obj <- as.tibble(
      generate_analysis(
        input$report_city_custom, 
        "district",
        dist_name = input$focus_district_custom,
        file_obj = police_dist(), 
        dist_name_var = input$district_name_var, 
        geometry_var = input$geometry_name_var))
    
    return(obj)
  })
  
  
  # Citywide plot outputs
  output$calcdist_plot_1_cust <- renderImage({ 
    outfile <- city_calc_custom()$police_dist_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_2_cust <- renderImage({
    outfile <- city_calc_custom()$bg_population_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_3_cust <- renderImage({ 
    outfile <- city_calc_custom()$dist_bg_areraintersection_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_4_cust <- renderImage({ 
    outfile <- city_calc_custom()$dist_bg_numresident_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$calcdist_plot_5_cust <- renderImage({ 
    outfile <- city_calc_custom()$dist_pop_map_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  
  # Custom focus district plot outputs
  output$focusdist_plot_1_cust <- renderImage({ 
    outfile <- focusdist_calc_custom()$police_dist_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_2_cust <- renderImage({ 
    outfile <- focusdist_calc_custom()$bg_population_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_3_cust <- renderImage({ 
    outfile <- focusdist_calc_custom()$dist_bg_areraintersection_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_4_cust <- renderImage({ 
    outfile <- focusdist_calc_custom()$dist_bg_numresident_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  output$focusdist_plot_5_cust <- renderImage({ 
    outfile <- focusdist_calc_custom()$dist_pop_map_ggplot 
    list(src = normalizePath(outfile),
         contentType = "image/png", 
         width = "auto", 
         height = "auto")
  }, deleteFile = FALSE)
  
  disttab_reactive_custom <- reactive({
    req(input$create_custom)
    
    citywide <- city_calc_custom()
    disttab <- citywide$policedist_sf_df
    disttab <- disttab[[1]] |>
      st_drop_geometry()
    
    disttab <- disttab |>
      mutate(
        W_nH = pmap(list(W_nH, W_nH_perc), ~ reformat_num(..1, ..2)),
        B_nH = pmap(list(B_nH, B_nH_perc), ~ reformat_num(..1, ..2)),
        AmIn_nH = pmap(list(AmIn_nH, AmIn_nH_perc), ~ reformat_num(..1, ..2)),
        Asi_nH = pmap(list(Asi_nH, Asi_nH_perc), ~ reformat_num(..1, ..2)),
        HaPI_nH = pmap(list(HaPI_nH, HaPI_nH_perc), ~ reformat_num(..1, ..2)),
        Hispan = pmap(list(Hispan, Hispan_perc), ~ reformat_num(..1, ..2)), 
        Total = formatC(round(Total), format = 'd', big.mark = ',')) |> 
      select(Total, DISTRICT, W_nH, B_nH, AmIn_nH, Asi_nH, HaPI_nH, Hispan)
    
    names(disttab) <- map(names(disttab), ~ lab_ethnic_group(.x))
    disttab <- disttab |> 
      rename(
        'Total Population' = total,
        District = DISTRICT)
    
    return(disttab)
  })
  
  # Add download handler for CSV
  output$downloadDataCustom <- downloadHandler(
    filename = function() {
      paste("patrol_district_populations_", input$report_city_custom, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      citywide <- city_calc_custom()
      to_download <- citywide$policedist_sf_df
      csv_data <- to_download[[1]] |>
        st_drop_geometry() |>
        mutate(across(W_nH_perc:Hispan_perc, ~ .x * 100)) |>
        mutate(across(Total:Hispan, function(x) round(as.numeric(x))))      
      
      write.csv(csv_data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$create_custom, {
    citywide <- city_calc_custom()
    focusdist <- focusdist_calc_custom()
    
    ethnic_group <- "B_nH" ### THIS IS A DEFAULT and can be easily made into a selectInput and interactive
    ethnic_group_perc <- paste0(ethnic_group, "_perc")
    
    num_dist <- citywide |> pull(district_num)
    
    district_info <- focusdist$policedist_sf_df[[1]]
    dist_totalpop <- formatC(round(district_info$Total), format = "d", big.mark = ',')
    dist_ethnicpop <- formatC(round(district_info |> pull(!!sym(ethnic_group))), format = "d", big.mark = ',')
    dist_ethnicperc <- formatC(round(district_info |> pull(!!sym(ethnic_group_perc)) * 100), format = "d", big.mark = ',')
    ethnic_label <- lab_ethnic_group(ethnic_group)
    
    output$compositeOutputCustom <- renderUI({
      tagList(
        tags$h1("Citywide Calculations", style = "font-size:32px; margin-top:20px;"),
        citycalc_intro_1(input$report_city_custom, input$focus_district_custom),
        tags$h2("Police Districts", style = "font-size:24px; margin-top:20px;"),
        citycalc_poldist_2(input$report_city_custom, num_dist),
        div(class = "plot-box", imageOutput("calcdist_plot_1_cust", width = "auto", height = "auto")),
        tags$h2("Census Neighborhood Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_bgpop_3,
        div(class = "plot-box", imageOutput("calcdist_plot_2_cust", width = "auto", height = "auto")),
        tags$h2("District Area Overlap", style = "font-size:24px; margin-top:20px;"),
        citycalc_areaoverlap_4,
        div(class = "plot-box", imageOutput("calcdist_plot_3_cust", width = "auto", height = "auto")),
        tags$h2("Neighborhood-District Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_bgdistpop_5,
        div(class = "plot-box", imageOutput("calcdist_plot_4_cust", width = "auto", height = "auto")),
        tags$h2("Police Patrol District Populations", style = "font-size:24px; margin-top:20px;"),
        citycalc_poldistpop_6,
        div(class = "plot-box", imageOutput("calcdist_plot_5_cust", width = "auto", height = "auto")),
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-top: 20px;",
          tags$h2("Patrol District Populations", style = "font-size:24px; margin: 0;"),
          downloadButton("downloadDataCustom", "Download CSV", 
                         class = "btn-primary")
        ),
        HTML(knitr::kable(disttab_reactive_custom(), format = "html", digits = 0) |>
               kableExtra::kable_styling("striped", full_width = FALSE)),
        br(),
        tags$h1("District-Specific Calculations", style = "font-size:32px; margin-top:20px;"),
        tags$h2("Single Police District", style = "font-size:24px; margin-top:20px;"),
        focusdist_intro_1(input$report_city_custom, input$focus_district_custom),
        div(class = "plot-box",  imageOutput("focusdist_plot_1_cust", width = "auto", height = "auto")),
        tags$h2("Census Neighborhood Populations", style = "font-size:24px; margin-top:20px;"),
        focusdist_bgpop_2,
        div(class = "plot-box",  imageOutput("focusdist_plot_2_cust", width = "auto", height = "auto")),
        tags$h2("District Area Overlap", style = "font-size:24px; margin-top:20px;"),
        focusdist_areaoverlap_3,
        div(class = "plot-box", imageOutput("focusdist_plot_3_cust", width = "auto", height = "auto")),
        tags$h2("Neighborhood-District Populations", style = "font-size:24px; margin-top:20px;"),
        focusdist_bgdistpop_4,
        div(class = "plot-box", imageOutput("focusdist_plot_4_cust", width = "auto", height = "auto")),
        tags$h2("Police Patrol District Population", style = "font-size:24px; margin-top:20px;"),
        focusdist_poldist_5(input$focus_district_custom, dist_totalpop, dist_ethnicpop, dist_ethnicperc, ethnic_label),
        div(class = "plot-box", imageOutput("focusdist_plot_5_cust", width = "auto", height = "auto")),
      )
    })
  })
  
}

shinyApp(ui = ui, server = server)
