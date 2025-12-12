library(here)

#Run once, not reactive
source(here("code", "initialize_app.R"))

ui = fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal"),
  
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2pdf.js/0.9.2/html2pdf.bundle.min.js"),
    
    ## ===== CSS only =====
    tags$style(HTML("
    * { font-family: 'Times New Roman', Times, serif !important; }

    body, .container-fluid, .well, .nav, .navbar, .btn, input, select, textarea, 
    .form-control, .help-block, .control-label, h1, h2, h3, h4, h5, h6, p, div, span, 
    .shiny-input-container label, .selectize-input, .selectize-dropdown {
      font-family: 'Times New Roman', Times, serif !important;
    }

    .container-fluid { max-width: 1200px; margin-left: auto; margin-right: auto; background-color: white; padding-left: 30px; padding-right: 30px; }
    table, th, td, .table { font-family: 'Times New Roman', Times, serif !important; }
    .nav-tabs > li > a { font-size: 24px !important; }

    h2 { padding: 10px 0 5px 0; border-bottom: 1px solid #ddd; margin-top: 30px; margin-bottom: 15px; }

    .input-box { border: 1px solid #ccc; border-radius: 8px; padding: 25px; margin: 40px auto; background-color: #f9f9f9; box-shadow: 0 2px 6px rgba(0,0,0,0.1); max-width: 500px; text-align: center; }
    .input-box .shiny-input-container { margin: 0 auto; text-align: left; padding: 10px; width: 80%; }
    .input-box label { font-size: 18px; font-weight: 600; margin-bottom: 10px; display: block; }

    .custom-header { display: flex; justify-content: space-between; align-items: center; padding: 10px 20px; border-bottom: 1px solid #ddd; margin-bottom: 20px; background-color: #f8f9fa; }
    .logo-container { flex: 0 0 auto; } .title-container { flex: 1; }

    .plot-box { min-height: 0 !important; max-width: 60% !important; height: auto !important; margin: 10px auto; display: block; text-align: center; }
    .plot-box img, .plot-box .shiny-image-output { min-height: 0 !important; max-width: 100% !important; height: auto !important; margin: 0 auto; display: block; border: none; }

    .loading-spinner { border: 4px solid #f3f3f3; border-top: 4px solid #3498db; border-radius: 50%; width: 40px; height: 40px; animation: spin 2s linear infinite; margin: 0 auto; }
    @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }

    /* file input alignment */
    .shiny-file-input { display: flex !important; align-items: center !important; gap: 12px; flex-wrap: nowrap; }
    .shiny-file-input .btn, .shiny-file-input .btn.btn-default, .shiny-file-input .btn.btn-primary { height: 44px !important; padding-top: 0 !important; padding-bottom: 0 !important; line-height: 44px !important; display: inline-flex !important; align-items: center !important; }
    .shiny-file-input .form-control, .shiny-file-input .file-input { height: 30px !important; line-height: 1.2 !important; padding: 6px 12px !important; display: block !important; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
    .shiny-file-input .progress, .shiny-file-input .upload-progress { width: 100%; margin-top: 8px; order: 2; }
    .shiny-file-input .form-control { flex: 1 1 auto; min-width: 120px; }
    @media (max-width: 520px) { .shiny-file-input { flex-direction: column; align-items: stretch; } .shiny-file-input .btn, .shiny-file-input .form-control { width: 100% !important; } }

    /* Printable report */
    #report-for-pdf {
      width: 720px;       /* target <= printable px for margin=8 (≈756) */
      max-width: 100%;
      padding: 12px;      /* smaller padding */
      box-sizing: border-box;
    }
    .report-section img, .report-section table {
      max-width: 100%;
      height: auto;
    }
    .hidden-report { display: none; }
    @media print { .hidden-report { display: block !important; } }
    .page-break { page-break-after: always; }
    .report-title { text-align: center; margin-bottom: 12px; }
    .report-section { margin-top: 18px; margin-bottom: 18px; }

    /* Style to make secondary buttons match primary look (optional) */
    .btn-secondary { background-color: #0d6efd; color: #fff; border-color: #0d6efd; }
  ")),
    
    ## ===== JS only =====
    tags$script(HTML("
    function downloadReport(elementId, filename, options) {
      var el = document.getElementById(elementId);
      if (!el) {
        alert('Report not ready yet. Please click Create Report first.');
        return;
      }
    
      // default options for html2pdf
      var opt = {
        margin:       12,                // mm
        filename:     filename || 'report.pdf',
        image:        { type: 'jpeg', quality: 0.95 },
        html2canvas:  { scale: 2, useCORS: true, logging: false },
        jsPDF:        { unit: 'mm', format: 'a4', orientation: 'portrait' }
      };
      if (options) Object.assign(opt, options);
    
      // Remember previous inline display value so we can restore it
      var prevDisplay = el.style.display || '';
      var hiddenCls = 'hidden-report';
      var hadHidden = el.classList.contains(hiddenCls);
    
      // Force it visible for accurate rendering
      if (hadHidden) el.classList.remove(hiddenCls);
      el.style.display = 'block';
      el.style.visibility = 'visible';
    
      // Wait for all images inside the element to load (or error)
      var imgs = Array.prototype.slice.call(el.querySelectorAll('img'));
      var loadPromises = imgs.map(function(img) {
        return new Promise(function(resolve) {
          // If already loaded and has natural size, resolve immediately
          if (img.complete && img.naturalWidth !== 0) {
            return resolve();
          }
          // otherwise resolve on load or error (we still proceed even on error)
          var onDone = function() {
            img.removeEventListener('load', onDone);
            img.removeEventListener('error', onDone);
            resolve();
          };
          img.addEventListener('load', onDone);
          img.addEventListener('error', onDone);
          // Safety: if image never fires, set a soft timeout (10s) to avoid hanging forever
          setTimeout(resolve, 10000);
        });
      });
    
      Promise.all(loadPromises).then(function() {
        // Give the browser a moment to layout fonts/images fully
        setTimeout(function() {
          html2pdf().set(opt).from(el).save().then(function() {
            // restore previous hidden/display state
            if (hadHidden) el.classList.add(hiddenCls);
            el.style.display = prevDisplay;
          }).catch(function(err) {
            // restore state and surface error
            if (hadHidden) el.classList.add(hiddenCls);
            el.style.display = prevDisplay;
            console.error('html2pdf error', err);
            alert('PDF generation failed in your browser. Try a different browser or reduce report size.');
          });
        }, 300); // layout settle delay
      }).catch(function(err){
        // fallback restore & message
        if (hadHidden) el.classList.add(hiddenCls);
        el.style.display = prevDisplay;
        console.error('Image loading wait failed', err);
        alert('Failed waiting for images to load; PDF generation aborted.');
      });
    }

    document.addEventListener('DOMContentLoaded', function(){
      var btnPreg = document.getElementById('print_pregenerated');
      if (btnPreg) {
        btnPreg.addEventListener('click', function(){
          var el = document.getElementById('report-for-pdf');
          if (!el || el.innerHTML.trim().length === 0) {
            alert('Report not ready. Click \"Create Report\" first.');
            return;
          }
          var city = document.querySelector('[name=\"report_city\"]') ? document.querySelector('[name=\"report_city\"]').value : 'city';
          downloadReport('report-for-pdf', 'patrol_report_' + city + '_' + new Date().toISOString().slice(0,10) + '.pdf');
        });
      }
      var btnCust = document.getElementById('print_custom');
      if (btnCust) {
        btnCust.addEventListener('click', function(){
          var el = document.getElementById('report-for-pdf');
          if (!el || el.innerHTML.trim().length === 0) {
            alert('Report not ready. Click \"Create Report\" first.');
            return;
          }
          var city = document.querySelector('[name=\"report_city_custom\"]') ? document.querySelector('[name=\"report_city_custom\"]').value : 'custom';
          downloadReport('report-for-pdf', 'patrol_report_custom_' + city + '_' + new Date().toISOString().slice(0,10) + '.pdf');
        });
      }
    });
  "))
  ),
  
  div(
    class = "custom-header",
    # Title on the left
    div(
      class = "title-container",
      h1("Police District Population Denominators", 
         style = "margin: 0; font-size: 34px; color: #333;")
    ),
    # Logo on the right
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
             # Add loading indicator
             conditionalPanel(
               condition = "($('html').hasClass('shiny-busy'))",
               div(
                 style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; background: rgba(255,255,255,0.9); padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                 div(
                   style = "text-align: center;",
                   div(
                     style = "border: 4px solid #f3f3f3; border-top: 4px solid #3498db; border-radius: 50%; width: 40px; height: 40px; animation: spin 2s linear infinite; margin: 0 auto;",
                     tags$style(HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"))
                   ),
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
          fileInput("spatial_file", "Upload Spatial Files (ZIP folder)", accept = c(".zip")),
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
             div(
               style = "border: 4px solid #f3f3f3; border-top: 4px solid #3498db; border-radius: 50%; width: 40px; height: 40px; animation: spin 2s linear infinite; margin: 0 auto;",
               tags$style(HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"))
             ),
             br(),
             h4("Generating Report...", style = "margin: 10px 0; color: #333;")
           )
         )
       ),
       br(),
       uiOutput("compositeOutputCustom")
    )
  ),
  div(
    id = "report-wrapper",
    div(
      id = "report-for-pdf",
      class = "hidden-report",
      uiOutput("report_for_pdf")
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
        mutate(across(Total:Hispan_perc, function(x) round(as.numeric(x))))
      
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
        div(
          style = "text-align: center; margin-top: 30px; margin-bottom: 30px;",
          actionButton(
            inputId = "print_pregenerated",
            label = "Print / Download PDF",
            class = "btn-primary",
            style = "padding:10px 24px; font-size:16px;",
            onclick = "(function(){
              var city = document.querySelector('[name=\"report_city\"]') ? document.querySelector('[name=\"report_city\"]').value : 'city';
              var filename = 'patrol_report_' + city + '_' + new Date().toISOString().slice(0,10) + '.pdf';
              downloadReport('report-for-pdf', filename);
            })();"
          )
        )
        
        )
    })
  })
  
  
  
  ############################### CUSTOM GEOMETRY ##############################
  #----------------------------------------------------------------------------#
  
  police_dist <- reactive({
    req(input$spatial_file)
    unzip(input$spatial_file$datapath, exdir = "../data/temp_dir")
    shp_file <- list.files("../data/temp_dir", pattern = "\\.shp$", full.names = TRUE)
    police_dist_obj <- st_read(shp_file)
    return(police_dist_obj)
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
        mutate(across(Total:Hispan_perc, function(x) round(as.numeric(x))))
      
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
        div(
          style = "text-align: center; margin-top: 30px; margin-bottom: 30px;",
          actionButton(
            inputId = "print_custom",
            label = "Print / Download PDF",
            class = "btn-primary",
            style = "padding:10px 24px; font-size:16px;",
            onclick = "(function(){
      var city = document.querySelector('[name=\"report_city_custom\"]') ? document.querySelector('[name=\"report_city_custom\"]').value : 'custom';
      var filename = 'patrol_report_custom_' + city + '_' + new Date().toISOString().slice(0,10) + '.pdf';
      downloadReport('report-for-pdf', filename);
    })();"
          )
        )
        )
    })
  })
  
  # ---------------- Helper: convert image file to data URI (base64) ----------------
  img_to_datauri <- function(img_path) {
    # Return a data URI string or NULL if not available
    if (is.null(img_path)){
      print('null image path')
      return(NULL)
    }
    # If it's already a URL (http/https), just return it (browser can fetch)
    if (grepl("^https?://", img_path)){ 
      print(img_path)
      return(img_path)
      }
    if (!file.exists(img_path)){
      print('image path does not exist')
      return(NULL)
    }
    mime <- ifelse(grepl("\\.png$", img_path, ignore.case = TRUE), "image/png",
                   ifelse(grepl("\\.(jpg|jpeg)$", img_path, ignore.case = TRUE), "image/jpeg",
                          "application/octet-stream"))
    bin <- readBin(img_path, "raw", n = file.info(img_path)$size)
    b64 <- base64enc::base64encode(bin)
    returnobj <- paste0("data:", mime, ";base64,", b64)
    return(returnobj)
  }
  
  # ---------------- Build the printable report HTML ----------------
  render_report_html <- function(which = c("pregenerated", "custom")) {
    which <- match.arg(which)
    # choose reactives and input names according to which report
    if (which == "pregenerated") {
      req(input$report_city, input$focus_district) # ensure user selected/generate
      city_obj  <- city_calc()
      focus_obj <- focusdist_calc()
      city_name <- input$report_city
      focus_name <- input$focus_district
    } else {
      req(input$report_city_custom, input$focus_district_custom)
      city_obj  <- city_calc_custom()
      focus_obj <- focusdist_calc_custom()
      city_name <- input$report_city_custom
      focus_name <- input$focus_district_custom
    }
    
    # Intro text: use your existing intro objects (assumed to be HTML/tagList objects)
    intro_html <- tagList(
      introduction_general_1,
      
      tags$h2("Understanding Racial Disparities"),
      introduction_racialdisparities_2,
      introduction_racialdisparities_quote_3,
      introduction_racialdisparities_citation_4,
      
      tags$h2("What are Patrol Areas?"),
      introduction_patrol_areas_5
    )
    city_intro_html <- citycalc_intro_1(city_name, focus_name)
    focus_intro_html <- focusdist_intro_1(city_name, focus_name)
    
    # Get plots: these objects in your reactives appear to be file paths in list columns
    # guard against NULLs
    safe_get <- function(obj, nm) {
      if (is.null(obj)) return(NULL)
      val <- obj[[nm]]
      # When obj$police_dist_ggplot is itself a list-column, it may be length-1 vector
      if (is.list(val) && length(val) == 1) val <- val[[1]]
      val
    }
    city_plots <- list(
      police_dist = safe_get(city_obj, "police_dist_ggplot"),
      bg_population = safe_get(city_obj, "bg_population_ggplot"),
      area_overlap = safe_get(city_obj, "dist_bg_areraintersection_ggplot"),
      bgdist_pop = safe_get(city_obj, "dist_bg_numresident_ggplot"),
      dist_map = safe_get(city_obj, "dist_pop_map_ggplot")
    )
    focus_plots <- list(
      police_dist = safe_get(focus_obj, "police_dist_ggplot"),
      bg_population = safe_get(focus_obj, "bg_population_ggplot"),
      area_overlap = safe_get(focus_obj, "dist_bg_areraintersection_ggplot"),
      bgdist_pop = safe_get(focus_obj, "dist_bg_numresident_ggplot"),
      dist_map = safe_get(focus_obj, "dist_pop_map_ggplot")
    )
    # Convert plots to data URIs (so browser can render them even though they are server files)
    city_plot_datauris <- lapply(city_plots, function(p) {
      if (is.null(p)) return(NULL)
      # if p is a vector of filenames, take first
      if (is.vector(p) && length(p) > 1) p <- p[1]
      img_to_datauri(p)
    })
    focus_plot_datauris <- lapply(focus_plots, function(p) {
      if (is.null(p)) return(NULL)
      if (is.vector(p) && length(p) > 1) p <- p[1]
      img_to_datauri(p)
    })
    # Tables: drop geometry and convert to HTML
    city_table <- tryCatch({
      tbl <- city_obj$policedist_sf_df[[1]] |> sf::st_drop_geometry()
      # optional formatting: keep the same formatting you use for displayed table
      tbl
    }, error = function(e) NULL)
    focus_table <- tryCatch({
      tbl <- focus_obj$policedist_sf_df[[1]] |> sf::st_drop_geometry()
      tbl
    }, error = function(e) NULL)
    # Build tagList for report
    # Use inline CSS styles for images to control size; images are embedded as data URIs
    report <- tagList(
      div(class = "report-title",
          tags$h1(sprintf("Police District Report — %s", city_name)),
          tags$p(sprintf("Focus district: %s", focus_name)),
          tags$p(sprintf("Generated: %s", format(Sys.time())))),
      div(class = "report-section",
          tags$h2("Introduction"),
          HTML(as.character(intro_html))
      ),
      div(class = "page-break"), # start new page
      div(class = "report-section",
          tags$h2("Citywide Calculations"),
          HTML(as.character(city_intro_html)),
          # include city plots
          lapply(city_plot_datauris, function(uri) {
            if (is.null(uri)) return(NULL)
            tags$div(style = "margin-top:10px; margin-bottom:10px;",
                     tags$img(src = uri, style = "width:100%; max-width:760px; display:block; margin:auto;"))
          }),
          # city table
          if (!is.null(city_table)) {
            tags$div(style = "margin-top:12px;",
                     HTML(as.character(
                       knitr::kable(city_table, format = "html", digits = 0) |>
                         kableExtra::kable_styling("striped", full_width = FALSE)
                     )))
          } else NULL
      ),
      div(class = "page-break"),
      div(class = "report-section",
          tags$h2(sprintf("%s — District-specific calculations", focus_name)),
          HTML(as.character(focus_intro_html)),
          lapply(focus_plot_datauris, function(uri) {
            if (is.null(uri)) return(NULL)
            tags$div(style = "margin-top:10px; margin-bottom:10px;",
                     tags$img(src = uri, style = "width:100%; max-width:760px; display:block; margin:auto;"))
          }),
          if (!is.null(focus_table)) {
            tags$div(style = "margin-top:12px;",
                     HTML(as.character(
                       knitr::kable(focus_table, format = "html", digits = 0) |>
                         kableExtra::kable_styling("striped", full_width = FALSE)
                     )))
          } else NULL
      ),
      # final notes / footer
      div(class = "page-break"),
      div(style = "font-size:12px; color:#666; margin-top:12px;",
          tags$p("Note: Report generated by Police District Population Denominators tool."),
          tags$p("Data sources: Census / internal datasets."))
    )
    return(report)
  }
  
  # ---------------- Hook up: when user creates a pregenerated report, build report_for_pdf ----------------
  observeEvent(input$print_pregenerated, {
    try({
      output$report_for_pdf <- renderUI({
        # build report HTML
        rpt <- render_report_html("pregenerated")
        
        # create a filename
        city <- input$report_city %||% "city"
        filename <- sprintf("patrol_report_%s_%s.pdf", city, format(Sys.Date(), "%Y-%m-%d"))
        
        # script will run AFTER the DOM insertion; delay gives time for layout & images
        download_script <- tags$script(HTML(sprintf("
          (function(){
            var filename = '%s';
            function waitForContent(el, timeoutMs){
              timeoutMs = timeoutMs || 20000;
              return new Promise(function(resolve, reject){
                if(!el){ return reject('no-element'); }
                if(el.innerHTML && el.innerHTML.trim().length>0){ return resolve(); }
                var obs = new MutationObserver(function(){
                  if(el.innerHTML && el.innerHTML.trim().length>0){
                    obs.disconnect();
                    resolve();
                  }
                });
                obs.observe(el, { childList: true, subtree: true, characterData: true });
                // safety timeout
                setTimeout(function(){
                  try{ obs.disconnect(); }catch(e){}
                  reject('waitForContent timeout');
                }, timeoutMs);
              });
            }
          
            function waitForImages(el, timeoutMs){
              timeoutMs = timeoutMs || 20000;
              var imgs = Array.prototype.slice.call(el.querySelectorAll('img'));
              var promises = imgs.map(function(img){
                return new Promise(function(resolve){
                  if(img.complete && img.naturalWidth !== 0){ return resolve(); }
                  function done(){ img.removeEventListener('load', done); img.removeEventListener('error', done); resolve(); }
                  img.addEventListener('load', done); img.addEventListener('error', done);
                  // fallback timeout per image
                  setTimeout(resolve, timeoutMs);
                });
              });
              return Promise.all(promises);
            }
          
            function tryDownload(){
              var el = document.getElementById('report-for-pdf');
              if(!el){ console.warn('report-for-pdf not found'); return; }
              // If hidden-report class present, temporarily show it for rendering
              var hiddenCls = 'hidden-report';
              var hadHidden = el.classList.contains(hiddenCls);
              if(hadHidden) el.classList.remove(hiddenCls);
              el.style.display = 'block';
              el.style.visibility = 'visible';
          
              waitForContent(el, 20000).then(function(){
                return waitForImages(el, 20000);
              }).then(function(){
                // tiny delay to let layout settle
                setTimeout(function(){
                  try {
                    downloadReport('report-for-pdf', filename);
                    // restore hidden class after some time (optional)
                    setTimeout(function(){
                      if(hadHidden) el.classList.add(hiddenCls);
                    }, 500);
                  } catch(e){
                    console.error('downloadReport failed', e);
                    if(hadHidden) el.classList.add(hiddenCls);
                  }
                }, 300);
              }).catch(function(err){
                console.warn('Auto-download aborted or timed out:', err);
                // still try a last-ditch attempt
                try {
                  downloadReport('report-for-pdf', filename);
                } catch(e){ console.error('final download attempt failed', e); }
                if(hadHidden){
                  el.classList.add(hiddenCls);
                }
              });
          })();
        ", filename)))
        # return the report plus the auto-download script
        print('returning')
        tagList(rpt, download_script)
      })
    }, silent = FALSE)
  })
  
  
  # ---------------- Hook up: when user creates a custom report, build report_for_pdf ----------------
  observeEvent(input$print_custom, {
    try({
      output$report_for_pdf <- renderUI({
        # build report HTML
        rpt <- render_report_html("custom")
        
        # create a filename
        city <- input$report_city %||% "city"
        filename <- sprintf("patrol_report_%s_%s.pdf", city, format(Sys.Date(), "%Y-%m-%d"))
        
        # script will run AFTER the DOM insertion; delay gives time for layout & images
        download_script <- tags$script(HTML(
          sprintf("
          (function(){
            var filename = '%s';
            function waitForContent(el, timeoutMs){
              timeoutMs = timeoutMs || 20000;
              return new Promise(function(resolve, reject){
                if(!el){ return reject('no-element'); }
                if(el.innerHTML && el.innerHTML.trim().length>0){ return resolve(); }
                var obs = new MutationObserver(function(){
                  if(el.innerHTML && el.innerHTML.trim().length>0){
                    obs.disconnect();
                    resolve();
                  }
                });
                obs.observe(el, { childList: true, subtree: true, characterData: true });
                // safety timeout
                setTimeout(function(){
                  try{ obs.disconnect(); }catch(e){}
                  reject('waitForContent timeout');
                }, timeoutMs);
              });
            }
          
            function waitForImages(el, timeoutMs){
              timeoutMs = timeoutMs || 20000;
              var imgs = Array.prototype.slice.call(el.querySelectorAll('img'));
              var promises = imgs.map(function(img){
                return new Promise(function(resolve){
                  if(img.complete && img.naturalWidth !== 0){ return resolve(); }
                  function done(){ img.removeEventListener('load', done); img.removeEventListener('error', done); resolve(); }
                  img.addEventListener('load', done); img.addEventListener('error', done);
                  // fallback timeout per image
                  setTimeout(resolve, timeoutMs);
                });
              });
              return Promise.all(promises);
            }
          
            function tryDownload(){
              var el = document.getElementById('report-for-pdf');
              if(!el){ console.warn('report-for-pdf not found'); return; }
              // If hidden-report class present, temporarily show it for rendering
              var hiddenCls = 'hidden-report';
              var hadHidden = el.classList.contains(hiddenCls);
              if(hadHidden) el.classList.remove(hiddenCls);
              el.style.display = 'block';
              el.style.visibility = 'visible';
          
              waitForContent(el, 20000).then(function(){
                return waitForImages(el, 20000);
              }).then(function(){
                // tiny delay to let layout settle
                setTimeout(function(){
                  try {
                    downloadReport('report-for-pdf', filename);
                    // restore hidden class after some time (optional)
                    setTimeout(function(){
                      if(hadHidden) el.classList.add(hiddenCls);
                    }, 500);
                  } catch(e){
                    console.error('downloadReport failed', e);
                    if(hadHidden) el.classList.add(hiddenCls);
                  }
                }, 300);
              }).catch(function(err){
                console.warn('Auto-download aborted or timed out:', err);
                // still try a last-ditch attempt
                try {
                  downloadReport('report-for-pdf', filename);
                } catch(e){ console.error('final download attempt failed', e); }
                if(hadHidden){
                  el.classList.add(hiddenCls);
                }
              });
          })();
        ", filename)
        ))
        
        # return the report plus the auto-download script
        tagList(rpt, download_script)
      })
    }, silent = FALSE)
  })
  
  
}

shinyApp(ui = ui, server = server)
