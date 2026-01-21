reformat_num <- function(total, perc) {
  temp <- paste0(formatC(round(total), format = "d", big.mark = ','), " (", perc * 100, "%)")
  return(temp)
}

#Convert ethnic group variable to text for labeling 
lab_ethnic_group <- function(ethnic_var) {
  str_replace_all(
    ethnic_var, 
  c("Hispan" = "Hispanic", 
    "Total" = "total",
    "W_nH" = "White Non-Hispanic", 
    "B_nH" = "Black Non-Hispanic", 
    "Asi_nH" = "Asian Non-Hispanic", 
    "AmIn_nH" = "American Indian",
    "HaPI_nH" = "Hawaiian or Pacific Islander"))
}

font_add_google("Tinos", "TimesNewRoman") 
showtext_auto()

theme_times <- function() {
  theme_void(base_family = "TimesNewRoman") %+replace%
    theme(
      text = element_text(family = "TimesNewRoman", size = 15),
      plot.title = ggtext::element_markdown(size = 15, hjust = 0.5,
                                            margin = margin(b = 5)),
      plot.subtitle = ggtext::element_markdown(size = 10, hjust = 0.5,
                                               margin = margin(t = 2, b = 7)),
      plot.margin = margin(t = 5, r = 20, b = 5, l = 20),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"),
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      axis.title   = element_blank(),
      axis.line    = element_blank(),
      panel.grid   = element_blank()
    )
}

#Map 1 (City): Police Districts Map 
police_district_map <- function(police_dist_sf, city, map_unit, focus_dist = NULL) {
  if (map_unit == "district" & is.null(focus_dist)) {
    stop("If map unit is district, focus district must be specified.")
  }
  
  police_dist_sf <- police_dist_sf |>
    group_by(DISTRICT) |> 
    summarize(geometry = st_union(geometry)) |>
    ungroup()
  
  if (map_unit == "district") {
    police_dist_sf <- police_dist_sf |>
      mutate(
        focus_dist = case_when(
          DISTRICT == focus_dist ~ "Yes",
          TRUE ~ "No"
        )) 
    
    title_text <- paste0(
      "Zoom-in on ",
      str_to_title(city),
      " police district: ",
      str_to_title(focus_dist)
    )
    district_var <- "focus_dist"
  } else { 
    district_var <- "DISTRICT"
    
    police_dist_sf <- police_dist_sf |>
      mutate(num = row_number())
    
    title_text <- paste0(
      str_to_title(city), 
      " has ", 
      nrow(police_dist_sf), 
      " police district", 
      if_else(
        nrow(police_dist_sf) > 1,
        's.',
        '.'
      )
    )
  }
  
  police_dist_map <- ggplot(
    police_dist_sf, 
    aes(
      geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300), 
      fill = !!sym(district_var))
    ) + 
    geom_sf() +
    theme_times() + 
    guides(fill = "none") +
    labs(
      title = str_wrap(title_text, width =  50)
    ) +
    scale_fill_brewer(palette = "Set3") +
    coord_sf(clip = "off")
  
  if (map_unit == "city") {
    police_dist_map <- police_dist_map +
      geom_sf_label(
        aes(label = num), 
        fill = 'white',
        family = "TimesNewRoman"
      ) 
  }
  
  return(police_dist_map)
}

#Map 2: Census Neighborhood Population Map 
bg_population_map <- function(subset_tbl_long, city, ethnic_group) {
  total_count <- subset_tbl_long |> filter(Group == ethnic_group) |> pull(Count) 
  total_max_min_populations <- subset_tbl_long |> 
    filter(
      Group == ethnic_group,
      Count %in% c(
        max(total_count), 
        min(total_count[total_count > 5])
      )
    )
  
  text_ethnic_group <- lab_ethnic_group(ethnic_group)
  
  population_all_citywide <- ggplot() + 
    geom_sf(
      data = subset_tbl_long |> 
        filter(Group == ethnic_group), 
      aes(
        geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300), 
        fill = Count
        ), 
      alpha = .65
    ) + 
    geom_sf(
      data = total_max_min_populations, 
      aes(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300)), 
      fill = NA, 
      color = "black", 
      linewidth = .5
    ) +
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    geom_sf_label(
      data = total_max_min_populations, 
      aes(label = Count),
      family = "TimesNewRoman") + 
    theme_times() +
    coord_sf(clip = "off") + 
    labs(
      title = str_wrap(paste0(
        str_to_title(city),
        "'s census neighborhoods range from ", 
        max(total_count),
        " to ", 
        min(total_count[total_count > 5]),
        " ",
        text_ethnic_group,
        " residents."), width =  50),
      subtitle = str_wrap("Census neighborhoods are unevenly populated.", width =  50),
      fill = paste0(
        if_else(
          text_ethnic_group == "total", 
          "Total ", 
          paste0(text_ethnic_group, " ")
          ), 
        "population")
      )
  
  return(population_all_citywide)
}

#Map 3: District Area Overlap Map 
area_intersection_map <- function(all_bg_overlapping_dist, police_dist, city, total_bg, fully_included_bg, focus_dist = NULL) {
  police_dist_census_blocks_citywide <- ggplot() + 
    geom_sf(
      data = all_bg_overlapping_dist, 
      aes(
        geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300), 
        fill = drop_units(bg_perc_area)
      ), 
      alpha = .7) + 
    geom_sf(
      data = police_dist, 
      aes(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300)), # add threshold for simplify if too complex?
      fill = NA,
      color = 'black', 
      linewidth = .5) + 
    scale_fill_distiller(
      palette = "Greens",
      direction = 1, 
      labels = scales::percent) + 
    coord_sf(clip="off") +
    theme_times() +
    labs(
      title = str_wrap(paste0(
        str_to_title(city),
        "'s ",
        if (!is.null(focus_dist)) paste0(str_to_title(focus_dist), ' district touches ') else 'police districts touch ',
        total_bg, 
        " census neighborhoods."), width =  50), 
      subtitle = str_wrap(
        paste0(
          fully_included_bg, 
        " of these neighborhoods are fully in the district."), width =  50),
      fill = "% of block group in district")
  
  return(police_dist_census_blocks_citywide)
}

#Map 4: Neighborhood-District Specific Population Map
resident_intersection_map <- function(all_bg_overlapping_dist, police_dist, city, map_unit) {
  swd_bg_total <- ggplot() + 
    geom_sf(
      data = all_bg_overlapping_dist, 
      aes(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300), fill = drop_units(Total.x)), 
      alpha = .7) + 
    geom_sf(
      data = police_dist, 
      aes(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300)), 
      fill = NA, 
      color = 'black', 
      linewidth = .5) + 
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    coord_sf(clip="off") +
    theme_times() +
    labs(
      title = if (map_unit == "district") 
        "Some geographically small neighborhoods have many in-district residents." 
        else "A neighborhood containing a district boundary line will contribute to\nmultiple patrol areas.",
      subtitle = if (map_unit == "district")
        "Relatedly, large neighborhoods may contribute only a few, depending on their district overlap"
        else "On the other hand, a neighborhood within district boundary lines will only contribute to\none patrol area.",
      fill = "Num. of residents")
  
  return(swd_bg_total)
}

#Formatting thousands for labels -- helper function
format_si_custom <- function(x) {
  abs_x <- abs(x)
  ifelse(
    abs_x >= 1e3, 
    paste0(round(x / 1e3, 0), "K"),
    as.character(round(x, 0))
  )
}

#Map 5: All Police District Population Map
dist_population_map <- function(police_dist_sf, all_bg_overlapping_dist, city, map_unit, ethnic_group, dist_name = NULL, perc_total = "total") {
  if (perc_total == "percent" & (ethnic_group == "Total" | map_unit == "district")) {
    stop("Map can only be in percent units if map is at city-level and ethnic group is not Total.")
  }
  
  # Argument sanity check to help with debugging
  stopifnot(
    "data.frame" %in% class(police_dist_sf), 
    "data.frame" %in% class(all_bg_overlapping_dist), 
    is.character(city), 
    map_unit %in% c("city", "district"),
    perc_total %in% c("percent", "total"),
    ethnic_group %in% c(
      "Total",
      "B_nH",
      "W_nH", 
      "AmIn_nH", 
      "Asi_nH",
      "HaPI_nH",
      "Hispan"
    )
  )
  
  if (perc_total == "percent") {
    ethnic_var <- paste0(ethnic_group, "_perc")
  } else {
    ethnic_var <- ethnic_group
  }
  
  text_ethnic_group <- lab_ethnic_group(ethnic_group)
  
  if (map_unit == "city") {
    low_county <- police_dist_sf |> 
      filter(!!sym(ethnic_var) == min(!!sym(ethnic_var))) |> 
      pull(DISTRICT)
    high_county <- police_dist_sf |> 
      filter(!!sym(ethnic_var) == max(!!sym(ethnic_var))) |> 
      pull(DISTRICT)
    
    title_text <- paste0(
      str_to_title(city),
      "'s ",
      low_county,
      " has the lowest ",
      if (perc_total == "percent") "proportion of " else "number of ",
      if (ethnic_group != "Total") paste0(text_ethnic_group, ' ') else '',
      "residents."
      )
    
    subtitle_text <- paste0(
      high_county, 
      " has the highest ", 
      if (perc_total == "percent") "proportion of " else "number of ",
      if (ethnic_group != "Total") paste0(text_ethnic_group, ' ') else '',
      "residents."
    )
  } else { #map_unit is district
    num_res <- police_dist_sf |> pull(!!sym(ethnic_var)) |> as.numeric() |> round()
    
    title_text <- paste0(
      str_to_title(city),
      "'s ",
      str_to_title(dist_name), 
      " patrol district has ", 
      prettyNum(num_res, big.mark = ','), 
      ' ', 
      if (text_ethnic_group == 'total') text_ethnic_group else paste0(' ', text_ethnic_group), 
      " residents."
    )
    
    if (ethnic_group != "Total") {
      percent <- police_dist_sf |> 
        pull(!!sym(paste0(ethnic_group, "_perc"))) |> 
        as.numeric() |> 
        round(1)
      
      subtitle_text = paste0(
        percent * 100, 
        '% of ',
        str_to_title(city),
        "'s total population is ",
        text_ethnic_group
      )
    }
  }

  all_dist_blacknh_perc <- ggplot() + 
    geom_sf(
      data = police_dist_sf, 
      aes(
        geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300), 
        fill = if (perc_total == "percent") {
          as.numeric(!!sym(ethnic_var))   
        } else {
          round(as.numeric(!!sym(ethnic_var)))
        }, 
      )
    ) +
    theme_times() +
    coord_sf(clip = "off") +
    labs(
      title = str_wrap(title_text, width =  50), 
      fill = paste0(
        if (perc_total == "percent") "Percent of District (" else "Number of Residents (",
        text_ethnic_group, 
        ")")
    )
  
  if (map_unit == "city" & perc_total == "total") {
    police_dist_sf <- police_dist_sf |>
      mutate(label_val = format_si_custom(
        as.numeric(
          !!sym(ethnic_var)
          )
        )
        )
    
    all_dist_blacknh_perc <- all_dist_blacknh_perc +
      geom_sf_label(
        data = police_dist_sf, 
        label = police_dist_sf$label_val,
        fill = "white",
        family = "TimesNewRoman") +
      scale_fill_distiller(
        palette = "Purples",
        direction = 1,
        labels = function(x) format_si_custom(x)
      )
  } else if (perc_total == "percent") { 
    min_val <- min(police_dist_sf[[ethnic_var]], na.rm = TRUE)
    max_val <- max(police_dist_sf[[ethnic_var]], na.rm = TRUE) 
    
    all_dist_blacknh_perc <- all_dist_blacknh_perc +
      geom_sf_label(
        data = police_dist_sf, 
        aes(
          label = paste0(
            round( as.numeric(!!sym(ethnic_var)) * 100, 1), 
            "%")
          ),
        fill = "white",
        family = "TimesNewRoman")  +
      scale_fill_distiller(
        palette = "Purples", 
        direction = 1, 
        limits = c(min_val, max_val),
        labels = scales::percent)
  }
  
  if (map_unit == "district") { 
    all_dist_blacknh_perc <- all_dist_blacknh_perc +
      geom_sf(
        data = all_bg_overlapping_dist, 
        aes(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 300)),
        fill = NA) +
      scale_fill_distiller(
        palette = "Purples",
        direction = 1) +
      guides(fill = "none")
  }
  if (!(map_unit == "district" & ethnic_group == "Total")) {
    all_dist_blacknh_perc <- all_dist_blacknh_perc +
      labs(subtitle = str_wrap(subtitle_text), width =  50) 
  }
  
  return(all_dist_blacknh_perc)
}

