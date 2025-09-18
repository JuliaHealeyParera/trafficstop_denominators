generate_analysis <- function(
    city, # city name (raleigh, charlotte, durham, etc.)
    map_unit, # "city" or "district" 
    dist_name = NULL, # if map_unit == "district", dist_name is district name
    file = NULL, # if city is not in district_calculations, file path required
    dist_name_var = NULL, # required with file, name of district name variable
    geometry_var = NULL, # required with file, name of geometry variable
    ethnic_group = "Total", # default is Total (no ethnic group, full population)
    year = NULL # year of census data, (default is most current year available)
    ) {
  
  city_lower <- str_to_lower(city)
  
  # If current police district is not in master district file, update.
  if (!(city_lower %in% current_policedistricts$city)) { 
    if (is.null(file) | is.null(dist_name_var) | is.null(geometry_var)) {
      stop("Must provide file, district name variable, and geometry variable. City is not in current list.")
    }
    # from update_policedist_shpfiles -- automatically updates environment variables
    append_shp(file, city_lower, dist_name_var, geometry_var)
  }
  
  if (map_unit == "district" & is.null(dist_name)) {
    stop("Must provide district name if selecting district-specific geometry")
  }
  
  # Load police data and prepare for future geography calculations
  police_curr <- current_policedistricts |>
    filter(city == city_lower) |>
    mutate(policedist_full_area = st_area(geometry)) |>
    st_make_valid(police_curr)
  
  # ggplot #1, simple police district map. function from map_generator
  dist_str <- ifelse(is.null(dist_name), "city", dist_name)
  police_dist_map_obj <- police_district_map(police_curr, city_lower, map_unit, focus_dist = dist_name)
  police_dist_map_name <- paste0(city_lower, '_', dist_str, '_police_dist_map.png')
  police_dist_map_path <- here('plots', city_lower, dist_str, police_dist_map_name)
  ggsave(police_dist_map_path, police_dist_map_obj, create.dir = TRUE, width = 6, height = 4, dpi = 150, units = "in")
  
  #If focus-district, filter out irrelevant districts
  if (!is.null(dist_name)) {
    police_curr <- police_curr |>
      filter(DISTRICT == dist_name)
  }
  
  # Metadata
  num_dist <- nrow(police_curr)
  name_dist <- police_curr |> pull(DISTRICT)
 
  # If user does not specify year, use currently-maintained current year
  if (is.null(year)) {
    census_year <- read_csv(here('data', 'census_data', 'census_data_metadata.csv')) |> 
      filter(status == "current") |> 
      pull(year)
  } else {
    census_year <- year
  }
  
  # Load census data, calling API if current year is bg_to_policedist available
  # function from census_data
  census_info <- read_census_data(census_year)
  nc_bg_sf <- census_info$bgsf[[1]]
  acs_data_tbl <- census_info$acstbl[[1]]
  
  # Geography recalculations, functions from 
  city_geography <- nc_bg_sf[nc_bg_sf$NAME==city,'geometry'][[1]]
  city_bg_tbl <- bg_dist_subset(acs_data_tbl, police_curr)
  city_bg_long <- pivot_long_tidy(city_bg_tbl) 
  
  # ggplot #2, census neighborhood populations. function from map_generator
  bg_pop_map_obj <- bg_population_map(city_bg_long, city_lower, 'Total')
  bg_pop_map_name <- paste0(city_lower, '_', dist_str, '_bg_pop_map.png')
  bg_pop_map_path <- here('plots', city_lower, dist_str, bg_pop_map_name)
  ggsave(bg_pop_map_path, bg_pop_map_obj, width = 6, height = 4, units = "in", dpi = 150, create.dir = TRUE)
  
  # functions from bg_to_policedist -- geography recalculations
  city_intersection_sf <- bgtbl_to_bgsf(city_bg_tbl, police_curr)
  police_dist_sf <- bgsf_to_poldistsf(city_intersection_sf)
  bg_overlap <- all_bg_overlapping_dist(city_intersection_sf, city_bg_tbl)
  
  # Metadata
  date_added <- Sys.Date()
  # Number of total touching census neighborhoods 
  total_bg <- nrow(bg_overlap |> group_by(GEOID) |> slice(1))
  # Number of census neighborhoods fully within a district
  fully_included_bg <- nrow(bg_overlap |> filter(round(as.vector(bg_perc_area), 2) == 1))
  
  # ggplot #3, district area overlap. function from map_generator
  police_dist_census_blocks_citywide_map_obj <- area_intersection_map(
    bg_overlap, 
    police_curr,
    city_lower,
    total_bg, 
    fully_included_bg)
  police_dist_census_blocks_citywide_map_name <- paste0(city_lower, '_', dist_str, '_police_dist_census_blocks_citywide_map.png')
  police_dist_census_blocks_citywide_map_path <- here('plots', city_lower, dist_str, police_dist_census_blocks_citywide_map_name)
  ggsave(police_dist_census_blocks_citywide_map_path, police_dist_census_blocks_citywide_map_obj, width = 6, height = 4, units = "in", dpi = 150, create.dir = TRUE)
  
  # ggplot #4, neighborhood-district specific populations. function from map_generator
  dist_bg_pop_map_obj <- resident_intersection_map(bg_overlap, police_curr, city_lower, map_unit)
  dist_bg_pop_map_name <- paste0(city_lower, '_', dist_str, '_dist_bg_pop_map.png')
  dist_bg_pop_map_path <- here('plots', city_lower, dist_str, dist_bg_pop_map_name)
  ggsave(dist_bg_pop_map_path, dist_bg_pop_map_obj, width = 6, height = 4, units = "in", dpi = 150, create.dir = TRUE)
  
  # ggplot #5, police district populations
  dist_only_pop_map_obj <- dist_population_map(
    police_dist_sf, 
    bg_overlap, 
    city, 
    map_unit, 
    ethnic_group,
    dist_name = dist_name
  )
  dist_only_pop_map_name <- paste0(city_lower, '_', dist_str, '_dist_only_pop_map.png')
  dist_only_pop_map_path <- here('plots', city_lower, dist_str, dist_only_pop_map_name)
  ggsave(dist_only_pop_map_path, dist_only_pop_map_obj, width = 6, height = 4, units = "in", dpi = 150, create.dir = TRUE)
  
  # Join new objects together
  new_district_objects <- list(
    city_lower,
    census_year,
    date_added,
    num_dist,
    list(name_dist),
    list(police_curr),
    total_bg,
    fully_included_bg,
    list(police_dist_sf),
    list(nc_bg_sf),
    police_dist_map_path,
    bg_pop_map_path,
    police_dist_census_blocks_citywide_map_path,
    dist_bg_pop_map_path,
    dist_only_pop_map_path
  )
  
  names_master_analysis <- c(
    "city_name", "census_year", "date_added", 
    "district_num", "district_names",
    "police_district_df",  
    "total_bg_num", "fullyinc_bg_num",
    "policedist_sf_df", "bg_sf_df",
    "police_dist_ggplot", 
    "bg_population_ggplot", 
    "dist_bg_areraintersection_ggplot", 
    "dist_bg_numresident_ggplot", 
    "dist_pop_map_ggplot"
  )
  names(new_district_objects) <- names_master_analysis
  
  return(new_district_objects)
}

append_analysis <- function(new_district_objects) {
  master_analysis_path <- here('data','district_calculations.rds')
  if (file.exists(master_analysis_path)) {
    district_calculations <- readRDS(master_analysis_path)
    joined_district_calculations <- rbind(district_calculations, as_tibble(new_district_objects))
    saveRDS(joined_district_calculations, here('data','district_calculations.rds'))
  }
  else {
    new_obj_tibble <- as_tibble(new_district_objects)
    saveRDS(new_obj_tibble, here('data','district_calculations.rds'))
  }
}