# Loads data and calls census API if necessary
source(here("code", "census_data.R"))
# Converts block group demographics to police district demographics
source(here("code", "bg_to_policedist.R"))
# Maintains and loads previously uploaded police district (master) shp files
source(here("code", "update_policedist_shpfiles.R"))
# Functions to create custom ggplot map objects
source(here("code", "map_generator.R"))

generate_analysis <- function(file, dist_name_var, geometry_var, city, ethnic_group = "Total", year = NULL) {
  city_lower <- str_to_lower(city)
  
  # If current police district is not in master district file, update.
  if (!(city_lower %in% current_policedistricts$city)) { 
    # from update_policedist_shpfiles -- automatically updates environment variables
    append_shp(file, city_lower, dist_name_var, geometry_var)
  }
  
  # Load police data and prepare for future geography calculations
  police_curr <- current_policedistricts |>
    filter(city == city_lower) |>
    mutate(policedist_full_area = st_area(geometry)) |>
    st_make_valid(police_curr)
  
  # Metadata
  num_dist <- nrow(police_curr)
  name_dist <- police_curr |> pull(dist_name_var)
  
  # ggplot #1, simple police district map. function from map_generator
  police_dist_map <- police_district_map(police_curr, city_lower)
  
  # If user does not specify year, use currently-maintained current year
  if (is.null(year)) {
    yr <- read_csv('data/census_data/census_data_metadata.csv') |> 
      filter(status == "current") |> 
      pull(year)
  } else {
    yr <- year
  }
  
  # Load census data, calling API if current year is bg_to_policedist available
  # function from census_data
  census_info <- read_census_data(yr)
  nc_bg_sf <- census_info$bgsf[[1]]
  acs_data_tbl <- census_info$acstbl[[1]]
  
  # Geography recalculations, functions from 
  city_geography <- nc_bg_sf[nc_bg_sf$NAME==city,'geometry'][[1]]
  city_bg_tbl <- bg_dist_subset(acs_data_tbl, police_curr)
  city_bg_long <- pivot_long_tidy(city_bg_tbl) 
  
  # ggplot #2, census neighborhood populations. function from map_generator
  bg_pop_map <- bg_population_map(city_bg_long, city_lower, 'Total')
  
  # functions from bg_to_policedist -- geography recalculations
  city_intersection_sf <- bgtbl_to_bgsf(city_bg_tbl, police_curr)
  bg_overlap <- all_bg_overlapping_dist(city_intersection_sf, city_bg_tbl)
  
  # Metadata, also used in map generation 
  # Number of total touching census neighborhoods 
  total_bg <- nrow(bg_overlap |> group_by(GEOID) |> slice(1))
  # Number of census neighborhoods fully within a district
  fully_included_bg <- nrow(bg_overlap |> filter(round(as.vector(bg_perc_area), 2) == 1))
  
  # ggplot #3, district area overlap. function from map_generator
  police_dist_census_blocks_citywide_map <- area_intersection_map(
    bg_overlap, 
    police_curr,
    city_lower,
    total_bg, 
    fully_included_bg)
  
  # ggplot #4, neighborhood-district specific populations. function from map_generator
  dist_bg_pop_map <- resident_intersection_map(bg_overlap, police_curr, city_lower)
  
  # ggplot #5, police district populations
  dist_only_pop_map <- dist_population_map(
    police_dist_sf, 
    bg_overlap, 
    city, 
    map_unit, 
    perc_total, 
    ethnic_group
  )
  
  # Rbind to nested dataframe structure stored in memory:
  district_calculations <- readRDS("data/district_calculations.rds")
  new_district_objects <- list(
    city_lower,
    list(police_curr),
    num_dist,
    name_dist,
    total_bg,
    fully_included_bg,
    list(nc_bg_sf),
    list(police_dist_map),
    list(bg_pop_map),
    list(police_dist_census_blocks_citywide_map),
    list(dist_bg_pop_map),
    list(dist_only_pop_map)
  )
  
  joined_district_calculations <- rbind(district_calculations, new_district_objects)
  saveRDS(joined_district_calculations, "data/district_calculations.rds")
}

