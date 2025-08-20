reload_census_data <- function(year) {
  #Variables to extract from census
  census_data <- load_variables(year = year, dataset = "acs5") 
  acs_var_tbl = tribble(
    ~group, ~variable, 
    "Total", "B03002_001",
    "White_nH", "B03002_003",
    "Black_nH", "B03002_004",
    "Am.Ind._nH", "B03002_005",
    "Asian_nH", "B03002_006",
    "Hispanic", "B03002_012",
    "Hawaiian_or_PI_nH", "B03002_007", 
    "Other_nH", "B03002_008", 
    "Two_race_nH", "B03002_009", 
  )
  
  #Get block group names for post-pivoting
  nc_bg_sf = get_acs("block group", year = year, variables = "B03002_001", geometry = T, state = "NC") |>  #2022 FOR 7/14
    select(GEOID, NAME) 

  #Get specified ethnicity variables per block group
  acs_data_tbl = get_acs("block group", year = year, variables = acs_var_tbl$variable, state = "NC") |> 
    left_join(acs_var_tbl) |> 
    select(GEOID, group, estimate) |> 
    pivot_wider(names_from = group, values_from = estimate) |> #Pivot so each ethnicity is a var.
    right_join(nc_bg_sf) |> #Bring in block group names
    mutate(Other = Other_nH + Two_race_nH) |> #Combine two smaller categories
    select(-c(Other_nH, Two_race_nH))
  st_geometry(acs_data_tbl) <- acs_data_tbl$geometry #Assert that geometry is interpreted as sf object
  acs_data_tbl <- st_transform(acs_data_tbl, 2264) |> #Convert to state plane 
    rename('W_nH' = White_nH, 
           'B_nH' = Black_nH, 
           "AmIn_nH" = Am.Ind._nH,
           "Asi_nH" = Asian_nH, 
           "HaPI_nH" = Hawaiian_or_PI_nH, 
           "Hispan" = Hispanic)

  #Hard-downloaded because of issues with census, replace later with API call
  nc_bg_sf = get_acs("place", year = year, variables = "B03002_001", geometry = T, state = "NC") |>
    select(GEOID, NAME, geometry) |>
    st_transform(2264)
  
  # Create new directory if necessary 
  if (!dir.exists(paste0('data/census_data/', year))) {
    dir.create(
      paste0('data/census_data/', year), 
      recursive = TRUE) 
  }
  
  # Add data to repository 
  nc_bg_sf_path <- paste0('data/census_data/', as.character(year),'/nc_bg_sf_', year, '.shp')
  acs_data_tbl_path <- paste0('data/census_data/', as.character(year), '/acs_data_tbl_', year, '.shp')
  st_write(nc_bg_sf, here(nc_bg_sf_path))
  st_write(acs_data_tbl, here(acs_data_tbl_path))
  
  # Add new census information metadata to csv
  census_data_metadata <- read_csv(here('data', 'census_data', 'census_data_metadata.csv')) |>
    rbind(
      data.frame(year = year, 
                 date_added = Sys.Date(), 
                 status = "current", 
                 nc_bg_sf_path = nc_bg_sf_path, 
                 acs_data_tbl_path = acs_data_tbl_path)
      ) 
  
  # Reload current default year as most recent year
  max_yr <- max(as.numeric(c(census_data_metadata$year)))
  census_data_metadata <- census_data_metadata |> 
    mutate(
      status = case_when(
        year == as.character(max_yr) ~ "current", 
        TRUE ~ "deprecated"
      ))

  write_csv(census_data_metadata, here('data', 'census_data', 'census_data_metadata.csv'))
}

read_census_data <- function(yr = NULL) {
  census_data_metadata <- read_csv(here('data', 'census_data', 'census_data_metadata.csv'))
  
  if (!is.null(yr) && !(yr %in% census_data_metadata$year)) {
    reload_census_data(yr)
  }
  
  if (is.null(yr)) {
    yr <- census_data_metadata |> filter(status == "current") |> pull(year)
  }
  
  # Read data
  nc_bg_sf_path <- census_data_metadata[census_data_metadata$year == yr, 'nc_bg_sf_path'] 
  acs_data_tbl_path <- census_data_metadata[census_data_metadata$year == yr, 'acs_data_tbl_path'] 
  nc_bg_sf <- st_read(here(nc_bg_sf_path))
  acs_data_tbl <- st_read(here(acs_data_tbl_path))
  
  return(tibble(bgsf = list(nc_bg_sf), acstbl = list(acs_data_tbl)))
}
