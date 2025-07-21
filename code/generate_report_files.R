source(here("code", "census_data.R"))
source(here("code", "bg_to_policedist.R"))
source(here("code", "update_policedist_shpfiles.R"))

generate_analysis <- function(file, dist_name_var, city) {
  city_lower <- str_to_lower(city)
  police_curr <- current_policedistricts |>
    filter(city = city_lower)
  
  num_dist <- nrow(police_curr)
  name_dist <- police_curr |> pull(dist_name_var)
  
  police_dist_map <- police_district_map(police_curr, city_lower)
  
  yr <- read_csv('data/census_data/census_data_metadata.csv') |> 
    filter(status == "current") |> 
    pull(year)
  census_info <- read_census_data(yr)
  nc_bg_sf <- census_info$bgsf[[1]]
  acs_data_tbl <- census_info$acstbl[[1]]
  
  city_geography <- nc_bg_sf[nc_bg_sf$NAME==city,'geometry'][[1]]
  city_bg_tbl <- bg_dist_subset(acs_data_tbl, police_curr)
  city_bg_long <- pivot_long_tidy(city_bg_tbl) 
  
  bg_pop_map <- bg_population_map(city_bg_long, city_lower, 'Total')
  
  police_curr <- police_curr |> 
    mutate(policedist_full_area = st_area(geometry)) |>
    st_make_valid(police_curr)
  
  city_intersection_sf <- bgtbl_to_bgsf(city_bg_tbl, police_curr)
  bg_overlap <- all_bg_overlapping_dist(city_intersection_sf, city_bg_tbl)
  
  total_bg <- nrow(bg_overlap |> group_by(GEOID) |> slice(1))
  fully_included_bg <- nrow(bg_overlap |> filter(round(bg_perc_area, 2) == 1))
  
  police_dist_census_blocks_citywide_map <- area_intersection_map(
    bg_overlap, 
    police_curr,
    city_lower,
    total_bg, 
    fully_included_bg)
  
  dist_bg_pop_map <- resident_intersection_map(bg_overlap, police_curr, city_lower)
}

tibble(
  dist_name_str = city_lower, 
  police_dist_df = list(police_curr), 
  num_dist_int = num_dist, 
  name_dist_list = name_dist, 
  num_bg_int = total_bg, 
  num_fullyinc_bg_int = fully_included_bg,
  bgsf_df = list(nc_bg_sf),
  
  police_dist_ggplot = police_dist_map,
  bg_population_ggplot = bg_pop_map,
  dist_bg_areaintersection_ggplot = police_dist_census_blocks_citywide_map, 
  dist_bg_numresident_ggplot = dist_bg_pop_map
)