library(dplyr)
library(tidycensus)

original_sourcing <- function() {
  nc_places <- get_decennial(
    geography = "place",
    state = "NC",
    variables = "P1_001N",
    year = 2020,
    geometry = FALSE
  )
  
  # Extract just city/town names
  nc_city_names <- nc_places |>
    select(NAME) |>
    mutate(city = str_remove_all(
      NAME,
      ", North Carolina| town| CDP| city"
    )) 
  
  write_csv(nc_city_names, 'data/census_data/nc_city_names.csv')
}

nc_city_names <- read_csv('../data/census_data/nc_city_names.csv') |> pull(city)

