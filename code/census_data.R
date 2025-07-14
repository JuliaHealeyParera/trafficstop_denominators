library(tidyverse)
library(tidycensus)
library(sf)

#Variables to extract from census
census_data <- load_variables(year = 2023, dataset = "acs5") 
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
nc_bg_sf = get_acs("block group", year = 2022, variables = "B03002_001", geometry = T, state = "NC") |>  #2022 FOR 7/14
  select(GEOID, NAME) 
#Get specified ethnicity variables per block group
acs_data_tbl = get_acs("block group", year = 2022, variables = acs_var_tbl$variable, state = "NC") |> 
  left_join(acs_var_tbl) |> 
  select(GEOID, group, estimate) |> 
  pivot_wider(names_from = group, values_from = estimate) |> #Pivot so each ethnicity is a var.
  right_join(nc_bg_sf) |> #Bring in block group names
  mutate(Other = Other_nH + Two_race_nH) |> #Combine two smaller categories
  select(-c(Other_nH, Two_race_nH))
st_geometry(acs_data_tbl) <- acs_data_tbl$geometry #Assert that geometry is interpreted as sf object
acs_data_tbl <- st_transform(acs_data_tbl, 2264) #Convert to state plane

#Hard-downloaded because of issues with census, replace later with API call
nc_bg_sf = get_acs("place", year = 2022, variables = "B03002_001", geometry = T, state = "NC") |>
  select(GEOID, NAME, geometry) |>
  st_transform(2264)
