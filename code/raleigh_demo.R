library(tidyverse)
library(tidycensus)
library(sf)
library(units)
library(here)

#Load current_policedistricts (master dataframe of all patrol districts)
source('code/update_policedist_shpfiles.R')

#Choosing city and focus district of choice 332
city_name <- 'raleigh'
dist <- 'SWD'

#raleigh object (from master list)
police_raleigh <- current_policedistricts |>
  filter(city == city_name) |>
  mutate(num = row_number())
#SWD raleigh district object (from master list)
police_swd_raleigh <- police_raleigh |>
  filter(DISTRICT == dist)

#Map functions
source('code/map_generator.R')
#Introductory map (city) 1: All Raleigh districts
#Data source: patrol district coordinates (NO CENSUS)
police_dist_map <- police_district_map(police_raleigh, 'raleigh', 'city')
#Introductory map (district) 1: Zoom-in on SWD of Raleigh
police_swd_dist_map <- police_district_map(police_raleigh, 'raleigh', 'district', 'SWD')
#Save maps
ggsave('plots/raleigh/city/city_raleigh_districts_1.png', police_dist_map)
ggsave('plots/raleigh/district_swd/swd_raleigh_district_1.png', police_swd_dist_map)

#Load census data manipulation functions
source('code/census_data.R')
#Ignore this, specific to this example (using current collected year):
yr <- read_csv('data/census_data/census_data_metadata.csv') |> 
  filter(status == "current") |> 
  pull(year)

#Load census data for relevant year 
census_info <- read_census_data(yr)
acs_data_tbl <- census_info$acstbl[[1]]

#Script for census manipulations
source('code/bg_to_policedist.R')
#Subset of census data relevant to raleigh
raleigh_tbl <- bg_dist_subset(acs_data_tbl, police_raleigh)
#Subset of census data relevant to raleigh SWD
raleigh_swd_tbl <- bg_dist_subset(acs_data_tbl, police_swd_raleigh)

#Make tidy to be able to facet for ethnic groups
raleigh_swd_long <- pivot_long_tidy(raleigh_swd_tbl)
raleigh_long <- pivot_long_tidy(raleigh_tbl) 

#Block group population maps with pivoted raleigh census data 
#Pivoting allows us to filter for relevant ethnic groups
swd_raleigh_bg_pop_map <- bg_population_map(raleigh_swd_long, 'southwest raleigh', 'Total')
raleigh_bg_pop_map <- bg_population_map(raleigh_long, 'raleigh', 'Total')
#Black non-Hispanic plot
swd_raleigh_bg_bnH_map <- bg_population_map(raleigh_swd_long, 'southwest raleigh', 'B_nH')

#Start ethnicity recalculations
police_raleigh <- police_raleigh |> 
  #Calculate area of patrol areas 
  mutate(policedist_full_area = st_area(geometry)) |>
  st_make_valid(police_swd_raleigh) #CRS check

#Calculate % area overlap, use that to estimate counts per block group
ral_intersection_sf <- bgtbl_to_bgsf(raleigh_tbl, police_raleigh)
swd_ral_intersection_sf <- ral_intersection_sf |> filter(DISTRICT == "SWD")
police_swd_raleigh <- police_raleigh |> filter(DISTRICT == "SWD")

#Police and census block overlay 
annotated_bg <- raleigh_swd_tbl |> 
  filter(GEOID %in% c(371830516003, 371830530102)) #manually chosen
bg_overlap_swd <- all_bg_overlapping_dist(swd_ral_intersection_sf, raleigh_swd_tbl)

#New area intersection map 
bg_overlap_ral <- all_bg_overlapping_dist(ral_intersection_sf, raleigh_tbl)
total_bg <- bg_overlap_ral |> group_by(GEOID) |> slice(1) |> nrow()
fully_included_bg <- nrow(bg_overlap_ral |> filter(round(as.vector(bg_perc_area), 2) == 1))
police_dist_census_blocks_citywide_map <- area_intersection_map(
  bg_overlap_ral, 
  police_raleigh,
  "raleigh",
  total_bg,
  fully_included_bg
  )
total_bg_swd <- bg_overlap_swd |> group_by(GEOID) |> slice(1) |> nrow()
fully_included_bg_swd <- bg_overlap_swd |> filter(round(as.vector(bg_perc_area), 2) == 1) |> nrow()
police_dist_census_blocks_swd_map <- area_intersection_map(
  bg_overlap_swd, 
  police_swd_raleigh,
  "raleigh",
  total_bg_swd,
  fully_included_bg_swd, 
  "SWD"
)

#Residents in-district per block group
dist_bg_pop_map <- resident_intersection_map(bg_overlap_ral, police_raleigh, "raleigh", map_unit = "city")
dist_bg_pop_swd <- resident_intersection_map(bg_overlap_swd, police_swd_raleigh, "southwest raleigh", map_unit = "district")

#Residents in each police district -- percent or number of residents
#Final relevant dataframe: 
poldist_sf_ral <- bgsf_to_poldistsf(ral_intersection_sf)
poldist_sf_swd <- bgsf_to_poldistsf(swd_ral_intersection_sf)

dist_pop_map_swd <- dist_population_map(
  police_dist_sf = poldist_sf_swd,                    # Police district sf df
  all_bg_overlapping_dist = bg_overlap_swd,           # Overlapping block groups df
  city = "southwest raleigh",                         # City/district name
  map_unit = "district",                              # "district" or "city" (scale/size of map)
  ethnic_group = "B_nH"                               # ethnic group: "W_nH", "B_nH", "AmIn_nH", "Asi_nH", "HaPI_nH", "Hispan", "Total"
)

dist_pop_map_ral <- dist_population_map(
  police_dist_sf = poldist_sf_ral,
  all_bg_overlapping_dist = bg_overlap_ral,
  city = "raleigh",
  map_unit = "city",
  ethnic_group = "Total"
)
