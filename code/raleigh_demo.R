library(tidyverse)
library(tidycensus)
library(sf)
library(units)

#Load current_policedistricts
source('code/update_policedist_shpfiles.R')
#No need to append for Raleigh (one of 3 original shp files)
city_name <- 'raleigh'
dist <- 'SWD' # somehow need to spatially? name districts 
police_raleigh <- current_policedistricts |>
  filter(city == city_name) |>
  mutate(num = row_number())
police_swd_raleigh <- police_raleigh |>
  filter(DISTRICT == dist)

source('code/map_generator.R')
#Introductory map 1: All Raleigh districts
police_dist_map <- police_district_map(police_raleigh, 'raleigh')
#Introductory map 1 (EXTRA): Zoom-in on SWD of Raleigh
police_swd_dist_map <- police_district_map(police_swd_raleigh, 'southwest raleigh')
#Save maps
ggsave('plots/swd_raleigh/all_raleigh_districts_1.png', police_dist_map)
ggsave('plots/swd_raleigh/swd_raleigh_district_extra.png', police_swd_dist_map)

#Load census data
source('code/census_data.R')
yr <- read_csv('data/census_data/census_data_metadata.csv') |> 
  filter(status == "current") |> 
  pull(year)
census_info <- read_census_data(yr)
nc_bg_sf <- census_info$bgsf[[1]]
acs_data_tbl <- census_info$acstbl[[1]]

#Script for census manipulations
source('code/bg_to_policedist.R')
#All of Raleigh - census info
raleigh_geography <- nc_bg_sf[nc_bg_sf$NAME=="Raleigh",'geometry'][[1]]
#Raleigh SWD ethnicity data (geometry subset)
raleigh_swd_tbl <- bg_dist_subset(acs_data_tbl, police_swd_raleigh)
raleigh_tbl <- bg_dist_subset(acs_data_tbl, police_raleigh)

#Make tidy to be able to facet for ethnic groups
raleigh_swd_long <- pivot_long_tidy(raleigh_swd_tbl)
raleigh_long <- pivot_long_tidy(raleigh_tbl) 

#Block group population maps 
swd_raleigh_bg_pop_map <- bg_population_map(raleigh_swd_long, 'southwest raleigh', 'Total')
raleigh_bg_pop_map <- bg_population_map(raleigh_long, 'raleigh', 'Total')
#Save maps
ggsave('plots/swd_raleigh/swd_raleigh_bg_pop_2.png', swd_raleigh_bg_pop_map)
ggsave('plots/swd_raleigh/raleigh_bg_pop_alt.png', raleigh_bg_pop_map)

#Black non-Hispanic plot
swd_raleigh_bg_bnH_map <- bg_population_map(raleigh_swd_long, 'southwest raleigh', 'B_nH')
#Save
ggsave('plots/swd_raleigh/swd_bg_blacknhpop_extra.png', swd_raleigh_bg_bnH_map)

#Start ethnicity recalculations
police_raleigh <- police_raleigh |> 
  mutate(policedist_full_area = st_area(geometry)) |>
  st_make_valid(police_swd_raleigh) 

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

#Residents in-district per block group
dist_bg_pop_map <- resident_intersection_map(bg_overlap_ral, police_raleigh, "raleigh")
dist_bg_pop_swd <- resident_intersection_map(bg_overlap_swd, police_swd_raleigh, "southwest raleigh")

#Residents in each police district -- percent or number of residents
poldist_sf_ral <- bgsf_to_poldistsf(ral_intersection_sf, "DISTRICT")
poldist_sf_swd <- bgsf_to_poldistsf(swd_ral_intersection_sf, "DISTRICT")

dist_pop_map_swd <- dist_population_map(
  poldist_sf_swd,                                # Police district sf df
  bg_overlap_swd,                                # Overlapping block groups df
  "southwest raleigh",                           # City/district name
  "district",                                    # "district" or "city" (scale/size of map)
  "total",                                       # "percent" or "total" (units)
  "B_nH"                                         # ethnic group: "W_nH", "B_nH", "AmIn_nH", "Asi_nH", "HaPI_nH", "Hispan", "Total"
)

dist_pop_map_ral <- dist_population_map(
  poldist_sf_ral,
  bg_overlap_ral,
  "raleigh",
  "city",
  "total",
  "Total"
)