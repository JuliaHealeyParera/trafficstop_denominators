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
police_dist_census_blocks_citywide_map <- area_intersection_map(
  bg_overlap_ral, 
  police_raleigh,
  "raleigh")

#Residents in-district 
dist_bg_pop_map <- resident_intersection_map(bg_overlap_ral, police_raleigh, "raleigh")

#######OLD########
#Census block SW Raleigh police district intersections  
total_bg_swd <- nrow(bg_overlap_swd)
fully_included_bg_swd <- nrow(bg_overlap_swd |> filter(round(bg_perc_area, 2) == 1))
bg_dist_intersect <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd, 
    aes(geometry = geometry), 
    fill = NA) +
  geom_sf(
    data = police_swd_raleigh, 
    aes(geometry = geometry), 
    fill = "blue4", 
    alpha = 0.6) + 
  theme_void() + 
  labs(
    title = paste0(
      "Raleigh's Southwest police district touches\n", 
      total_bg_swd, 
      " census neighborhoods."), 
    subtitle = paste0(
      fully_included_bg_swd, 
      " of these neighborhoods are fully in the district.")
    )
ggsave('plots/swd_raleigh/bg_dist_intersect_extra.png', bg_dist_intersect)

#Percent area overlay
police_dist_census_blocks <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd_swd, 
    aes(geometry = geometry, fill = bg_perc_area), 
    alpha = .7) + 
  geom_sf(
    data = annotated_bg,
    aes(geometry = geometry), 
    fill = NA, 
    color = "gray30", 
    linewidth = .6) + 
  geom_sf(
    data = police_swd_raleigh, 
    aes(geometry = geometry), 
    fill = NA, 
    color = 'black', 
    linewidth = .8) + 
  annotate("text", x = 2130000, y = 751000, label = "100% of area\nin district") +
  annotate("segment", x = 2103500, y = 747000, xend = 2123000, yend = 751000) + #could also use st_centroid of obj and extract coord
  annotate("text", x = 2070000, y = 712000, label = "7% of area\nin district") +
  annotate("segment", x = 2077350, y = 712000, xend = 2088000, yend = 712000) +
  scale_fill_distiller(
    palette = "Greens",
    direction = 1, 
    labels = scales::percent) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(
    title = paste0(
      "Raleigh's Southwest police district touches\n", 
      total_bg_swd, 
      " census neighborhoods."), 
    subtitle = paste0(
      fully_included_bg_swd, 
      " of these neighborhoods are fully in the district."),
    fill = "% of block\ngroup in district")
ggsave('plots/swd_raleigh/swd_bg_policedist_overlay_3.png', police_dist_census_blocks)

total_bg_citywide <- nrow(citywide_bgs_with_overlap)
fully_included_bg_citywide <- nrow(citywide_bgs_with_overlap |> filter(round(bg_perc_area, 2) == 1))

citywide_bgs_with_overlap <- 
  st_drop_geometry(ral_intersection_sf) |>
  right_join(raleigh_tbl, by = join_by(GEOID == GEOID))

#Percent area overlap citywide
police_dist_census_blocks_citywide <- ggplot() + 
  geom_sf(
    data = citywide_bgs_with_overlap, 
    aes(geometry = geometry, fill = bg_perc_area), 
    alpha = .7) + 
  geom_sf(
    data = police_raleigh, 
    aes(geometry = st_simplify(geometry, dTolerance = 750)), 
    fill = NA, 
    color = 'black', 
    linewidth = .75) + 
  scale_fill_distiller(
    palette = "Greens",
    direction = 1, 
    labels = scales::percent) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(
    title = paste0(
      "Raleigh's Southwest police district touches\n", 
      total_bg_citywide, 
      " census neighborhoods."), 
    subtitle = paste0(
      fully_included_bg_citywide, 
      " of these neighborhoods are fully in the district."),
    fill = "% of block\ngroup in district")

ggsave(
  'plots/swd_raleigh/swd_bg_policedist_overlay_citywide_3_alt.png', 
  police_dist_census_blocks_citywide
  )

#Map of num. total residents in district per block group 
label_val <- swd_ral_intersection_sf[
  swd_ral_intersection_sf$GEOID %in% annotated_bg$GEOID, 
  c('GEOID', 'Total')
  ]

swd_bg_total <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd, 
    aes(geometry = geometry, fill = B_nH.x), 
    alpha = .7) + 
  geom_sf(
    data = annotated_bg, 
    aes(geometry = geometry), 
    fill = NA, 
    color = "gray30", 
    linewidth = .6) + 
  geom_sf(
    data = police_swd_raleigh, 
    aes(geometry = geometry), 
    fill = NA, 
    color = 'black', 
    linewidth = .8) + 
  annotate(
    "text", 
    x = 2130000, y = 751000, 
    label = paste0(
      round(label_val |> filter(GEOID == 371830516003) |> pull(Total)),
      " residents\nin district")) +
  annotate("segment", x = 2103500, y = 747000, xend = 2123000, yend = 751000) + #could also use st_centroid of obj and extract coord
  annotate(
    "text", 
    x = 2065000, y = 712000, 
    label = paste0(
      round(label_val |> filter(GEOID == 371830530102) |> pull(Total)),
      " residents\nin district")) +
  annotate("segment", x = 2071000, y = 711000, xend = 2088000, yend = 712000) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(
    title = "Some geographically small neighborhoods have many residents.",
    subtitle = "Relatedly, large neighborhoods may contribute only a few,\ndepending on their district overlap",
    fill = "Num. of\nresidents")
ggsave('plots/swd_raleigh/swd_bg_intersection_totalpop_4.png', swd_bg_total)

#Convert from block group unit to police district unit
police_final <- bgsf_to_poldistsf(ral_intersection_sf, "DISTRICT") 
police_final_swd <- police_final |> filter(DISTRICT == "SWD")

#Full police district demographics overlap map
swd_bg_full_dist <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd, 
    aes(geometry = geometry),
    fill = NA) +
  geom_sf(
    data = police_final |> 
      filter(DISTRICT == "SWD"), 
    aes(geometry = geometry), 
    fill = "blue4", 
    alpha = 0.6) + 
  theme_void() + 
  labs(
    title = paste0(
      "The estimated total population\nof SW Raleigh is ",
      prettyNum(round(police_final_swd |> pull(Total)), big.mark = ",")), 
    subtitle = paste0(
      police_final_swd |> pull(B_nH_perc) * 100,
      "% of the total population (",
      prettyNum(round(police_final_swd |> pull(B_nH)), , big.mark = ","),
      ")\nis Black Non-Hispanic."))
ggsave('plots/swd_raleigh/swd_bg_full_dist_5.png', swd_bg_full_dist)

#Demographic specific choropleth map (by police district)
all_dist_blacknh_perc <- dist_population_map(police_final, "raleigh", "B_nH")
ggsave('plots/swd_raleigh/all_dist_blacknh_perc_6.png', all_dist_blacknh_perc)


