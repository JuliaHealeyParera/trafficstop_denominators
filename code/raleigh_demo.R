library(tidyverse)
library(tidycensus)
library(sf)
library(units)

#Load current_policedistricts
source('code/update_policedist_shpfiles.R')
#No need to append for Raleigh (one of 3 original shp files)
city <- 'raleigh'
dist <- 'SWD' # somehow need to spatially? name districts 
police_raleigh <- current_policedistricts |>
  filter(city == city) |>
  mutate(num = row_number())
police_swd_raleigh <- police_raleigh |>
  filter(DISTRICT == dist)

#Introductory map 1: All Raleigh districts
police_dist <- police_district_map(police_raleigh, 'raleigh')
#Introductory map 1 (EXTRA): Zoom-in on SWD of Raleigh
police_swd <- police_district_map(police_swd_raleigh, 'southwest raleigh')
#Save maps
ggsave('plots/swd_raleigh/all_raleigh_districts_1.png', police_dist)
ggsave('plots/swd_raleigh/swd_raleigh_district_extra.png', police_swd)

#Load census data
source('code/census_data.R')
yr <- read_csv('data/census_data/census_data_metadata.csv') |> 
  filter(status == "current") |> 
  pull(year)
read_census_data(yr)

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

#SWD block groups
#TODO: Adjust label of minimum population region to be outside of map 
total_citywide_count <- raleigh_long |> filter(Group == 'Total') |> pull(Count) 
total_max_min_populations_citywide <- raleigh_long |> 
  filter(
    Group == "Total",
    Count %in% c(
      max(total_citywide_count), 
      min(total_citywide_count[total_citywide_count > 5])
      )
    )

population_all_citywide <- ggplot() + 
  geom_sf(
    data = raleigh_long |> 
      filter(Group == "Total"), 
    aes(geometry = geometry, fill = Count), 
    alpha = .65
    ) + 
  geom_sf(
    data = total_max_min_populations_citywide, 
    aes(geometry = geometry), 
    fill = NA, 
    color = "black", 
    linewidth = .75
    ) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  geom_sf_label(data = total_max_min_populations_citywide, aes(label = Count)) + 
  theme_void() +
  labs(
    title = paste0(
      "Raleigh's census neighborhoods range\nfrom ", 
      max(total_citywide_count),
      " to ", 
      min(total_citywide_count[total_citywide_count > 5]),
      " residents."),
    subtitle = "Census neighborhoods are unevenly populated.",
    fill = "Population")

total_swd_count <- raleigh_swd_long |> filter(Group == 'Total') |> pull(Count) 
total_max_min_populations <- raleigh_swd_long |> 
  filter(
    Group == "Total",
    Count %in% c(
      max(total_swd_count), 
      min(total_swd_count)
    )
  )

population_all <- ggplot() + 
  geom_sf(
    data = raleigh_swd_long |> 
      filter(Group == "Total"), 
    aes(geometry = geometry, fill = Count), 
    alpha = .65
  ) + 
  geom_sf(
    data = total_max_min_populations, 
    aes(geometry = geometry), 
    fill = NA, 
    color = "black", 
    linewidth = .75
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  geom_sf_label(data = total_max_min_populations, aes(label = Count)) + 
  theme_void() +
  labs(
    title = paste0(
      "Southwest Raleigh's census neighborhoods range\nfrom ", 
      max(total_swd_count),
      " to ", 
      min(total_swd_count),
      " residents."),
    subtitle = "Census neighborhoods are unevenly populated.",
    fill = "Population")

#Save maps
ggsave('plots/swd_raleigh/swd_bg_totalpop_2.png', population_all)
ggsave('plots/swd_raleigh/bg_totalpop_2_alt.png', population_all_citywide)

#Black non-Hispanic plot
blacknh_swd_count <- raleigh_swd_long |> filter(Group == 'B_nH') |> pull(Count) 
blacknh_max_min_populations <- raleigh_swd_long |> 
  filter(
    Group == "B_nH", 
    Count %in% c(
      max(blacknh_swd_count), 
      min(blacknh_swd_count)
      )
    ) |> 
  group_by(Count) |> 
  slice(1)

population_blacknH <- ggplot() + 
  geom_sf(
    data = raleigh_swd_long |> 
      filter(Group %in% c('B_nH')), 
    aes(geometry = geometry, fill = Count), alpha = .65
    ) +
  geom_sf(
    data = blacknh_max_min_populations, 
    aes(geometry = geometry), 
    fill = NA, 
    color = "black", 
    linewidth = .75
    ) +
  scale_fill_distiller(palette = "Purples", direction = 1) + 
  geom_sf_label(data = blacknh_max_min_populations, aes(label = Count)) + 
  theme_void() +
  labs(
    title = paste0(
      "SW Raleigh's Black Non-Hispanic census neighborhood\npopulations range from ", 
      max(blacknh_swd_count),
      " to ",
      min(blacknh_swd_count),
      "."), 
    fill = "Population")

#Save
ggsave('plots/swd_raleigh/swd_bg_blacknhpop_extra.png', population_blacknH)

#Start ethnicity recalculations
raleigh_tbl <- raleigh_tbl |> 
  mutate(bg_full_area = st_area(geometry)) 
police_raleigh <- police_raleigh |> 
  mutate(policedist_full_area = st_area(geometry)) |>
  st_make_valid(police_swd_raleigh) #Need this line, otherwise police_charlotte fails st_is_valid()
#Check with: st_is_valid(police_swd_raleigh), st_is_valid(raleigh_swd_tbl)

ral_intersection_sf = raleigh_tbl |> st_intersection(police_raleigh) #Areas of intersection in geometry column
ral_intersection_sf <- ral_intersection_sf|> 
  mutate(intersection_area = st_area(geometry),
         bg_perc_area = drop_units(intersection_area / bg_full_area),
         police_perc_area = drop_units(intersection_area / policedist_full_area)) |>
  mutate(across(matches("Total|nH|Hispanic"), ~ .x * bg_perc_area)) #Recalculate ethnicity columns to be intersection-specific

raleigh_swd_tbl <- raleigh_swd_tbl |> mutate(bg_full_area = st_area(geometry)) 
swd_ral_intersection_sf <- ral_intersection_sf |> filter(DISTRICT == "SWD")
police_swd_raleigh <- police_raleigh |> filter(DISTRICT == "SWD")

#Police and census block overlay 
#TODO: clean-up, currently messy and overcrowded
annotated_bg <- raleigh_swd_tbl |> 
  filter(GEOID %in% c(371830516003, 371830530102)) #manually chosen
all_bgs_with_overlap <- 
  st_drop_geometry(swd_ral_intersection_sf) |>
  right_join(raleigh_swd_tbl, by = join_by(GEOID == GEOID))

#Census block SW Raleigh police district intersections 
total_bg_swd <- nrow(all_bgs_with_overlap)
fully_included_bg_swd <- nrow(all_bgs_with_overlap |> filter(round(bg_perc_area, 2) == 1))
bg_dist_intersect <- ggplot() + 
  geom_sf(
    data = all_bgs_with_overlap, 
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
    data = all_bgs_with_overlap, 
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
    data = all_bgs_with_overlap, 
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
police_final <- ral_intersection_sf |> 
  group_by(DISTRICT) |>
  summarize(across(matches("Total|nH|Hispanic"), ~ sum(.x))) |> #Counts by ethnic group
  mutate(across(matches("nH|Hispanic"), ~ .x/Total, .names = "{.col}_perc")) |> #Percentages (of district) by ethnic group
  #Round counts and percentages post-calculation so percentages are not calculated with rounded numerators
  mutate(across(matches("nH|Hispanic"), ~ round(.x, 3)))

#Full police district demographics overlap map
police_final_swd <- police_final |> filter(DISTRICT == "SWD")
swd_bg_full_dist <- ggplot() + 
  geom_sf(
    data = all_bgs_with_overlap, 
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
all_dist_blacknh_perc <- ggplot() + 
  geom_sf(
    data = police_final, 
    aes(
      geometry = geometry, 
      fill = as.vector(B_nH_perc), 
      )
    ) +
  geom_sf_label(
    data = police_final, 
    aes(label = paste0(as.vector(B_nH_perc)*100, "%")), 
    fill = "white") +
  scale_fill_distiller(
    palette = "Purples", 
    direction = 1, 
    labels = scales::percent) + 
  theme_void() + 
  labs(
    title = "The Southwest Raleigh police district has the second lowest\nproportion of Black residents in the city.", 
    subtitle  = paste0(
      "The Northwest Raleigh district has the lowest, with a ",
      police_final |> filter(DISTRICT == "NWD") |> pull(B_nH_perc) * 100,
      "% Black population."),
    fill = "Percent of District\n(Black, non-Hispanic)")
ggsave('plots/swd_raleigh/all_dist_blacknh_perc_6.png', all_dist_blacknh_perc)


