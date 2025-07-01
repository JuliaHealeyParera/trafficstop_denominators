library(tidyverse)
library(tidycensus)
library(sf)
library(units)

#Read in custom shape file
police_raleigh <- st_read('data/currpolicedist_files/current_policedistricts.shp') |>
  filter(city == "raleigh") 
police_swd_raleigh <- police_raleigh |>
  filter(DISTRICT == "SWD")

#Introductory map 1: All Raleigh districts
police_dist <- ggplot(police_raleigh, aes(geometry = geometry, fill = DISTRICT)) + 
  geom_sf() +
  theme_void() + 
  guides(fill = "none") +
  labs(title = "Raleigh has 5 police districts") 
#Introductory map 2: Zoom-in on SWD of Raleigh
police_swd <- ggplot(police_swd_raleigh, aes(geometry = geometry, fill = DISTRICT)) + 
  geom_sf() +
  theme_void() + 
  guides(fill = "none") +
  labs(title = "The Southwest District of Raleigh \nis made up of multiple complicated polygons.",
       subtitle = "Many of these irregularities are due to independently-operated\nuniversity campus police departments.")
#Save maps
ggsave('plots/swd_raleigh/all_raleigh_districts_1.png', police_dist)
ggsave('plots/swd_raleigh/swd_raleigh_district_2.png', police_swd)

#Load census data
source('census_data.R')

#All of Raleigh - census info
raleigh_geography <- nc_sf[nc_sf$NAME=="Raleigh",'geometry'][[1]]
#Raleigh SWD ethnicity data (geometry subset)
raleigh_swd_tbl <- acs_data_tbl[lengths(
  st_intersects(
    acs_data_tbl$geometry, 
    st_union(police_swd_raleigh$geometry)
  )) > 0, ]

#Make tidy to be able to facet for ethnic groups
raleigh_swd_long <- raleigh_swd_tbl |>
  pivot_longer(cols = Total:Hispanic, names_to = "Group", values_to = "Count") |>
  mutate(area = drop_units(st_area(geometry)))

#SWD block groups
#TODO: Adjust label of minimum population region to be outside of map 
total_swd_count <- raleigh_swd_long |> filter(Group == 'Total') |> pull(Count) 
total_max_min_populations <- raleigh_swd_long |> filter(Group == "Total" & Count %in% c(max(total_swd_count), min(total_swd_count)))
population_all <- ggplot() + 
  geom_sf(data = raleigh_swd_long |> filter(Group == "Total"), aes(geometry = geometry, fill = Count), alpha = .65) + 
  geom_sf(data = total_max_min_populations, aes(geometry = geometry), fill = NA, color = "black", linewidth = .75) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  geom_sf_label(data = total_max_min_populations, aes(label = Count)) + 
  theme_void() +
  labs(title = "Southwest Raleigh's block groups range\nfrom 5518 to 334 residents.", 
       fill = "Population")
#White non-Hispanic plot
whitenh_swd_count <- raleigh_swd_long |> filter(Group == 'White_nH') |> pull(Count) 
whitenh_max_min_populations <- raleigh_swd_long |> filter(Group == "White_nH" & Count %in% c(max(whitenh_swd_count), min(whitenh_swd_count)))
population_whitenH <- ggplot() + 
  geom_sf(data = raleigh_swd_long |> filter(Group %in% c('White_nH')), aes(geometry = geometry, fill = Count), alpha = .65) +
  geom_sf(data = whitenh_max_min_populataions, aes(geometry = geometry), fill = NA, color = "black", linewidth = .75) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  geom_sf_label(data = whitenh_max_min_populations, aes(label = Count)) + 
  theme_void() +
  labs(title = "SW Raleigh's White Non-Hispanic block group\npopulationsrange from 3838 to 48.", 
       #TODO: Rework this title and subtitle
       subtitle = "Most high-population block groups of all ethnic groups\nare also high-population White Non-Hispanic.",
       fill = "Population")
#Save maps
ggsave('plots/swd_raleigh/swd_bg_totalpop_3.png', population_all)
ggsave('plots/swd_raleigh/swd_bg_whitenhpop_4.png', population_whitenH)

#Start ethnicity recalculations
raleigh_swd_tbl <- raleigh_swd_tbl |> 
  mutate(bg_full_area = st_area(geometry)) 
police_swd_raleigh <- police_swd_raleigh |> 
  mutate(policedist_full_area = st_area(geometry)) |>
  st_make_valid(police_swd_raleigh) #Need this line, otherwise police_charlotte fails st_is_valid()
#Check with: st_is_valid(police_swd_raleigh), st_is_valid(raleigh_swd_tbl)

ral_intersection_sf = raleigh_swd_tbl |> st_intersection(police_swd_raleigh) #Areas of intersection in geometry column
ral_intersection_sf <- ral_intersection_sf|> 
  mutate(intersection_area = st_area(geometry),
         bg_perc_area = drop_units(intersection_area / bg_full_area),
         police_perc_area = drop_units(intersection_area / policedist_full_area)) |>
  mutate(across(matches("Total|nH|Hispanic"), ~ .x * bg_perc_area)) #Recalculate ethnicity columns to be intersection-specific

#Police and census block overlay 
#TODO: clean-up, currently messy and overcrowded
annotated_bg <- raleigh_swd_tbl |> filter(GEOID %in% c(371830516003, 371830530102)) #manually chosen
all_bgs_with_overlap <- st_drop_geometry(ral_intersection_sf) |> right_join(raleigh_swd_tbl, by = join_by(GEOID == GEOID))
police_dist_census_blocks <- ggplot() + 
  geom_sf(data = all_bgs_with_overlap, aes(geometry = geometry, fill = bg_perc_area * 100), alpha = .7) + 
  geom_sf(data = annotated_bg, aes(geometry = geometry), fill = NA, color = "gray30", linewidth = .6) + 
  geom_sf(data = police_swd_raleigh, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .8) + 
  annotate("text", x = 2130000, y = 751000, label = "100% of area\nin district") +
  annotate("segment", x = 2103500, y = 747000, xend = 2123000, yend = 751000) + #could also use st_centroid of obj and extract coord
  annotate("text", x = 2065000, y = 712000, label = "7% of area\nin district") +
  annotate("segment", x = 2073000, y = 712000, xend = 2088000, yend = 712000) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(title = "The SW Raleigh police district contains multiple block groups.",
       subtitle = "Some census block groups are only partially in the district.",
       fill = "% of block\ngroup in district")
ggsave('plots/swd_raleigh/swd_bg_policedist_overlay_5.png', police_dist_census_blocks)

##NEXT: maps with fill = num white * perc area
whitenh_ <- ggplot() + 
  geom_sf(data = all_bgs_with_overlap, aes(geometry = geometry, fill = bg_perc_area * White_nH.x), alpha = .7) + 
  geom_sf(data = annotated_bg, aes(geometry = geometry), fill = NA, color = "gray30", linewidth = .6) + 
  geom_sf(data = police_swd_raleigh, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .8) + 
  annotate("text", x = 2130000, y = 751000, label = "100% of area\nin district") +
  annotate("segment", x = 2103500, y = 747000, xend = 2123000, yend = 751000) + #could also use st_centroid of obj and extract coord
  annotate("text", x = 2065000, y = 712000, label = "7% of area\nin district") +
  annotate("segment", x = 2073000, y = 712000, xend = 2088000, yend = 712000) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(title = "The SW Raleigh police district contains multiple block groups.",
       subtitle = "Some census block groups are only partially in the district.",
       fill = "% of block\ngroup in district")
ggsave('plots/swd_raleigh/swd_bg_policedist_overlay_5.png', police_dist_census_blocks)

#Convert from block group unit to police district unit
policdist_final <- ral_intersection_sf |> 
  group_by(DISTRICT) |>
  summarize(across(matches("Total|nH|Hispanic"), ~ sum(.x))) |> #Counts by ethnic group
  mutate(across(matches("nH|Hispanic"), ~ .x/Total, .names = "{.col}_perc")) |> #Percentages (of district) by ethnic group
  #Round counts and percentages post-calculation so percentages are not calculated with rounded numerators
  mutate(across(matches("nH|Hispanic"), ~ round(.x, 3)))

#Demographic specific choropleth map (by police district)
population_blacknH <- ggplot(policdist_final, aes(geometry = geometry, fill = as.vector(Black_nH))) + 
  geom_sf() +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_void() + 
  labs(title = "Some police districts have many more Black residents than others.", 
       fill = "Population \n(Black, non-Hispanic)")

percent_blacknH <- ggplot(policdist_final, aes(geometry = geometry, fill = as.vector(Black_nH_perc), label = Black_nH_perc)) + 
  geom_sf() +
  geom_sf_label() +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_void() + 
  labs(title = "Some police districts have much higher concentrations \nof Black residents than others.", 
       fill = "Percent of District\n(Black, non-Hispanic)")

ggsave('plots/charlotte/population_blacknH.png', population_blacknH)
ggsave('plots/charlotte/percent_blacknH.png', percent_blacknH)


#TODO: Need to come up with centroid recalculation for labels... or maybe just use leaflet?
#Leaflet wouldn't work in PDF format...would it?
