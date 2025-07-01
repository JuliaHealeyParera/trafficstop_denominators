library(tidyverse)
library(tidycensus)
library(sf)
library(units)

#TODO: rebuild script into various R files, converted into functions for generalizability
#File structure: 
  # 1. Data prep: shape file/police district data (returns df ready for mapping)
    # a. read-in, clean, reformat
  # 2. Data prep: census data (returns df ready for mapping)
    # b. read-in, reformat into one row per block group with relevant variables
  # 3. Data prep: recalculation (returns df ready for mapping and tabular presentation)
    # c. join, recalculate, deal with intricacies
  # 4. Map scripts -- should these just be built into app.R? Cleaner to split apart 
  # 5. ASK MIKE: In order to produce proper PDF download, should visuals + narrative 
    # printed in app be recreated in a Quarto and formatted with Typst?


#Read in custom shape file
police_charlotte <- st_read('data/currpolicedist_files/current_policedistricts.shp') |>
  filter(city == "charlotte")
#TODO: build in series of stop()s and other requirements

#TODO:  Determine common themes, customizations, etc for maps
#Police district map 
police_dist <- ggplot(police_charlotte, aes(geometry = geometry, fill = DNAME)) + 
  geom_sf() +
  theme_void() + 
  guides(fill = "none") + 
  labs(title = "Charlotte has 14 police districts")

ggsave('plots/charlotte/police_dist.png', police_dist)

source('census_data.R')

#Get geometry unit of relevant city -- PREVIOUSLY USED FOR st_intersects, but replaced 
 #with alternative method below because police districts sometimes extend beyond "place" designation
charlotte_geography <- nc_sf[nc_sf$NAME=="Charlotte",'geometry'][[1]]
#Get subset of all state ethnicity data for relevant city
charlotte_data_tbl <- acs_data_tbl[lengths(
  st_intersects(
    acs_data_tbl$geometry, 
    st_union(police_charlotte$geometry)
  )) > 0, ]

#Make tidy to be able to facet for ethnic groups
charlotte_data_longer <- charlotte_data_tbl |>
  pivot_longer(cols = Total:Hispanic, names_to = "Group", values_to = "Count")
#Quick population plot -- census block groups overlaid with police districts 
#Will follow best practice visualization standards later on 
population_all <- ggplot() + 
  geom_sf(data = charlotte_data_longer |> filter(Group == "Total"), aes(geometry = geometry, fill = Count), alpha = .65) + 
  #geom_sf(data = police_charlotte, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .8) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_void() + 
  labs(title = "Census block groups are not equally populated.", 
       fill = "Population")
#Faceted by ethnicity -- think about a better way to do this. Make maps separately? Only display Hispanic, White, and Black?
population_ethnicity <- ggplot() + 
  geom_sf(data = charlotte_data_longer |> filter(Group != "Total"), aes(geometry = geometry, fill = Count), alpha = .65) + 
  #geom_sf(data = police_charlotte, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .4) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  facet_wrap(~Group) +
  theme_void() + 
  labs(title = "Racial groups are concentrated in certain census block groups.", 
       fill = "Population")
#Cleaned up ethnicity plot -- how many racial groups?
population_ethnicity_v2 <- ggplot() + 
  geom_sf(data = charlotte_data_longer |> filter(Group %in% c('White_nH', 'Black_nH')), aes(geometry = geometry, fill = Count), alpha = .65) +
  #geom_sf(data = police_charlotte, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .4) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  facet_wrap(~Group) +
  theme_void() + 
  labs(title = "Racial groups are concentrated in certain census blocks groups.", 
       fill = "Population")

ggsave('plots/charlotte/population_all.png', population_all)
ggsave('plots/charlotte/population_ethnicity.png', population_ethnicity)
ggsave('plots/charlotte/population_ethnicity_v2.png', population_ethnicity_v2)

#Police and census block overlay 
police_dist_census_blocks <- ggplot() + 
  geom_sf(data = charlotte_data_longer |> filter(Group == "Total"), aes(geometry = geometry, fill = Count), alpha = .65) + 
  geom_sf(data = police_charlotte, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .75) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_void() + 
  labs(title = "Police districts contain multiple census block groups.",
       subtitle = "Some census block groups are in multiple police districts.",
       fill = "Population")

ggsave('plots/charlotte/police_district_bg.png', police_dist_census_blocks)

#Start ethnicity recalculations
charlotte_data_tbl <- charlotte_data_tbl |> 
  mutate(bg_full_area = st_area(geometry)) 
police_charlotte <- police_charlotte |> 
  mutate(policedist_full_area = st_area(geometry)) |>
  st_make_valid(police_charlotte) #Need this line, otherwise police_charlotte fails st_is_valid()
                           #Check with: st_is_valid(police_charlotte), st_is_valid(charlotte_data_tbl)

clt_intersection_sf = charlotte_data_tbl |> st_intersection(police_charlotte) #Areas of intersection in geometry column
clt_intersection_sf <- clt_intersection_sf|> 
  mutate(intersection_area = st_area(geometry),
         bg_perc_area = intersection_area / bg_full_area,
         police_perc_area = intersection_area / policedist_full_area) |>
  mutate(across(matches("Total|nH|Hispanic"), ~ .x * bg_perc_area)) #Recalculate ethnicity columns to be intersection-specific
#Highest police_perc_area is for GEOID 371199801001, which is airport district (pop = 0)

#Convert from block group unit to police district unit
policdist_final <- clt_intersection_sf |> 
  group_by(DNAME) |>
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
