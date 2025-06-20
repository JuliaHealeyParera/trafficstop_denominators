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
police_charlotte <- st_read('data/charlotte_files/CMPD_Police_Divisions.shp')
#unique to charlotte - develop protocol for general shp file cleaning 
#TODO: build in series of stop()s and other requirements
police_charlotte <- police_charlotte |> st_transform(2264) |> select(-c(OBJECTID,DIVISION_C,WNAME))

#TODO:  Determine common themes, customizations, etc for maps
#Police district map 
ggplot(police_charlotte, aes(geometry = geometry, fill = DNAME)) + 
  geom_sf() +
  theme_void() + 
  guides(fill = "none") + 
  labs(title = "Charlotte has 14 police districts")

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
nc_bg_sf = get_acs("block group", year = 2023, variables = "B03002_001", geometry = T, state = "NC") |>
  select(GEOID, NAME) 
#Get specified ethnicity variables per block group
acs_data_tbl = get_acs("block group", year = 2023, variables = acs_var_tbl$variable, state = "NC") |> 
  left_join(acs_var_tbl) |> 
  select(GEOID, group, estimate) |> 
  pivot_wider(names_from = group, values_from = estimate) |> #Pivot so each ethnicity is a var.
  right_join(nc_bg_sf) |> #Bring in block group names
  mutate(Other = Other_nH + Two_race_nH) |> #Combine two smaller categories
  select(-c(Other_nH, Two_race_nH))
st_geometry(acs_data_tbl) <- acs_data_tbl$geometry #Assert that geometry is interpreted as sf object
acs_data_tbl <- st_transform(acs_data_tbl, 2264) #Convert to state plane

#Hard-downloaded because of issues with census, replace later with API call
nc_sf <- st_read('data/city_census_files/cb_2022_37_place_500k.shp') |>
  select(GEOID, NAME, NAMELSAD, geometry) |>
  st_transform(2264)
#Get geometry unit of relevant city -- PREVIOUSLY USED FOR st_intersects, but replaced 
 #with alternative method below because police districts sometimes extend beyond "place" designation
charlotte_geography <- nc_sf[nc_sf$NAME=="Charlotte",'geometry'][[1]]
#Get subset of all state ethnicity data for relevant city
charlotte_data_tbl <- acs_data_tbl[lengths(
  st_intersects(
    acs_data_tbl$geometry, 
    st_union(charlotte$geometry)
  )) > 0, ]

#Make tidy to be able to facet for ethnic groups
charlotte_data_longer <- charlotte_data_tbl |>
  pivot_longer(cols = Total:Hispanic, names_to = "Group", values_to = "Count")
#Quick population plot -- census block groups overlaid with police districts 
#Will follow best practice visualization standards later on 
ggplot() + 
  geom_sf(data = charlotte_data_longer |> filter(Group == "Total"), aes(geometry = geometry, fill = Count), alpha = .65) + 
  geom_sf(data = charlotte, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .8) + 
  theme_void()
#Faceted by ethnicity -- think about a better way to do this. Make maps separately? Only display Hispanic, White, and Black?
ggplot() + 
  geom_sf(data = charlotte_data_longer |> filter(Group != "Total"), aes(geometry = geometry, fill = Count), alpha = .65) + 
  geom_sf(data = charlotte, aes(geometry = geometry), fill = NA, color = 'black', linewidth = .4) + 
  facet_wrap(~Group) +
  theme_void()

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
ggplot(policdist_final, aes(geometry = geometry, fill = as.vector(Black_nH), label = Black_nH_perc)) + 
  geom_sf() +
  geom_sf_label() +
  scale_fill_distiller(palette = "Blues", direction = -1) + 
  theme_void()
#TODO: Need to come up with centroid recalculation for labels... or maybe just use leaflet?
  #Leaflet wouldn't work in PDF format...would it?
