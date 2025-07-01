library(tidyverse)
library(tidycensus)
# install.packages("tidycensus")
library(sf)
library(janitor)
library(units)

# Ideas
# - data / maps / FOLDERNAME
# - output / FOLDERNAME

# load_variables(2022, "acs5")
options(tigris_use_cache = TRUE) # Cache shapefile
nc_county_sf = get_acs("county", variables = "B01001A_001", year = 2022, state = "NC", geometry = T)
nc_county_sf |> st_geometry() |> plot()
nc_county_sf = nc_county_sf |> 
  mutate(ind_place_type = "County", ind_place = NAME |> str_remove_all(" County, North Carolina")) |> 
  select(ind_place_type, ind_place, geometry) |> 
  mutate(county_area = st_area(geometry) |> set_units("miles^2"))
nc_county_sf

# Do with just one, initially. Then ALL polygons, big (regions?) and small?!
nc_senate_sf = st_read("data/SL 2023-146 Senate - Shapefile/SL 2023-146.shp") |> clean_names()
nc_senate_sf = nc_senate_sf |> st_transform(nc_county_sf |> st_crs())
nc_senate_sf = nc_senate_sf |> st_buffer(0) # Degenerate polygons with zero lengths.... FIX
nc_senate_sf = nc_senate_sf |> 
  mutate(ind_place_type = "2023 Senate Districts", ind_place = "Sen D" |> paste(district)) |> 
  select(ind_place_type, ind_place)
nc_senate_sf |> st_geometry() |> plot()
nc_senate_sf

# nc_house_sf = st_read("data/SL 2023-149 House - Shapefile/SL 2023-149.shp") |> clean_names()
# nc_house_sf = nc_house_sf |> st_transform(nc_county_sf |> st_crs())
# nc_house_sf = nc_house_sf |> st_buffer(0) # Degenerate polygons with zero lengths.... FIX
# nc_house_sf |> st_geometry() |> plot()

# Spatial combination ####
## Spatial intersection & part-areas #### 
spatial_intersections_sf = nc_county_sf |> 
  st_intersection(nc_senate_sf |> rename_with(\(x)x |> str_replace_all("^ind_", "new_"))) |> 
  mutate(part_area = geometry |> st_area() |> set_units("miles^2")) |>
  # group_by(new_place) |> 
  mutate(pct_county_area = part_area / county_area * 100) |> # probably round this
  arrange(new_place, desc(pct_county_area))
spatial_intersections_sf

## Plot and save the map here ####
ggplot()+
  geom_sf(data = county_sf, color = "grey", line_width = 3)+
  geom_sf(data = county_sf, color = "black")+
  geom_sf_text(data = spatial_intersections_sf, aes(label = ))+
  labs(title = "place_type to county map TBD label")

# 375 intersections between counties and NC senate districts. 
# Example: Sen District 1 is intersects 16 counties, with 4 counties making up 50% of the area.
# spatial_intersections_sf |> filter(new_place |> str_detect(" 1$"))
#  TODO map this. Markdown every process to document.

## Reduce to multiplier table ####
spatial_intersections_tbl = spatial_intersections_sf |> 
  st_set_geometry(NULL) |> 
  select(new_place_type, new_place, pct_county_area, ind_place_type, ind_place ) |> 
  ungroup()
spatial_intersections_sf |> filter(ind_place == "Wake")
# TODO 
# Consistent county (placename and placetype) and rando-place 
# Consistent translation placename and placetype

## Read area data ####
ind_data_tbl = read_csv("data/stacked_data_overall_yearly_only.csv")
ind_data_tbl |> count(ind_place_type, ind_place)

## Left join intersection areas to county metric data ####
pre_calc_tbl = ind_data_tbl |> 
  filter(ind_place_type == "County") |> 
  left_join(spatial_intersections_tbl, relationship = "many-to-many") # Many to many join complaint. Revisit

# Quick debug
pre_calc_tbl |> count(ind_id) |> filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD"))
# pre_calc_tbl |> filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD")) |> View()

paste_if_not_empty = function(...){
  paste(na.omit(...), collapse = "; ")
} # c(NA, NA, 4, 5) |> paste_if_not_empty()

## (Area) weighted average ####
new_region_tbl = pre_calc_tbl |> 
  select(-ind_place, -ind_place_type) |> 
  rename(ind_place = new_place, ind_place_type = new_place_type) |> 
  select(ind_place_type, ind_place, everything()) |> 
  arrange(ind_place_type, ind_place, ind_date_anchor, ind_date_span, ind_id) |> 
  mutate(across(c(ind_num, ind_denom, ind_rate), \(x) {x * pct_county_area / 100})) |> 
  group_by(ind_id, ind_place_type, ind_place, ind_date_anchor, ind_date_span, ind_group) |> 
  summarize(across(c(ind_num, ind_denom, ind_rate), sum),
            ind_obs_note = ind_obs_note |>paste_if_not_empty())
new_region_tbl
# TODO recalculate en-masse. Perhaps nested tibble

new_region_tbl |> write_csv("outputs/senate_test_ind_tbl.csv")


# TODO Size of the metric file? 
# TODO generalize to reading the polygons (place_name, place_type) in a folder recursively
# TODO Separate vis for Counties besides region?
# TODO informatics meeting to discuss
# TODO Markdown some of this, w examples

# One off example
pre_calc_tbl |> filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD")) |> filter(ind_place == "Wake")
