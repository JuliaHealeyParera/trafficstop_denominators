library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE) # Cache shapefile
library(sf)
library(janitor)
library(units)
nc_state_plane_crs = 2264 # NC state plane NAD83 in feet

# TODO Quickly pre-process map files Here

# Ideas
# - data / maps / FOLDERNAME
# - output / FOLDERNAME
# Set up processing plan ####
map_dir = "data/maps/maps to model/"
process_plan_tbl = tibble(file_to_read = list.files(map_dir, recursive = T, pattern = "shp$|geojson$", full.names = T)) |> 
  mutate(base_file_name = file_to_read |> basename()) |> 
  mutate(folder_name = file_to_read |> str_remove_all(map_dir) |> str_remove_all(base_file_name) |> str_remove_all("[:punct:]"))
process_plan_tbl

# GET NC COUNTY GEOGRAPHIES ####
# TODO make this a DL ONLY IF it doesn't exist as an rdata, so it works w/o internet
# load_variables(2022, "acs5")
nc_county_sf = get_acs("county", variables = "B01001A_001", year = 2022, state = "NC", geometry = T)
nc_county_sf = nc_county_sf |> 
  mutate(ind_place_type = "County", ind_place = NAME |> str_remove_all(" County, North Carolina")) |> 
  select(ind_place_type, ind_place, geometry)
nc_county_sf = nc_county_sf |> st_transform(nc_state_plane_crs)
nc_county_sf |> st_simplify(dTolerance = 5280*.1, preserveTopology = T) |> 
  st_geometry() |> plot()
nc_county_sf = nc_county_sf |> st_simplify(dTolerance = 5280*.1, preserveTopology = T)
nc_county_sf |> st_write("outputs/nc_county_sf.geojson", delete_dsn = T)
# nc_county_sf; nc_county_sf |> st_geometry() |> plot()

# Do with just one, initially. Then ALL polygons, big (regions?) and small?!
# nc_senate_sf = st_read("data/SL 2023-146 Senate - Shapefile/SL 2023-146.shp") |> clean_names()
# nc_senate_sf = nc_senate_sf |> st_transform(nc_county_sf |> st_crs())
# nc_senate_sf = nc_senate_sf |> st_buffer(0) # Degenerate polygons with zero lengths.... FIX
# nc_senate_sf = nc_senate_sf |> 
#   mutate(ind_place_type = "2023 Senate Districts", ind_place = "Sen D" |> paste(district)) |> 
#   select(ind_place_type, ind_place)
# nc_senate_sf |> st_geometry() |> plot()
# nc_senate_sf


# READ CUSTOM NC POLYS ####
read_custom_map = function(this_file_name, this_folder_name, target_crs = 2264){
  # create a regex for what field to read.
  this_sf = st_read(this_file_name) |> 
    clean_names() |> 
    st_transform(target_crs) |> 
    st_buffer(0) |> # Degenerate polygons with zero lengths.... FIX
    st_simplify(dTolerance = 5280*.1, preserveTopology = T) # remove fractaling
  # TODO only if ind_place_type and ind_place don't exist, use folder name to assign place type.
  this_sf = this_sf |> mutate(ind_place_type = this_folder_name)
  
  # Assign place name
  if(("district" %in% names(this_sf)) & str_detect(this_folder_name, "Senate")){
    this_sf = this_sf |> mutate(ind_place = paste0("NC Senate ", district))
  } else if(("district" %in% names(this_sf)) & str_detect(this_folder_name, "House")){
    this_sf = this_sf |> mutate(ind_place = paste0("NC House ", district))
  } else {
    this_sf = this_sf |> mutate(ind_place = paste("Place", n()))
  }
  return(this_sf)
}

process_plan_tbl = process_plan_tbl |> 
  mutate(custom_map = map2(file_to_read, folder_name, read_custom_map))
process_plan_tbl$custom_map

# CALC SPATIAL INTERSECTIONS ####
## Spatial intersection & part-areas #### 
get_spatial_intersections = function(new_sf, ref_sf){
  ref_sf$ref_area = st_area(ref_sf$geometry) |> set_units("miles^2") # calculated repeatedly, NBD
  this_spatial_intersections_sf = ref_sf |> 
    st_intersection(new_sf |> rename_with(\(x)x |> str_replace_all("^ind_", "new_"))) |> 
    mutate(part_area = geometry |> st_area() |> set_units("miles^2")) |>
    # group_by(new_place) |> 
    mutate(pct_ref_area = drop_units(part_area / ref_area * 100)) |> # probably round this
    filter(pct_ref_area >= 1) |> # All fragments greater than 0.1%
    arrange(new_place, desc(pct_ref_area))
  return(this_spatial_intersections_sf)
}
# TODO remove fragments by part_area - smaller than 0.1%
process_plan_tbl = process_plan_tbl |> 
  mutate(spatial_intersections_sf = map(custom_map, get_spatial_intersections, ref_sf = nc_county_sf))
process_plan_tbl$spatial_intersections_sf[[2]]


## Plot and save maps ####
# TODO Convert to multi-map approach
ggplot()+
  geom_sf(data = nc_county_sf, color = "grey", size = 3)+ #line size
  geom_sf(data = process_plan_tbl$spatial_intersections_sf[[2]], color = "grey", 
          aes(fill = new_place), alpha = 0.2)+
  geom_sf_text(data = process_plan_tbl$spatial_intersections_sf[[2]], size = 2,
               aes(label = paste0(round(pct_ref_area, 0), "%")))+
  geom_sf(data = process_plan_tbl$custom_map[[2]], color = "black", linewidth = 1, fill = NA)+
  geom_sf_label(data = process_plan_tbl$custom_map[[2]], size = 3,
                aes(label = ind_place))+
  labs(title = "County area to senate district transformer", 
       subtitle = "X% represents the % of the starting area (county) assigned to the target area (2023 senate district)")+
  guides(fill = "none")+
  theme_void()
TODO walk this, maybe map this into the table. need intersection sf and original map and name
# TODO walk this, maybe map this into the table. need intersection sf and original map and name
# TODO I could also caption this meaningfully with n in the 

# 375 intersections between counties and NC senate districts. 
# Example: Sen District 1 is intersects 16 counties, with 4 counties making up 50% of the area.
# spatial_intersections_sf |> filter(new_place |> str_detect(" 1$"))
#  TODO map this. Markdown every process to document.

## Reduce to multiplier table ####
reduce_spatial_intersection_to_multiplier_tbl = function(this_sf){
  this_sf |> st_set_geometry(NULL) |> ungroup() |> 
    select(new_place_type, new_place, pct_ref_area, ind_place_type, ind_place )
}
process_plan_tbl = process_plan_tbl |> 
  mutate(spatial_intersections_tbl = map(spatial_intersections_sf, reduce_spatial_intersection_to_multiplier_tbl))
spatial_intersections_tbl = process_plan_tbl |> 
  select(spatial_intersections_tbl) |> 
  unnest(spatial_intersections_tbl)

# READ METRIC DATA ####
ind_data_tbl = read_csv("data/stacked_data_overall_yearly_only.csv")
ind_data_tbl |> count(ind_place_type, ind_place)

# CALCULATE NEW METRIC RATERS ####
## Left join intersection areas to county metric data ####
pre_calc_tbl = ind_data_tbl |> 
  filter(ind_place_type == "County") |> 
  left_join(spatial_intersections_tbl, relationship = "many-to-many") # Many to many join complaint. Revisit
pre_calc_tbl

# Quick debug
# pre_calc_tbl |> count(ind_id) |> filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD"))
# pre_calc_tbl |> filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD", new_place)) |> 

paste_if_not_empty = function(...){
  paste(na.omit(...), collapse = "; ")
} # c(NA, NA, 4, 5) |> paste_if_not_empty()

## (Area) weighted average ####
new_region_tbl = pre_calc_tbl |> 
  select(-ind_place, -ind_place_type) |> 
  rename(ind_place = new_place, ind_place_type = new_place_type) |> 
  select(ind_place_type, ind_place, everything()) |> 
  arrange(ind_place_type, ind_place, ind_date_anchor, ind_date_span, ind_id) |> 
  mutate(across(c(ind_num, ind_denom, ind_rate), \(x) {x * pct_ref_area / 100})) |> 
  group_by(ind_id, ind_place_type, ind_place, ind_date_anchor, ind_date_span, ind_group) |> 
  summarize(across(c(ind_num, ind_denom, ind_rate), sum),
            ind_obs_note = ind_obs_note |> paste_if_not_empty()) |> 
  ungroup()
new_region_tbl
# TODO Could be done with populations.

new_region_tbl |> write_csv("outputs/region_ind_tbl.csv")

new_region_tbl |> count(ind_id) |> filter(ind_id |> str_detect("DRUG"))
plot_tbl = new_region_tbl |> 
  filter(ind_place_type |> str_detect("Senate")) |> 
  filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD|DEATH_ALCOHOL_OVERDOSE|DEATH_ANY_MED_DRUG"))
ggplot(plot_tbl)+
  geom_smooth(aes(ind_date_anchor, ind_rate, color = ind_place), linewidth = 0.5, se = F)+
  geom_text(data = plot_tbl |> group_by(ind_id) |> filter(ind_date_anchor == max(ind_date_anchor)), 
            aes(ind_date_anchor, ind_rate, color = ind_place, label = ind_place))+
  facet_grid(ind_id ~ ind_place_type, scales = "free_y") +
  guides(color="none",linewidth = "none")+
  scale_x_date(limits = c(ymd(NA), ymd("2026/12/31")))+
  theme_light()
ggsave("outputs/example_graph.png", height = 20, width = 10)
# TODO Size of the metric file? 
# TODO generalize to reading the polygons (place_name, place_type) in a folder recursively
# TODO Separate vis for Counties besides region?
# TODO informatics meeting to discuss
# TODO Markdown some of this, w examples

# One off example
pre_calc_tbl |> filter(ind_id |> str_detect("ED_SYND_MEDDRUG_OD")) |> filter(ind_place == "Wake")

#TODO unnest / bind_rows the region data into a new shapefile set 
combo_sf = process_plan_tbl |> 
  select(custom_map) |> # isolate...
  unnest(custom_map) |> # ...and unnest
  st_as_sf() # the unnest returns a "naive" tbl, but it has everything we need to cast as sf!

ggplot(combo_sf)+geom_sf(data = combo_sf)+facet_wrap(~ind_place_type) #YES

combo_sf |> st_write("outputs/nc_regions.geojson", delete_dsn = T)
