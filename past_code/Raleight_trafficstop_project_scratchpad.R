# Calculating Raleigh Police District Populations from Census Data
# Mike Dolan Fliss, last updated July 2024

library(tidyverse)
library(tidycensus)
library(sf)
library(gt)
library(gtsummary)
library(units)

# Load and check variables
load_variables(year = 2022, dataset = "acs5") |> count(geography)
acs_vars = load_variables(year = 2022, dataset = "acs5")
acs_topics = acs_vars |> count(concept, geography)
acs_topics |> filter(concept |> str_detect("Black")) |> print(n=Inf)
# Hispanic or Latino Origin by Race 
acs_vars |> filter(concept == "Hispanic or Latino Origin by Race") # B03002_001 
# Poverty Status in the Past 12 Months by Age
acs_vars |> filter(concept == "Poverty Status in the Past 12 Months by Age") # B03002_001 
#  Means of Transportation to Work (White Alone, Not Hispanic or Latino)    
acs_vars |> filter(concept == "Means of Transportation to Work (White Alone, Not Hispanic or Latino)") # B08105H_001 

acs_vars |> filter(concept |> str_detect("Race")) |> filter(concept |> str_detect("Eth"))

# Setup variable table
acs_var_tbl = tribble(
  ~group, ~variable, 
  "Total", "B03002_001",
  "White_nH", "B03002_003",
  "Black_nH", "B03002_004",
  "Am.Ind._nH", "B03002_005",
  "Asian_nH", "B03002_006"
)

# SPATIAL OBJECTS ####
# Download spatial polygons and populations from US Census API
## Counties ####
nc_county_sf = get_acs("county", year = 2022, variables = acs_var_tbl$variable[[1]], geometry = T, state = "NC")
acs_data_tbl = get_acs("county", year = 2022, variables = acs_var_tbl$variable, state = "NC") |> 
  left_join(acs_var_tbl) |> select(GEOID, group, estimate) |> pivot_wider(names_from = group, values_from = estimate)
nc_county_sf = nc_county_sf |> select(GEOID, NAME) |> left_join(acs_data_tbl)
nc_county_sf |> st_geometry() |> plot()

# Cities ####
nc_city_sf = get_acs("place", year = 2023, variables = acs_var_tbl$variable, geometry = T, state = "NC")
acs_data_tbl = get_acs("place", year = 2022, variables = acs_var_tbl$variable, state = "NC") |> 
  left_join(acs_var_tbl) |> select(GEOID, group, estimate) |> pivot_wider(names_from = group, values_from = estimate)
nc_city_sf = nc_city_sf |> slice_max(order_by = estimate, n = 20)
nc_city_sf = nc_city_sf |> select(GEOID, NAME) |> left_join(acs_data_tbl)

nc_county_sf |> st_geometry() |> plot()
nc_city_sf |> st_geometry() |> plot(add=T)

# Tracts ####
nc_tract_sf = get_acs("tract", variables = c("B03002_001"), geometry = T, state = "NC", year = 2020)
acs_data_tbl = get_acs("tract", year = 2022, variables = acs_var_tbl$variable, state = "NC") |> 
  left_join(acs_var_tbl) |> select(GEOID, group, estimate) |> pivot_wider(names_from = group, values_from = estimate)
nc_tract_sf = nc_tract_sf |> slice_max(order_by = estimate, n = 20)
nc_tract_sf = nc_tract_sf |> select(GEOID, NAME) |> left_join(acs_data_tbl)
nc_tract_sf

wake_tracts_sf = nc_tract_sf |> filter(NAME |> str_detect("Wake County"))
wake_tracts_sf

# BGs ####
nc_bg_sf = get_acs("block group", variables = c("B03002_001"), geometry = T, state = "NC", year = 2020)
acs_data_tbl = get_acs("block group", year = 2022, variables = acs_var_tbl$variable, state = "NC") |> 
  left_join(acs_var_tbl) |> select(GEOID, group, estimate) |> pivot_wider(names_from = group, values_from = estimate)
nc_bg_sf = nc_bg_sf |> select(GEOID, NAME) |> left_join(acs_data_tbl)

wake_bgs_sf = nc_bg_sf |> filter(NAME |> str_detect("Wake County"))
wake_bgs_sf

# Raleigh Objects ####
raleigh_sf = nc_city_sf |> filter(NAME |> str_detect("Raleigh"))
raleigh_tracts_sf = wake_tracts_sf[raleigh_sf,]
raleigh_bg_sf = wake_bgs_sf[raleigh_sf,]

raleigh_pd_districts_sf

# Make quick test map
ggplot()+geom_sf(data = nc_county_sf |> filter(NAME |> str_detect("Wake")))+geom_sf(data = raleigh_sf)

raleigh_pd_districts_sf = st_read("Raleigh_Police_Districts.geojson")
raleigh_pd_districts_sf = raleigh_pd_districts_sf |> st_transform(nc_city_sf |> st_crs())
raleigh_pd_districts_sf = raleigh_pd_districts_sf |> st_make_valid()

raleigh_simple_sf = raleigh_sf |> st_simplify(dTolerance = 50, preserveTopology = T)

# Make and save map
ggplot()+
  geom_sf(data = raleigh_pd_districts_sf, color = "grey", aes(fill = DISTRICT))+
  # geom_sf(data = raleigh_tracts_sf, color = "blue", fill = NA)+
  geom_sf(data = raleigh_sf, color = "black", fill = NA, alpha = 0.5)+
  # geom_sf(data = raleigh_simple_sf, linewidth = 1, fill = NA)+
  theme_void()+
  # theme(plot.background = element_rect(fill = 'white'))+
  labs(title = "Raleigh Police Department Districts over Census Block Groups",
       subtitle = paste0("Police districts, n = ", nrow(raleigh_pd_districts_sf),
                        "; census block groups, n =", nrow(raleigh_bg_sf)))
ggsave("outputs/raleigh police district and block group map.png", width = 6, height = 6)
# ggplot()+
#   geom_sf(data = raleigh_pd_districts_sf[1,], color = "red", aes(fill = DISTRICT))+
#   geom_sf(data = raleigh_tracts_sf, color = "blue", fill = NA)+
#   geom_sf(data = raleigh_bg_sf, color = "green", fill = NA)+
#   # geom_sf(data = raleigh_simple_sf, linewidth = 1, fill = NA)+
#   theme_void()

# Population Map ####
raleigh_bg_sf |> pull(Total) |> range()

raleigh_bg_sf_long = raleigh_bg_sf  |> 
  pivot_longer(cols = c(acs_var_tbl$group), names_to = "Population Group", values_to = "Estimate") |> 
  left_join(raleigh_bg_sf |> select(Total_pop = Total, GEOID) |> st_set_geometry(NULL)) |> 
  mutate(pct = Estimate / Total_pop * 100)

ggplot()+
  geom_sf(data = raleigh_bg_sf, aes(fill = Total), color = "white")+
  scale_fill_distiller(type = "seq", direction = 1)+
  theme_void()+
  labs(title = "Raleigh Population by Census Block Group",
       caption = "Population data from American Communities Survey, 2018-2022")
ggsave("outputs/Raleigh Population by Census Block Group.png", width = 6, height = 6)

ggplot()+
  geom_sf(data = raleigh_bg_sf, aes(fill = Total), color = "white")+
  geom_sf(data = raleigh_pd_districts_sf, fill = NA, color = "black")+
  geom_sf_label(data = raleigh_pd_districts_sf, aes(label = DISTRICT))+
  scale_fill_distiller(type = "seq", direction = 1)+
  theme_void()+
  labs(title = "Raleigh Population by Census Block Group w RPD Districts",
       caption = "Population data from American Communities Survey, 2018-2022")
ggsave("outputs/Raleigh Population by Census Block Group w RPD Districts.png", width = 6, height = 6)

# Raleigh population ####
raleigh_sf |> 
  mutate(WhitenH_pct = White_nH/Total*100,
         Black_pct = Black_nH /Total*100,
         AmInd_pct = Am.Ind._nH/Total*100)


## Map: Raleigh BG by Black nH ####
ggplot()+
  geom_sf(data = raleigh_bg_sf |> mutate(`% Black` = Black_nH / Total * 100), 
          aes(fill = `% Black`), color = "white")+
  geom_sf(data = raleigh_pd_districts_sf, fill = NA, color = "black")+
  geom_sf_label(data = raleigh_pd_districts_sf, aes(label = DISTRICT), size = 2)+
  scale_fill_distiller(type = "seq", direction = 1, palette = 2)+
  theme_void()+
  labs(title = "Raleigh % Black nH by Census Block Group w RPD Districts",
       caption = "Population data from American Communities Survey, 2018-2022")
ggsave("outputs/Raleigh % Black nH by Census Block Group w RPD Districts.png", width = 6, height = 6)


ggplot(raleigh_bg_sf |> mutate(`% Black` = Black_nH / Total * 100))+
  geom_histogram(aes(y = `% Black`, fill = `% Black`))+
  coord_flip()+
  # scale_fill_distiller(type = "seq", direction = 1, palette = 2)+
  labs(x = "# of Block Groups",
       title = "Distribution of Raleigh Block Groups by % Black nH")+
  theme_minimal()
ggsave("outputs/Raleigh % Black nH by Census Block Group Histogram.png", width = 6, height = 3)
  
  
nc_city_sf |> st_geometry() |> filter(NAME |> str_detect("Raleigh")) |> plot()
raleigh_pd_districts_sf |> st_geometry() |> plot()

nc_city_sf

# Calculate % overlap ####
raleigh_bg_sf$bg_area_m2 = st_area(raleigh_bg_sf)
raleigh_pd_districts_sf$rpd_area_m2 = st_area(raleigh_pd_districts_sf)
raleigh_pd_districts_sf |> arrange(desc(DISTRICT))

rpd_intersection_sf = raleigh_bg_sf |> st_intersection(raleigh_pd_districts_sf)
rpd_intersection_sf |> count(DISTRICT, ZONEID, name = "# of Block Group Intersections") |> st_set_geometry(NULL) |> gt()
rpd_intersection_sf = rpd_intersection_sf |> 
  mutate(sub_area = st_area(rpd_intersection_sf)) |> 
  mutate(pct_bg = sub_area / bg_area_m2 * 100) |> 
  mutate(across(matches("Total|nH"), ~ .x * pct_bg / 100)) |> 
  mutate(pct_bg_txt = round(pct_bg, 0) |> as.character()) |> 
  mutate(pct_bg = pct_bg |> drop_units())

ggplot()+
  geom_sf(data = raleigh_bg_sf, fill = NA, color = "white", linewidth = 1)+
  geom_sf(data = rpd_intersection_sf, aes(fill = pct_bg), color = "grey", linewidth = 0.2)+
  geom_sf(data = raleigh_pd_districts_sf, fill = NA, color = "black", linewidth = 1.5)+
  geom_sf_text(data = rpd_intersection_sf, aes(label = pct_bg_txt), size = 1)+
  geom_sf_text(data = raleigh_pd_districts_sf, aes(label = DISTRICT), size = 5, color = "white", alpha = 0.5)+
  scale_fill_distiller(type = "seq", direction = 1, palette = 1)+
  theme_void()+
  labs(fill = "% Area Overlap",
       title = "% Area Overlap",
       caption = "% area overlaps between census block groups and RPD areas\ndata from American Communities Survey, 2018-2022")
ggsave("outputs/Raleigh RPD percent area overlaps with NC census block groups.png", width = 12, height = 12)

rpd_aggregation_sf = rpd_intersection_sf |> 
  group_by(DISTRICT, ZONEID) |> 
  summarize(across(matches("Total|nH"), sum)) |> 
  mutate(pct_Black = Black_nH/Total*100) |> 
  mutate(across(matches("Total|nH|pct"), units::drop_units)) |> 
  mutate(label = paste0(DISTRICT, "\n", ZONEID))

rpd_intersection_sf
rpd_intersection_sf |> st_geometry() |> plot()

rpd_pop_gt = rpd_intersection_sf |> 
  arrange(GEOID, DISTRICT, ZONEID) |> 
  mutate(across(where(is.numeric), ~round(.x, 0))) |> st_set_geometry(NULL) |> gt()
rpd_pop_gt
rpd_pop_gt |> gtsave("outputs/RPD District Demographics.html")
sum(rpd_intersection_sf$Total)

raleigh_bg_pop_gt = raleigh_bg_sf |> 
  arrange(GEOID) |> 
  mutate(across(where(is.numeric), ~round(.x, 0))) |> st_set_geometry(NULL) |> gt()
raleigh_bg_pop_gt
raleigh_bg_pop_gt |> gtsave("outputs/Block Group Demographics.html")

ggplot()+
  geom_sf(data = rpd_aggregation_sf, aes(fill = Total), color = "white")+
  geom_sf_label(data = rpd_aggregation_sf, aes(label = label), size = 2)+
  scale_fill_distiller(type = "seq", direction = 1, palette = 1)+
  theme_void()+
  labs(fill = "Estimated Population",
       title = "Raleigh Total Population by RPD Districts",
       caption = "Population estimated using census block groups \ndata from American Communities Survey, 2018-2022")
ggsave("outputs/Raleigh Total Population by RPD Districts.png", width = 6, height = 6)

ggplot()+
  geom_sf(data = rpd_aggregation_sf, aes(fill = pct_Black), color = "white")+
  geom_sf_label(data = rpd_aggregation_sf, aes(label = label), size = 2)+
  scale_fill_distiller(type = "seq", direction = 1, palette = 2)+
  theme_void()+
  labs(fill = "Estimated % Black",
       title = "Raleigh % Black nH by RPD Districts",
       caption = "Population estimated using census block groups \ndata from American Communities Survey, 2018-2022")
ggsave("outputs/Raleigh % Black nH by RPD Districts.png", width = 6, height = 6)



# rpd_intersection_index = raleigh_pd_districts_sf |> st_intersects(raleigh_pd_districts_sf) # |> tibble() |>  set_names("bg_list")
# rpd_intersection_sf = raleigh_pd_districts_sf |> mutate(bg_list = map(rpd_intersection_list, ~.x))
# rpd_bg_intersection_sf = st_intersection(raleigh_bg_sf, raleigh_pd_districts_sf)


# poly_grid_tbl = expand_grid(rpd_id = raleigh_pd_districts_sf |> pull(ZONEID), 
#                             bg_id = raleigh_bg_sf |> pull(GEOID))
# poly_grid_tbl = poly_grid_tbl |> 
#   left_join(raleigh_bg_sf |> select(bg_id = GEOID, bg_area_m2 = area_m2) |> st_set_geometry(NULL)) |> 
#   left_join(raleigh_pd_districts_sf |> select(rpd_id = ZONEID, rpd_area_m2 = area_m2) |> st_set_geometry(NULL)) |> 
#   mutate(shared_area_m2  = map2_dbl(bg_id, rpd_id, ~ st_area(raleigh_bg_sf |> filter(bg_id == .x),
#                                                              raleigh_pd_districts_sf |> filter(ZONEID == .y))))
# 




# st_intersection(raleigh_bg_sf[1,], raleigh_pd_districts_sf[1,]) |> st_area()
# 
# get_pct_overlap = function(sf1, sf2, verbose = T){
#   # return intersecting polygon of sf 1 and 2, with numeric vars from sf2 divided by fraction of sf2 touching sf1
#   area_sf1 = sf1 |> st_area()
#   area_sf2 = sf2 |> st_area()
#   intersection_sf = sf1 |> st_intersection(sf2)
#   area_intersection = intersection_sf |> st_area()
#   pct_multiplier = area_intersection / area_sf2 * 100
#   return(pct_multiplier)
# }
# 
# raleigh_intersection_sf = raleigh_intersection_sf |> 
#   mutate(pct_multiplier = map2_dbl(GEOID, DISTRICT, 
#                                    ~ get_pct_overlap(raleigh_bg_sf |> filter(GEOID == .x),
#                                                      raleigh_pd_districts_sf |> filter(DISTRICT == .y))))
# 
# 
# raleigh_intersection_list
# get_fractional_geometry(raleigh_pd_districts_sf[1,], raleigh_bg_sf[3,]) # bg1 intersects with pd 7, 13, 15
# 
# hold = map(raleigh_intersection_list[[1]], ~ get_fractional_geometry(raleigh_pd_districts_sf[1], raleigh_bg_sf[.x,]))
# hold
# 
# raleigh_bg_sf
# 
# 
# 
# # https://gis.stackexchange.com/questions/362466/calculate-percentage-overlap-of-2-sets-of-polygons-in-r
# 
# raleigh_intersection_list