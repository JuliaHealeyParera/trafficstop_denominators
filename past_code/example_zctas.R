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
census_data <- load_variables(year = 2023, dataset = "acs5") 
nc_county_sf = get_acs(geography = "county", year = 2023, variables = "B03002_001", geometry = T, state = "NC")
us_zcta_sf = get_acs(geography = "zcta", year = 2020, variables = "B03002_001", geometry = T)
nc_zcta_sf = us_zcta_sf[nc_county_sf, ]
nc_zcta_sf |> st_geometry() |> plot()
ggplot()+
  geom_sf(data = nc_zcta_sf, color = "black")+
  geom_sf(data = nc_county_sf, color = "blue", linewidth = 1, fill = NA)
  
