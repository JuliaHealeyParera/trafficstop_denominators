#Initialize current police districts
current_policedistricts <- st_read(here("data", "currpolicedist_files", "current_policedistricts.shp"))

#Function for appending new shp file to running database, if needed
append_shp <- function(file_name, city_name, district_var, geometry_var) {
  temp_sf <- st_read(file_name) |>
    st_transform(2264) |> 
    select(district_var, geometry_var) |>
    rename(
      geometry = geometry_var,
      DISTRICT = district_var
      ) |>
    mutate(city = city_name)
  
  updated_policedist <- rbind(current_policedistricts, temp_sf)
  st_write(updated_policedist, 'data/currpolicedist_files/current_policedistricts.shp') #Update files
  current_policedistricts <- updated_policedist #Update environment object 
}