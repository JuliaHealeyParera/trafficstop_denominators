#Initialize current police districts
current_policedistricts <- st_read(here("data", "currpolicedist_files", "current_policedistricts.shp"))

#Function for appending new shp file to running database, if needed

rename_col <- function(file_obj, city_name, district_var, geometry_var) {
  temp_sf <- file_obj |>
    st_transform(2264) |> 
    select(!!sym(district_var), !!sym(geometry_var)) |>
    rename(
      geometry = !!sym(geometry_var),
      DISTRICT = !!sym(district_var)
    ) |>
    mutate(city = city_name)
  
  return(temp_sf)
}
  
append_shp <- function(file_obj) {
  updated_policedist <- rbind(current_policedistricts, temp_sf)
  st_write(updated_policedist, here("data", "currpolicedist_files", "current_policedistricts.shp"))
  current_policedistricts <- updated_policedist
}
