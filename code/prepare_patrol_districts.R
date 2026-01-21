library(tidyverse)
library(sf)
library(patchwork)

patrol_district_tbl_nested = tibble(full_file = list.files(path = "data/patrol_districts/", 
                                                       recursive = T, pattern = "*.geojson", full.names = T)) |> 
  mutate(file_name = full_file |> basename())

# Read all the data
patrol_district_tbl_nested = patrol_district_tbl_nested |> 
  mutate(sf = map(full_file, st_read)) |> 
  mutate(sf = map(sf, st_transform, crs = 2264)) |> 
  mutate(lea_name = map_chr(full_file, str_extract, pattern = "[a-z]+(?= patrol districts)") |> str_to_title()) |> 
  mutate(sf = map2(sf, lea_name, \(s, l){s = s |> mutate(lea_name = l); return(s)}))
patrol_district_tbl_nested

# unnest gets very confused. Bind_rows seems to work



patrol_district_sf = bind_rows(patrol_district_tbl_nested$sf)
patrol_district_sf |> count(lea_name)

ggplot(patrol_district_sf, aes(fill = lea_name))+
  geom_sf()+
  facet_wrap(~lea_name)+
  theme_void()+guides(fill = "none")

get_patrol_plot = function(this_sf, this_lea){
  ggplot(data = this_sf |> filter(lea_name == this_lea))+
    geom_sf()+
    theme_void()+
    labs(title = this_lea)
}
get_patrol_plot(patrol_district_sf, "Charlotte")
g = get_patrol_plot(patrol_district_sf, "Charlotte") + 
  get_patrol_plot(patrol_district_sf, "Durham") + 
  get_patrol_plot(patrol_district_sf, "Raleigh")

