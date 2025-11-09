library(tidyverse)
library(sf)

#One-time code for INITIAL running shp file: 
charlotte_policedist <- st_read('data/charlotte_files/CMPD_Police_Divisions.shp') |>
  st_transform(2264) |> #just in case, as always
  select(DNAME, geometry) |>
  rename(DISTRICT = DNAME) |>
  mutate(city = "charlotte") 

raleigh_policedist <- st_read('data/raleigh_files/Raleigh_Police_Districts.shp') |>
  st_transform(2264) |>
  count(DISTRICT) |>
  select(-n) |>
  mutate(city = "raleigh")

durham_policedist <- st_read('data/durham_files/Police_Districts.shp') |>
  st_transform(2264) |>
  count(DISTNUM) |>
  select(-n) |>
  rename(DISTRICT = DISTNUM) |>
  mutate(city = "durham")

curr_policedist <- rbind(charlotte_policedist, raleigh_policedist, durham_policedist)
st_write(curr_policedist, 'data/currpolicedist_files/current_policedistricts.shp')


