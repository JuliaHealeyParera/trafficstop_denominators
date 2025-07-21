bg_dist_subset <- function(census_tbl, police_dist) {
  subset <- census_tbl[lengths(
    st_intersects(
      census_tbl$geometry, 
      st_union(police_dist$geometry)
    )) > 0, ]
  
  subset <- subset |>
    mutate(bg_full_area = st_area(geometry)) 
  
  return(subset)
}

pivot_long_tidy <- function(subset_tbl) {
  subset_tbl_long <- subset_tbl |>
    pivot_longer(cols = Total:Hispan, names_to = "Group", values_to = "Count") |>
    mutate(area = drop_units(st_area(geometry)))
  
  return(subset_tbl_long)
}

bgtbl_to_bgsf <- function(bg_tbl, poldist_sf) {
  bg_sf = bg_tbl |> 
    st_intersection(poldist_sf) |> #Areas of intersection in geometry column 
  mutate(
      intersection_area = st_area(geometry),
      bg_perc_area = intersection_area / bg_full_area,
      police_perc_area = intersection_area / policedist_full_area
      ) |>
    mutate(
      across(
        matches("Total|nH|Hispan"),
        ~ .x * bg_perc_area
        )
      ) #Recalculate ethnicity columns to be intersection-specific
}

bgsf_to_poldistsf <- function(bg_sf, dist_name_var) {
  dist_sym <- rlang::sym(dist_name_var)
  
  policedist_sf <- bg_sf |> 
    group_by(!!dist_sym) |>
    summarize(
      across(
        matches("Total|nH|Hispan"), 
        ~ sum(.x)
        )
      ) |> #Counts by ethnic group
    mutate(
      across(
        matches("nH|Hispan"), 
        ~ .x/Total, 
        .names = "{.col}_perc"
        )
      ) |> #Percentages (of district) by ethnic group
    #Round counts and percentages post-calculation so percentages are not calculated with rounded numerators
    mutate(
      across(
        matches("nH|Hispan"), 
        ~ round(.x, 3)
        )
      )
  
  return(policedist_sf)
}

all_bg_overlapping_dist <- function(bgsf, bg_dist_subset) {
  overlap <- st_drop_geometry(bgsf) |>
    right_join(bg_dist_subset, by = join_by(GEOID == GEOID))
  
  return(overlap)
}
