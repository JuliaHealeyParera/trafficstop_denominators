police_district_map <- function(police_dist_sf, city) {
  police_dist_map <- ggplot(police_dist_sf, aes(geometry = geometry, fill = DISTRICT)) + 
    geom_sf() +
    theme_void() + 
    guides(fill = "none") +
    labs(
      title = paste0(
        str_to_title(city), 
        " has ", 
        nrow(police_dist_sf), 
        " police district", 
        if_else(
          nrow(police_dist_sf) > 1,
          's.',
          '.'
        )
      )
    ) +
    scale_fill_brewer(palette = "Set3")
  
  if (nrow(police_dist_sf) > 1) {
    police_dist_map <- police_dist_map +
      geom_sf_label(aes(label = num), fill = 'white') 
  }
  
  return(police_dist_map)
}

bg_population_map <- function(subset_tbl_long, city, ethnic_group) {
  total_count <- subset_tbl_long |> filter(Group == ethnic_group) |> pull(Count) 
  total_max_min_populations <- subset_tbl_long |> 
    filter(
      Group == ethnic_group,
      Count %in% c(
        max(total_count), 
        min(total_count[total_count > 5])
      )
    )
  
  lab_ethnic_group <- str_replace_all(
    ethnic_group, 
    c("Hispan" = "Hispanic", 
      "Total" = "total",
      "W_nH" = "White Non-Hispanic", 
      "B_nH" = "Black Non-Hispanic", 
      "AmIn_nH" = "American Indian",
      "HaPI_nH" = "Hawaiian or Pacific Islander"))
  
  population_all_citywide <- ggplot() + 
    geom_sf(
      data = subset_tbl_long |> 
        filter(Group == ethnic_group), 
      aes(geometry = geometry, fill = Count), 
      alpha = .65
    ) + 
    geom_sf(
      data = total_max_min_populations, 
      aes(geometry = geometry), 
      fill = NA, 
      color = "black", 
      linewidth = .75
    ) +
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    geom_sf_label(data = total_max_min_populations, aes(label = Count)) + 
    theme_void() +
    labs(
      title = paste0(
        str_to_title(city),
        "'s census neighborhoods range\nfrom ", 
        max(total_count),
        " to ", 
        min(total_count[total_count > 5]),
        " ",
        lab_ethnic_group,
        " residents."),
      subtitle = "Census neighborhoods are unevenly populated.",
      fill = paste0(
        if_else(
          lab_ethnic_group == "total", 
          "Total", 
          paste0(lab_ethnic_group, "\n")
          ), 
        "population")
      )
  
  return(population_all_citywide)
}

area_intersection_map <- function(all_bg_overlapping_dist, police_dist, city, total_bg, full_included_bg) {
  police_dist_census_blocks_citywide <- ggplot() + 
    geom_sf(
      data = all_bg_overlapping_dist, 
      aes(geometry = geometry, fill = bg_perc_area), 
      alpha = .7) + 
    geom_sf(
      data = police_dist, 
      aes(geometry = st_simplify(geometry, dTolerance = 750)), # add threshold for simplify if too complex?
      fill = NA, 
      color = 'black', 
      linewidth = .75) + 
    scale_fill_distiller(
      palette = "Greens",
      direction = 1, 
      labels = scales::percent) + 
    coord_sf(clip="off") +
    theme_void() +
    labs(
      title = paste0(
        str_to_title(city),
        "'s police district touches\n", 
        total_bg, 
        " census neighborhoods."), 
      subtitle = paste0(
        fully_included_bg, 
        " of these neighborhoods are fully in the district."),
      fill = "% of block\ngroup in district")
  
  return(police_dist_census_blocks_citywide)
}

resident_intersection_map <- function(all_bg_overlapping_dist, police_dist, city) {
  swd_bg_total <- ggplot() + 
    geom_sf(
      data = all_bg_overlapping_dist, 
      aes(geometry = geometry, fill = "Total"), 
      alpha = .7) + 
    geom_sf(
      data = police_dist, 
      aes(geometry = geometry), 
      fill = NA, 
      color = 'black', 
      linewidth = .8) + 
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    coord_sf(clip="off") +
    theme_void() +
    labs(
      title = "Some geographically small neighborhoods have have many in-district residents.",
      subtitle = "Relatedly, large neighborhoods may contribute only a few,\ndepending on their district overlap",
      fill = "Num. of\nresidents")
  
  return(swd_bg_total)
}

dist_population_map <- function(police_dist_sf, city, ethnic_group = "B_nH") {
  ethnic_perc <- paste0(ethnic_group, "_perc")

  all_dist_blacknh_perc <- ggplot() + 
    geom_sf(
      data = police_dist_sf, 
      aes(
        geometry = geometry, 
        fill = as.numeric(!!sym(ethnic_perc)), 
      )
    ) +
    geom_sf_label(
      data = police_dist_sf, 
      aes(label = paste0(round(as.numeric(!!sym(name)) * 100, 1), "%")), 
      fill = "white") +
    scale_fill_distiller(
      palette = "Purples", 
      direction = 1, 
      labels = scales::percent) + 
    theme_void() + 
    labs(
      title = "Dynamic title about county with lowest perc of ethnic group", 
      subtitle = "Dynamic subtitle about county with highest perc of ethnic group",
      fill = paste0("Percent of District\n(", 
                    ethnic_group, 
                    " reformat this Julia")
    )
}

