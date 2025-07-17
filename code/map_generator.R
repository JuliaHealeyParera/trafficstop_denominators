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

bg_population_map <- function(subset_tbl_long, city) {
  total_count <- subset_tbl_long |> filter(Group == 'Total') |> pull(Count) 
  total_max_min_populations <- subset_tbl_long |> 
    filter(
      Group == "Total",
      Count %in% c(
        max(total_count), 
        min(total_count[total_count > 5])
      )
    )
  
  population_all_citywide <- ggplot() + 
    geom_sf(
      data = subset_tbl_long |> 
        filter(Group == "Total"), 
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
        " residents."),
      subtitle = "Census neighborhoods are unevenly populated.",
      fill = "Population")
  
  return(population_all_citywide)
}
