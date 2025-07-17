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
