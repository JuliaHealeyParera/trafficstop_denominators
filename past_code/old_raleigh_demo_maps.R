#######OLD########, reload objects by running code/raleigh_demo.R
#Census block SW Raleigh police district intersections  
total_bg_swd <- nrow(bg_overlap_swd)
fully_included_bg_swd <- nrow(bg_overlap_swd |> filter(round(bg_perc_area, 2) == 1))
bg_dist_intersect <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd, 
    aes(geometry = geometry), 
    fill = NA) +
  geom_sf(
    data = police_swd_raleigh, 
    aes(geometry = geometry), 
    fill = "blue4", 
    alpha = 0.6) + 
  theme_void() + 
  labs(
    title = paste0(
      "Raleigh's Southwest police district touches\n", 
      total_bg_swd, 
      " census neighborhoods."), 
    subtitle = paste0(
      fully_included_bg_swd, 
      " of these neighborhoods are fully in the district.")
  )
ggsave('plots/swd_raleigh/bg_dist_intersect_extra.png', bg_dist_intersect)

#Percent area overlay
police_dist_census_blocks <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd_swd, 
    aes(geometry = geometry, fill = bg_perc_area), 
    alpha = .7) + 
  geom_sf(
    data = annotated_bg,
    aes(geometry = geometry), 
    fill = NA, 
    color = "gray30", 
    linewidth = .6) + 
  geom_sf(
    data = police_swd_raleigh, 
    aes(geometry = geometry), 
    fill = NA, 
    color = 'black', 
    linewidth = .8) + 
  annotate("text", x = 2130000, y = 751000, label = "100% of area\nin district") +
  annotate("segment", x = 2103500, y = 747000, xend = 2123000, yend = 751000) + #could also use st_centroid of obj and extract coord
  annotate("text", x = 2070000, y = 712000, label = "7% of area\nin district") +
  annotate("segment", x = 2077350, y = 712000, xend = 2088000, yend = 712000) +
  scale_fill_distiller(
    palette = "Greens",
    direction = 1, 
    labels = scales::percent) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(
    title = paste0(
      "Raleigh's Southwest police district touches\n", 
      total_bg_swd, 
      " census neighborhoods."), 
    subtitle = paste0(
      fully_included_bg_swd, 
      " of these neighborhoods are fully in the district."),
    fill = "% of block\ngroup in district")
ggsave('plots/swd_raleigh/swd_bg_policedist_overlay_3.png', police_dist_census_blocks)

total_bg_citywide <- nrow(citywide_bgs_with_overlap)
fully_included_bg_citywide <- nrow(citywide_bgs_with_overlap |> filter(round(bg_perc_area, 2) == 1))

citywide_bgs_with_overlap <- 
  st_drop_geometry(ral_intersection_sf) |>
  right_join(raleigh_tbl, by = join_by(GEOID == GEOID))

#Percent area overlap citywide
police_dist_census_blocks_citywide <- ggplot() + 
  geom_sf(
    data = citywide_bgs_with_overlap, 
    aes(geometry = geometry, fill = bg_perc_area), 
    alpha = .7) + 
  geom_sf(
    data = police_raleigh, 
    aes(geometry = st_simplify(geometry, dTolerance = 750)), 
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
      "Raleigh's Southwest police district touches\n", 
      total_bg_citywide, 
      " census neighborhoods."), 
    subtitle = paste0(
      fully_included_bg_citywide, 
      " of these neighborhoods are fully in the district."),
    fill = "% of block\ngroup in district")

ggsave(
  'plots/swd_raleigh/swd_bg_policedist_overlay_citywide_3_alt.png', 
  police_dist_census_blocks_citywide
)

#Map of num. total residents in district per block group 
label_val <- swd_ral_intersection_sf[
  swd_ral_intersection_sf$GEOID %in% annotated_bg$GEOID, 
  c('GEOID', 'Total')
]

swd_bg_total <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd, 
    aes(geometry = geometry, fill = B_nH.x), 
    alpha = .7) + 
  geom_sf(
    data = annotated_bg, 
    aes(geometry = geometry), 
    fill = NA, 
    color = "gray30", 
    linewidth = .6) + 
  geom_sf(
    data = police_swd_raleigh, 
    aes(geometry = geometry), 
    fill = NA, 
    color = 'black', 
    linewidth = .8) + 
  annotate(
    "text", 
    x = 2130000, y = 751000, 
    label = paste0(
      round(label_val |> filter(GEOID == 371830516003) |> pull(Total)),
      " residents\nin district")) +
  annotate("segment", x = 2103500, y = 747000, xend = 2123000, yend = 751000) + #could also use st_centroid of obj and extract coord
  annotate(
    "text", 
    x = 2065000, y = 712000, 
    label = paste0(
      round(label_val |> filter(GEOID == 371830530102) |> pull(Total)),
      " residents\nin district")) +
  annotate("segment", x = 2071000, y = 711000, xend = 2088000, yend = 712000) +
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  coord_sf(clip="off") +
  theme_void() +
  labs(
    title = "Some geographically small neighborhoods have many residents.",
    subtitle = "Relatedly, large neighborhoods may contribute only a few,\ndepending on their district overlap",
    fill = "Num. of\nresidents")
ggsave('plots/swd_raleigh/swd_bg_intersection_totalpop_4.png', swd_bg_total)

#Convert from block group unit to police district unit
police_final <- bgsf_to_poldistsf(ral_intersection_sf, "DISTRICT") 
police_final_swd <- police_final |> filter(DISTRICT == "SWD")

#Full police district demographics overlap map
swd_bg_full_dist <- ggplot() + 
  geom_sf(
    data = bg_overlap_swd, 
    aes(geometry = geometry),
    fill = NA) +
  geom_sf(
    data = police_final |> 
      filter(DISTRICT == "SWD"), 
    aes(geometry = geometry), 
    fill = "blue4", 
    alpha = 0.6) + 
  theme_void() + 
  labs(
    title = paste0(
      "The estimated total population\nof SW Raleigh is ",
      prettyNum(round(police_final_swd |> pull(Total)), big.mark = ",")), 
    subtitle = paste0(
      police_final_swd |> pull(B_nH_perc) * 100,
      "% of the total population (",
      prettyNum(round(police_final_swd |> pull(B_nH)), , big.mark = ","),
      ")\nis Black Non-Hispanic."))
ggsave('plots/swd_raleigh/swd_bg_full_dist_5.png', swd_bg_full_dist)

#Demographic specific choropleth map (by police district)
all_dist_blacknh_perc <- dist_population_map(police_final, "raleigh", "B_nH")
ggsave('plots/swd_raleigh/all_dist_blacknh_perc_6.png', all_dist_blacknh_perc)