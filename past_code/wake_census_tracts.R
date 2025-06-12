# Redlining & Tracts
# Libraries ####
library(tidyverse)
library(sf)
library(tidycensus)

# Get NC Tracts from census API
# "D:/Public Health Data/_GIS/General GIS/Base shape files/NC Tracts - Tiger Census 2014"
census_vars = load_variables("acs5", year = 2019)
pop_vars = census_vars %>% filter(concept %>% str_detect("HISPANIC OR LATINO ORIGIN BY RACE"))
nc_tracts = tidycensus::get_acs(geography = "tract", state = "NC", year = 2019, 
                                geometry = T, output = "wide",
                                variables = c("total_pop" = "B01001_001",
                                              "pop_whitenh" = "B03002_003",
                                              "pop_blacknh" = "B03002_004"))
# ^ how cool is that.
nc_tracts = nc_tracts %>% 
  mutate(pct_blacknh = pop_blacknhE / total_popE * 100) %>% 
  select(-matches("M$")) %>% 
  st_transform(2264) # NC state plane
wake_tracts = nc_tracts %>% 
  filter(NAME %>% str_detect("Wake"))

wake_tracts %>% filter(pct_blacknh > 60)
wake_tracts %>% mutate(pct_blacknh %>% forcats::fct_lump_n(n=5))

wake_tracts$pct_blacknh_cut = wake_tracts$pct_blacknh %>% cut(breaks=seq(0, 100, 10))
wake_tracts %>% count(pct_blacknh_cut)

ggplot(wake_tracts)+
  geom_sf(aes(fill = pct_blacknh))+
  theme_void()+
  labs(title = "Wake County Census Tracts % Black non-Hispanic",
       subtitle = "American Communities Survey, 2019 5 year estimates", 
       fill = "% Black non-Hispanic", 
       caption = "Includes 187 tracts in Wake County. 8 tracts have over 60% Black non-Hispanic, 
       but none have more than 80% Black non-Hispanic. Based on previous work in NC, 
       Black non-Hispanic drivers make up a lower percent of vehicles on the road, 
       because of vehicle access, driving volume, and wealth disparities." %>% 
         str_wrap(80))

nc_touching_tracts = nc_tracts[nc_redlines, ] # Magic - spatial subset to relevant tracts for speed
nc_touching_tracts %>% st_geometry() %>% plot
ggplot(nc_touching_tracts) + geom_sf() + theme_void()+labs(title="touching nc tracts"); ggsave("outputs/touching nc tracts.png")

local_tracts = nc_touching_tracts[nc_redlines %>% filter(city == "Durham"),]
#.......................

nc_tracts = tidycensus::get_acs(geography = "tract", state = "NC", year = 2019, geometry = T, variables = c("total_pop" = "B01001_001"))
