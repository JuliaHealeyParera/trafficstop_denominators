# Load everything
library(here)
source(here("code", "initialize_app.R"))

# Remove current district calculations RDS
unlink(here("data", "district_calculations.rds"))

# Add starter cities (Raleigh, Charlotte, and Durham)
raleigh_obj <- generate_analysis(city = "raleigh", map_unit = "city") 
append_analysis(raleigh_obj)

charlotte_obj <- generate_analysis("charlotte", "city") 
append_analysis(charlotte_obj)

durham_obj <- generate_analysis("durham", "city") 
append_analysis(durham_obj)

  