# Load everything
library(here)
source(here("code", "initialize_app.R"))

# Remove current district calculations RDS
unlink(here("data", "district_calculations.rds"))

# (This could be cleaned up with map())
# Add starter cities and their individual distict counterparts

# Raleigh
raleigh_obj <- generate_analysis(city = "raleigh", map_unit = "city") 
append_analysis(raleigh_obj)

raleigh_obj_dtd <- generate_analysis(city = "raleigh", map_unit = "district", dist_name = 'DTD') 
append_analysis(raleigh_obj_dtd)

raleigh_obj_ned <- generate_analysis(city = "raleigh", map_unit = "district", dist_name = 'NED') 
append_analysis(raleigh_obj_ned)

raleigh_obj_nod <- generate_analysis(city = "raleigh", map_unit = "district", dist_name = 'NOD') 
append_analysis(raleigh_obj_nod)

raleigh_obj_nwd <- generate_analysis(city = "raleigh", map_unit = "district", dist_name = 'NWD') 
append_analysis(raleigh_obj_nwd)

raleigh_obj_sed <- generate_analysis(city = "raleigh", map_unit = "district", dist_name = 'SED') 
append_analysis(raleigh_obj_sed)

raleigh_obj_swd <- generate_analysis(city = "raleigh", map_unit = "district", dist_name = 'SWD') 
append_analysis(raleigh_obj_swd)


#Charlotte
charlotte_obj <- generate_analysis("charlotte", "city") 
append_analysis(charlotte_obj)

charlotte_obj_airport <- generate_analysis("charlotte", "district", dist_name = "Airport Division") 
append_analysis(charlotte_obj_airport)

charlotte_obj_eastway <- generate_analysis("charlotte", "district", dist_name = "Eastway Division") 
append_analysis(charlotte_obj_eastway)

charlotte_obj_central <- generate_analysis("charlotte", "district", dist_name = "Central Division") 
append_analysis(charlotte_obj_central)

charlotte_obj_freedom <- generate_analysis("charlotte", "district", dist_name = "Freedom Division") 
append_analysis(charlotte_obj_freedom)

charlotte_obj_hick <- generate_analysis("charlotte", "district", dist_name = "Hickory Grove Division") 
append_analysis(charlotte_obj_hick)

charlotte_obj_indep <- generate_analysis("charlotte", "district", dist_name = "Independence Division") 
append_analysis(charlotte_obj_indep)

charlotte_obj_metro <- generate_analysis("charlotte", "district", dist_name = "Metro Division") 
append_analysis(charlotte_obj_metro)

charlotte_obj_north <- generate_analysis("charlotte", "district", dist_name = "North Division") 
append_analysis(charlotte_obj_north)

charlotte_obj_ntyron <- generate_analysis("charlotte", "district", dist_name = "North Tryon Division") 
append_analysis(charlotte_obj_ntyron)

charlotte_obj_prov <- generate_analysis("charlotte", "district", dist_name = "Providence Division") 
append_analysis(charlotte_obj_prov)

charlotte_obj_south <- generate_analysis("charlotte", "district", dist_name = "South Division") 
append_analysis(charlotte_obj_south)

charlotte_obj_steele <- generate_analysis("charlotte", "district", dist_name = "Steele Creek Division") 
append_analysis(charlotte_obj_steele)

charlotte_obj_univ <- generate_analysis("charlotte", "district", dist_name = "University City Division") 
append_analysis(charlotte_obj_univ)

charlotte_obj_westover <- generate_analysis("charlotte", "district", dist_name = "Westover Division") 
append_analysis(charlotte_obj_westover)


#Durham
durham_obj <- generate_analysis("durham", "city") 
append_analysis(durham_obj)

durham_obj_0 <- generate_analysis("durham", "district", dist_name = '0') 
append_analysis(durham_obj_0)

durham_obj_1 <- generate_analysis("durham", "district", dist_name = '1') 
append_analysis(durham_obj_1)

durham_obj_2 <- generate_analysis("durham", "district", dist_name = '2') 
append_analysis(durham_obj_2)

durham_obj_3 <- generate_analysis("durham", "district", dist_name = '3') 
append_analysis(durham_obj_3)

durham_obj_4 <- generate_analysis("durham", "district", dist_name = '4') 
append_analysis(durham_obj_4)

durham_obj_5 <- generate_analysis("durham", "district", dist_name = '5') 
append_analysis(durham_obj_5)
