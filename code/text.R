#### INTRODUCTION ####

introduction_general_1 <- "Individual court cases are decided on the merits of the evidence and arguments. However, in recent years, courts have allowed the introduction of relevant aggregate traffic stop data to support numerous defendants’ Motions to Suppress in state and federal trial courts (Johnson, Caldwell-Thompson, Planter, Whitaker). This aggregate data consists of totals and averages of traffic enforcement-related events, such as the number of traffic stops by an officer or law enforcement agency; as well as an agency’s and specific officer’s traffic stop rates broken down by motorist race/ethnicity."

introduction_patrolareas_2 <- "Local law enforcement agencies (LEAs), such as municipal police departments and county sheriff’s offices, often divide their jurisdiction into separate areas for designated officers to patrol. These areas, or patrol districts, are often drawn separately from the boundaries used by other government agencies to divide counties."

introduction_patrolareas_3 <- "Police and sheriff departments may create their own patrol districts. The above patrol districts were collected from city-specific Open Data Portals in June 2025."

introduction_racialdisparities_4 <- "This tool is designed to provide accurate racial population metrics for police patrol districts, such that traffic stop data for individual officers might be compared against those metrics in line with courts’ need for appropriate benchmarks to assess potential discriminatory effect.
North Carolina maintains one of the more comprehensive traffic stop data systems in the country through the State Bureau of Investigation’s (SBI) Traffic Stop Database. The database was established by the NC General Assembly in 1999 under N.C. Gen. Stat. § 114-10.01, now codified at § 143B-903, which requires certain law enforcement agencies to report information about each stop, including driver demographics, the reason for the stop, whether a search occurred, contraband was recovered, or force was employed, among other things. Having been in effect for more than 25 years, data is available for more than 31 million stops from 340 agencies statewide, including all municipal departments serving populations over 10,000; and the North Carolina State Highway Patrol. 
Larger law enforcement agencies often divide their jurisdictions into smaller patrol districts, divisions, or zones. Officers may be assigned to a specific patrol district, in which they respond to calls for service, conduct proactive policing, and engage in traffic enforcement. In North Carolina, claims of selective enforcement based on traffic stop data and involving officers assigned to an individual patrol district are evaluated by comparing the officer’s data to the population of people in the patrol district. Courts rely on these patrol district-level population benchmarks to determine whether an officer’s traffic stop enforcement patterns present stark disparities that demonstrate discriminatory effect.
The methodology described below focuses on individual patrol districts, which are the primary geographic units by which police departments organize officer assignments and daily enforcement activity. Because traffic stops occur within these geographically defined areas, courts have found patrol districts offer an appropriate spatial unit for purposes of comparing traffic enforcement data to local population demographics, as opposed to city-wide demographics.
Conducting research on traffic enforcement at the patrol district level is necessary for identifying patterns that may raise constitutional or policy concerns. Comparing officers’ stop-level data against district-level population benchmarks permits courts and stakeholders to evaluate whether certain groups experience different enforcement practices."

introduction_census_5 <- "This tool is designed to provide accurate racial population metrics for police patrol districts, such that traffic stop data for individual officers might be compared against those metrics in line with courts’ need for appropriate benchmarks to assess potential discriminatory effect.
North Carolina maintains one of the more comprehensive traffic stop data systems in the country through the State Bureau of Investigation’s (SBI) Traffic Stop Database. The database was established by the NC General Assembly in 1999 under N.C. Gen. Stat. § 114-10.01, now codified at § 143B-903, which requires certain law enforcement agencies to report information about each stop, including driver demographics, the reason for the stop, whether a search occurred, contraband was recovered, or force was employed, among other things. Having been in effect for more than 25 years, data is available for more than 31 million stops from 340 agencies statewide, including all municipal departments serving populations over 10,000; and the North Carolina State Highway Patrol. 
Larger law enforcement agencies often divide their jurisdictions into smaller patrol districts, divisions, or zones. Officers may be assigned to a specific patrol district, in which they respond to calls for service, conduct proactive policing, and engage in traffic enforcement. In North Carolina, claims of selective enforcement based on traffic stop data and involving officers assigned to an individual patrol district are evaluated by comparing the officer’s data to the population of people in the patrol district. Courts rely on these patrol district-level population benchmarks to determine whether an officer’s traffic stop enforcement patterns present stark disparities that demonstrate discriminatory effect.
The methodology described below focuses on individual patrol districts, which are the primary geographic units by which police departments organize officer assignments and daily enforcement activity. Because traffic stops occur within these geographically defined areas, courts have found patrol districts offer an appropriate spatial unit for purposes of comparing traffic enforcement data to local population demographics, as opposed to city-wide demographics.
Conducting research on traffic enforcement at the patrol district level is necessary for identifying patterns that may raise constitutional or policy concerns. Comparing officers’ stop-level data against district-level population benchmarks permits courts and stakeholders to evaluate whether certain groups experience different enforcement practices."


#### CITY-WIDE CALCULATIONS ####

citycalc_intro_1 <- function(city_name, district_name) {
  ret <- paste0(
  "Let’s apply these calculations to a city of your choosing, ",
  str_to_title(city_name),
  ". We can then zoom in to a single district, ",
  str_to_title(district_name),
  ", to better understand the results."
  )
  return(ret)
}

citycalc_poldist_2 <- function(city_name, num_dist) {
  ret <- paste0(
    "The entire city of ",
    str_to_title(city_name),
    " has ",
    num_dist, 
    " police districts, all of which are made up of one or more complicated polygons." 
  )
  return(ret)
}

citycalc_bgpop_3 <- "Independently of overlapping police districts, each census neighborhood has a certain population. This population includes individuals of various racial and age groups. Because neighborhoods are shaped a certain way because of a variety of factors, they are unevenly populated. One neighborhood may have several thousand residents, while another may have only a few hundred."

citycalc_areaoverlap_4 <- "In order to calculate the populations of each police district, we refer to smaller census neighborhoods. While we can overlay census neighborhoods with our police district, the geometries are not easily relatable. Some census neighborhoods, which are fully within the district borders, have 100% of their area within the district. However, many more are only partially in the district; these neighborhoods can either have very minimal area overlap (ex. 5%) or quite a lot (80%). Thus, in order to accurately calculate the number of residents within a district from a given census neighborhood, we calculate the area overlap between each neighborhood and the district."

citycalc_bgdistpop_5 <- "Using this area overlap, we calculate the number of residents in each census block that live in each police district. These values are directly representative of population density within a given census block and its geographical overlap with neighboring patrol areas."

citycalc_poldistpop_6 <- "By aggregating each of these individual populations, we can calculate the populations and relevant demographics of each patrol area."


#### DISTRICT-SPECIFIC CALCULATIONS ####

focusdist_intro_1 <- function(city_name, dist_name) {
  ret <- paste0(
    "We will now focus on ",
    str_to_title(dist_name),
    ", a particular district in ",
    str_to_title(city_name),
    ", to make our calculations crystal-clear."
  )
  return(ret)
}

focusdist_bgpop_2 <- "As with our citywide calculations, we begin with each census neighborhood’s total populations. At this scale, we can see that even within a given patrol district, populations are unevenly distributed. While some neighborhoods are extremely populated, others are geographically smaller or sparser in terms of number of residents. Displayed below are all census neighborhoods that touch the patrol district area. These neighborhoods may not be fully included in the district, but they are at least partially included in it."
 
focusdist_areaoverlap_3 <- "Our second step, as in the citywide calculations, is to calculate the area overlap for each census neighborhood. Because of the irregular shape of some patrol districts, this may include neighborhoods with less than 10% of their geographic area overlapping with the patrol districts."

focusdist_bgdistpop_4 <- "To calculate the number of residents from a given census neighborhoods that are included in the relevant police district, we information from both of the prior maps: total number of neighborhood residents and percent overlap with the district. By multiplying these values, we calculate the number of residents that each neighborhood contributes to the larger district population."

focusdist_poldist_5 <- function(dist_name, dist_totalpop, dist_ethnicpop, dist_ethnicperc, ethnic_group = "Black") {
  paste0(
    "Using each neighborhood’s individual resident population that is part of the district, we can calculate the full district population. For the ",
    str_to_title(dist_name),
    " police district, that number is ",
    dist_totalpop,
    ". Since each census neighborhood contains population counts at the racial and ethnic level, we can also estimate the racial makeup of each police district. For example, ",
    dist_ethnicpop,
    " (",
    dist_ethnicperc,
    "%) of this patrol district’s total population is ",
    str_to_title(ethnic_group),
    "."
  )
}

conclusion_technical_1 <- "The findings presented in this report detail the estimated residential populations of select police districts in North Carolina. As previously mentioned, these estimates are calculated using the American Community Survey block groups. Block groups are not the smallest unit of geographical measurement that the census offers–as such, these estimates operate on certain assumptions about population distribution. 
This report opted to use block groups because the data on their counts is more recent. ACS produces running averages in five-year blocks. The values used for this assessment are thus averages from the years 2020-2023. While using the more granular blocks provided by the decennial census would be ideal, this should only be done in the first two years following the full-fledged census data collection. In using block groups, we assume that the demographic groups in each area are equally distributed. This assumption allows us to directly jump from the proportion of area overlap to the proportion of block group population. This is an assumption that should be noted but not obsessed over, as block groups are geographically small enough that they do not tend to encompass multiple ethnic neighborhoods in one area."

conclusion_judicial_2 <- 'The application of district-level population benchmarks enables a structured comparison between those benchmarks and traffic stop data reported by LEAs and officers. These comparisons assist courts in evaluating whether officer enforcement patterns reflect disparities sufficient to establish a prima facie case of selective enforcement.
The use of patrol district population data allows for localized (as opposed to city-wide), demographically specific analysis that reflects where officers are assigned to patrol and where traffic enforcement is actually occurring. This spatial alignment strengthens the reliability of any comparison and ensures that data employed for benchmarking purposes is appropriately tailored to the context of each officer’s activity. This data can serve as an adequate population benchmark from which the racial composition of individuals and motorists "faced by" a particular officer can be assessed.'
