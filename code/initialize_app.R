library(tidyverse)
library(sf)
library(units)
library(shiny)
library(bslib)
library(tools)
library(tidycensus)
library(units)
library(ggtext)
library(stringr)
library(shinythemes)
library(showtext)
library(sysfonts)
library(knitr)
library(kableExtra)

# Loads data and calls census API if necessary
source(here("code", "census_data.R"))
# Converts block group demographics to police district demographics
source(here("code", "bg_to_policedist.R"))
# Maintains and loads previously uploaded police district (master) shp files
source(here("code", "update_policedist_shpfiles.R"))
# Functions to create custom ggplot map objects
source(here("code", "map_generator.R"))
# Wrapper function called in app 
source(here("code", "generate_report_files.R"))

# Text file #1 
source(here("code", "text.R"))
