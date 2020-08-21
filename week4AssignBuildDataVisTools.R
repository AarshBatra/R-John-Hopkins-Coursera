# Week 4 Assignment: Building Data Visualization Tools-------------------------

# libraries--------------------------------------------------------------------
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(microbenchmark)
library(purrr)
library(readr)
library(tidyr)
library(gridExtra)
library(plotly)

# read in data-----------------------------------------------------------------
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# Cleaning Data----------------------------------------------------------------

# adding a column combining storm name and year
ext_tracks <- ext_tracks %>% 
  dplyr::mutate(storm_name_year = str_c(storm_name, year, sep = "_"))

# converting the longitude column to numeric (and making it negative)
# as all data belong to the western hemisphere, given the documentation
# provided

ext_tracks$longitude <- as.numeric(ext_tracks$longitude)
ext_tracks$longitude <- map(ext_tracks$longitude, function(x){
  if(x < 0){
    x
  } else{
    -x
  }
}) %>% unlist()

#