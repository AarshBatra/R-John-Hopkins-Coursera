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
library(lubridate)
library(geosphere)
library(grid)
library(ggmap)

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

# creating a date time column from existing columns

# changing relevant columns to type numeric
ext_tracks <- mutate(ext_tracks, month = as.numeric(month), 
                     year = as.numeric(year), day = as.numeric(day), 
                     hour =  as.numeric(hour)) 

# adding a date_time column
ext_tracks <- ext_tracks %>% mutate(
  date_time = lubridate::make_datetime(year = year, 
                      month = month, day = day, hour = hour, tz = "UTC")
)

# Convert data to a long format with separate rows for each wind speed

# Creating 2 columns: wind speed and direction (making data longer)
ext_tracks <- ext_tracks %>% pivot_longer(cols = radius_34_ne: radius_64_nw,   
                                 names_to = c("windSpeed", "direc"), 
                                 names_pattern = "radius_(..)_(..)",
                                 values_to = "radiiGivenDirec")

# Making a column for each  of 4 directions (making data wider)
ext_tracks_tidy <- ext_tracks %>% pivot_wider(names_from = direc, 
                                         values_from = "radiiGivenDirec")

# Note: Both of the above steps result in the type of dataset as 
# required in the assignment instructi=ons.

# Rearranging columns in the tidy dataset
ext_tracks_tidy <- ext_tracks_tidy %>% select(
  storm_id, storm_name, date_time, latitude, longitude, windSpeed, 
  ne, se, nw, sw, everything()
)

# subsetting to a single observation time for the storm IKE
# (the observation time chosen was the one when storm was over the
# united states)

# using the observation date time 2008-09-14, 12:00:00 UTC, at this time
# the latitude = 37.6 N, and longitude = 91 w, which means it was over the
# United States.

ext_tracks_tidy_IKE <- filter(ext_tracks_tidy,  storm_name %in% "IKE", 
                              date_time == as_datetime("2008-09-13 12:00:00"))
View(ext_tracks_tidy_IKE)

# geom hurricane function----------------------------------

draw_panel_function <- function(data, panel_scales, coord){
  coords <- coord$transform(data, panel_scales)
  
  nautMileInMeters <- 1852
  
  data <- data %>% 
    mutate(ne = ne * nautMileInMeters * scale_radii, 
           se = se * nautMileInMeters * scale_radii, 
           nw = nw * nautMileInMeters * scale_radii, 
           sw = sw * nautMileInMeters * scale_radii)
  
  
  finDf <- data.frame()
  
  for(i in 1 : nrow(data)){
    
    
    data_ne <- data.frame(colour = data[i, ]$colour, 
                          fill = data[i, ]$fill,
                          geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y), 
                                               b = 0 : 90,
                                               d = data[i, ]$ne), 
                          group = data[i, ]$group, 
                          PANEL = data[i, ]$PANEL, 
                          alpha = data[i, ]$alpha)
    
    data_se <- data.frame(colour = data[i, ]$colour, 
                          fill = data[i, ]$fill, 
                          geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y),
                                               b = 90 : 180, 
                                               d = data[i, ]$se), 
                          group = data[i, ]$group, 
                          PANEL = data[i, ]$PANEL, 
                          alpha = data[i, ]$alpha)
    
    data_sw <- data.frame(colour = data[i, ]$colour, 
                          fill = data[i, ]$fill, 
                          geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y), 
                                               b = 180 : 270, 
                                               d = data[i, ]$sw), 
                          group = data[i, ]$group, 
                          PANEL = data[i, ]$PANEL, 
                          alpha = data[i, ]$alpha)
    
    data_nw <- data.frame(colour = data[i, ]$colour, 
                          fill = data[i, ]$fill, 
                          geosphere::destPoint(p = c(data[i, ]$x, data[i, ]$y), 
                                               b = 270 : 360, 
                                               d = data[i, ]$nw), 
                          group = data[i, ]$group, 
                          PANEL = data[i, ]$PANEL, 
                          alpha = data[i, ]$alpha)
    
    finDf <- dplyr::bind_rows(finDf, data_sw, data_se, data_nw, data_ne)
  }
  
  finDf <- finDf %>%
    rename(x = lon, 
           y = lat)
  
  finDf$colour <- as.character(finDf$colour)
  finDf$fill <- as.character(finDf$fill)
  
  coords_df <- coord$transform(finDf, panel_scales)
  
  print(str(coords_df))
  
  polygonGrob(
    x = coords_df$x, 
    y = coords_df$y, 
    gp = gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha)
  )
  
}





GeomHurricane <- ggproto("GeomHurricane", Geom, 
                         required_aes = c("x", "y", "ne", "se", "nw", "sw"), 
                         default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1), 
                         draw_key = draw_key_polygon, 
                         draw_group = draw_panel_function)


geom_hurricane <- function(mapping = NULL, data = NULL,
  stat = "identity", position = "identity",na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...){
  
  ggplot2::layer(geom = GeomHurricane, stat = stat, data = data,
                          mapping = mapping, position = position,
                          inherit.aes = inherit.aes, show.legend = show.legend,
                          params = list(
                            na.rm = na.rm, ...
                          ))
}




x <- get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = ext_tracks_tidy_IKE,
                 aes(x = longitude, y = latitude, 
                     ne = ne, se = se, nw = nw, sw = sw,
                     fill = windSpeed, color = windSpeed, alpha = 0.5, scale_radii = 1)) + 
  guides(alpha = FALSE) +
  theme(legend.justification=c(1,0), legend.position=c(0.97,0.03)) +
  
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red4", "orange4", "yellow4")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow")) -> hurricane_ike


ggmap(Louis) + geom_hurricane(data = ext_tracks_tidy_IKE,
                              aes(x = longitude, y = latitude, 
                                  ne = ne, se = se, nw = nw, sw = sw,
                                  fill = windSpeed, color = windSpeed, alpha = 0.5, scale_radii = 1))


