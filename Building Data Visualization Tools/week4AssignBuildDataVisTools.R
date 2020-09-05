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
library(roxygen2)
library(devtools)

# load in data----------------------------------------------------------------- 

#' Load Hurricane Data
#' 
#' Loads hurricane dataset from the path specified. The path should point to  
#' the location where the ext_tracks dataset is stored.
#' 
#' @importFrom readr read_fwf
#' @param fullPathToDataFile Path to the file where data is stored
#' @examples 
#' 


loadHurrData <- function(fullPathToDataFile){
  if(file.exists(fullPathToDataFile) == TRUE){
    ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 
                           5, 4, 4, 5, 3, 4, 3, 3, 3,
                           4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
    ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                             "hour", "year", "latitude", "longitude",
                             "max_wind", "min_pressure", "rad_max_wind",
                             "eye_diameter", "pressure_1", "pressure_2",
                    paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                    paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                    paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                             "storm_type", "distance_to_land", "final")
    
    ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", 
                           fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                           na = "-99")
    ext_tracks
  } else {
    stop("File not found, recheck the path entered in the 
         loadHurrData function!")
  }
}


# Cleaning Data----------------------------------------------------------------

#' Cleaning raw hurricane dataset
#' 
#' This function cleans the raw hurricane dataset. In the process of cleaning
#' it does the following: 
#'   1) adds a column combining \code{storm_name} 
#'      and \code{year} columns. 
#'    
#'   2) adds a \code{date_time} column, which is made by concatenating 
#'      \code{year}, \code{month}, \code{day}, \code{hour} columns.
#'    
#'   3) Converts the longitude column to type "numeric" and further
#'      converting all longitude values to their "negative", as the
#'      data belongs entirely to the western hemisphere.
#'      
#'   4) First make the data long and then wide again, to finally get it
#'      in a format as required by the assignment.   
#'    
#'   5) Rearranging columns, putting the most relevant ones in the 
#'      start.
#'    
#' @importFrom dplyr mutate select    
#' @importFrom lubridate make_datetime
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom  purrr map
#' 
#' @param rawHurrData Dataset loaded using the \code{loadHurrData} function
#' @examples 
#' tidyRawHurrData(rawHurrData = "rawHurrDataset")


tidyRawHurrData <- function(rawHurrData){
  
  # adding a column combining storm name and year
  ext_tracks <- rawHurrData %>% 
    dplyr::mutate(storm_name_year = str_c(storm_name, year, sep = "_"))
  
  # converting the longitude column to numeric (and making it negative)
  # as all data belong to the western hemisphere, given the documentation
  # provided
  
  ext_tracks$longitude <- as.numeric(ext_tracks$longitude)
  ext_tracks$longitude <- purrr::map(ext_tracks$longitude, function(x){
    if(x < 0){
      x
    } else{
      -x
    }
  }) %>% unlist()
  
  # creating a date time column from existing columns
  
  # changing relevant columns to type numeric
  ext_tracks <- dplyr::mutate(ext_tracks, month = as.numeric(month), 
                       year = as.numeric(year), day = as.numeric(day), 
                       hour =  as.numeric(hour)) 
  
  # adding a date_time column
  ext_tracks <- ext_tracks %>% dplyr::mutate(
    date_time = lubridate::make_datetime(year = year, 
                    month = month, day = day, hour = hour, tz = "UTC")
  )
  
  # Convert data to a long format with separate rows for each wind speed
  
  # Creating 2 columns: wind speed and direction (making data longer)
  ext_tracks <- ext_tracks %>%
    tidyr::pivot_longer(cols = radius_34_ne: radius_64_nw,   
                                      names_to = c("windSpeed", "direc"), 
                                      names_pattern = "radius_(..)_(..)",
                                      values_to = "radiiGivenDirec")
  
  # Making a column for each  of 4 directions (making data wider)
  ext_tracks_tidy <- ext_tracks %>% tidyr::pivot_wider(names_from = direc, 
                                      values_from = "radiiGivenDirec")
  
  # Note: Both of the above steps result in the type of dataset as 
  # required in the assignment instructions.
  
  # Rearranging columns in the tidy dataset (putting most relevant colums 
  # in the start)
  ext_tracks_tidy <- ext_tracks_tidy %>% dplyr::select(
    storm_id, storm_name, date_time, latitude, longitude, windSpeed, 
    ne, se, nw, sw, everything()
  )
  
  ext_tracks_tidy
  
} 

# Subset tidy data-------------------------------------------------------------

#' Subset tidy data
#' 
#' Subsetting to a single observation time for the storm: \code{storm_name}
#' ( choose the observation time chosen to be the one when storm was near or 
#' over the United States)
#' 
#' @importFrom dplyr filter
#' @param tidiedHurrDataset this is the dataset returned by the
#'        \code{tidyRawHurrData} function.
#' @param stormName the name of the storm for which the data is needed. This
#'        should be a character vector of length 1. 
#' @param dateTime this is the \code{date_time} to pick a single instance 
#'        of the \code{stormName}, a single observation time. This should be
#'        a character vector of length 1.
#'            
#' @examples 
#' subsetTidyHurrData(tidiedHurrDataset = tidyDatasetName, stormName = "IKE", 
#' dateTime = c("2018-09-13 12:00:00"))          


subsetTidyHurrData <- function(tidiedHurrDataset, stormName, dateTime){
  ext_tracks_tidy_IKE <- dplyr::filter(tidiedHurrDataset,  
                                stormName %in% storm_name, 
                                date_time == as_datetime(dateTime))
}



# geom hurricane function code ------------------------------------------------

#' Draw panel function: Part of the geom_hurricane function
#' 
#' The \code{draw_panel_function} function is implemented
#' separately due to its length. This function is set as one of the arguments
#' of the geom_hurricane function.
#' 
#' @importFrom dplyr mutate rename bind_rows
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob
#' 
#' @params data 
#' @params panel_scales
#' @params coord 
#' 
#' @examples 
#' This will be used inside the geom_hurricane function. Look at that function
#' for examples.

draw_panel_function <- function(data, panel_scales, coord){
  
  # transforming the dataset into a dataframe
  coords <- coord$transform(data, panel_scales)
  
  # one nautical mile =  1,852 meters.
  nautMileInMeters <- 1852
  
  # converting radii columns (corresponding to each direction) into meters
  # and further scaling it by the "scale_radii" parameter.
  data <- data %>% 
    dplyr::mutate(ne = ne * nautMileInMeters * scale_radii, 
           se = se * nautMileInMeters * scale_radii, 
           nw = nw * nautMileInMeters * scale_radii, 
           sw = sw * nautMileInMeters * scale_radii)
  
  # create a data frame that will bind all upcoming data frames together
  finDf <- data.frame()
  
  # main loop, for creating dataframes corresponding to each direction
  # and accumulating it in the "finDf" dataframe at the end of each iteration
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
    
  } # main for loop ends here
  
  # renaming longitude and latitude columns
  finDf <- finDf %>%
    dplyr::rename(x = lon, 
           y = lat)
  
  # converting the "colour" and "fill" columns to type character
  finDf$colour <- as.character(finDf$colour)
  finDf$fill <- as.character(finDf$fill)
  
  # rescaling the finDf
  coords_df <- coord$transform(finDf, panel_scales)
  
  print(str(coords_df))
  
  # specify the polygon Graphical Object
  polygonGrob(
    x = coords_df$x, 
    y = coords_df$y, 
    gp = grid::gpar(col = coords_df$colour, 
                    fill = coords_df$fill, alpha = coords_df$alpha)
  )
  
}

# creating an object named GeomHurricane which is the basis for the 
# geom_hurricane function
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom, 
                         required_aes = c("x", "y", "ne", "se", "nw", "sw"), 
                         default_aes = aes(fill = 1, colour = 1, 
                         alpha = 1, scale_radii = 1), 
                         draw_key = draw_key_polygon, 
                         draw_group = draw_panel_function)

#' geom_hurricane function
#' 
#' This function uses the \code{GeomHurricane} object to create a layer for
#' the new hurricane map
#' 
#' @importFrom ggplot2 layer
#' @note takes in all the standard parameters as a polygon geom, with 
#'       geom = \code{GeomHurricane} specified

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


# using the above functions to load, tidy, subset and-------------------------- 
# plot the hurricane data

# loading hurricane data
rawHurrData <- loadHurrData(fullPathToDataFile = "C:/Users/Aarsh Batra/Desktop/GitHub/R John Hopkins Coursera/Building Data Visualization Tools/ebtrk_atlc_1988_2015.txt")

# tidying hurricane data
tidiedHurrData <- tidyRawHurrData(rawHurrData = rawHurrData)

# subset tidy data to get data for a single observation 
# time for Hurricane "IKE"

ext_tracks_tidy_IKE <- subsetTidyHurrData(tidiedHurrDataset = tidiedHurrData,
                  stormName = "IKE", dateTime = c("2008-09-13 12:00:00")) 


# Map showing wind_radii overlayed for Hurricane IKE
x <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = ext_tracks_tidy_IKE,
                 aes(x = longitude, y = latitude, 
                     ne = ne, se = se, nw = nw, sw = sw,
                     fill = windSpeed, color = windSpeed, alpha = 0.5, 
                     scale_radii = 1)) + 
  guides(alpha = FALSE) +
  theme(legend.justification=c(1,0), legend.position=c(0.97,0.03)) +
  ggtitle("IKE Storm 2008-09-13 12:00:00") +
  
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red4", "orange4", "yellow4")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow")) -> hurricane_ike


# ggmap("Louis") + geom_hurricane(data = ext_tracks_tidy_IKE,
#                   aes(x = longitude, y = latitude, 
#                  ne = ne, se = se, nw = nw, sw = sw,
#          fill = windSpeed, color = windSpeed, alpha = 0.5, scale_radii = 1))
# 

