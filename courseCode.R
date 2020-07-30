# R Advanced Programming Coursera John Hopkins 

# packages
library(swirl)
library(tidyverse)
library(dplyr)
library(readr)

# Given the date and package name, the function downloads the appropriate download 
# logs from the RStudio server, reads the CSV file, and then returns the number of downloads 
# for the package.


num_download <- function(pkgname, date = "2017-07-07") { # Note the default value of the date variable
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  
  ## Construct path for storing local file
  dest <- file.path("data", basename(src))
  
  ## Don't download if the file is already there!
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

# Demonstrating the use of a stop function--------------------------------

errorMessageReturn <- function(input) {
    if(data.class(input) == "numeric"){
      stop("Stop, you Fucking Asshole!")
    }
    else {
      for(i in 1:length(LETTERS))
      print(LETTERS[i])
    }
}





