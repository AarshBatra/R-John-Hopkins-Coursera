# R Advanced Programming Coursera John Hopkins 

# packages
library(swirl)
library(tidyverse)
library(dplyr)
library(readr)

# Given the date and package name, the function downloads the appropriate download 
# logs from the RStudio server, reads the CSV file, and then returns the number of downloads 
# for the package.

# Note the default value of the date variable

num_download <- function(pkgname, date = "2017-07-07") { 
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  
  ## Construct path for storing local file
  dest <- file.path("data", basename(src))
  
  ## Don't download if the file is already there!
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)

  # with column types you can control how your input is rendered in R (when read in). 
  # This is especially useful when you are reading from multiple sources with different date
  # formats, and you can read all of that into a single common date format, by using this col_types 
  # function.
  
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

# We can write a separate function to check that the packages are installed.

check_pkg_deps <- function() {
  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first")
}

# Write all of the above on your own-------------

num_download_v2 <- function(pkgName, dateInput = "2019-10-19"){
  check_pkg_deps()
  dsList <- list()
  for(i in 1:length(dateInput)){
    year <- stringr::str_sub(dateInput[i], 1, 4)
    srcLink <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                       year, dateInput[i])
    
    filePath <- basename(srcLink) # relative to the cwd
    if(!(file.exists(filePath))){
      download.file(srcLink, filePath)
      dsList[[i]] <- read_csv(filePath, col_types = "ccicccccci")
      
    } else {
      dsList[[i]] <- read_csv(filePath, col_types = "ccicccccci")
    }
  }
  
  finTibble <- dsList[[1]]
  for(j in 1:length(dsList)){
    if(length(dsList) == 1){
     break 
    } else {
      finTibble <- rbind(finTibble,  dsList[[j + 1]])
      if((j + 1) == length(dsList)){
        break
      }
    }
  }
  
  finTibble %>% filter(package %in% pkgName) %>% group_by(package, date) %>% summarise(count = n())
}


