# Assignment Part 2: Longitudnal Data class and methods (using S3 system)------


# packages---------------------------------------------------------------------
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
library(methods)

# read in data-----------------------------------------------------------------
longData <- read_csv("MIE.csv")

# creating a few generic methods 

subject <- function(dataObj, id){
  UseMethod("subject")
}

visit <- function(subjObj, visitNum){
  UseMethod("visit")
}

room <- function(subjObj, roomNum){
  UseMethod("room")
}


# convert a data frame into a longitudnal data object
make_LD <- function(dataObj){
  structure(dataObj, class = c("LongitudnalData", "data.frame"))
}

# function definition: print method for the objects of class "LongitudnalData"
print.LongitudnalData <- function(longDataObj){
  paste("Longitudnal Dataset with", length(unique(longDataObj$id)), "subjects.")
  
}

# function definition: subject method for the longitudnal data object
# returns an object of class "subject"
subject.LongitudnalData <- function(dataObj, id){
 objToRet <- list(data = dataObj, id = id)
 structure(objToRet, class = c("subject"))
}

# function definition: print method for the object of class "subject"
print.subject <- function(subjObj){
  extractId <- subjObj[["id"]]
  if(extractId %in% subjObj[["data"]]$id){
    paste("Subject ID:", extractId) 
  } else {
    return(NULL)
  }
  
}

# function definition: summary method for the object of class "subject"
# returns an object of class "summary"
summary.subject <- function(subjObj){
  extractId <- subjObj[["id"]]
  extractData <- subjObj[["data"]]
  summData <- extractData %>% 
    pivot_wider(names_from = "room", values_from = value) %>%
    group_by(id, visit) %>%
    dplyr::summarise(bedroom = mean(bedroom, na.rm = TRUE), 
                     den = mean(den, na.rm = TRUE), 
                     `living room` = mean(`living room`, na.rm = TRUE), 
                     office = mean(office, na.rm = TRUE)) %>%
    dplyr::filter(id == extractId)
  retSummList <- list(subId = extractId, summData = summData)
  structure(retSummList, class = c("summary"))
}

# function definition: print method for the object of class "summary"
print.summary <- function(summaryDataObj){
  print(summaryDataObj[["summData"]])
}

# function definition: visit method for the object of class "subject"
# returns an object of class "visit"
visit.subject <- function(subjObj, visitNum){
  objToRet <- list(dataList = subjObj, visNum = visitNum)
  structure(objToRet, class = c("visit"))
}


# function definition: room method for the object of class "visit"
# returns an object of class "room"
room.visit <- function(visObj, roomNum){
  objToRet <- list(visObjList = visObj, roomNum = roomNum)
  structure(objToRet, class = c("room"))
}

# function definition: print method for the object of class "room"
print.room <- function(x){
  
}

# function definition: summary method for the object of class "room"
summary.room <- function(x){
   
}

foo <- make_LD(longData)
foo2 <- subject(foo, 54) %>% summary
foo3 <- subject(foo, 14) %>% summary

  
