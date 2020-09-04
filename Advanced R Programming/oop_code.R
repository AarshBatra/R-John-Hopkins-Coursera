# Assignment Part 2: Longitudnal Data class and methods------------------------


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

# using S3 system create a longitudnal data object
make_LD <- function(dataObj){
  structure(dataObj, class = c("LongitudnalData", "data.frame"))
}

print.LongitudnalData <- function(longDataObj){
  paste("Longitudnal Dataset with", length(unique(longDataObj$id)), "subjects.")
  
}

subject <- function(dataObj, id){
  UseMethod("subject")
}

visit <- function(subjObj, visitNum){
  UseMethod("visit")
}

room <- function(subjObj, roomNum){
  UseMethod("room")
}

subject.LongitudnalData <- function(dataObj, id){
 objToRet <- list(data = dataObj, id = id)
 structure(objToRet, class = c("subject"))
}

print.subject <- function(subjObj){
  extractId <- subjObj[["id"]]
  if(extractId %in% subjObj[["data"]]$id){
    paste("Subject ID:", extractId) 
  } else {
    return(NULL)
  }
  
}

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

print.summary <- function(summaryDataObj){
  print(summaryDataObj[["summData"]])
  
}
  
visit.subject <- function(subjObj, visitNum){
  objToRet <- list(dataList = subjObj, visNum = visitNum)
  structure(objToRet, class = c("visit"))
}

room.visit <- function(visObj, roomNum){
  objToRet <- list(visObjList = visObj, roomNum = roomNum)
  structure(objToRet, class = c("room"))
}

print.room <- function(x){
  
}

summary.room <- function(x){
   
}

foo <- make_LD(longData)
foo2 <- subject(foo, 54) %>% summary
foo3 <- subject(foo, 14) %>% summary

  
