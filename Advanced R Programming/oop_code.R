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

# longitudnal data object------------------------------------------------------
setClass("longitudnalData", representation(id = "numeric", 
                                visit = "numeric", room = "character",
                                value = "numeric", timepoint = "numeric"))

# converts a data frame to longitudnalData object------------------------------
make_LD <- function(object){
  object <- new("longitudnalData", id = object$id, 
                visit = object$visit, 
                room = object$room, 
                value = object$value, 
                timepoint =  object$timepoint)
}

# creating a print method for the class longitudnalData------------------------
setGeneric("print")
setMethod("print", c(x = "longitudnalData"),
          function(x){
            paste("Longitudnal Dataset with", length(unique(x@id)), "subjects.")
          })

# creating a generic function for extracting subject specific information------

setGeneric("subject", function(x, iden){
  standardGeneric("subject")
})

setMethod("subject", c(x = "longitudnalData"),
          function(x, iden){
            if(iden %in% x@id){
              subIDString <- paste("Subject ID:", iden)
              dataForSub <- subset(x, x@id == iden)
              retList <- list(strToPrint = subIDString, subData = dataForSub)
              setClass("subject", representation(subInfo = "list"))
              retList <- new("subject", sub = retList)
            } else {
              paste("NULL")
            }
          })

# creating a generic for the summary function for the 

setGeneric("summary", function(x, iden){
  standardGeneric("summary")
})

setGeneric("pivot_wider", function(x, names_from, values_from){
  standardGeneric("pivot_wider")
})

setMethod("pivot_wider", c(x = "longitudnalData"), 
          function(x){
            x <- pivot_wider(x, names_from = x@room, values_from = x@value)
          })

setMethod("summary", c(x = "longitudnalData", "data.frame"), 
          function(x, iden){
            ext_id <- as.numeric(str_extract(iden, "[0-9]+"))
            x <- make_LD(pivot_wider(x, names_from = x@room, values_from = x@value))
            x %>% group_by(visit) %>% dplyr::summarise(
              bedroom =  mean(bedroom, na.rm = TRUE), 
              den = mean(den, na.rm = TRUE), 
              `living room` = mean(`living room`, na.rm = TRUE), 
              office = mean(office, na.rm = TRUE)
            )
          })




kjaskj



