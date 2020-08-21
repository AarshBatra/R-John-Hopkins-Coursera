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

# get data---------------------------------------------------------------------
longData <- read_csv("MIE.csv")

make_LD <- function(data){
  class(data) <- "LongitudnalData"
  data
}

longData <- make_LD(longData)

longData <- setRefClass("LongitudnalData", 
                        fields = list(
                          id = "numeric",
                          visit = "numeric", 
                          room = "character",
                          value = "numeric", 
                          timepoint = "numeric"
                        ))

subject <- setRefClass("subject", 
                       fields = list(
                         id = "numeric", 
                         data = "data.frame"), 
                         methods = list(
                           subject = function(data, id){
                             if(id %in% data$speed){
                               print(paste0("Subject ID: " , id))
                             } else {
                               NULL
                             }
                           }
                         )
                       )

visit <- setRefClass("visit", 
                     fields = list(
                       visitNumber = "numeric"
                     ))

room <- setRefClass("room", 
                    fields = list(
                      roomType = "character"
                    ))


Student <- setRefClass("Student",
                       fields = list(name = "character",
                                     grad_year = "numeric",
                                     credits = "numeric",
                                     id = "character",
                                     courses = "list"),
                       methods = list(
                         hello = function(){
                           paste("Hi! My name is", name)
                         },
                         add_credits = function(n){
                           credits <<- credits + n
                         },
                         get_email = function(){
                           paste0(id, "@jhu.edu")
                         }
                       ))

brooke <- Student$new(name = "Brooke", grad_year = 2019, credits = 40,
                      id = "ba123", courses = list("Ecology", "Calculus III"))
