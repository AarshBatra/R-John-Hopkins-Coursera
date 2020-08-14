# practice code

# setwd
setwd("C:/Users/Aarsh Batra/Desktop/GitHub/R John Hopkins Coursera")

# libraries
library(swirl)
library(maps)
library(ggplot2)
library(dplyr)
library(viridis)

# install course
swirl::install_course("Advanced R Programming")

# debugging 

check_n_value <- function(n) {
  if(n > 0) {
    stop("n should be <= 0")
  }
}

check_n_value_1 <- function(n) {
  if(n > 0) {
    stop("n should be <= 0")
  }
}

error_if_n_is_greater_than_zero <- function(n){
  check_n_value_1(n)
  n
}
error_if_n_is_greater_than_zero(-1)
as.list(body(check_n_value_1)[[2]])
trace(check_n_value_1, browser, at = list(c(2, 3)))


options(error = recover)

# check how to untrace a function
# profiling
