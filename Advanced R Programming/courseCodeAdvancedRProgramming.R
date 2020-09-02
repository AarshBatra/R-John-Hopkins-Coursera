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

# object oriented programming

shape_s3 <- function(sideLengths){
  structure(list(sideLengths = sideLengths), class = "shape_s3")
}

triangle <- shape_s3(c(1, 1, 1))
square <- shape_s3(c(1, 1, 1, 1))
pentagon <- shape_s3(c(1, 2, 3, 4, 5))

isSquare <- function(x) UseMethod("isSquare")

isSquare.shape_s3 <- function(x){
  (length(x$sideLengths) == 4) &&
    (x$sideLengths[1] == x$sideLengths[2]) &&
    (x$sideLengths[2] == x$sideLengths[3]) &&
    (x$sideLengths[3] == x$sideLengths[4])
}

isEquilateralTriangle <- function(x) UseMethod("isEquilateralTriangle")

isEquilateralTriangle.shape_s3 <- function(x){
  (length(x$sideLengths) == 3) &&
    (x$sideLengths[1] == x$sideLengths[2]) &&
    (x$sideLengths[2] == x$sideLengths[3])
}

isEquilateralTriangle(triangle)

isEquilateralTriangle.default <- function(x){
  NA
}

#----------------------------------------------

shapeDetect <- function(x){
  structure(baseNameOfShape = baseNameOfShape, class = "shapeDetect")
}

eqTriangle <- shapeDetect(c("Triangle"))

isShape <- function(x){
  UseMethod("isShape")
}  

isShape.shapeDetect <- function(x){
  
}

# Reference Classes practice

Aarsh <- setRefClass("Aarsh", fields = list(height = "numeric",
                     nn = "character", weight = "numeric"), 
                     methods =  list(
                       hello = function(){
                         paste("My Nick Name is", nn)
                       }, 
                       weight = function(){
                         paste("Weight: ", weight )
                       }
                     ))





















