# Assignment Part 1: Factorial function----------------------------------------


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

# Version 1 (Factorial Loop)---------------------------------------------------

Factorial_loop <- function(n) {
  stopifnot(n >= 0) # input to the factorial function should be a whole number. 
  
  if(n == 0){
    return(1)
  } else if(n == 1){
    return(1)
  } else {
    facAns <- n
    for(i in n : 2){
      facAns <- facAns*(i-1)
    }  
  }
  facAns
}

# Version 2 (Factorial Reduce)-------------------------------------------------

Factorial_reduce <- function(n){
  stopifnot(n >= 0)
  if(n == 0){
    return(1)
  } else {
    reduce(c(1 : n), function(x, y) x * y) 
  }
}

# Version 3 (Factorial Recursion)----------------------------------------------

Factorial_func <- function(n){
  stopifnot(n >= 0)
  if(n == 0){
    return(1)
  } else if(n == 1){
    return(1)
  } else {
    n * Factorial_func(n - 1)
  }
}

# Version 4 (Factorial Recursion using Memoization)----------------------------

# setting up the memoization table
facMemTable <- c(1, c(rep(NA, times = 9)))

# memoization function
Factorial_mem <- function(n){
  stopifnot(n >= 0)
  if(n == 0){
    return(1)
  } else {
      if(!is.na(facMemTable[n])){
        facMemTable[n]
      } else {
        facMemTable[n - 1] <<- Factorial_mem(n - 1) 
        n * facMemTable[n - 1]
    }
    
  }
  
}

# Profiling factorial code-----------------------------------------------------

# function to get benchmarks data for different type of factorial functions
getProfFunData <- function(fun, funcTypeName){
  fac_data <- map(1:10, function(x){microbenchmark(fun(x), times = 100)$time})
  names(fac_data) <- paste0(letters[1:10], 1:10)
  fac_data <- as_tibble(fac_data)
  
  fac_data_sum <- fac_data %>%
    gather(num, time) %>%
    group_by(num) %>%
    summarise(med_time = median(time)) %>% 
    mutate(funcType = funcTypeName)
  
  fac_data
}

# using the function to get benchmarks datasets cooresponding to each function
facLoopData <- getProfFunData(Factorial_loop, "for loop")
facReduceData <- getProfFunData(Factorial_reduce, "Reduce")
facRecursionData <- getProfFunData(Factorial_func, "Recursion")
facMemoizationData <- getProfFunData(Factorial_mem, 
                                     "Recursion using Memoization")

# row binding all data into a single profiling dataset used for plotting 
profDataset <- rbind(facLoopData, facReduceData, facRecursionData, 
                     facMemoizationData)

# changing colnames of the profiling dataset
colnames(profDataset) <- c("Number", "Median Time (in nanoseconds)", "Function Type")


# making the profDataset wider for ease in interpretation----------------------
profDatasetWider <- profDataset %>%
  pivot_wider(names_from = `Function Type`, 
              values_from = `Median Time (in nanoseconds)`)

# plotting profiling data
ggplot(profDataset, mapping = aes(x = Number, y = `Median Time (in nanoseconds)`)) + 
  geom_point(mapping = aes(colour = `Function Type`))
