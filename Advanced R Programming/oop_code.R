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
