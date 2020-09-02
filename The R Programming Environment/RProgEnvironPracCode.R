# The R Programming environment practice code--------------------

# libraries------------------------------------------------------
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
library(lubridate)
library(geosphere)
library(grid)
library(ggmap)
library(roxygen2)
library(devtools)
library(readxl)
library(R.utils)
library(data.table)


# week 4 programming assignment------------------------------------------------


# read in daily SPEC data------------------------------------------------------
dailySpecData <- data.table::fread("daily_SPEC_2014.csv.bz2")

# Q1: What is average Arithmetic.Mean for "Bromine PM2.5 LC" in the 
# state of Wisconsin in this dataset?

data_Q1 <- filter(dailySpecData, `Parameter Name` == "Bromine PM2.5 LC", 
       `State Name` == "Wisconsin") %>% select(`Arithmetic Mean`)

mean(data_Q1$`Arithmetic Mean`, na.rm = TRUE)  

# Q2: Calculate the average of each chemical constituent across all states, 
# monitoring sites and all time points.

data_Q2 <- dailySpecData %>% group_by(`State Code`, `County Code`, `Site Num`, 
                           `Parameter Name`, `Date Local`) %>%
  dplyr::summarise(avgLevel = mean(`Arithmetic Mean`, na.rm = TRUE))


View(data_Q2[which(data_Q2$avgLevel == max(data_Q2$avgLevel)), ])


# Q3: Which monitoring site has the highest average level of 
# "Sulfate PM2.5 LC" across all time?

data_Q3 <- dailySpecData %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  dplyr::summarise(avgLevSulf = mean(`Arithmetic Mean`, na.rm = TRUE))
  

data_Q3[which(data_Q3$avgLevSulf == max(data_Q3$avgLevSulf)), ]


# Q4: What is the absolute difference in the average levels of 
#"EC PM2.5 LC TOR" between the states California and Arizona, 
# across all time and all monitoring sites? 

data_Q4 <- dailySpecData %>% 
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>%
  group_by(`State Name`) %>% 
  dplyr::summarise(avgECPM2.5 = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  filter(`State Name` %in% c("California", "Arizona"))

absDiff <- abs(data_Q4$avgECPM2.5[1] - data_Q4$avgECPM2.5[2])

# Q5: What is the median level of "OC PM2.5 LC TOR" 
# in the western United States, across all time?
# Define western as any monitoring location that 
# has a Longitude LESS THAN -100.

data_Q5 <- dailySpecData %>% 
  filter(Longitude < -100, `Parameter Name` == "OC PM2.5 LC TOR")

medLevOCPM2.5LCTOR <- median(data_Q5$`Arithmetic Mean`, na.rm = TRUE)

#----------------------------------------------------------------------


# different dataset for Q6 to Q10: aqs_sites.xlsx

# read in data
aqsSitesData <- readxl::read_xlsx("aqs_sites.xlsx", sheet = "aqs_sites")

# Q6: How many monitoring sites are labelled as both RESIDENTIAL 
# for "Land Use" and SUBURBAN for "Location Setting"?

data_Q6 <- aqsSitesData %>% 
  filter(`Land Use` == "RESIDENTIAL", `Location Setting` == "SUBURBAN")

numResSitesSubUrb <- nrow(data_Q6)

# Q7: What is the median level of "EC PM2.5 LC TOR" amongst
# monitoring sites that are labelled as both "RESIDENTIAL" and 
# "SUBURBAN" in the eastern U.S., where eastern is defined as 
# Longitude greater than or equal to -100?

data_Q7 <- data_Q6

data_Q7_exp <- dplyr::left_join(data_Q7, dailySpecData, by = c("State Code" = "State Code", "County Code" = "County Code", 
                                         "Site Number" = "Site Num"))
  
data_Q7_fin <- data_Q7_exp %>% 
  filter(Longitude.x >= -100, `Parameter Name` == "EC PM2.5 LC TOR")

medLevECPM2.5Ques7 <- median(data_Q7_fin$`Arithmetic Mean`, na.rm = TRUE) 

# Q8: Amongst monitoring sites that are labeled as COMMERCIAL
# for "Land Use", which month of the year has the highest average
# levels of "Sulfate PM2.5 LC"? 

# Please note tha the year is not specified, although I checked this
# all corresponds to the year 2014. Only data for 2014 is available

data_Q8 <- left_join(aqsSitesData, dailySpecData, by = c("State Code" = "State Code", "County Code" = "County Code", 
                                                         "Site Number" = "Site Num"))

data_Q8_fin <- data_Q8 %>% 
  filter(`Land Use` == "COMMERCIAL", `Parameter Name` == "Sulfate PM2.5 LC") %>%
  mutate(`Date Local` = lubridate::ymd(`Date Local`), 
         monthLocal = lubridate::month(`Date Local`)) %>%
  group_by(monthLocal) %>%
  dplyr::summarise(avgLevSulfPmQues8 = mean(`Arithmetic Mean`, na.rm = TRUE))

# Q9

data_Q9 <- data_Q8

data_Q9_inter <- data_Q9 %>% 
  filter(`State Code` == 6, `County Code` == 65, `Site Number` == 8001) %>%
  select(`State Code`, `County Code`, `Site Number`,
         `Date Local`, `Parameter Name`, `Arithmetic Mean`)

data_Q9_fin <- data_Q9_inter %>% 
  group_by(`State Code`, `County Code`, `Site Number`, `Parameter Name`, 
           `Date Local`) %>%
  dplyr::summarise(n = n(), avgArMean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  filter(`Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(`Date Local`) %>% dplyr::summarise(sumSulfateAndTotalNitrate = sum(avgArMean, na.rm = TRUE))

# Q10:

data_Q10 <- data_Q8

data_Q10_int <- data_Q10 %>%
  select(`State Code`, `County Code`, `Site Number`, `Parameter Name`, `Arithmetic Mean`, `Date Local`) 

  
data_Q10_fin <- data_Q10_int %>% 
  filter(`Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(`State Code`, `County Code`, `Site Number`, `Parameter Name`, `Date Local`) %>%
  dplyr::summarize(avgArMean = mean(`Arithmetic Mean`, na.rm = TRUE), n = n()) %>%
  pivot_wider(names_from = `Parameter Name`, 
              values_from = avgArMean) %>% 
  mutate(cor = NA) %>% 
  group_by(`State Code`, `County Code`, `Site Number`) %>%
  dplyr::summarise(correlSulfAndTotNit = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`))


Q10_ans <- data_Q10_fin[which(data_Q10_fin$correlSulfAndTotNit == max(data_Q10_fin$correlSulfAndTotNit, na.rm = TRUE)), ]
