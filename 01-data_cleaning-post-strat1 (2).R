#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from IPUMS USA
# Author: Yuxin Xie, Zehui Yu, Xinyi Chen
# Data: 2 November 2020
# Contact: xinyic.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/chenxinyi/Desktop/PS3 -304")
raw_data <- read_dta("usa_00002.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest
reduced_data_census <- 
  raw_data %>% 
  select(perwt,
         sex, 
         age,
         educd)

rm(raw_data)  
reduced_data_census$age <- as.numeric(reduced_data_census$age)

filtered_data_census <- reduced_data_census %>% filter(age >= 18)
filtered_data_census <- na.omit(filtered_data_census)    

#map data styles
filtered_data_census <- filtered_data_census %>%
  mutate(agegroup = case_when(age <= 20 ~ '20 or less',
                              age > 20 & age <= 35 ~ '21 to 35',
                              age > 35 & age <= 50 ~ '35 to 50',
                              age > 50 & age <= 65 ~ '50 to 65',
                              age > 65 & age <= 80 ~ '65 to 80',
                              age > 80 ~ 'above 80'))
unique(filtered_data_census$agegroup)

grade3.less <-c("no schooling completed","nursery school","preschool","kindergarden","grade 1","grade 2","grade 3")
grade4to8 <-c("grade 4","grade 5","grade 6", "grade 7", "grade 8")
grade9to11 <-c("grade 9","grade 10","grade 11", "12th grade, no diploma")
edu.highsch <- c("regular high school diploma", "	ged or alternative credential")
edu.somecoll <- c("1 or more years of college credit, no degree", "some college, but less than 1 year")

filtered_data_census <- filtered_data_census %>%
  mutate(educd2 = case_when(educd == "associate's degree, type not specified" ~ "Associate Degree",
                            educd == "master's degree" ~ "Masters degree",
                            educd == "doctoral degree" ~ "Doctorate degree",
                            educd == "bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd %in% edu.somecoll ~ "Completed some college, but no degree",
                            educd %in% edu.highsch ~ "High school graduate",
                            educd %in% grade9to11 ~ "Completed some high school",
                            educd %in% grade4to8 ~ "Middle School - Grades 4 - 8", 
                            educd %in% grade3.less ~"3rd Grade or less"))


filtered_data_census <- rename(filtered_data_census, education = educd2)
filtered_data_census$educd<- NULL
unique(filtered_data_census$education)

unique(filtered_data_census$sex)
filtered_data_census$sex<- ifelse(filtered_data_census$sex == "female","Female", "Male")
filtered_data_census <- rename(filtered_data_census, gender = sex)
unique(filtered_data_census$gender)

filtered_data <- na.omit(filtered_data_census) 
        

# Saving the census data as a csv file in my
# working directory
write_csv(filtered_data, "PS3-census.csv")



         