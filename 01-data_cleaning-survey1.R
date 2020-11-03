#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from voter study group
# Author: Yuxin Xie, Zehui Yu, Xinyi Chen
# Data: 2 November 2020
# Contact: xinyic.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


library(haven)
library(tidyverse)
setwd("/Users/chenxinyi/Desktop/PS3 -304")
# Read in the raw data
raw_data <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data_survey <- 
  raw_data %>% 
  select(registration,
         vote_intention,
         vote_2020,
         gender,
         education,
         age)

rm(raw_data)  
reduced_data_survey$age <- as.numeric(reduced_data_survey$age)

filtered_data_survey<- reduced_data_survey %>% 
  filter(registration == "Registered" &
           vote_intention == "Yes, I will vote" &
           (vote_2020 == "Donald Trump"| vote_2020 == "Joe Biden"))
filtered_data_survey <- na.omit(filtered_data_survey)  

# map data style
filtered_data_survey <- filtered_data_survey %>%
  mutate(agegroup = case_when(age <= 20 ~ '20 or less',
                              age > 20 & age <= 35 ~ '21 to 35',
                              age > 35 & age <= 50 ~ '35 to 50',
                              age > 50 & age <= 65 ~ '50 to 65',
                              age > 65 & age <= 80 ~ '65 to 80',
                              age > 80 ~ 'above 80'))
unique(filtered_data_survey$agegroup)

filtered_data_survey$education[filtered_data_survey$education=='Other post high school vocational training'] <-"High school graduate"
filtered_data_survey$education[filtered_data_survey$education=='Completed some graduate, but no degree'] <-"College Degree (such as B.A., B.S.)"
unique(filtered_data_survey$education)

unique(filtered_data_survey$gender)


  
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(filtered_data_survey, "PS3-survey.csv")

