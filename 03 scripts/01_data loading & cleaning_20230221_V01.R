# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of baseline legacy data (i.e., 2023)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2023-03-01
# Background: I worked as part of a team to complete the midterm evaluation for an international client. 
# The client project was about education. Amongst others, the client was interested in whether or not 
# students at project schools exhibited higher levels of self-reported school engagement compared to 
# students control at control schools. The example project presented here contains the latent factor 
# modelling to answer this question. To do so, the lavaan package in R was used. At the time of writing, 
# a major caveat of the package has been that the output tables could not be exported into csv automatically 
# using tools such as stargazer. Thus, some of the statistics produced had to be manually included into 
# the report prepared for the client. 
# Purpose: Clean up and make the data set ready for complex survey analysis



# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/third-sample-analysis")
MAR_ORIGINAL <- par("mar")
par(mar = c(5, 4, 1, 1))
rm(list = ls())
options(scipen = 999)



# Packages --------------------------- --------------------------- ---------------------------
library(readxl)
library(tidyverse)
library(robotoolbox)
library(janitor)



# Import and clean up midline (primary school) data --------------------------- --------------------------- ---------------------------
# Import the data from kobo toolbox. The credentials are listed below. 

kobo_setup(url = "https://kf.kobotoolbox.org/",
           token = kobo_token(username = "domib",
                              password = "******",
                              url = "https://kf.kobotoolbox.org"))
assets <- kobo_asset_list()
uid <- assets %>%
  filter(assets$name == "5.1 استمارة الطلبة من صف 1-4") %>%
  pull(uid) %>%
  first()
asset <- kobo_asset(uid)
midterm <- kobo_submissions(kobo_asset(uid))
rm(asset, assets, uid)

# Variables of interest were renamed to make their names more accessible. Redundant variables were removed.  
midterm <- midterm %>%
  rename(West_Bank = `PS0.1`,
         "code" = `PS0.4`,
         "type" = `PS0.45`,
         "female" = `PS2`,
         "age" = `PS3`,
         "school_sharing_ideas" = PS5,
         "school_asking_questions" = PS6,         
         "school_ask_help" = PS7) %>%
  mutate(code = as.numeric(as.character(code)),
         female = ifelse(female == "Female", 1, 0),
         West_Bank = ifelse(West_Bank == "westbank", 1, 0),
         schools_boys = ifelse(type == "boys", 1, 0),
         schools_girls = ifelse(type == "girls", 1, 0),
         ) %>%
  select(West_Bank, code, schools_boys, schools_girls,
         female, age,
         school_sharing_ideas, school_asking_questions, school_ask_help)



# Clean up engagement scale
# The items that operationalize school engagement need to be numerical. 
midterm <- midterm %>%
  mutate(across(c(school_sharing_ideas, school_asking_questions, school_ask_help), 
            ~ recode(., "never" = 0, "Rarely" = 1, "Sometimes" = 2, "Often" = 3)))



# Import and clean up midline school sample (meta) data --------------------------- --------------------------- ---------------------------
# The midterm school survey does not contain information on treatment status of project schools. 
# This data is only provided in the school sample file 
school_sample <- read_xlsx("01 raw data/midline_school sample_20230220_V01.xlsx") %>%
  rename("code" = Code) %>%
  select(code, Status)



# Merge student and school data and do final clean up --------------------------- --------------------------- ---------------------------
# To carry out the latent factor analysis merge midterm and school-sample file. Also perform some data cleaning
midterm <- merge(midterm, school_sample, by = "code", all.x = TRUE) %>%
  select(West_Bank, code, schools_boys, schools_girls, Status,
         female, age,
         school_sharing_ideas, school_asking_questions, school_ask_help) %>%
  mutate(Status = ifelse(Status == "Treatment", 1, 0)) %>%
  rename("treatment" = Status) %>%
  filter(!is.na(treatment))
rm(school_sample)



# Save midline data --------------------------- --------------------------- ---------------------------
write.csv(midterm, "02 processed data/midline_clean_20230221.csv", row.names = FALSE)




