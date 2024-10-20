# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of baseline legacy data (i.e., 2023)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2023-03-15
# Background: The client was a consortium of Germany’s biggest NGOs working with refugee/ IDP communities in Asia, Africa, and 
# Latin America. The consortium initiated a project to strengthen child protection amongst refugees/ IDPs as well as their host 
# communities. To guide programming, I was tasked to design a pre/ post evaluation design. As part of the evaluation design, a 
# baseline was commissioned as well. I designed the baseline framework, which included of a multi-stage sampling design. Data 
# was collected in country by local consultants. The data was collected using KOBO toolbox. The data was then pulled into R using 
# an API. I used the data collected in country to perform the global baseline analysis. Within the current excerpt from the global 
# baseline analysis, I determine the typical socio-demographic profile of primary caregivers that were interviewed across the 
# different levels. To do so, I used sampling weights.
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
                              password = "!23KeZA2023",
                              url = "https://kf.kobotoolbox.org"))
assets <- kobo_asset_list()
uid <- assets %>%
  filter(assets$name == "5.1 استمارة الطلبة من صف 1-4") %>%
  pull(uid) %>%
  first()
asset <- kobo_asset(uid)
midterm <- kobo_submissions(kobo_asset(uid))
rm(asset, assets, uid)

midterm <- midterm %>%
  rename("territory" = `PS0.1`,
         "code" = `PS0.4`,
         "type" = `PS0.45`,
         "female" = `PS2`,
         "age" = `PS3`,
         school_preparing = GOAL1,
         school_catch_up = GOAL2,
         school_concentration = PERS1) %>%
  select(territory, code, 
         type,
         female, age,
         school_preparing, school_catch_up, school_concentration) %>%
  mutate(code = as.numeric(as.character(code)),
         female = ifelse(female == "Female", 1, 0),
         territory = as.factor(territory),
         type = as.factor(type)) %>%
  as.data.frame()



# Clean up social-connected scale
midterm <- midterm %>%
  mutate(across(c(school_preparing, school_catch_up, school_concentration), 
            ~ recode(., "never" = 0, "Rarely" = 1, "Sometimes" = 2, "Often" = 3)))



# We assume multilevel modeling (see ICC in analysis sript)





# Import and clean up midline school sample (meta) data --------------------------- --------------------------- ---------------------------
school_sample <- read_xlsx("01 raw data/midline_school sample_20230220_V01.xlsx") %>%
  rename("code" = Code) %>%
  select(code, Status)



# Merge student and school data and do final clean up --------------------------- --------------------------- ---------------------------
midterm <- merge(midterm, school_sample, by = "code", all.x = TRUE) %>%
  select(territory, code, type, Status,
         female, age,
         school_preparing, school_catch_up, school_concentration) %>%
  mutate(Status = ifelse(Status == "Treatment", 1, 0)) %>%
  rename("treatment" = Status)



# Save midline data --------------------------- --------------------------- ---------------------------
write.csv(midterm, "02 processed data/midline_clean_20230221.csv", row.names = FALSE)

