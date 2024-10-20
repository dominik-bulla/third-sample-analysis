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
# Purpose: Perform the latent factor modelling using lavaan.



# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/third-sample-analysis")
MAR_ORIGINAL <- par("mar")
par(mar = c(5, 4, 1, 1))
rm(list = ls())
options(scipen = 999)



# Packages --------------------------- --------------------------- ---------------------------
library(tidyverse)
library(lavaan)
library(psych)
library(janitor)
library(lme4)


# Import midline --------------------------- --------------------------- ---------------------------
midterm <- read.csv("02 processed data/midline_clean_20230221.csv")



# Some stats on the school sample (tabke 1) --------------------------- --------------------------- --------------------------- 
# resulting table could also have been generated using gt package as well. However, client preferred tables that
# could easily be edited in word. 
sample <- midterm %>%
  group_by(code) %>%
  summarise(Status = mean(treatment),
            `located in West Bank` = sum(West_Bank), 
            n = n(),
            female = sum(female)) %>%
  mutate(count = 1) %>%
  mutate(`located in West Bank` = ifelse(`located in West Bank` > 0, 1, 0)) %>%
  group_by(Status) %>%
  summarise(`# of schools` = n(),
            `located in West Bank (in %)` = round(sum(`located in West Bank`) / sum(count), 4) * 100,
            `# of students` = sum(n),
            `female (in %)` = round(sum(female) / sum(n), 4) * 100) %>%
  mutate(Status = ifelse(Status == 1, "Treatment", "Control")) %>%
  adorn_totals("row") 
write.csv(sample, "04 results/sample_overview_20230301.csv", row.names = FALSE)  



# item correlations (table 3) --------------------------- --------------------------- --------------------------- 
# Note that data is categorical. Thus, polychoric correlation analysis was performed. 
# resulting table could also have been generated using gt package as well. However, client preferred tables that
# could easily be edited in word. 
cor_matrix <- polychoric(midterm[, grep("school_", colnames(midterm))])
round(cor_matrix$rho, 2)
write.csv(round(cor_matrix$rho, 2), "04 results/item correlations_20230301.csv")



# item ICC (table 2) --------------------------- --------------------------- --------------------------- 
# ICCs provide an idnciation of the extent to which items vary at the between level. 
# In case of substantial between-school variation, I would have performed multilevel analysis.   
for (var in grep("school_", colnames(midterm))) {
  data <- midterm
  colnames(data)[var] <- "item"
  
  model <- lmer(item ~ 1 + (1 | code), data = data)
  summary(model)
  variance_components <- VarCorr(model)
  between_group_variance <- as.numeric(variance_components$code[1])
  within_group_variance <- attr(variance_components, "sc")^2
  icc <- between_group_variance / (between_group_variance + within_group_variance)
  print(round(icc, 2))
}
# ICC's are: .27/ .14/ .11
rm(data, model, variance_components, between_group_variance, within_group_variance, icc, var)


# Structural equation modeling to determine treatment effects --------------------------- --------------------------- ---------------------------
# The lavaan package does not yet provide a feature to conviniently export modeling output tables into csv. 
# Thus, factor and regression results were exported using 'write.csv'. This is not ideal but works. 
# Maybe write a function to automatize this?

# Latent model to be estimated
model_1 <- ' 
 # latent variables:
 
   engagement =~ school_sharing_ideas + school_asking_questions + school_ask_help
   
 # regression:
 
   engagement ~ West_Bank + female + age + treatment'

# Estimate latent model specified
fit <- sem(model = model_1, 
           data = midterm,
           ordered = c("school_sharing_ideas", "school_asking_questions", "school_ask_help"), 
           estimator = "WLSMV") 
summary(fit, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit)

results_factor <- parameterEstimates(fit)[1:3, c("rhs", "est", "se", "pvalue")] %>%
  mutate(est = round(est, 3),
         se = round(se, 3),
         pvalue = round(pvalue, 3))
results_regression <- parameterEstimates(fit)[4:7, c("rhs", "est", "se", "pvalue")] %>%
  mutate(est = round(est, 3),
         se = round(se, 3),
         pvalue = round(pvalue, 3))

write.csv(results_factor, "04 results/factor_results_20230301.csv", row.names = FALSE)
write.csv(results_regression, "04 results/regression_results_20230301.csv", row.names = FALSE)
rm(fit, model_1)

