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
library(tidyverse)
library(lavaan)
library(psych)
library(janitor)
library(lme4)


# Import midline --------------------------- --------------------------- ---------------------------
midterm <- read.csv("02 processed data/midline_clean_20230221.csv")



# Some stats on the school sample --------------------------- --------------------------- --------------------------- 

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



# item correlations --------------------------- --------------------------- --------------------------- 
cor_matrix <- polychoric(midterm[, grep("school_", colnames(midterm))])
round(cor_matrix$rho, 2)
write.csv(round(cor_matrix$rho, 2), "04 results/item correlations_20230301.csv")
rm(cor_matrix)


# item ICC --------------------------- --------------------------- --------------------------- 

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

model_1 <- ' 
 # latent variables:
 
   engagement =~ school_sharing_ideas + school_asking_questions + school_ask_help
   
 # regression:
 
   engagement ~ West_Bank + female + age + treatment'

fit <- sem(model = model_1, 
           data = midterm,
           ordered = c("school_sharing_ideas", "school_asking_questions", "school_ask_help"), 
           estimator = "WLSMV") 
summary(fit, fit.measures = TRUE, standardized = TRUE)

results_factor <- parameterEstimates(fit)[1:3, c("rhs", "est", "se", "pvalue")]
results_regression <- parameterEstimates(fit)[4:7, c("rhs", "est", "se", "pvalue")]

write.csv(results_factor, "04 results/factor_results_20230301.csv", row.names = FALSE)
write.csv(results_regression, "04 results/regression_results_20230301.csv", row.names = FALSE)
rm(fit, model_1)

