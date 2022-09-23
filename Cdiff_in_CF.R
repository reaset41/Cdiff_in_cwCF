title: "C diff in cwCF"
author: "Seth Reasoner"
date: "8/25/2022"

#Packages
library(tidyverse)
library(gtsummary)

## read in data
cf <- read.csv("https://raw.githubusercontent.com/reaset41/Cdiff_in_cwCF/main/Cdiff_CF.csv")

## define variables of interest 
explanatory_vars <- c("Sex", "Age", "BMI", "F508_mutations", "PancreaticInsufficient", "Asthma", "GERD", "Prior_CDiff_diagnosis", "FeedingTube", "Probiotics","Immunosuppression_Steriods", "CFTRmodulators",  "PPI", "H2Blocker","Abx_30Days", "Hospitalization_90Days")

##univariate logistic regression 
cf%>% 
  select(NAAT_result,Sex, Age, BMI, F508_mutations, PancreaticInsufficient, Asthma, GERD, Prior_CDiff_diagnosis, FeedingTube, Probiotics, Immunosuppression_Steriods,CFTRmodulators,PPI, H2Blocker, Abx_30Days, Hospitalization_90Days) %>%
  tbl_uvregression(
    method = glm,
    y = NAAT_result,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2))


##multiple variable logistic regression model
mult_logistic <- glm(formula = NAAT_result ~ Sex + Age + BMI + F508_mutations + PancreaticInsufficient + Asthma + GERD + Prior_CDiff_diagnosis + FeedingTube + Probiotics + Immunosuppression_Steriods + CFTRmodulators  + PPI + H2Blocker + Abx_30Days + Hospitalization_90Days,  family = "binomial", data=cf)

summary(mult_logistic)

##odds ratio and 95% confidence interval from multiple variable logistic model
tbl_regression(mult_logistic, exponentiate = TRUE)


sessionInfo()
