# PROJECT: Gestational Nutrition Data Analysis - Descriptive Analysis
# AUTHOR: Dayane Cristina Duda Bittencourt
# TARGET PROGRAM: The Europubhealth+ Programme - Erasmus Mundus Joint Master Degree
#
# DATA PRIVACY & ETHICS NOTICE:
# To comply with General Data Protection Regulation (GDPR) and Brazilian General
# Personal Data Protection Law (LGPD), all real clinical data has been fully 
# anonymized and modified to synthetic/fictional values. 
# The variables and structure reflect my actual clinical scenario
# for analysis and demonstration purposes only.

if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(epitools)) install.packages("epitools");library(epitools)
if(!require(summarytools)) install.packages("summarytools");library(summarytools)
if(!require(gtsummary)) install.packages("gtsummary");library(gtsummary)
if(!require(dplyr)) install.packages("dplyr");library(dplyr)
if(!require(janitor)) install.packages("janitor");library(janitor)
if(!require(broom)) install.packages("broom");library(broom)
if(!require(geepack)) install.packages("geepack");library(geepack)
if(!require(lme4)) install.packages("lme4");library(lme4)
if(!require(car)) install.packages("car");library(car)
if(!require(tidytext)) install.packages("tidytext");library(tidytext)
if(!require(ggalluvial)) install.packages("ggalluvial");library(ggalluvial)
if(!require(MASS)) install.packages("MASS");library(MASS) 
if(!require(scales)) install.packages("scales");library(scales)

# to read Office Calc
if(!require(readODS)) install.packages("readODS");library(readODS)

# upload file 
pregnancy_df <- read_ods("C:/Users/dayane.bittencourt//Desktop/gestational-nutrition-data-analysis/data/data.ods")

# ==============================================================================
# Synthetic Pregnant Women Dataset Generator (100 observations)
# ==============================================================================

set.seed(42) #reproducibility

n <- 100

# 1. Identification and Dates
appointment_day <- as.Date("2024-01-15") + sample(0:180, n, replace = TRUE)
system_number   <- sprintf("SYS-%05d", 10000 + 1:n)
name            <- sprintf("SUBJ_%03d", 1:n) # Anônimo (GDPR)
age_years       <- sample(18:42, n, replace = TRUE)
birthday        <- appointment_day - (age_years * 365.25 + sample(0:364, n, replace = TRUE))

# 2. Obstetric History
n_pregnancies          <- sample(c(1, 2, 3, 4), n, replace = TRUE, prob = c(0.45, 0.30, 0.15, 0.10))
abortion               <- sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.75, 0.18, 0.07)) # 0=no, 1=1, 2=2+
pregnancy_age_pa_weeks <- round(runif(n, 6, 38), 1)
twins                  <- sample(c(0, 1), n, replace = TRUE, prob = c(0.97, 0.03)) # 0 = no, 1 = yes

# 3. Referral and reasons
referral        <- sample(c(1, 2), n, replace = TRUE, prob = c(0.60, 0.40)) # 1 = BHU, 2 = WHC
reason_referral <- sample(1:16, n, replace = TRUE) # 1 a 16

# 4. BMI and heigth
height_meters        <- round(rnorm(n, mean = 1.63, sd = 0.06), 2)
bmi_pre_pregnancy    <- round(rnorm(n, mean = 24.8, sd = 3.8), 1)
weight_pre_pregnancy <- round(bmi_pre_pregnancy * (height_meters^2), 1)

# Weight gain proportional to pregnancy age
weight_gain <- (pregnancy_age_pa_weeks / 40) * runif(n, 6, 14)
weight_kg   <- round(weight_pre_pregnancy + weight_gain, 1)
bmi         <- round(weight_kg / (height_meters^2), 1)

# 5. Water intake, bowel movements, blood glucose and insulin use
daily_water_intake_ml <- sample(seq(1000, 3000, by = 250), n, replace = TRUE)
bowel_movements       <- sample(1:5, n, replace = TRUE, prob = c(0.50, 0.20, 0.15, 0.10, 0.05)) # 1 a 5
fasting_blood_glucose <- round(rnorm(n, mean = 87, sd = 11), 1)
gc                    <- ifelse(fasting_blood_glucose >= 92, 1, 0) # 0 = no, 1 = yes
gc_adequate           <- ifelse(gc == 0, 1, 0) # 0 = no, 1 = yes
insulin_use           <- ifelse(gc == 1, sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)), 0) # 0 = no, 1 = yes

# 6. Socioeconomic aspects and benefits
paid_job               <- sample(c(0, 1), n, replace = TRUE, prob = c(0.35, 0.65))
number_residents_house <- sample(1:6, n, replace = TRUE, prob = c(0.1, 0.35, 0.3, 0.15, 0.07, 0.03))
taxa_cambio_eur        <- 6.00
monthly_income_brl     <- round(pmax(rnorm(n, mean = 3300, sd = 1400), 1412), 2)
per_capita_income_brl  <- round(monthly_income_brl / number_residents_house, 2)
monthly_income_eur     <- round(monthly_income_brl / taxa_cambio_eur, 2)
per_capita_income_eur  <- round(per_capita_income_brl / taxa_cambio_eur, 2)
government_benefit     <- ifelse(monthly_income_brl < 2000, 1, sample(c(0, 1), n, replace = TRUE, prob = c(0.9, 0.1)))

# 7. Nutritional Diagnosis (Codes 1 to 15)
nutritional_diagnosis <- sample(1:15, n, replace = TRUE)

# ==============================================================================
# Creating Data Frame
# ==============================================================================

pregnancy_df <- data.frame(
  Appointment_day = appointment_day,
  System_number = system_number,
  Name = name,
  Birthday = birthday,
  Age_years = age_years,
  N_pregnancies = n_pregnancies,
  Abortion = abortion,
  Pregnancy_age_PA_weeks = pregnancy_age_pa_weeks,
  Twins = twins,
  Referral = referral,
  Reason_referral = reason_referral,
  Weight_pre_pregnancy = weight_pre_pregnancy,
  BMI_pre_pregnancy = bmi_pre_pregnancy,
  Height_meters = height_meters,
  Weight_kg = weight_kg,
  BMI = bmi,
  Daily_water_intake_ml = daily_water_intake_ml,
  Bowel_movements = bowel_movements,
  Fasting_blood_glucose = fasting_blood_glucose,
  GC = gc,
  GC_adequate = gc_adequate,
  Insulin_use = insulin_use,
  Paid_job = paid_job,
  Number_residents_house = number_residents_house,
  Monthly_income_eur = monthly_income_eur,
  Per_capita_income_eur = per_capita_income_eur,
  Government_benefit = government_benefit,
  Nutritional_diagnosis = nutritional_diagnosis
)

head(pregnancy_df)
