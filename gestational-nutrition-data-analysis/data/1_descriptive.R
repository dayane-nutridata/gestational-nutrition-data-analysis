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
if (!require("writexl")) install.packages("writexl");library(writexl)

# to read Office Calc
if(!require(readODS)) install.packages("readODS");library(readODS)

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

# Simularing 4 returs
simular_retornos <- function(df) {
  n <- nrow(df)
  
  # Using the column 'Name' as the only identificator
  col_id <- "Name"
  retornos_df <- data.frame(id_temp = df[[col_id]])
  names(retornos_df)[1] <- col_id
  
  # Assigning a baseline adherence profile
  paciente_adesao <- runif(n, min = 0.5, max = 0.95)
  
  # A tool to track which pregnant women have dropped out of follow-up care during previous appointments
  status_abandono <- rep(FALSE, n)
  
  for (i in 1:4) {
    # Average gestational age per follow-up visit (R1: ~14 sem, R2: ~22 sem, R3: ~30 sem, R4: ~38 sem)
    base_semana <- c(14, 22, 30, 38)[i]
    preg_age <- round(rnorm(n, mean = base_semana, sd = 1.5))
    preg_age[preg_age < 10] <- 10
    preg_age[preg_age > 42] <- 42
    
    ret_status <- numeric(n)
    
    for (j in 1:n) {
      if (status_abandono[j]) {
        # If a pregnant woman has previously dropped out of prenatal care, she remains absent
        ret_status[j] <- sample(c(0, 2, 3), 1, prob = c(0.5, 0.3, 0.2))
      } else {
        # The likelihood of attendance decreases slightly as subsequent sessions take place
        prob_comp <- paciente_adesao[j] - (i * 0.04) 
        prob_comp <- max(min(prob_comp, 0.9), 0.15)
        
        # Randomly determine whether they attended (1) or were absent (0, 2, 3)
        compareceu_j <- runif(1) < prob_comp
        
        if (compareceu_j) {
          ret_status[j] <- 1
        } else {
          # If absent, draw the reason for the absence by lot.
          ret_status[j] <- sample(c(0, 2, 3), 1, prob = c(0.6, 0.25, 0.15))
          
          # If there was a simple absence (0) or a rescheduling due to a prior absence (3), 
          # there is a chance she might abandon future returns
          if (ret_status[j] %in% c(0, 3) && runif(1) < 0.35) {
            status_abandono[j] <- TRUE
          }
        }
      }
    }
    
    # Attendance validation (if present = 1, clinical data are generated; otherwise, NA is generated)
    compareceu <- (ret_status == 1)
    
    # Return variables
    appointment    <- ifelse(compareceu, 1, 0)
    meal_adherence <- ifelse(compareceu, sample(0:3, n, replace = TRUE, prob = c(0.2, 0.3, 0.35, 0.15)), NA)
    weight         <- ifelse(compareceu, round(rnorm(n, mean = 70 + (i * 1.5), sd = 10), 1), NA)
    height_dummy   <- 1.62 # Altura média estimada para o cálculo do IMC
    bmi            <- ifelse(compareceu, round(weight / (height_dummy^2), 1), NA)
    weight_control <- ifelse(compareceu, sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)), NA)
    water_intake   <- ifelse(compareceu, round(runif(n, min = 1.0, max = 3.5), 1), NA)
    bowel_mov      <- ifelse(compareceu, sample(1:5, n, replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.1, 0.1)), NA)
    glycemia       <- ifelse(compareceu, round(rnorm(n, mean = 88, sd = 12)), NA)
    gc             <- ifelse(compareceu, sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4)), NA)
    gc_adequate    <- ifelse(compareceu, ifelse(gc == 1, sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)), 0), NA)
    insulin        <- ifelse(compareceu, sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15)), NA)
    nutri_diag     <- ifelse(compareceu, sample(1:15, n, replace = TRUE), NA)
    
    # Creating current return data frame
    df_ret_i <- data.frame(
      Appointment           = appointment,
      Return                = ret_status,
      Pregnancy_age         = preg_age,
      Meal_plan_adherence   = meal_adherence,
      Weight                = weight,
      BMI                   = bmi,
      Weight_control        = weight_control,
      Daily_water_intake    = water_intake,
      Bowel_movements       = bowel_mov,
      Fasting_blood_glucose = glycemia,
      GC                    = gc,
      GC_adequate           = gc_adequate,
      Insulin_use           = insulin,
      Nutritional_diagnosis = nutri_diag
    )
    
    # Renaming the columns by adding the return suffix (_R1, _R2, _R3, _R4)
    names(df_ret_i) <- paste0(names(df_ret_i), "_R", i)
    
    # Includes the ID column in the current return data frame
    df_ret_i[[col_id]] <- df[[col_id]]
    
    # Perform an incremental merge based on the "Name" column
    retornos_df <- left_join(retornos_df, df_ret_i, by = col_id)
  }
  
  return(retornos_df)
}

# 1. Cleans up old return columns in pregnancy_df
colunas_antigas <- grep("_R[1-4]$", names(pregnancy_df), value = TRUE)
if(length(colunas_antigas) > 0) {
  pregnancy_df <- pregnancy_df %>% select(-all_of(colunas_antigas))
}

# 2. Generates the new corrected return data
df_retornos <- simular_retornos(pregnancy_df)

# 3. Merging the new, updated data with the original `pregnancy_df` using the "Name" column
pregnancy_df <- left_join(pregnancy_df, df_retornos, by = "Name")

# Viewing first few rows
head(pregnancy_df)

# saving on 'outputs' paste
write_xlsx(pregnancy_df, "C:/Users/dayane.bittencourt/Desktop/gestational-nutrition-data-analysis/data/pregnancy_df.xlsx")

#====================================================================================
#                                 DESCRIPTIVE ANALYSES
#====================================================================================
# 2. Define the path to the 'outputs' directory
output_path <- "C:/Users/dayane.bittencourt/Desktop/gestational-nutrition-data-analysis/outputs/1_descriptive"

# Create the directory if it does not already exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# ==========================================
# 1. NUMBER OF CONSULTATIONS (FIRST + FOLLOW-UPS)
# ==========================================
n_initial <- nrow(pregnancy_df)
n_r1 <- sum(pregnancy_df$Return_R1 == 1, na.rm = TRUE)
n_r2 <- sum(pregnancy_df$Return_R2 == 1, na.rm = TRUE)
n_r3 <- sum(pregnancy_df$Return_R3 == 1, na.rm = TRUE)
n_r4 <- sum(pregnancy_df$Return_R4 == 1, na.rm = TRUE)

df_consultations <- data.frame(
  Consultation_Type = c("Initial Visit", "Follow-up 1", "Follow-up 2", "Follow-up 3", "Follow-up 4"),
  Count = c(n_initial, n_r1, n_r2, n_r3, n_r4)
)

df_consultations$Consultation_Type <- factor(df_consultations$Consultation_Type, levels = df_consultations$Consultation_Type)

p1 <- ggplot(df_consultations, aes(x = Consultation_Type, y = Count, fill = Consultation_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  theme_minimal() +
  labs(title = "Number of Pregnant Women per Consultation Visit",
       x = "Consultation Type", y = "Number of Patients") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(output_path, "1_number_of_consultations.png"), p1, width = 7, height = 5)

# ==========================================
# 2. AGE GROUP OF PREGNANT WOMEN
# ==========================================
pregnancy_df <- pregnancy_df %>%
  mutate(Age_Group = case_when(
    Age_years <= 19 ~ "Up to 19 years",
    Age_years >= 20 & Age_years <= 30 ~ "20 to 30 years",
    Age_years >= 31 & Age_years <= 35 ~ "31 to 35 years",
    Age_years >= 36 & Age_years <= 39 ~ "36 to 39 years",
    Age_years >= 40 & Age_years <= 44 ~ "40 to 44 years",
    Age_years >= 45 ~ "45 years or older"
  ))

age_levels <- c("Up to 19 years", "20 to 30 years", "31 to 35 years", "36 to 39 years", "40 to 44 years", "45 years or older")
pregnancy_df$Age_Group <- factor(pregnancy_df$Age_Group, levels = age_levels)

p2 <- ggplot(pregnancy_df[!is.na(pregnancy_df$Age_Group), ], aes(x = Age_Group)) +
  geom_bar(fill = "steelblue", width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution by Age Group", x = "Age Group", y = "Count") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(output_path, "2_age_groups.png"), p2, width = 8, height = 5)

# ==========================================
# 3. NUMBER OF PREGNANCIES
# ==========================================
pregnancy_df$N_pregnancies_cat <- factor(pregnancy_df$N_pregnancies, 
                                         levels = 1:4, 
                                         labels = c("First", "Second", "Third", "Fourth or more"))

p3 <- ggplot(pregnancy_df[!is.na(pregnancy_df$N_pregnancies_cat), ], aes(x = N_pregnancies_cat)) +
  geom_bar(fill = "darkseagreen", width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Number of Pregnancies", x = "Pregnancy Count", y = "Count")

ggsave(file.path(output_path, "3_number_of_pregnancies.png"), p3, width = 6, height = 4)

# ==========================================
# 4. GESTATIONAL AGE AT FIRST APPOINTMENT (TRIMESTRES)
# ==========================================
pregnancy_df <- pregnancy_df %>%
  mutate(Trimester = case_when(
    Pregnancy_age_PA_weeks <= 13 ~ "1st Trimester (up to 13 weeks)",
    Pregnancy_age_PA_weeks >= 14 & Pregnancy_age_PA_weeks <= 27 ~ "2nd Trimester (14–27 weeks)",
    Pregnancy_age_PA_weeks >= 28 ~ "3rd Trimester (≥ 28 weeks)"
  ))

trimester_levels <- c("1st Trimester (up to 13 weeks)", "2nd Trimester (14–27 weeks)", "3rd Trimester (≥ 28 weeks)")
pregnancy_df$Trimester <- factor(pregnancy_df$Trimester, levels = trimester_levels)

p4 <- ggplot(pregnancy_df[!is.na(pregnancy_df$Trimester), ], aes(x = Trimester)) +
  geom_bar(fill = "coral", width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Gestational Age Stratification at First Appointment", x = "Trimester", y = "Count")

ggsave(file.path(output_path, "4_gestational_trimester.png"), p4, width = 7, height = 4)

# ==========================================
# 5. PRE-GESTATIONAL BMI CLASSIFICATION
# ==========================================
pregnancy_df <- pregnancy_df %>%
  mutate(BMI_class = case_when(
    BMI_pre_pregnancy < 18.5 ~ "Underweight",
    BMI_pre_pregnancy >= 18.5 & BMI_pre_pregnancy < 25 ~ "Adequate",
    BMI_pre_pregnancy >= 25 & BMI_pre_pregnancy < 30 ~ "Overweight",
    BMI_pre_pregnancy >= 30 & BMI_pre_pregnancy < 35 ~ "Obesity Class I",
    BMI_pre_pregnancy >= 35 & BMI_pre_pregnancy < 40 ~ "Obesity Class II",
    BMI_pre_pregnancy >= 40 ~ "Obesity Class III"
  ))

bmi_levels <- c("Underweight", "Adequate", "Overweight", "Obesity Class I", "Obesity Class II", "Obesity Class III")
pregnancy_df$BMI_class <- factor(pregnancy_df$BMI_class, levels = bmi_levels)

p5 <- ggplot(pregnancy_df[!is.na(pregnancy_df$BMI_class), ], aes(x = BMI_class)) +
  geom_bar(fill = "mediumpurple", width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Pre-gestational BMI Classification", x = "BMI Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(output_path, "5_pre_gestational_bmi.png"), p5, width = 8, height = 5)

# ==========================================
# 6. BLOOD GLUCOSE MONITORING
# ==========================================
pregnancy_df$GC_label <- factor(pregnancy_df$GC, levels = c(0, 1), labels = c("No", "Yes"))

p6 <- ggplot(pregnancy_df[!is.na(pregnancy_df$GC_label), ], aes(x = GC_label)) +
  geom_bar(fill = "indianred", width = 0.5) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Blood Glucose Monitoring (GC)", x = "Monitoring Blood Glucose", y = "Count")

ggsave(file.path(output_path, "6_blood_glucose_monitoring.png"), p6, width = 6, height = 4)

# ==========================================
# 7. NUTRITIONAL DIAGNOSIS AT FIRST CONSULTATION
# ==========================================
# Labeling codes from 1 to 15
nutri_labels <- c(
  "Disordered eating pattern",                               # 1
  "Excessive carbohydrate intake",                           # 2
  "Excessive carb intake (simple carbohydrates)",            # 3
  "Suboptimal fiber intake",                                 # 4
  "Suboptimal calcium intake",                               # 5
  "Suboptimal protein intake",                               # 6
  "Altered GI function (constipation)",                      # 7
  "Overweight/obesity (adequate control)",                   # 8
  "Excessive oral intake",                                   # 9
  "No nutrition diagnosis at this time",                     # 10
  "Altered GI function (nausea/morning sickness)",           # 11
  "Suboptimal oral intake",                                  # 12
  "Excessive sodium intake",                                 # 13
  "Excessive intake of ultra-processed foods",               # 14
  "Excessive caffeine intake"                                # 15
)

# Converting the column into a factor with descriptive labels
pregnancy_df$Nutritional_diagnosis_label <- factor(
  pregnancy_df$Nutritional_diagnosis, 
  levels = 1:15, 
  labels = nutri_labels
)

# Generating a horizontal bar chart
p7 <- ggplot(pregnancy_df[!is.na(pregnancy_df$Nutritional_diagnosis_label), ], 
             aes(x = Nutritional_diagnosis_label)) +
  geom_bar(fill = "goldenrod", width = 0.7) +
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.2, size = 3.5) +
  coord_flip() + # Inverte os eixos para que os nomes fiquem legíveis na vertical
  theme_minimal() +
  labs(title = "Nutritional Diagnosis (First Consultation)", 
       x = "Nutritional Diagnosis", y = "Count") +
  theme(
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Saving on 'outputs' folder
ggsave(file.path(output_path, "7_nutritional_diagnosis.png"), p7, width = 10, height = 7)

cat("Success! All plots have been generated and saved to:", output_path, "\n")
