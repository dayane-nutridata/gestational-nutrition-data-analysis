# PROJECT: Gestational Nutrition Data Analysis - Intervention Analysis
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
if(!require(geepack)) install.packages("geepack");library(geepack)
if(!require(readxl)) install.packages("readxl");library(readxl)

#Uploading df
pregnancy_df <- read_excel("C:/Users/dayane.bittencourt/Desktop/gestational-nutrition-data-analysis/data/pregnancy_df.xlsx", sheet = 1)

# Define the output directory path
output_dir <- "C:/Users/dayane.bittencourt/Desktop/gestational-nutrition-data-analysis/outputs/2_intervention"

# ========================================================================
#          MEAL PLAN ADHERENCE
# ========================================================================
# Import the dataset
df <- read_excel("pregnancy_df.xlsx")

# View the first few rows and structure
head(df)
str(df)
# Select only the adherence columns
adherence_columns <- c("Meal_plan_adherence_R1", "Meal_plan_adherence_R2", 
                       "Meal_plan_adherence_R3", "Meal_plan_adherence_R4")

# Create a function or loop to view frequency tables (including NAs/missing values)
lapply(df[adherence_columns], function(x) {
  table(x, useNA = "ifany")
})

# To get percentage proportions considering only those who attended:
lapply(df[adherence_columns], function(x) {
  prop.table(table(x)) * 100
})

# Convert to long format
df_long <- df %>%
  select(System_number, all_of(adherence_columns)) %>%
  pivot_longer(
    cols = all_of(adherence_columns),
    names_to = "Return",
    values_to = "Adherence",
    names_prefix = "Meal_plan_adherence_"
  ) %>%
  mutate(
    Adherence_f = factor(Adherence, 
                         levels = 0:3, 
                         labels = c("Not following", "Low", "Partial", "Adequate")),
    Return = factor(Return, levels = c("R1", "R2", "R3", "R4"))
  )

# 100% Stacked Bar Chart (Proportion of each category per return)
ggplot(df_long %>% filter(!is.na(Adherence_f)), aes(x = Return, fill = Adherence_f)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Evolution of Meal Plan Adherence by Return",
    x = "Return Visit",
    y = "Proportion of Pregnant Women",
    fill = "Adherence Level"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")


# Count how many pregnant women attended each return visit
df %>%
  summarise(
    R1 = sum(!is.na(Meal_plan_adherence_R1)),
    R2 = sum(!is.na(Meal_plan_adherence_R2)),
    R3 = sum(!is.na(Meal_plan_adherence_R3)),
    R4 = sum(!is.na(Meal_plan_adherence_R4))
  )

ggsave(
  filename = file.path(output_dir, "1.meal_adherence_evolution.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)

# ========================================================================
#    WEIGHT CONTROL
# ========================================================================
# Select the weight control columns
weight_control_cols <- c("Weight_control_R1", "Weight_control_R2", 
                         "Weight_control_R3", "Weight_control_R4")

# View absolute frequencies (including NAs/dropouts)
lapply(df[weight_control_cols], function(x) {
  table(x, useNA = "ifany")
})

# View percentage proportions considering only those who attended each visit
lapply(df[weight_control_cols], function(x) {
  prop.table(table(x)) * 100
})

# Convert to long format
df_wc_long <- df %>%
  select(System_number, all_of(weight_control_cols)) %>%
  pivot_longer(
    cols = all_of(weight_control_cols),
    names_to = "Return",
    values_to = "Weight_Control",
    names_prefix = "Weight_control_"
  ) %>%
  mutate(
    Weight_Control_f = factor(Weight_Control, 
                              levels = c(0, 1), 
                              labels = c("No (0)", "Yes (1)")),
    Return = factor(Return, levels = c("R1", "R2", "R3", "R4"))
  )

# 100% Stacked Bar Chart for Weight Control across returns
ggplot(df_wc_long %>% filter(!is.na(Weight_Control_f)), aes(x = Return, fill = Weight_Control_f)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Weight Control by Return Visit",
    x = "Return Visit",
    y = "Percentage of Pregnant Women",
    fill = "Weight Control"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No (0)" = "#E74C3C", "Yes (1)" = "#2ECC71"))

ggsave(
  filename = file.path(output_dir, "2.Weight_Control_by_return.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)

# Calculate individual summary metrics
df_wc_summary <- df %>%
  rowwise() %>%
  mutate(
    # Proportion of successful weight control among attended visits (0 to 1 scale)
    Consistency_Weight_Control = mean(c(Weight_control_R1, Weight_control_R2, 
                                        Weight_control_R3, Weight_control_R4), na.rm = TRUE),
    
    # Total number of visits with successful weight control (Yes = 1)
    Total_Success_Visits = sum(c(Weight_control_R1, Weight_control_R2, 
                                 Weight_control_R3, Weight_control_R4) == 1, na.rm = TRUE),
    
    # Total returns attended
    Total_Returns_Attended = sum(!is.na(c(Weight_control_R1, Weight_control_R2, 
                                          Weight_control_R3, Weight_control_R4)))
  ) %>%
  ungroup()

# View summary statistics of the consistency score
summary(df_wc_summary$Consistency_Weight_Control)

ggplot(df_wc_summary, aes(x = Consistency_Weight_Control)) +
  geom_histogram(binwidth = 0.2, fill = "forestgreen", color = "white") +
  labs(
    title = "Distribution of Weight Control Consistency per Patient",
    x = "Consistency Ratio (0 = Never, 1 = Always controlled)",
    y = "Number of Pregnant Women"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "3.Weight_Control_per_patient.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)
# ========================================================================
#    GLYCEMIC CONTROL
# ========================================================================
# Select the GC adequate columns for returns
gc_adequate_cols <- c("GC_adequate_R1", "GC_adequate_R2", 
                      "GC_adequate_R3", "GC_adequate_R4")

# View absolute frequencies (including NAs/dropouts)
lapply(df[gc_adequate_cols], function(x) {
  table(x, useNA = "ifany")
})

# View percentage proportions considering only those who attended each visit
lapply(df[gc_adequate_cols], function(x) {
  prop.table(table(x)) * 100
})

# Convert to long format
df_gc_long <- df %>%
  select(System_number, all_of(gc_adequate_cols)) %>%
  pivot_longer(
    cols = all_of(gc_adequate_cols),
    names_to = "Return",
    values_to = "GC_Adequate",
    names_prefix = "GC_adequate_"
  ) %>%
  mutate(
    GC_Adequate_f = factor(GC_Adequate, 
                           levels = c(0, 1), 
                           labels = c("Inadequate (0)", "Adequate (1)")),
    Return = factor(Return, levels = c("R1", "R2", "R3", "R4"))
  )

# 100% Stacked Bar Chart for Glycemic Control Adequacy across returns
ggplot(df_gc_long %>% filter(!is.na(GC_Adequate_f)), aes(x = Return, fill = GC_Adequate_f)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Adequate Glycemic Control by Return Visit",
    x = "Return Visit",
    y = "Percentage of Pregnant Women",
    fill = "Glycemic Control Adequacy"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Inadequate (0)" = "#E74C3C", "Adequate (1)" = "#2ECC71"))

ggsave(
  filename = file.path(output_dir, "4.Glycemic_control_by_Return.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)

# Calculate individual summary metrics for GC adequacy
df_gc_summary <- df %>%
  rowwise() %>%
  mutate(
    # Consistency ratio of adequate glycemic control among attended visits (0 to 1)
    Consistency_GC_Adequate = mean(c(GC_adequate_R1, GC_adequate_R2, 
                                     GC_adequate_R3, GC_adequate_R4), na.rm = TRUE),
    
    # Total number of visits with adequate glycemic control (Yes = 1)
    Total_Adequate_Visits = sum(c(GC_adequate_R1, GC_adequate_R2, 
                                  GC_adequate_R3, GC_adequate_R4) == 1, na.rm = TRUE),
    
    # Total returns attended
    Total_Returns_Attended = sum(!is.na(c(GC_adequate_R1, GC_adequate_R2, 
                                          GC_adequate_R3, GC_adequate_R4)))
  ) %>%
  ungroup()

# View summary statistics of the consistency ratio
summary(df_gc_summary$Consistency_GC_Adequate)
ggplot(df_gc_summary, aes(x = Consistency_GC_Adequate)) +
  geom_histogram(binwidth = 0.2, fill = "purple", color = "white") +
  labs(
    title = "Distribution of Glycemic Control Adequacy Consistency per Patient",
    x = "Consistency Ratio (0 = Never adequate, 1 = Always adequate)",
    y = "Number of Pregnant Women"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "5.Glycemic_control_per_patient.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)
# ========================================================================
#    INSULIN USE
# ========================================================================
# Select all insulin use columns (including initial assessment)
insulin_cols <- c("Insulin_use", "Insulin_use_R1", "Insulin_use_R2", 
                  "Insulin_use_R3", "Insulin_use_R4")

# View absolute frequencies (including NAs/dropouts)
lapply(df[insulin_cols], function(x) {
  table(x, useNA = "ifany")
})

# View percentage proportions considering only attending patients
lapply(df[insulin_cols], function(x) {
  prop.table(table(x)) * 100
})
# Convert to long format
df_insulin_long <- df %>%
  select(System_number, all_of(insulin_cols)) %>%
  pivot_longer(
    cols = all_of(insulin_cols),
    names_to = "Visit",
    values_to = "Insulin_Use"
  ) %>%
  mutate(
    Visit = factor(Visit, 
                   levels = insulin_cols, 
                   labels = c("Initial", "R1", "R2", "R3", "R4")),
    Insulin_Use_f = factor(Insulin_Use, 
                           levels = c(0, 1), 
                           labels = c("No (0)", "Yes (1)"))
  )

# Stacked Bar Chart for Insulin Use prevalence across visits
ggplot(df_insulin_long %>% filter(!is.na(Insulin_Use_f)), aes(x = Visit, fill = Insulin_Use_f)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Insulin Use across Visits",
    x = "Appointment Visit",
    y = "Percentage of Pregnant Women",
    fill = "Insulin Use"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No (0)" = "#3498DB", "Yes (1)" = "#E74C3C"))
# Calculate patient-level summary metrics
df_insulin_summary <- df %>%
  rowwise() %>%
  mutate(
    # Total returns where insulin was used (Yes = 1)
    Total_Insulin_Visits = sum(c(Insulin_use, Insulin_use_R1, Insulin_use_R2, 
                                 Insulin_use_R3, Insulin_use_R4) == 1, na.rm = TRUE),
    
    # Ever used insulin at any point (0 = Never, 1 = Ever used)
    Ever_Used_Insulin = ifelse(Total_Insulin_Visits > 0, 1, 0),
    
    # Proportion of attended visits requiring insulin
    Insulin_Visit_Ratio = mean(c(Insulin_use, Insulin_use_R1, Insulin_use_R2, 
                                 Insulin_use_R3, Insulin_use_R4), na.rm = TRUE)
  ) %>%
  ungroup()

# Frequency of patients who ever required insulin during follow-up
table(df_insulin_summary$Ever_Used_Insulin)

ggsave(
  filename = file.path(output_dir, "6.Insulin_use.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)

# ========================================================================
#    MAIN NUTRITIONAL DIAGNOSES
# ========================================================================
# Define label mapping for Nutritional Diagnosis codes
diag_labels <- c(
  "1"  = "Disordered eating pattern",
  "2"  = "Excessive carbohydrate intake",
  "3"  = "Excessive carbohydrate intake (simple)",
  "4"  = "Suboptimal fiber intake",
  "5"  = "Suboptimal calcium intake",
  "6"  = "Suboptimal protein intake",
  "7"  = "Altered GI function (constipation)",
  "8"  = "Overweight/obesity (with adequate weight control)",
  "9"  = "Excessive oral intake",
  "10" = "No nutrition diagnosis at this time",
  "11" = "Altered GI function (nausea/morning sickness)",
  "12" = "Suboptimal oral intake",
  "13" = "Excessive sodium intake",
  "14" = "Excessive intake of ultra-processed foods",
  "15" = "Excessive caffeine intake"
)

# Select diagnosis columns and convert to long format
df_diag_long <- df %>%
  select(System_number, Nutritional_diagnosis, Nutritional_diagnosis_R1, 
         Nutritional_diagnosis_R2, Nutritional_diagnosis_R3, Nutritional_diagnosis_R4) %>%
  pivot_longer(
    cols = starts_with("Nutritional_diagnosis"),
    names_to = "Visit",
    values_to = "Diagnosis_Code"
  ) %>%
  mutate(
    Visit = factor(Visit, 
                   levels = c("Nutritional_diagnosis", "Nutritional_diagnosis_R1", 
                              "Nutritional_diagnosis_R2", "Nutritional_diagnosis_R3", 
                              "Nutritional_diagnosis_R4"),
                   labels = c("Initial Assessment", "Return 1", "Return 2", "Return 3", "Return 4")),
    Diagnosis_Label = factor(Diagnosis_Code, 
                             levels = names(diag_labels), 
                             labels = diag_labels)
  )
# Top 3 Nutritional Diagnoses per Visit
top3_diagnoses <- df_diag_long %>%
  filter(!is.na(Diagnosis_Label)) %>%
  group_by(Visit, Diagnosis_Label) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  slice_max(order_by = Count, n = 3, with_ties = TRUE) %>%
  arrange(Visit, desc(Count))

# Print top 3 table
print(top3_diagnoses)
# Horizontal bar plot of top diagnoses per visit
ggplot(top3_diagnoses, aes(x = reorder(Diagnosis_Label, Count), y = Count, fill = Visit)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Visit, scales = "free_y", ncol = 2) +
  labs(
    title = "Top Nutritional Diagnoses by Appointment",
    x = "Nutritional Diagnosis",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "7.Main_nutritional_diagnoses.png"),
  plot = last_plot(),
  width = 8, 
  height = 6, 
  dpi = 300
)
# ========================================================================
# CORRELATIONS BETWEEN GLYCEMIC CONTROL & MEAL PLAN ADHERENCE,WEIGHT CONTROL,INSULIN USE, NUTRITIONAL DIAGNOSIS
# ========================================================================
# Reshape data from wide to long format for returns R1 to R4
df_long_gc <- df %>%
  select(System_number, 
         starts_with("GC_adequate_R"), 
         starts_with("Meal_plan_adherence_R"), 
         starts_with("Weight_control_R"), 
         starts_with("Insulin_use_R"), 
         starts_with("Nutritional_diagnosis_R")) %>%
  pivot_longer(
    cols = -System_number,
    names_to = c(".value", "Return"),
    names_pattern = "(.*)_(R[1-4])"
  ) %>%
  mutate(
    Return = factor(Return, levels = c("R1", "R2", "R3", "R4")),
    System_number = as.factor(System_number),
    # Convert Nutritional diagnosis to factor if treating categorically
    Nutritional_diagnosis = as.factor(Nutritional_diagnosis)
  ) %>%
  drop_na(GC_adequate) # Keep rows where glycemic control outcome is available

# Check the prepared data
head(df_long_gc)

# Fit Logistic GEE model with GC_adequate as the outcome
gee_gc_comparison <- geeglm(
  GC_adequate ~ Return + Meal_plan_adherence + Weight_control + Insulin_use + Nutritional_diagnosis,
  id = System_number,
  data = df_long_gc %>% drop_na(Meal_plan_adherence, Weight_control, Insulin_use, Nutritional_diagnosis),
  family = binomial(link = "logit"),
  corstr = "exchangeable" # Assumes uniform correlation across the 4 return visits
)

# View the comprehensive model summary
summary(gee_gc_comparison)