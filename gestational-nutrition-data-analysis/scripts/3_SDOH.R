# PROJECT: Gestational Nutrition Data Analysis - Social determinants of health (SDOH)
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
output_dir <- "C:/Users/dayane.bittencourt/Desktop/gestational-nutrition-data-analysis/outputs/3_SDOH"

# ==========================================================================
# HOUSEHOLD EARNINGS (monthly)
# ==========================================================================
# 1. Create salary brackets/ranges
# Adjust the breaks according to your data distribution (min ~235, max ~1164)
income_breaks <- c(0, 400, 600, 800, Inf)
income_labels <- c("< €400", "€400 - €600", "€600 - €800", "> €800")

df_income <- pregnancy_df %>%
  mutate(
    Salary_Range = cut(Monthly_income_eur, 
                       breaks = income_breaks, 
                       labels = income_labels, 
                       right = FALSE)
  )

p_salary_bar <- ggplot(df_income %>% drop_na(Salary_Range), aes(x = Salary_Range, fill = Salary_Range)) +
  geom_bar(color = "white", show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Teal") +
  labs(
    title = "Distribution of Pregnant Women by Monthly Income Range",
    x = "Monthly Income Range (Euro)",
    y = "Number of Pregnant Women"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "monthly_income_barchart.png"),
  plot = p_salary_bar,
  width = 8, height = 6, dpi = 300
)

# ==========================================================================
# PER CAPITA HOUSEHOLD EARNINGS (monthly)
# ==========================================================================
# 1. Define per capita income brackets and labels
pc_income_breaks <- c(0, 200, 350, 500, Inf)
pc_income_labels <- c("< €200", "€200 - €350", "€350 - €500", "> €500")

# 2. Create the salary range category column
df_pc_income <- pregnancy_df %>%
  mutate(
    Per_Capita_Range = cut(Per_capita_income_eur, 
                           breaks = pc_income_breaks, 
                           labels = pc_income_labels, 
                           right = FALSE)
  )

# 3. Graph 1: Bar Chart of Patients per Per Capita Income Range
p_pc_bar <- ggplot(df_pc_income %>% drop_na(Per_Capita_Range), aes(x = Per_Capita_Range, fill = Per_Capita_Range)) +
  geom_bar(color = "white", show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Distribution of Pregnant Women by Per Capita Income Range",
    x = "Per Capita Monthly Income Range (Euro)",
    y = "Number of Pregnant Women"
  ) +
  theme_minimal() +
  ylim(0, max(table(df_pc_income$Per_Capita_Range)) * 1.15) # Headroom for labels

# Save Bar Chart
ggsave(
  filename = file.path(output_dir, "per_capita_income_barchart.png"),
  plot = p_pc_bar,
  width = 8, 
  height = 6, 
  dpi = 300
)


# ==========================================================================
# INCOME CLASSIFICATION 
# ==========================================================================
# 1. Calculate Relative Poverty Threshold (60% of sample median per capita income)
median_pc_income <- median(pregnancy_df$Per_capita_income_eur, na.rm = TRUE)
relative_poverty_line <- 0.6 * median_pc_income

# 2. Classify each pregnant woman into poverty categories
# We can use standard international/regional brackets for monthly per capita income (EUR):
# - Extreme Vulnerability / Extreme Poverty: < €150 / month (~€5/day)
# - Moderate Vulnerability / Relative Poverty: €150 to < 60% of median
# - Low Income / Near Poverty: 60% of median to €350 / month
# - Adequate / Above Poverty: > €350 / month

poverty_breaks <- c(0, 150, relative_poverty_line, 350, Inf)
poverty_labels <- c("Extreme Poverty (< €150)", 
                    "Relative Poverty (150 - 60% Median)", 
                    "Near Poverty / Low Income (€128 - €350)", 
                    "Above Poverty Line (> €350)")

df_poverty <- pregnancy_df %>%
  mutate(
    Poverty_Status = cut(Per_capita_income_eur,
                         breaks = c(0, relative_poverty_line, 350, Inf),
                         labels = c("In Poverty (< 60% Median)", 
                                    "Low Income / Vulnerable (€128 - €350)", 
                                    "Above Poverty Line (> €350)"),
                         right = FALSE)
  )

# 3. Summarize Poverty Counts and Percentages
poverty_summary <- df_poverty %>%
  count(Poverty_Status) %>%
  mutate(Percentage = (n / sum(n)) * 100)

print("Poverty Summary Table:")
print(poverty_summary)

# 4. Generate a Professional Bar Chart
p_poverty <- ggplot(df_poverty %>% drop_na(Poverty_Status), aes(x = Poverty_Status, fill = Poverty_Status)) +
  geom_bar(color = "white", show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = sprintf("%d (%.1f%%)", after_stat(count), (after_stat(count)/nrow(df_poverty))*100)), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("In Poverty (< 60% Median)" = "#C0392B", 
                               "Low Income / Vulnerable (€128 - €350)" = "#E67E22", 
                               "Above Poverty Line (> €350)" = "#27AE60")) +
  labs(
    title = "Classification of Pregnant Women by Poverty Status",
    subtitle = paste0("Relative Poverty Threshold (60% of Median): €", round(relative_poverty_line, 2), " per capita/month"),
    x = "Poverty & Income Classification Category",
    y = "Number of Pregnant Women"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  ylim(0, max(table(df_poverty$Poverty_Status)) * 1.2) # Headroom for labels

ggsave(
  filename = file.path(output_dir, "pregnancy_poverty_classification.png"),
  plot = p_poverty,
  width = 9, 
  height = 6, 
  dpi = 300
)

# ==========================================================================
# PRE GESTACIONAL BODY MASS INDEX AND PER CAPITA HOUSEHOLD EARNINGS (monthly)
# ==========================================================================
# 1. Test for normality to choose the right correlation coefficient
shapiro_bmi <- shapiro.test(pregnancy_df$BMI_pre_pregnancy)
shapiro_income <- shapiro.test(pregnancy_df$Per_capita_income_eur)

print(shapiro_bmi)
print(shapiro_income)

# 2. Calculate Correlation Coefficients (both Pearson and Spearman)
# Pearson (if normally distributed)
pearson_res <- cor.test(pregnancy_df$BMI_pre_pregnancy, pregnancy_df$Per_capita_income_eur, 
                        method = "pearson")

# Spearman (non-parametric rank-based, safest for income data which is often skewed)
spearman_res <- cor.test(pregnancy_df$BMI_pre_pregnancy, pregnancy_df$Per_capita_income_eur, 
                         method = "spearman")

print(pearson_res)
print(spearman_res)

# 3. Create a Scatter Plot with a Regression Trend Line
p_corr <- ggplot(pregnancy_df, aes(x = Per_capita_income_eur, y = BMI_pre_pregnancy)) +
  geom_point(color = "#2C3E50", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#E74C3C", fill = "#FADBD8", linewidth = 1) +
  labs(
    title = "Correlation between Per Capita Income and Pre-pregnancy BMI",
    subtitle = paste0("Spearman r = ", round(spearman_res$estimate, 3), 
                      " (p = ", signif(spearman_res$p.value, 3), ")"),
    x = "Monthly Per Capita Income (Euro)",
    y = "Pre-pregnancy BMI (kg/m²)"
  ) +
  theme_minimal()

# 4. Save the Scatter Plot to your folder
ggsave(
  filename = file.path(output_dir, "bmi_vs_percapita_income_scatter.png"),
  plot = p_corr,
  width = 8, 
  height = 6, 
  dpi = 300
)
# ==========================================================================
# GOVERNMENT BENEFITS
# ==========================================================================
# 1. Prepare data with labeled factor for Government Benefit
df_benefit <- pregnancy_df %>%
  mutate(
    Government_Benefit_f = factor(Government_benefit, 
                                  levels = c(0, 1), 
                                  labels = c("No Benefit (0)", "Receives Benefit (1)"))
  )

# 2. Calculate absolute counts and percentages
benefit_summary <- df_benefit %>%
  count(Government_Benefit_f) %>%
  mutate(Percentage = (n / sum(n)) * 100)

print("Government Benefit Summary Table:")
print(benefit_summary)

# 3. Create a Bar Chart visualization
p_benefit <- ggplot(df_benefit %>% drop_na(Government_Benefit_f), 
                    aes(x = Government_Benefit_f, fill = Government_Benefit_f)) +
  geom_bar(color = "white", show.legend = FALSE) +
  geom_text(stat = 'count', 
            aes(label = sprintf("%d (%.1f%%)", after_stat(count), (after_stat(count)/nrow(df_benefit))*100)), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("No Benefit (0)" = "#34495E", "Receives Benefit (1)" = "#E67E22")) +
  labs(
    title = "Distribution of Pregnant Women Receiving Government Benefits",
    x = "Government Benefit Status",
    y = "Number of Pregnant Women"
  ) +
  theme_minimal() +
  ylim(0, max(table(df_benefit$Government_Benefit_f)) * 1.2) # Headroom for labels

# 4. Save the Bar Chart to your folder
ggsave(
  filename = file.path(output_dir, "government_benefits_barchart.png"),
  plot = p_benefit,
  width = 8, 
  height = 6, 
  dpi = 300
)
# ==========================================================================
# WEIGHT CONTROL AND HOUSEHOLD EARNINGS (monthly)
# ==========================================================================
# 1. Reshape data from wide to long format to include Weight Control across R1-R4
df_long_weight <- pregnancy_df %>%
  select(System_number, Monthly_income_eur, Per_capita_income_eur, 
         starts_with("Weight_control_R")) %>%
  pivot_longer(
    cols = starts_with("Weight_control_R"),
    names_to = "Return",
    names_prefix = "Weight_control_",
    values_to = "Weight_control"
  ) %>%
  mutate(
    Return = factor(Return, levels = c("R1", "R2", "R3", "R4")),
    System_number = as.factor(System_number),
    Weight_control_f = factor(Weight_control, levels = c(0, 1), labels = c("No (0)", "Yes (1)"))
  ) %>%
  drop_na(Weight_control, Monthly_income_eur)

# 2. Run a Longitudinal Logistic GEE Model
# Testing if Monthly Income predicts Weight Control over time
gee_weight_income <- geeglm(
  Weight_control ~ Return + Monthly_income_eur,
  id = System_number,
  data = df_long_weight,
  family = binomial(link = "logit"),
  corstr = "exchangeable"
)

summary(gee_weight_income)

# 3. Create a Boxplot / Violin Plot Visualization
# Comparing Monthly Income distribution across Weight Control success categories
p_weight_income <- ggplot(df_long_weight, aes(x = Weight_control_f, y = Monthly_income_eur, fill = Weight_control_f)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "#2C3E50") +
  scale_fill_manual(values = c("No (0)" = "#E74C3C", "Yes (1)" = "#2ECC71")) +
  labs(
    title = "Monthly Household Income by Weight Control Success",
    subtitle = "Longitudinal evaluation across return visits (R1 - R4)",
    x = "Weight Control Status",
    y = "Monthly Household Earnings (Euro)",
    fill = "Weight Control"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 4. Save the Plot to your folder
ggsave(
  filename = file.path(output_dir, "weight_control_vs_monthly_income_boxplot.png"),
  plot = p_weight_income,
  width = 8, 
  height = 6, 
  dpi = 300
)

# ==========================================================================
# WEIGHT CONTROL AND GOVERNMENT BENEFITS
# ==========================================================================
# 1. Reshape data from wide to long format to include Weight Control across R1-R4
df_long_benefit <- pregnancy_df %>%
  select(System_number, Government_benefit, starts_with("Weight_control_R")) %>%
  pivot_longer(
    cols = starts_with("Weight_control_R"),
    names_to = "Return",
    names_prefix = "Weight_control_",
    values_to = "Weight_control"
  ) %>%
  mutate(
    Return = factor(Return, levels = c("R1", "R2", "R3", "R4")),
    System_number = as.factor(System_number),
    Government_Benefit_f = factor(Government_benefit, levels = c(0, 1), labels = c("No Benefit (0)", "Receives Benefit (1)")),
    Weight_control_f = factor(Weight_control, levels = c(0, 1), labels = c("No (0)", "Yes (1)"))
  ) %>%
  drop_na(Weight_control, Government_benefit)

# 2. Run a Longitudinal Logistic GEE Model
# Testing if Government Benefit status predicts Weight Control over time
gee_weight_benefit <- geeglm(
  Weight_control ~ Return + Government_benefit,
  id = System_number,
  data = df_long_benefit,
  family = binomial(link = "logit"),
  corstr = "exchangeable"
)

print("--- GEE Model Output: Weight Control vs Government Benefits ---")
summary(gee_weight_benefit)

# 3. Create a Grouped Bar Chart Visualization
# Showing success rates of weight control comparing beneficiaries vs non-beneficiaries
p_weight_benefit <- ggplot(df_long_benefit %>% drop_na(Weight_control_f, Government_Benefit_f), 
                           aes(x = Government_Benefit_f, fill = Weight_control_f)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No (0)" = "#E74C3C", "Yes (1)" = "#2ECC71")) +
  labs(
    title = "Proportion of Weight Control Success by Government Benefit Status",
    subtitle = "Longitudinal evaluation across return visits (R1 - R4)",
    x = "Government Benefit Status",
    y = "Percentage of Visits",
    fill = "Weight Control Success"
  ) +
  theme_minimal()

# 4. Save the Plot to your folder
ggsave(
  filename = file.path(output_dir, "weight_control_vs_government_benefits_barplot.png"),
  plot = p_weight_benefit,
  width = 8, 
  height = 6, 
  dpi = 300
)
