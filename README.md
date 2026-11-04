# Longitudinal Analysis Of Pregnant Women Receiving Nutritional Care

## 📌 Project Overview

This repository presents a data analysis workflow in **R** with descriptive, interventional, and social analyses of pregnant women attended by the nutritionist in a public healthcare setting.

The objective is to demonstrate what kind of information is collected, descriptive statistics, and longitudinal modeling techniques applied to public health surveillance data.

All data presented were fictitiously generated in R for demonstration purposes, in compliance with the General Data Protection Regulation (GDPR) and Brazil’s General Personal Data Protection Law (LGPD).


---

## 📊 Dataset Description
* **Synthetic public health surveillance dataset created in R.
* **Sample Size:** $N = 100$ pregnant women across multiple appointments with the nutritionist.
* **Key Variables:**
  * `System_number`: Unique identifier
  * `Name`: SUBJ_001; SUBJ_002... SUBJ_100 ($\text{g/dL}$)
  * `Pregnancy_age_PA_weeks`: Gestational age at visit (weeks)
  * `BMI`: Body Mass Index ($\text{kg/m}^2$)
  * `GC`: Glycemic Control ($\text{g}$)
  * `Nutritional_diagnosis`: nutritional diagnosis observed during the consultation ($\text{g}$)
  * Full Data Dictionary is available in the [gestational-nutrition-data-analysis/data/data_dictionary.odt) file.

---

## 🛠️ Tools & Packages Used
* **Language:** R (v4.5.2)
* **Data Manipulation:** `tidyverse`, `dplyr`, `tidyr`
* **Statistical Modeling & Tables:** `geepack`
* **Visualization:** `ggplot2`

---

## 🚀 Key Results & Outputs

### 1. Descriptive Summary
Descriptive statistics for baseline maternal characteristics:

| Characteristic | Overall ($N = 100$) |
| :--- | :---: |
| **Maternal Age (years)**, Mean (SD) | 29.5 (7.5) |
| **Pre-pregnancy BMI**, Median (IQR) | 24.7 (19.4, 30) | 
| **Main Nutritional Diagnosis**, % | Excessive sodium intake, 11% |


### 2. Main Findings
* Longitudinal GEE models indicated that weight control is  statistically associated with adequate glycemic control ($p = 0.023$, $\text{OR} \approx 2.27$).
* Full analysis scripts are available in the [`scripts/`](./scripts/) folder.

---

## 💻 How to Reproduce
1. Clone this repository:
   ```bash
   git clone [https://github.com/dayane-nutridata/gestational-nutrition-data-analysis.git](https://github.com/dayane-nutridata/gestational-nutrition-data-analysis.git)
