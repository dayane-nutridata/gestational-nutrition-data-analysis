# Longitudinal Analysis Of Pregnant Women Receiving Nutritional Care

## 📌 Project Overview
This repository presents a data analysis workflow in **R** examining the association between nutritional status during pregnancy and maternal-child health outcomes in a public healthcare setting.

The objective is to demonstrate reproducible data cleaning, descriptive statistics, and longitudinal modeling techniques applied to public health surveillance data.

---

## 📊 Dataset Description
* **Source:** Synthetic/anonymized public health surveillance dataset.
* **Sample Size:** $N = 500$ pregnant women across multiple prenatal visits.
* **Key Variables:**
  * `patient_id`: Unique identifier
  * `gestational_age`: Gestational age at visit (weeks)
  * `bmi`: Body Mass Index ($\text{kg/m}^2$)
  * `hemoglobin`: Hemoglobin levels ($\text{g/dL}$)
  * `birth_weight`: Infant birth weight ($\text{g}$)

---

## 🛠️ Tools & Packages Used
* **Language:** R (v4.3+)
* **Data Manipulation:** `tidyverse`, `dplyr`, `tidyr`
* **Statistical Modeling & Tables:** `gtsummary`, `geepack`
* **Visualization:** `ggplot2`

---

## 🚀 Key Results & Outputs

### 1. Descriptive Summary
Descriptive statistics for baseline maternal characteristics were generated using `gtsummary`:

| Characteristic | Overall ($N = 500$) |
| :--- | :---: |
| **Maternal Age (years)**, Mean (SD) | 27.4 (5.2) |
| **Pre-pregnancy BMI**, Median (IQR) | 24.1 (21.5, 27.8) |
| **Anemia Rate at 3rd Trimester**, % | 14.2% |

### 2. Main Findings
* Longitudinal GEE models indicated a significant association between maternal third-trimester hemoglobin levels and birth weight ($p < 0.05$).
* Full analysis scripts are available in the [`scripts/`](./scripts/) folder.

---

## 💻 How to Reproduce
1. Clone this repository:
   ```bash
   git clone [https://github.com/your-username/gestational-nutrition-data-analysis.git](https://github.com/your-username/gestational-nutrition-data-analysis.git)
