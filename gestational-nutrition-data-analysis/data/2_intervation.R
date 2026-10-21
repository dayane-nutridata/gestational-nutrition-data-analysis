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
if(!require(readODS)) install.packages("readODS");library(readODS)

