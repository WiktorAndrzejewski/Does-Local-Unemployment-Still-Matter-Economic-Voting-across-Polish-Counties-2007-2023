# ==============================================================================
# 2. MODELING & CLUSTERED ROBUST STANDARD ERRORS
# ==============================================================================
library(sjPlot)
library(openxlsx)
library(rvest)
library(xml2)
library(sandwich)
library(lmtest)
library(dplyr)

# Function to compute county-clustered standard errors (clustered by Code)
clustered_vcov <- function(model) {
  sandwich::vcovCL(model, cluster = ~Code)
}

# --- MODEL 1: BASIC LINEAR SPECIFICATION ---
Final_Basic_Model <- lm(Vote_Share ~ as.factor(year) + Delta_Unemployment_Rate - 1, data = Data)

# --- MODEL 2: QUADRATIC SPECIFICATION (NON-LINEARITY) ---
Delta_Squared_Weights <- lm(Vote_Share ~ as.factor(year) + Delta_Unemployment_Rate + I(Delta_Unemployment_Rate^2) - 1, data = Data)

# --- MODEL 3a: ASYMMETRIC UNEMPLOYMENT EFFECTS ---
Asymmetric_influence_Uneployment <- lm(Vote_Share ~ as.factor(year) + delta_unemployment_if_neg + delta_unemployment_if_pos - 1, data = Data)

# --- MODEL 3b: PARTISAN ASYMMETRY IN UNEMPLOYMENT EFFECTS ---
Asymmetric_influence_Partisan <- lm(Vote_Share ~ as.factor(year) + delta_unemployment_if_posxPiS + delta_unemployment_if_negxPiS + delta_unemployment_if_posxPO + delta_unemployment_if_negxPO - 1, data = Data)


# ==============================================================================
# 3. COMBINED SUMMARY TABLE (EXPORT TO EXCEL & HTML)
# ==============================================================================
target_dir <- "C:/Users/Marian/Documents/GitHub/Economic_Voting_paper"
setwd(target_dir)

html_file_path <- file.path(target_dir, "All_Models_Summary.html")
xlsx_file_path <- file.path(target_dir, "All_Models_Summary.xlsx")

# Generate combined summary table across Models 1–4
tab_model(
  Final_Basic_Model,
  Asymmetric_influence_Uneployment,
  Delta_Squared_Weights,
  Asymmetric_influence_Partisan,
  vcov.fun = clustered_vcov,
  dv.labels = c(
    "Model 1: Baseline", 
    "Model 2: Unemployment Asymmetric", 
    "Model 3: Quadratic", 
    "Model 4: Partisan Asymmetric"
  ),
  pred.labels = c(
    "as.factor(year)2007"           = "Election Year: 2007",
    "as.factor(year)2011"           = "Election Year: 2011",
    "as.factor(year)2015"           = "Election Year: 2015",
    "as.factor(year)2019"           = "Election Year: 2019",
    "as.factor(year)2023"           = "Election Year: 2023",
    "Delta_Unemployment_Rate"        = "Δ Unemployment Rate (p.p.)",
    "I(Delta_Unemployment_Rate^2)"   = "(Δ Unemployment Rate)²",
    "delta_unemployment_if_neg"      = "Δ Unemployment Decrease",
    "delta_unemployment_if_pos"      = "Δ Unemployment Increase",
    "delta_unemployment_if_posxPiS" = "Δ Unemployment (Increase) × PiS Incumbency",
    "delta_unemployment_if_negxPiS" = "Δ Unemployment (Decrease) × PiS Incumbency",
    "delta_unemployment_if_posxPO"  = "Δ Unemployment (Increase) × PO Incumbency",
    "delta_unemployment_if_negxPO"  = "Δ Unemployment (Decrease) × PO Incumbency"
  ),
  show.ci = FALSE, 
  show.se = TRUE, 
  collapse.se = TRUE,
  p.style = "stars", 
  digits = 3,
  file = html_file_path
)

# Open generated HTML table in default web browser
browseURL(html_file_path)