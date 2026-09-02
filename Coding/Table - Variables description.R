
library(dplyr)
library(tidyr)
library(knitr)

# ==============================================================================
# 1. VARIABLE PREPARATION AND PANEL LAGS
# ==============================================================================

Data_Clean <- final_merge_selected %>%
  mutate(
    # Type casting
    average_salary    = as.numeric(average_salary),
    population_size   = as.numeric(population_size),
    unemployment_rate = as.numeric(unemployment_rate),
    log_salary        = log(average_salary),
    
    # Vote shares
    Vote.share.PiS    = `Prawo i Sprawiedliwosc` / `Valid ballot papers` * 100,
    Vote.share.PO     = `Platforma Obywatelska` / `Valid ballot papers` * 100,
    
    # Incumbent party identifiers
    Ruling.Party_PiS  = ifelse(year %in% c("2007", "2019", "2023"), 1, 0),
    Ruling.Party_PO   = ifelse(year %in% c("2011", "2015"), 1, 0),
    Ruling.Party      = case_when(
      year %in% c("2007", "2019", "2023") ~ "Prawo i Sprawiedliwosc",
      year %in% c("2011", "2015")         ~ "Platforma Obywatelska",
      TRUE                                ~ "Other"
    )
  ) %>%
  select(
    Code, year, log_salary, average_salary, 
    Vote.share.PiS, unemployment_rate, Vote.share.PO, 
    population_size, Ruling.Party, `Prawo i Sprawiedliwosc`, 
    `Platforma Obywatelska`, Ruling.Party_PiS, Ruling.Party_PO, 
    `Valid ballot papers`
  ) %>%
  arrange(Code, year) %>%
  group_by(Code) %>%
  mutate(
    # Electoral first differences
    Vote_share_PiS_lag       = dplyr::lag(Vote.share.PiS, 1),
    Delta_Vote_Share_PiS     = Vote.share.PiS - Vote_share_PiS_lag,
    
    Vote_share_PO_lag        = dplyr::lag(Vote.share.PO, 1),
    Delta_Vote_Share_PO      = Vote.share.PO - Vote_share_PO_lag,
    
    # Economic and demographic first differences
    unemp_lag                = dplyr::lag(unemployment_rate, 1),
    Delta_Unemployment_Rate  = unemployment_rate - unemp_lag,
    
    Log.population_size      = log(population_size),
    Log.pop_lag              = dplyr::lag(Log.population_size, 1),
    Delta_Log_Population     = Log.population_size - Log.pop_lag
  ) %>%
  ungroup() %>%
  mutate(
    # Dependent variable (change in support for the current incumbent)
    Vote_Share = case_when(
      Ruling.Party == "Prawo i Sprawiedliwosc" ~ Delta_Vote_Share_PiS,
      Ruling.Party == "Platforma Obywatelska"  ~ Delta_Vote_Share_PO,
      TRUE ~ NA_real_
    ),
    
    # Squared unemployment term
    Delta_Unemployment_Rate_Sq = Delta_Unemployment_Rate^2,
    
    # Binary party dummies
    Ruling_Party_PO  = as.numeric(Ruling.Party_PO),
    Ruling_Party_PiS = as.numeric(Ruling.Party_PiS),
    
    # Interaction terms: Civic Platform (PO)
    POxLog_People        = Delta_Log_Population * Ruling_Party_PO,
    POxUnemployment      = Delta_Unemployment_Rate * Ruling_Party_PO,
    POxUnemployment_Sq   = Delta_Unemployment_Rate_Sq * Ruling_Party_PO,
    
    # Directional asymmetry for unemployment rate changes
    delta_unemployment_if_neg = ifelse(Delta_Unemployment_Rate < 0, Delta_Unemployment_Rate, 0),
    delta_unemployment_if_pos = ifelse(Delta_Unemployment_Rate > 0, Delta_Unemployment_Rate, 0),
    
    # Asymmetric interaction terms
    delta_unemployment_if_negxPiS = delta_unemployment_if_neg * Ruling_Party_PiS,
    delta_unemployment_if_posxPO  = delta_unemployment_if_pos * Ruling_Party_PO
  )

# ==============================================================================
# 2. DESCRIPTIVE STATISTICS AND VARIABLE METADATA
# ==============================================================================

metadata_table2 <- tibble::tribble(
  ~col_name,                       ~Variable,                                    ~Source,
  "Vote_Share",                    "Δ Vote Share",                               "PKW",
  "Delta_Unemployment_Rate",       "Δ Unemployment Rate",                        "GUS",
  "Delta_Log_Population",          "Δ Log Population",                           "GUS",
  "Delta_Unemployment_Rate_Sq",    "(Δ Unemployment Rate)²",                     "GUS / Own calc.",
  "POxLog_People",                 "Incumbent PO × Δ Log Population",            "PKW / GUS",
  "POxUnemployment",               "Incumbent PO × Δ Unemployment",              "PKW / GUS",
  "POxUnemployment_Sq",            "Incumbent PO × (Δ Unemployment)²",           "PKW / GUS",
  "delta_unemployment_if_neg",     "Δ Unemployment (decrease)",                  "GUS / Own calc.",
  "delta_unemployment_if_negxPiS", "Δ Unempl. (decrease) × Incumbent PiS",       "PKW / GUS",
  "delta_unemployment_if_posxPO",  "Δ Unempl. (increase) × Incumbent PO",        "PKW / GUS"
)

Table_2_results <- Data_Clean %>%
  select(all_of(metadata_table2$col_name)) %>%
  pivot_longer(cols = everything(), names_to = "col_name", values_to = "val") %>%
  filter(!is.na(val)) %>%
  group_by(col_name) %>%
  summarise(
    N          = n(),
    Mean       = mean(val),
    `St. Dev.` = sd(val),
    Min        = min(val),
    Max        = max(val),
    .groups    = "drop"
  ) %>%
  right_join(metadata_table2, by = "col_name") %>%
  select(Variable, Source, N, Mean, `St. Dev., Min, Max`) %>%
  mutate(across(c(Mean, `St. Dev.`, Min, Max), ~ round(.x, 3)))

# ==============================================================================
# 3. DISPLAY TABLE IN R CONSOLE
# ==============================================================================

kable(
  Table_2_results, 
  caption = "Table 2: Variables Description and Descriptive Statistics", 
  format = "pipe",
  align = c("l", "l", "r", "r", "r", "r", "r")
)