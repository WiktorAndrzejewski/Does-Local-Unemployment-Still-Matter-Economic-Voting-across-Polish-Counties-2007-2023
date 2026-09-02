library(dplyr)

# ==============================================================================
# 2. PREPARING PANEL DATA & METRICS
# ==============================================================================

# 2.1. Initial Transformations & Incumbency Indicators
Panel <- final_merge_selected %>%
  mutate(
    # Numeric conversions
    average_salary    = as.numeric(average_salary),
    population_size   = as.numeric(population_size),
    unemployment_rate = as.numeric(unemployment_rate),
    
    # Logarithmic transformations
    log_salary        = log(average_salary),
    
    # Vote Share Calculations
    Vote.share.PiS    = `Prawo i Sprawiedliwosc` / `Valid ballot papers` * 100,
    Vote.share.PO     = `Platforma Obywatelska` / `Valid ballot papers` *100,
    
    # Incumbency Indicators
    Ruling.Party_PiS  = ifelse(year %in% c("2007", "2019", "2023"), 1, 0),
    Ruling.Party_PO   = ifelse(year %in% c("2011", "2015"), 1, 0),
    Ruling.Party      = case_when(
      year %in% c("2007", "2019", "2023") ~ "Prawo i Sprawiedliwosc",
      year %in% c("2011", "2015")         ~ "Platforma Obywatelska",
      TRUE                                 ~ "Other"
    )
  )

# 2.2. Subset Construction
Data <- Panel %>%
  select(
    Code, year, log_salary, average_salary, 
    Vote.share.PiS, unemployment_rate, Vote.share.PO, 
    population_size, Ruling.Party, `Prawo i Sprawiedliwosc`, 
    `Platforma Obywatelska`, Ruling.Party_PiS, Ruling.Party_PO, 
    `Valid ballot papers`
  )

# ==============================================================================
# 3. COMPUTING LAGS AND FIRST-DIFFERENCES
# ==============================================================================

Data <- Data %>%
  arrange(Code, year) %>%
  group_by(Code) %>%
  mutate(
    # Political Lags and Deltas
    PiS_lag                   = dplyr::lag(`Prawo i Sprawiedliwosc`, 1),
    Delta_PiS                 = `Prawo i Sprawiedliwosc` - PiS_lag,
    
    PO_lag                    = dplyr::lag(`Platforma Obywatelska`, 1),
    Delta_PO                  = `Platforma Obywatelska` - PO_lag,
    PO_lag2                   = dplyr::lag(`Platforma Obywatelska`, 2),
    
    Vote_share_PiS_lag        = dplyr::lag(Vote.share.PiS, 1),
    Delta_Vote_Share_PiS      = Vote.share.PiS - Vote_share_PiS_lag,
    
    Vote_share_PO_lag         = dplyr::lag(Vote.share.PO, 1),
    Delta_Vote_Share_PO       = Vote.share.PO - Vote_share_PO_lag,
    
    # Economic Lags and Deltas
    unemp_lag                 = dplyr::lag(unemployment_rate, 1),
    Delta_Unemployment_Rate   = unemployment_rate - unemp_lag,
    
    log_salary_lag            = dplyr::lag(log_salary, 1),
    Delta_Log_Average_Salary  = log_salary - log_salary_lag,
    
    Log.population_size       = log(population_size),
    Log.pop_lag               = dplyr::lag(Log.population_size, 1),
    Delta_Log.population_size = Log.population_size - Log.pop_lag,
    
    # Incumbency Lags
    Ruling.Party_PO_lag       = dplyr::lag(Ruling.Party_PO, 1),
    Ruling.Party_PO_lag2      = dplyr::lag(Ruling.Party_PO, 2)
  ) %>%
  ungroup()

# ==============================================================================
# 4. DEPENDENT VARIABLE & INTERACTION TERMS
# ==============================================================================

Data <- Data %>%
  mutate(
    # Dependent Variable
    Vote_Share = case_when(
      Ruling.Party == "Prawo i Sprawiedliwosc" ~ Delta_Vote_Share_PiS,
      Ruling.Party == "Platforma Obywatelska" ~ Delta_Vote_Share_PO,
      TRUE ~ NA_real_
    ),
    
    # Standard Interaction terms for PO
    POxLog_People                 = Log.population_size * Ruling.Party_PO - Log.pop_lag * Ruling.Party_PO,
    POxLog_Salary                 = log_salary * Ruling.Party_PO - log_salary_lag * Ruling.Party_PO,
    POxUnemployment               = unemployment_rate * Ruling.Party_PO - unemp_lag * Ruling.Party_PO,
    PiSXUnemployment               = unemployment_rate * Ruling.Party_PiS - unemp_lag * Ruling.Party_PiS,
    
    # Aliases & Ruling Party Indicators
    Delta_Log_Population          = Delta_Log.population_size,
    Ruling_Party_PO               = as.numeric(Ruling.Party_PO),
    Ruling_Party_PiS              = as.numeric(Ruling.Party_PiS),
    
    # Asymmetric Unemployment Indicators
    delta_unemployment_if_neg     = ifelse(Delta_Unemployment_Rate < 0, Delta_Unemployment_Rate, 0),
    delta_unemployment_if_pos     = ifelse(Delta_Unemployment_Rate > 0, Delta_Unemployment_Rate, 0),
    
    # Asymmetric Interaction Terms
    delta_unemployment_if_negxPiS = delta_unemployment_if_neg * Ruling_Party_PiS,
    delta_unemployment_if_posxPO  = delta_unemployment_if_pos * Ruling_Party_PO,
    delta_unemployment_if_negxPO  = delta_unemployment_if_neg * Ruling_Party_PO,
    delta_unemployment_if_posxPiS = delta_unemployment_if_pos * Ruling_Party_PiS
  )

