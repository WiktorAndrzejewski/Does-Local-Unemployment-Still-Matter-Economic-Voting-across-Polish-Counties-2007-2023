# ==============================================================================
# REVISED AND OPTIMIZED PANEL DATA ANALYSIS SCRIPT
# ==============================================================================

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

packages <- c("readxl", "dplyr", "stringr", "janitor", "purrr", "tidyr", "plm")
lapply(packages, install_and_load)

# Years included in the analysis
selected_years <- c("Code", "County", "2007", "2011", "2015", "2019", "2023")

# ------------------------------------------------------------------------------
# 1. SOCIOECONOMIC DATA
# ------------------------------------------------------------------------------

# --- AVERAGE SALARY ---
average_salary_raw <- read_excel("Data/WYNA_2497_XTAB_20250329140152.xlsx", sheet = 2) %>%
  row_to_names(row_number = 1)
average_salary_raw <- average_salary_raw[-2, ]
colnames(average_salary_raw)[1:2] <- c("Code", "County")

panel_data_salary <- average_salary_raw %>%
  select(all_of(selected_years)) %>%
  pivot_longer(cols = -c(Code, County), names_to = "year", values_to = "average_salary") %>%
  mutate(
    Code = as.character(Code),
    year = as.character(year),
    average_salary = suppressWarnings(as.numeric(average_salary))
  )

# --- POPULATION SIZE ---
population_size_raw <- read_excel("Data/LUDN_2137_XTAB_20250329142524.xlsx", sheet = 2)
population_size_raw <- population_size_raw[-c(1, 3), ] %>%
  row_to_names(row_number = 1)
colnames(population_size_raw)[1:2] <- c("Code", "County")

panel_data_population_size <- population_size_raw %>%
  select(all_of(selected_years)) %>%
  pivot_longer(cols = -c(Code, County), names_to = "year", values_to = "population_size") %>%
  mutate(
    Code = as.character(Code),
    year = as.character(year),
    population_size = suppressWarnings(as.numeric(population_size))
  )

# --- UNEMPLOYMENT RATE ---
unemployment_rate_raw <- read_excel("Data/RYNE_2392_XTAB_20250329140926Bezrobocie.xlsx", sheet = 2) %>%
  row_to_names(row_number = 1)
colnames(unemployment_rate_raw)[1:2] <- c("Code", "County")

panel_data_unemployed <- unemployment_rate_raw %>%
  select(all_of(selected_years)) %>%
  pivot_longer(cols = -c(Code, County), names_to = "year", values_to = "unemployment_rate") %>%
  mutate(
    Code = as.character(Code),
    year = as.character(year),
    unemployment_rate = suppressWarnings(as.numeric(unemployment_rate))
  )

# --- MERGING SOCIOECONOMIC DATASETS ---
socioeconomic_panel_data <- list(panel_data_salary, panel_data_population_size, panel_data_unemployed) %>%
  reduce(left_join, by = c("Code", "year")) %>%
  mutate(
    County = coalesce(County.x, County.y, County),
    County = str_replace_all(County, c("Powiat " = "", "m. " = "", "st. " = ""))
  ) %>%
  select(Code, County, year, average_salary, population_size, unemployment_rate)

# ------------------------------------------------------------------------------
# 2. VOTING DATA
# ------------------------------------------------------------------------------

# --- 2007 ---
elections_2007_clean <- read_excel("Data/sejm2007-pow-listy.xlsx") %>%
  mutate(year = "2007") %>%
  select(`Kod pow.`, Powiat, year, `6 - Prawo i Sprawiedliwość`, `8 - Platforma Obywatelska`, `10 - Polskie Stronnictwo Ludowe`, Ważne) %>%
  mutate(`8 - Platforma Obywatelska` = `8 - Platforma Obywatelska` + `10 - Polskie Stronnictwo Ludowe`) %>%
  select(-`10 - Polskie Stronnictwo Ludowe`) %>%
  rename(
    Code = `Kod pow.`,
    County = Powiat,
    `Prawo i Sprawiedliwosc` = `6 - Prawo i Sprawiedliwość`,
    `Platforma Obywatelska` = `8 - Platforma Obywatelska`,
    `Valid ballot papers` = Ważne
  )

# --- 2011 ---
elections_2011_clean <- read_excel("Data/2011-sejm-pow-listy.xlsx") %>%
  mutate(year = "2011") %>%
  select(TERYT, Powiat, year, `Komitet Wyborczy Prawo i Sprawiedliwość`, `Komitet Wyborczy Platforma Obywatelska RP`, `Komitet Wyborczy Polskie Stronnictwo Ludowe`, `Głosy ważne`) %>%
  mutate(`Komitet Wyborczy Platforma Obywatelska RP` = `Komitet Wyborczy Platforma Obywatelska RP` + `Komitet Wyborczy Polskie Stronnictwo Ludowe`) %>%
  select(-`Komitet Wyborczy Polskie Stronnictwo Ludowe`) %>%
  rename(
    Code = TERYT,
    County = Powiat,
    `Prawo i Sprawiedliwosc` = `Komitet Wyborczy Prawo i Sprawiedliwość`,
    `Platforma Obywatelska` = `Komitet Wyborczy Platforma Obywatelska RP`,
    `Valid ballot papers` = `Głosy ważne`
  )

# --- 2015 ---
elections_2015_clean <- read_excel("Data/2015-gl-lis-pow.xlsx") %>%
  mutate(year = "2015") %>%
  select(TERYT, Powiat, year, `1 - Komitet Wyborczy Prawo i Sprawiedliwość`, `2 - Komitet Wyborczy Platforma Obywatelska RP`, `8 - Komitet Wyborczy Nowoczesna Ryszarda Petru`, `5 - Komitet Wyborczy Polskie Stronnictwo Ludowe`, `Głosy ważne`) %>%
  mutate(`2 - Komitet Wyborczy Platforma Obywatelska RP` = `2 - Komitet Wyborczy Platforma Obywatelska RP` + `8 - Komitet Wyborczy Nowoczesna Ryszarda Petru` + `5 - Komitet Wyborczy Polskie Stronnictwo Ludowe`) %>%
  select(-c(`8 - Komitet Wyborczy Nowoczesna Ryszarda Petru`, `5 - Komitet Wyborczy Polskie Stronnictwo Ludowe`)) %>%
  rename(
    Code = TERYT,
    County = Powiat,
    `Prawo i Sprawiedliwosc` = `1 - Komitet Wyborczy Prawo i Sprawiedliwość`,
    `Platforma Obywatelska` = `2 - Komitet Wyborczy Platforma Obywatelska RP`,
    `Valid ballot papers` = `Głosy ważne`
  )

# --- 2019 ---
elections_2019_clean <- read_excel("Data/wyniki_gl_na_listy_po_powiatach_sejm.xlsx") %>%
  mutate(
    year = "2019",
    `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19` = 
      suppressWarnings(as.numeric(`KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19`)) + 
      suppressWarnings(as.numeric(`KOMITET WYBORCZY POLSKIE STRONNICTWO LUDOWE - ZPOW-601-19/19`))
  ) %>%
  select(`Kod TERYT`, Powiat, year, `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ - ZPOW-601-9/19`, `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19`, `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`) %>%
  rename(
    Code = `Kod TERYT`,
    County = Powiat,
    `Prawo i Sprawiedliwosc` = `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ - ZPOW-601-9/19`,
    `Platforma Obywatelska` = `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI - ZPOW-601-6/19`,
    `Valid ballot papers` = `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`
  )

# --- 2023 ---
elections_2023_clean <- read_excel("Data/wyniki_gl_na_listy_po_powiatach_sejm_utf8.xlsx") %>%
  mutate(
    year = "2023",
    `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI` = 
      suppressWarnings(as.numeric(`KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`)) + 
      suppressWarnings(as.numeric(`KOMITET WYBORCZY NOWA LEWICA`)) + 
      suppressWarnings(as.numeric(`KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE`))
  ) %>%
  select(`TERYT Powiatu`, Powiat, year, `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ`, `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`, `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`) %>%
  rename(
    Code = `TERYT Powiatu`,
    County = Powiat,
    `Prawo i Sprawiedliwosc` = `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ`,
    `Platforma Obywatelska` = `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`,
    `Valid ballot papers` = `Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów`
  )

# --- COMBINING ELECTION DATASETS AND KEY CONVERSION ---
election_dfs <- list(
  elections_2007_clean, elections_2011_clean,
  elections_2015_clean, elections_2019_clean, elections_2023_clean
)

elections_combined <- bind_rows(election_dfs) %>%
  mutate(
    Code = as.character(Code),
    year = as.character(year),
    `Prawo i Sprawiedliwosc` = suppressWarnings(as.numeric(`Prawo i Sprawiedliwosc`)),
    `Platforma Obywatelska` = suppressWarnings(as.numeric(`Platforma Obywatelska`)),
    `Valid ballot papers` = suppressWarnings(as.numeric(`Valid ballot papers`))
  )

# ------------------------------------------------------------------------------
# 3. FINAL MERGE AND CLEANING
# ------------------------------------------------------------------------------

final_merge_selected <- left_join(elections_combined, socioeconomic_panel_data, by = c("Code", "year")) %>%
  select(
    Code, year, 
    `Prawo i Sprawiedliwosc`, 
    `Platforma Obywatelska`, 
    `Valid ballot papers`, 
    average_salary, 
    population_size, 
    unemployment_rate
  ) %>%
  filter(
    !as.numeric(Code) %in% c(265000, 2467000, 149900, 149800, 20600, 206000),
    !is.na(Code)
  )

# ------------------------------------------------------------------------------
# 4. PANEL DATA STRUCTURE AND EXPORT
# ------------------------------------------------------------------------------

# Create PLM panel data structure
final_panel_data <- pdata.frame(final_merge_selected, index = c("Code", "year"))

# Data preview
print(head(final_panel_data))