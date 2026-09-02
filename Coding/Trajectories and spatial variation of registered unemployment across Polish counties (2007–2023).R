# ==============================================================================
# SPAGHETTI PLOT: COUNTY-LEVEL UNEMPLOYMENT RATE OVER TIME (2007–2023)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(scales)

# 1. County-level data preparation (from panel dataset)
unemp_panel <- final_merge_selected %>%
  select(Code, year, unemployment_rate) %>%
  filter(!is.na(unemployment_rate)) %>%
  mutate(
    year = as.numeric(year),
    unemployment_rate = as.numeric(unemployment_rate)
  )

# 2. Calculate nationwide annual summary statistics
national_unemp_trend <- unemp_panel %>%
  group_by(year) %>%
  summarise(
    mean_unemployment   = mean(unemployment_rate, na.rm = TRUE),
    median_unemployment = median(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Generate publication-ready visualization
spaghetti_plot <- ggplot() +
  # --- A. Individual county trajectories (displaying between/within variance) ---
  geom_line(
    data = unemp_panel,
    aes(x = year, y = unemployment_rate, group = Code),
    color = "#8D99AE",
    alpha = 0.22,
    linewidth = 0.35
  ) +
  
  # --- B. Vertical reference lines for parliamentary election years ---
  geom_vline(
    xintercept = c(2007, 2011, 2015, 2019, 2023),
    linetype = "dotted",
    color = "grey60",
    linewidth = 0.4
  ) +
  
  # --- C. Nationwide mean trajectory ---
  geom_line(
    data = national_unemp_trend,
    aes(x = year, y = mean_unemployment),
    color = "#0B2545",
    linewidth = 1.4
  ) +
  geom_point(
    data = national_unemp_trend,
    aes(x = year, y = mean_unemployment),
    color = "#0B2545",
    fill = "white",
    shape = 21,
    size = 2.8,
    stroke = 1.2
  ) +
  
  # --- D. Direct line annotation for nationwide mean (in place of legend) ---
  annotate(
    "text",
    x = 2023.1,
    y = tail(national_unemp_trend$mean_unemployment, 1) + 0.3,
    label = " Mean",
    hjust = 0,
    vjust = 0.5,
    size = 3.2,
    fontface = "bold",
    color = "#0B2545"
  ) +
  
  # --- E. Axis scales ---
  scale_x_continuous(
    breaks = c(2007, 2011, 2015, 2019, 2023),
    limits = c(2006.8, 2024.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 35, by = 5),
    limits = c(0, max(unemp_panel$unemployment_rate, na.rm = TRUE) + 2),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  
  # --- F. Journal-standard axis labels ---
  labs(
    x = "Election Year",
    y = "Registered Unemployment Rate (%)"
  ) +
  
  # --- G. Clean, minimal publication theme ---
  theme_minimal(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 8)),
    axis.text = element_text(color = "black", size = 9),
    axis.line = element_line(color = "grey40", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed", linewidth = 0.3),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey45", margin = margin(t = 8)),
    plot.margin = margin(t = 8, r = 24, b = 8, l = 8)
  )

# Display plot
print(spaghetti_plot)