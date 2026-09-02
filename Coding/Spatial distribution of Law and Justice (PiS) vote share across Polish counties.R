# ==============================================================================
# SPATIAL PANEL 2x2: MAPS ONLY, YEAR STRIP LABELS, AND COMPACT LEGEND
# ==============================================================================

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# 1. Multi-year data preparation
selected_panel_years <- c("2011", "2015", "2019", "2023")

electoral_panel_map_data <- final_merge_selected %>%
  filter(year %in% selected_panel_years) %>%
  left_join(
    socioeconomic_panel_data %>% select(Code, County) %>% distinct(), 
    by = "Code"
  ) %>%
  mutate(
    county_clean = normalize_county_name(County),
    PiS_Vote_Share = (`Prawo i Sprawiedliwosc` / `Valid ballot papers`) * 100,
    year_facet = factor(year, levels = c("2011", "2015", "2019", "2023"))
  )

panel_map_sf <- left_join(powiaty_sf, electoral_panel_map_data, by = "county_clean") %>%
  filter(!is.na(year))

# 2. Generate visualization without titles or subtitles
map_panel_final <- ggplot(data = panel_map_sf) +
  geom_sf(aes(fill = PiS_Vote_Share), color = "grey90", linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    limits = c(10, 75),
    breaks = seq(10, 70, by = 15),
    labels = function(x) paste0(x, "%"),
    name = "PiS Vote Share (%)",
    guide = guide_colorbar(
      barwidth = unit(5.0, "cm"),
      barheight = unit(0.18, "cm"),
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      ticks.colour = "white",
      frame.colour = "grey60"
    )
  ) +
  facet_wrap(~year_facet, ncol = 2) +
  theme_void(base_size = 8.5) +
  theme(
    # Year strip labels above individual facet maps
    strip.text = element_text(face = "bold", size = 9.5, color = "grey20", margin = margin(b = 1, t = 1)),
    strip.background = element_blank(),
    
    # Compact bottom legend
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 7.5, margin = margin(b = 1.5)),
    legend.text = element_text(size = 7),
    legend.margin = margin(t = 2, b = 2),
    
    # Tight panel margins to maximize map plotting area
    panel.spacing.x = unit(0.05, "lines"),
    panel.spacing.y = unit(0.15, "lines"),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
  )

# Display plot
print(map_panel_final)