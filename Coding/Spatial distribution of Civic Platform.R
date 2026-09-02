# ==============================================================================
# SPATIAL PANEL 2x2: CIVIC PLATFORM / CIVIC COALITION (PO / KO) (2011–2023)
# ==============================================================================

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# 1. Data preparation for Civic Platform (PO)
selected_panel_years <- c("2011", "2015", "2019", "2023")

electoral_panel_map_data_po <- final_merge_selected %>%
  filter(year %in% selected_panel_years) %>%
  left_join(
    socioeconomic_panel_data %>% select(Code, County) %>% distinct(), 
    by = "Code"
  ) %>%
  mutate(
    county_clean = normalize_county_name(County),
    PO_Vote_Share = (`Platforma Obywatelska` / `Valid ballot papers`) * 100,
    year_facet = factor(year, levels = c("2011", "2015", "2019", "2023"))
  )

panel_map_po_sf <- left_join(powiaty_sf, electoral_panel_map_data_po, by = "county_clean") %>%
  filter(!is.na(year))

# 2. Generate 2x2 faceted spatial panel for PO
map_panel_po_final <- ggplot(data = panel_map_po_sf) +
  geom_sf(aes(fill = PO_Vote_Share), color = "grey90", linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "rocket",        # Warm palette suitable for the liberal/PO bloc
    direction = -1,
    limits = c(10, 75),
    breaks = seq(10, 70, by = 15),
    labels = function(x) paste0(x, "%"),
    name = "PO / KO Vote Share (%)",
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
    # Year strip labels above maps
    strip.text = element_text(face = "bold", size = 9.5, color = "grey20", margin = margin(b = 1, t = 1)),
    strip.background = element_blank(),
    
    # Compact bottom legend
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 7.5, margin = margin(b = 1.5)),
    legend.text = element_text(size = 7),
    legend.margin = margin(t = 2, b = 2),
    
    # Maximize map plot area
    panel.spacing.x = unit(0.05, "lines"),
    panel.spacing.y = unit(0.15, "lines"),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
  )

# Display plot
print(map_panel_po_final)