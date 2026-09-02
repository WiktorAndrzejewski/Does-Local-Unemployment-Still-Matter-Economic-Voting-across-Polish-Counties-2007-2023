library(dplyr)
library(tidyr)
library(ggplot2)

# ==============================================================================
# 1. DATA PREPARATION WITH INCUMBENCY FLAG
# ==============================================================================

# --- A. Data for Plot 1: Percentage vote share ---
bloc_shares <- elections_individual_combined %>%
  group_by(year) %>%
  summarise(
    Total_Valid   = sum(`Valid ballot papers`, na.rm = TRUE),
    PiS_Votes     = sum(PiS, na.rm = TRUE),
    PO_Bloc_Votes = sum(`PO / KO`, na.rm = TRUE) + 
      sum(`PSL / Trzecia Droga`, na.rm = TRUE) + 
      sum(Nowoczesna, na.rm = TRUE) + 
      sum(`Nowa Lewica`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(PiS_Votes, PO_Bloc_Votes),
    names_to = "Bloc",
    values_to = "Votes"
  ) %>%
  mutate(
    Bloc = ifelse(Bloc == "PiS_Votes", "PiS Coalition", "PO Coalition / Democratic Bloc"),
    Vote_Share = (Votes / Total_Valid) * 100,
    is_incumbent = (Bloc == "PiS Coalition" & year %in% c("2007", "2019", "2023")) |
      (Bloc == "PO Coalition / Democratic Bloc" & year %in% c("2011", "2015")),
    Label = ifelse(is_incumbent, sprintf("%.1f%%*", Vote_Share), sprintf("%.1f%%", Vote_Share)),
    year = factor(year, levels = c("2007", "2011", "2015", "2019", "2023")),
    Bloc = factor(Bloc, levels = c("PiS Coalition", "PO Coalition / Democratic Bloc"))
  )

# --- B. Data for Plot 2: Parliamentary seats (Sejm) ---
seats_detailed <- data.frame(
  year = rep(c("2007", "2011", "2015", "2019", "2023"), each = 3),
  Category = rep(c("PiS Coalition", "PO Coalition / Democratic Bloc", "Other / Non-aligned"), 5),
  Seats = c(
    166, 240, 54,   # 2007: PiS (166), PO+PSL (240), LiD+MN (54)
    157, 235, 68,   # 2011: PiS (157), PO+PSL (235), RP+SLD+MN (68)
    235, 182, 43,   # 2015: PiS (235), PO+.N+PSL (182), Kukiz'15+MN (43)
    235, 164, 61,   # 2019: PiS (235), KO+PSL (164), SLD+Konf+MN (61)
    194, 248, 18    # 2023: PiS (194), KO+TD+NL (248), Konfederacja (18)
  )
) %>%
  mutate(
    year = factor(year, levels = c("2007", "2011", "2015", "2019", "2023")),
    Category = factor(Category, levels = c("PiS Coalition", "PO Coalition / Democratic Bloc", "Other / Non-aligned")),
    is_incumbent = (Category == "PiS Coalition" & year %in% c("2007", "2019", "2023")) |
      (Category == "PO Coalition / Democratic Bloc" & year %in% c("2011", "2015")),
    Label = case_when(
      Seats < 20 ~ "",
      is_incumbent ~ paste0(Seats, "*"),
      TRUE ~ as.character(Seats)
    )
  ) %>%
  group_by(year) %>%
  mutate(Share = Seats / sum(Seats)) %>%
  ungroup()

# ==============================================================================
# PLOT 1: BAR CHART IN GRAYSCALE
# ==============================================================================

plot_vote_share_gray <- ggplot(bloc_shares, aes(x = year, y = Vote_Share, fill = Bloc)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.75), 
    width = 0.65, 
    color = "black", 
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = Label),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 2.4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "PiS Coalition"                  = "grey25",  # Dark grey
      "PO Coalition / Democratic Bloc" = "grey75"   # Light grey
    )
  ) +
  scale_y_continuous(
    limits = c(0, max(bloc_shares$Vote_Share) + 8),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Election Year",
    y = "Aggregate Vote Share (%)",
    fill = "Political Bloc:",
    caption = "* Indicates the governing incumbent held accountable for the preceding parliamentary term."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    axis.text.x = element_text(face = "bold", size = 10, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linetype = "dashed"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9.5),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40", margin = margin(t = 8))
  )

print(plot_vote_share_gray)