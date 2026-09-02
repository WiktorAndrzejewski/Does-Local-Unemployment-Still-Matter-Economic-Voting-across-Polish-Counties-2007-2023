library(ggplot2)

# 1. Pobranie współczynników i macierzy kowariancji
coefs <- coef(Asymmetric_influence_Uneployment)
vcov_mat <- clustered_vcov(Asymmetric_influence_Uneployment)

b_neg <- coefs["delta_unemployment_if_neg"]
b_pos <- coefs["delta_unemployment_if_pos"]

se_neg <- sqrt(vcov_mat["delta_unemployment_if_neg", "delta_unemployment_if_neg"])
se_pos <- sqrt(vcov_mat["delta_unemployment_if_pos", "delta_unemployment_if_pos"])

# 2. Zakresy zmiennych z danych
min_val <- min(Data$delta_unemployment_if_neg, na.rm = TRUE)
max_val <- max(Data$delta_unemployment_if_pos, na.rm = TRUE)

# 3. Wyznaczenie punktów krzywej i przedziałów ufności (95% CI)
grid_neg <- data.frame(
  Delta_Unemployment_Rate = seq(min_val, 0, length.out = 150)
)
grid_neg$estimate  <- grid_neg$Delta_Unemployment_Rate * b_neg
grid_neg$conf_low  <- grid_neg$Delta_Unemployment_Rate * (b_neg + 1.96 * se_neg)
grid_neg$conf_high <- grid_neg$Delta_Unemployment_Rate * (b_neg - 1.96 * se_neg)

grid_pos <- data.frame(
  Delta_Unemployment_Rate = seq(0, max_val, length.out = 150)
)
grid_pos$estimate  <- grid_pos$Delta_Unemployment_Rate * b_pos
grid_pos$conf_low  <- grid_pos$Delta_Unemployment_Rate * (b_pos - 1.96 * se_pos)
grid_pos$conf_high <- grid_pos$Delta_Unemployment_Rate * (b_pos + 1.96 * se_pos)

pred_asym <- rbind(grid_neg, grid_pos)

# 4. Wykres ggplot2
p_asymmetric <- ggplot(pred_asym, aes(x = Delta_Unemployment_Rate, y = estimate)) +
  # 95% przedział ufności (Clustered SE)
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), fill = "#2c3e50", alpha = 0.2) +
  # Załamana linia regresji asymetrycznej
  geom_line(color = "#1a365d", linewidth = 1.1) +
  # Linie odniesienia (zero)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.6) +
  # Rug plot rozkładu danych empirycznych
  geom_rug(data = Data, aes(x = Delta_Unemployment_Rate, y = NULL), 
           sides = "b", alpha = 0.3, color = "black", length = unit(0.03, "npc")) +
  # Skale i etykiety
  scale_x_continuous(
    breaks = seq(floor(min_val), ceiling(max_val), by = 2),
    labels = function(x) paste0(x, " p.p.")
  ) +
  scale_y_continuous(
    labels = function(y) paste0(ifelse(y > 0, "+", ""), round(y, 1), "%")
  ) +
  labs(
    x = expression(Delta * " Unemployment Rate (percentage points)"),
    y = expression("Predicted " * Delta * " Incumbent Vote Share (p.p.)")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0, margin = margin(b = 8)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

print(p_asymmetric)