library(ggplot2)

# 1. Extract estimates and clustered covariance matrix
coefs <- coef(Asymmetric_influence_Partisan)
vcov_mat <- clustered_vcov(Asymmetric_influence_Partisan)

# Coefficients
b_neg_pis <- coefs["delta_unemployment_if_negxPiS"]
b_pos_pis <- coefs["delta_unemployment_if_posxPiS"]
b_neg_po  <- coefs["delta_unemployment_if_negxPO"]
b_pos_po  <- coefs["delta_unemployment_if_posxPO"]

# Standard errors
se_neg_pis <- sqrt(vcov_mat["delta_unemployment_if_negxPiS", "delta_unemployment_if_negxPiS"])
se_pos_pis <- sqrt(vcov_mat["delta_unemployment_if_posxPiS", "delta_unemployment_if_posxPiS"])
se_neg_po  <- sqrt(vcov_mat["delta_unemployment_if_negxPO", "delta_unemployment_if_negxPO"])
se_pos_po  <- sqrt(vcov_mat["delta_unemployment_if_posxPO", "delta_unemployment_if_posxPO"])

# 2. Variable empirical ranges from data
min_val <- min(Data$Delta_Unemployment_Rate, na.rm = TRUE)
max_val <- max(Data$Delta_Unemployment_Rate, na.rm = TRUE)

# 3. Compute predicted values and confidence intervals for PiS
grid_neg_pis <- data.frame(Delta_Unemployment_Rate = seq(min_val, 0, length.out = 150), Party = "Law and Justice (PiS)")
grid_neg_pis$estimate  <- grid_neg_pis$Delta_Unemployment_Rate * b_neg_pis
grid_neg_pis$conf_low  <- grid_neg_pis$Delta_Unemployment_Rate * (b_neg_pis + 1.96 * se_neg_pis)
grid_neg_pis$conf_high <- grid_neg_pis$Delta_Unemployment_Rate * (b_neg_pis - 1.96 * se_neg_pis)

grid_pos_pis <- data.frame(Delta_Unemployment_Rate = seq(0, max_val, length.out = 150), Party = "Law and Justice (PiS)")
grid_pos_pis$estimate  <- grid_pos_pis$Delta_Unemployment_Rate * b_pos_pis
grid_pos_pis$conf_low  <- grid_pos_pis$Delta_Unemployment_Rate * (b_pos_pis - 1.96 * se_pos_pis)
grid_pos_pis$conf_high <- grid_pos_pis$Delta_Unemployment_Rate * (b_pos_pis + 1.96 * se_pos_pis)

# 4. Compute predicted values and confidence intervals for PO
grid_neg_po <- data.frame(Delta_Unemployment_Rate = seq(min_val, 0, length.out = 150), Party = "Civic Platform (PO)")
grid_neg_po$estimate  <- grid_neg_po$Delta_Unemployment_Rate * b_neg_po
grid_neg_po$conf_low  <- grid_neg_po$Delta_Unemployment_Rate * (b_neg_po + 1.96 * se_neg_po)
grid_neg_po$conf_high <- grid_neg_po$Delta_Unemployment_Rate * (b_neg_po - 1.96 * se_neg_po)

grid_pos_po <- data.frame(Delta_Unemployment_Rate = seq(0, max_val, length.out = 150), Party = "Civic Platform (PO)")
grid_pos_po$estimate  <- grid_pos_po$Delta_Unemployment_Rate * b_pos_po
grid_pos_po$conf_low  <- grid_pos_po$Delta_Unemployment_Rate * (b_pos_po - 1.96 * se_pos_po)
grid_pos_po$conf_high <- grid_pos_po$Delta_Unemployment_Rate * (b_pos_po + 1.96 * se_pos_po)

pred_partisan <- rbind(grid_neg_pis, grid_pos_pis, grid_neg_po, grid_pos_po)

# 5. Define grayscale ggplot2 visualization with dual encoding (color + linetype)
p_partisan <- ggplot(pred_partisan, aes(x = Delta_Unemployment_Rate, y = estimate, color = Party, fill = Party, linetype = Party)) +
  # 95% Confidence intervals
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.22, color = NA) +
  # Piecewise fitted lines
  geom_line(linewidth = 1.0) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey65", linewidth = 0.5) +
  # Rug plot of empirical data distribution
  geom_rug(data = Data, aes(x = Delta_Unemployment_Rate, y = NULL, color = NULL, fill = NULL, linetype = NULL), 
           sides = "b", alpha = 0.25, color = "black", length = unit(0.03, "npc")) +
  # Grayscale palettes and line styling for black-and-white print legibility
  scale_color_manual(values = c("Civic Platform (PO)" = "grey55", "Law and Justice (PiS)" = "grey10")) +
  scale_fill_manual(values = c("Civic Platform (PO)" = "grey55", "Law and Justice (PiS)" = "grey10")) +
  scale_linetype_manual(values = c("Civic Platform (PO)" = "dashed", "Law and Justice (PiS)" = "solid")) +
  scale_x_continuous(
    breaks = seq(floor(min_val), ceiling(max_val), by = 2),
    labels = function(x) paste0(x, " p.p.")
  ) +
  scale_y_continuous(
    labels = function(y) paste0(ifelse(y > 0, "+", ""), round(y, 1), "%")
  ) +
  labs(
    x = expression(Delta * " Unemployment Rate (percentage points)"),
    y = expression("Predicted " * Delta * " Incumbent Vote Share (p.p.)"),
    color = "Incumbent Party:",
    fill = "Incumbent Party:",
    linetype = "Incumbent Party:"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0, margin = margin(b = 8)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

print(p_partisan)
