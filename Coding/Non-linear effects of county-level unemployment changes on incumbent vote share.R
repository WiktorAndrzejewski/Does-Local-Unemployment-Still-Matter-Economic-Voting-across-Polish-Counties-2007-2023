# ==============================================================================
# QUADRATIC EFFECT PLOT (MODEL 2: NON-LINEAR ECONOMIC VOTING)
# ==============================================================================

library(ggplot2)
library(dplyr)

# 1. Sequence range for unemployment changes based on empirical data
unemp_seq <- seq(
  from = min(Data$Delta_Unemployment_Rate, na.rm = TRUE),
  to   = max(Data$Delta_Unemployment_Rate, na.rm = TRUE),
  length.out = 300
)

# 2. Extract coefficient estimates from Model 2
# (Extracting partial effects associated with unemployment changes)
b1 <- coef(Delta_Squared_Weights)["Delta_Unemployment_Rate"]
b2 <- coef(Delta_Squared_Weights)["I(Delta_Unemployment_Rate^2)"]

# Variance-covariance matrix (clustered at the county level)
vcov_mat <- sandwich::vcovCL(Delta_Squared_Weights, cluster = ~Code)
cov_b1_b2 <- vcov_mat["Delta_Unemployment_Rate", "I(Delta_Unemployment_Rate^2)"]
var_b1    <- vcov_mat["Delta_Unemployment_Rate", "Delta_Unemployment_Rate"]
var_b2    <- vcov_mat["I(Delta_Unemployment_Rate^2)", "I(Delta_Unemployment_Rate^2)"]

# 3. Prepare data frame for predicted effects and 95% confidence intervals
pred_df <- tibble(
  Delta_Unemployment = unemp_seq,
  # Partial effect (zeroed out when unemployment change Delta = 0)
  Predicted_Effect   = b1 * unemp_seq + b2 * (unemp_seq^2),
  # Standard error of the linear combination: Var(b1*x + b2*x^2)
  SE = sqrt((unemp_seq^2) * var_b1 + (unemp_seq^4) * var_b2 + 2 * (unemp_seq^3) * cov_b1_b2)
) %>%
  mutate(
    CI_lower = Predicted_Effect - 1.96 * SE,
    CI_upper = Predicted_Effect + 1.96 * SE
  )

# 4. Generate publication-ready visualization (ggplot2)
quadratic_plot <- ggplot(pred_df, aes(x = Delta_Unemployment, y = Predicted_Effect)) +
  # 95% Confidence interval ribbon
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = "#0B2545", alpha = 0.15) +
  # Zero reference line (no effect on support)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60", linewidth = 0.4) +
  # Quadratic curve
  geom_line(color = "#0B2545", linewidth = 1.1) +
  # Rug plot showing the distribution of actual observations in the sample
  geom_rug(
    data = Data %>% filter(!is.na(Delta_Unemployment_Rate)),
    aes(x = Delta_Unemployment_Rate),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.25,
    color = "grey30"
  ) +
  scale_x_continuous(
    breaks = seq(-14, 8, by = 2),
    labels = function(x) paste0(x, " p.p.")
  ) +
  scale_y_continuous(
    breaks = seq(-4, 6, by = 1),
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")
  ) +
  labs(
    x = "Δ Unemployment Rate (percentage points)",
    y = "Marginal Impact on Incumbent Vote Share (p.p.)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 8)),
    axis.text = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(t = 8, r = 12, b = 8, l = 8)
  )

# Display plot
print(quadratic_plot)