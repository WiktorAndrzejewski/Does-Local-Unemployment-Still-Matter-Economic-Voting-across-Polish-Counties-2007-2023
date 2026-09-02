library(ggplot2)
library(marginaleffects)

# 1. Compute marginal predictions incorporating clustered SEs
pred_baseline <- predictions(
  Final_Basic_Model,
  newdata = datagrid(Delta_Unemployment_Rate = seq(
    min(Data$Delta_Unemployment_Rate, na.rm = TRUE),
    max(Data$Delta_Unemployment_Rate, na.rm = TRUE),
    length.out = 200
  )),
  vcov = clustered_vcov
)

# 2. Define ggplot2 visualization
p_baseline <- ggplot(pred_baseline, aes(x = Delta_Unemployment_Rate, y = estimate)) +
  # 95% confidence interval ribbon
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#2c3e50", alpha = 0.2) +
  # Baseline model fitted line
  geom_line(color = "#1a365d", linewidth = 1.1) +
  # Reference lines (zero intercepts)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.6) +
  # Rug plot showing empirical data distribution
  geom_rug(data = Data, aes(x = Delta_Unemployment_Rate, y = NULL), 
           sides = "b", alpha = 0.3, color = "black", length = unit(0.03, "npc")) +
  # Axis scales and formatting
  scale_x_continuous(
    breaks = seq(
      floor(min(Data$Delta_Unemployment_Rate, na.rm = TRUE)),
      ceiling(max(Data$Delta_Unemployment_Rate, na.rm = TRUE)),
      by = 2
    ),
    labels = function(x) paste0(x, " p.p.")
  ) +
  scale_y_continuous(
    labels = function(y) paste0(ifelse(y > 0, "+", ""), y, "%")
  ) +
  # Labels
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

# 3. Display plot
print(p_baseline)