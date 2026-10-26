model_results <- data.frame(
  Model = c(
    "ARMA(1,1) + DOW",
    "ARMA(1,1) + Yr + DOW",
    "ARMA(1,1) + Yr + DOW (combined weekends)",
    "ARMA(1,1) + Yr + DOW + M6 + M7",
    "ARMA(1,1) + Yr + DOW + freeze_day + wz_fog",
    "ARMA(1,1) + Yr + DOW + M6 + M7 + freeze_day + wz_fog",
    "ARMA(1,1) + Yr + DOW + M6 + M7 + freeze_day + wz_fog + holiday_lead1"
  ),
  MAE = c(
    12.36568,
    12.32779,
    12.33408,
    12.12693,
    12.19101,
    11.99495,
    11.92417
  ),
  RMSE = c(
    14.87595,
    14.82944,
    14.79694,
    14.71726,
    14.70391,
    14.58523,
    14.62455
  )
)

plot_data <- model_results %>%
  mutate(
    Model = factor(Model, levels = Model)
  ) %>%
  pivot_longer(
    cols = c(MAE, RMSE),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(
  plot_data,
  aes(
    x = Model,
    y = Value
  )
) +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) +
  coord_flip() +
  facet_wrap(
    ~ Metric,
    scales = "free_x"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Forecast Performance by Model"
  ) +
  theme_minimal(base_size = 12)