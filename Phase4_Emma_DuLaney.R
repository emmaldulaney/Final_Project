baseline_pred <- mean(merged_data$SOVI_SCORE, na.rm = TRUE)
baseline_rmse <- sqrt(mean((merged_data$SOVI_SCORE - baseline_pred)^2, na.rm = TRUE))
baseline_rmse

linear_model <- lm(
  SOVI_SCORE ~ 
    pct_owner_occupied +
    pct_renter_occupied +
    pct_hs_or_higher +
    pct_ba_or_higher +
    median_hh_income +
    poverty_rate +
    pct_in_labor_force +
    pct_unemployed +
    pct_armed_forces,
  data = merged_data
)

summary(linear_model)
linear_pred <- predict(linear_model, merged_data)

linear_rmse <- sqrt(mean((merged_data$SOVI_SCORE - linear_pred)^2, na.rm = TRUE))
linear_rmse

# Linear performed far better than the naive, and had a reduced RMSE of 11.
