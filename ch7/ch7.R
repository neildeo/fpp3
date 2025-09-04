# TIME SERIES REGRESSION MODELS ----
library(fpp3)

# Linear models ----
# Scatter plot of predictor against outcome: gauge suitability of lm
us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(
    y = "Consumption, quarterly % change",
    x = "Income, quarterly % change"
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# TSLM function fits a linear model
us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

# Multiple regression: try comparative line plots for a sense of relationships
us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y = "% change")

# Plot matrix is a more purposeful method to do this
us_change |>
  GGally::ggpairs(columns = 2:6) # No need to plot Quarter

# Least squares ----
fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
report(fit_consMR)

# Plot fitted values in a time plot
fit_consMR |>
  augment() |> # Gives fitted values and residuals info
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption") +
  scale_colour_manual(values = c(Data = "black", Fitted = "green4")) +
  guides(colour = guide_legend(title = NULL))

# Plot fitted values in a scatter plot
fit_consMR |>
  augment() |>
  ggplot(aes(Consumption, .fitted)) +
  geom_point() +
  labs(
    y = "Fitted",
    x = "Actual",
    title = "Percent change in US consumption"
  ) +
  geom_abline(intercept = 0, slope = 1, colour = "#666")

# Model evaluation ----
# gg_tsresiduals gives useful diagnostic plots of residuals
fit_consMR |> gg_tsresiduals()

# features can extract useful summary statistics, e.g. Ljung-Box test
augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

# Plot residuals against predictors to check for patterns
us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(
    Income:Unemployment,
    names_to = "regressor",
    values_to = "x"
  ) |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# Plot residuals against fitted values to check for patterns
fit_consMR |>
  augment() |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

# Useful predictors ----
# Use beer production dataset
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

recent_production |>
  autoplot(Beer) +
  labs(
    y = "Megalitres",
    title = "Australian quarterly beer production"
  )

# trend() and season() specials in TSLM automatically generate the requisite
# time and dummy variables
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

# Time plot of fitted values
fit_beer |>
  augment() |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "orchid4")
  ) +
  labs(
    y = "Megalitres",
    title = "Australian quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Series"))

# Scatterplot of fitted values
fit_beer |>
  augment() |>
  ggplot(aes(x = Beer, y = .fitted, colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(
    y = "Fitted",
    x = "Actual",
    title = "Australian quarterly beer production"
  ) +
  geom_abline(intercept = 0, slope = 1, colour = "#444") +
  guides(colour = guide_legend(title = "Quarter"))

# fourier is a special which generates Fourier features
fourier_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

fourier_beer |>
  augment() |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Actual = "#333", Fitted = "firebrick")) +
  guides(colour = guide_legend(title = "Series"))

# Model selection ----
# glance shows all model summaries
beer_mable <- bind_cols(fit_beer, fourier_beer)

beer_mable |> glance()
# In this case, the Fourier and seasonal dummy models are identical in terms of
# their fitted values (though comparable coefficients differ slightly), so all
# of these metrics are identical.

# Forecasting ----
# With fit_beer, the features are known for all time so we can forecast easily
# over arbitrary horizons (though this is not a great idea!)
fc_beer <- forecast(fit_beer, h = 50)

fc_beer |>
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production with linear model",
    y = "Megalitres"
  )

# The scenarios and new_data functions make it easy to perform scenario-based
# forecasting
fit_consBest <- us_change |>
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income = 1, Savings = 0.5, Unemployment = 0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income = -1, Savings = -0.5, Unemployment = 0),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")
