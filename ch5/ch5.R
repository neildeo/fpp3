# FORECASTER'S TOOLBOX ----
library(fpp3)
# Simple methods ----
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")

beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    Naive = NAIVE(Beer),
    SNaive = SNAIVE(Beer)
  )

beer_fc <- beer_fit |> forecast(h = 14)

beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    titel = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Residual diagnostics ----
# Simple methods as a benchmark
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

google_2015 <- google_stock |> filter(year(Date) == 2015)

google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)

google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(
    y = "USD",
    title = "Google daily closing stock prices",
    subtitle = "Jan 2015 - Jan 2016"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Examine residuals
aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()

autoplot(aug, .innov) +
  labs(
    y = "USD",
    title = "Residuals from the naive method"
  )

# Histogram to check for normality
aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")
# Definite positive skew

aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residual ACF from the naive method")

# Convenience function gg_tsresiduals does all 3 plots in one call
google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

# Portmanteau tests
aug |>
  features(.innov, list(box_pierce, ljung_box), lag = 10)

# Distributional forecasts ----
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  autoplot(google_2015) +
  labs(title = "Google daily closing stock price", y = "USD")
# By default, 80% and 95% confidence regions are plotted.

# Bootstrap prediction interval
fit <- google_2015 |>
  model(NAIVE(Close))
sim <- fit |>
  generate(h = 30, times = 5, bootstrap = TRUE)
sim

# Possible trajectories sampled using bootstrap
google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  labs(title = "Google daily closing stock price", y = "$US") +
  guides(colour = "none")

fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

autoplot(fc, google_2015)

# Evaluating point accuracy ----
accuracy(google_fc, google_stock)

# Evaluating distributional forecasts ----
google_fc |>
  filter(.model == "Naive") |>
  autoplot(bind_rows(google_2015, google_jan_2016), level = 80) +
  labs(
    y = "USD",
    title = "Google closing stock prices"
  )
