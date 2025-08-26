library(fpp3)

# US private employment ----
ts1 <- us_employment |>
  filter(Series_ID == "CEU0500000001")

# Strong linear upward trend, dips around big recessions
# Annual seasonality
# Clear cyclicality around economic cycle
ts1 |> autoplot()

# Seasonal peak roughly in Aug/Sept, trough at new year
# Trend is still the main component
ts1 |> gg_season()
ts1 |> gg_subseries()

# Very strong autocorrelation, as is to be expected of a strong linear trend
ts1 |> gg_lag(geom = "point")
ts1 |> ACF(Employed) |> autoplot()

# Australian brick production ----
ts2 <- aus_production |>
  select(Bricks)

# Increasing then decreasing trend
# Annual seasonality
# Cyclicality after 1980: steady increases then sharp fall
ts2 |> autoplot()

# Seasonal peak in Q3, low in Q1
ts2 |> gg_season()
ts2 |> gg_subseries()

# Decent positive autocorrelation, typical scalloped ACF
ts2 |> gg_lag(geom = "point")
ts2 |> ACF(Bricks) |> autoplot()

# Hare pelts ----
ts3 <- pelt |>
  select(Hare)

# No clear trend
# Clear cyclicality which dominates the behaviour
ts3 |> autoplot()

# Periodic with roughly 10-year period
# Autocorrelation is not hugely strong but clearly distinct from white noise
ts3 |> gg_lag(geom = "point")
ts3 |> ACF(Hare) |> autoplot()

# PBS corticosteroid cost ----
ts4 <- PBS |>
  filter(ATC2 == "H02") |>
  select(Cost)

ts4 |> autoplot()

ts4 |> gg_season()
ts4 |> gg_subseries()

ts4 |> ACF(Cost) |> autoplot()
