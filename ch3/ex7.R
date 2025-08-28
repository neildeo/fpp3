library(fpp3)

gas <- tail(aus_production, 5 * 4) |>
  select(Gas)

gas |> autoplot()
# Clear annual seasonality peaking in Q3 and troughing in Q1.
# Trend is linearly increasing.

gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  autoplot()

# This corroborates the initial impression: increasing, roughly linear trend is
# the main component, with seasonality playing a lesser role. The remainder is
# small in magnitude (0.97 - 1.03).

gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  autoplot(season_adjust)

gas$Gas[10] <- gas$Gas[10] + 300
gas |> autoplot()
gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  autoplot(season_adjust)

# The outlier makes the seasonal component much smaller.

gas$Gas[10] <- gas$Gas[10] - 300
gas$Gas[19] <- gas$Gas[19] + 300
gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  autoplot(season_adjust)

# The seasonal component estimate is similar - one expects the distortion to be
# less pronounced since the trend is less affected, but this is only slight.
