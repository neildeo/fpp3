library(fpp3)

exports <- global_economy |>
  filter(Country == "Australia") |>
  select(Exports)

exports |> autoplot()

fit <- exports |>
  model(NAIVE(Exports))

fit |> gg_tsresiduals()

# Residuals actually look good: autocorrelation is plausibly white noise and
# the histogram looks roughly normal. There is no obvious pattern in the time
# plot.

fit |> forecast() |> autoplot(exports)


bricks <- aus_production |>
  select(Bricks)

bricks |> autoplot()

# This series is seasonal

fit <- bricks |>
  model(SNAIVE(Bricks))

fit |> gg_tsresiduals()

# These residuals are BAD - they are huge, asymmetrically distributed and have
# a very structured ACF suggesting a clear seasonality.
