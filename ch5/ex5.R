library(fpp3)

vic <- aus_livestock |>
  filter(State == "Victoria") |>
  select(-State)

vic |> autoplot()

vic |>
  model(SNAIVE(Count)) |>
  forecast() |>
  autoplot(vic, level = 95)

# The forecasts look pretty reasonable - certainly good enough as a
# benchmark. The seasonal patterns are slowly changing enough to have the
# naive forecast be acceptable for short forecast horizons.
