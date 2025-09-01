library(fpp3)

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Beer)

fit <- recent_production |>
  model(SNAIVE(Beer))

fit |> forecast() |> autoplot(recent_production)

fit |> gg_tsresiduals()

# The residuals aren't white noise - 3/4 of the first coefficients exceed the
# threshold.
#
# The autocorrelation spike at a lag of one year suggests that the seasonal
# pattern is shifting over time - the seasonal naive model doesn't capture
# this effect, so some of this pattern is left in the residuals.
