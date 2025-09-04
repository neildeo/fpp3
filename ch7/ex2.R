library(fpp3)

olympic_running |>
  autoplot(Time)

# Times tend to decrease over the years, with occasional kinks. There are
# missing periods for the two world wars.

fit <- olympic_running |>
  model(lm = TSLM(Time ~ trend()))

fit |> filter(Length == 100, Sex == "men") |> report()

# For the men's 100m, times have been decreasing at a rate of roughly 0.2s every
# Olympics.

fit |> first() |> gg_tsresiduals()

# The 1896 time is an outlier - it's very slwo. The rest of the residuals look
# pretty good.

fit |>
  forecast(h = 1) |>
  autoplot(olympic_running) +
  facet_grid(Length ~ Sex, scales = "free_y")

# The linear trend is an optimistic assumption - a decaying trend feels more
# reasonable. For example, the men's 100m PI mean would be a world record. Races
# with very old times have steeper coefficients which is arguable throwing off
# the line of best fit.
