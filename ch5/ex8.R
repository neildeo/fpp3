library(fpp3)

nswpigs <- aus_livestock |>
  filter(Animal == "Pigs", State == "New South Wales") |>
  select(-State, -Animal)

nswpigs |> gg_tsdisplay()

nswpigs_train <- nswpigs |> filter(year(Month) <= 2012)

nswpigs_train |> gg_tsdisplay()

fit <- nswpigs_train |>
  model(
    NAIVE(Count),
    SNAIVE(Count),
    RW(Count ~ drift())
  )

# Naive
# There is a clear seasonality so the the residual ACF reflects this, since
# the model cannot. The residuals have a heavy negative skew.
fit[1] |> gg_tsresiduals()

# Seasonal naive
# The residuals are quite trended as is the time plot in parts, suggesting
# evolving behaviour which isn't captured in the most recent season. Again,
# there is a heavy negative skew in the residuals.
fit[2] |> gg_tsresiduals()

# Drift
# Pretty similar to the naive model, maybe slightly less crap.
fit[3] |> gg_tsresiduals()

fc <- fit |> forecast(anti_join(nswpigs, nswpigs_train))
fc |> autoplot(nswpigs, level = NULL)
fc |> accuracy(nswpigs)

# None of these are really any good, but seasonal naive at least provides a
# reasonable benchmark.
