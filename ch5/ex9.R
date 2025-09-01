library(fpp3)

wealth <- hh_budget |> select(Wealth)

wealth$Year |> max()

wealth_train <- wealth |> filter(Year < 2013)

wealth_train |> autoplot()

fit <- wealth_train |>
  model(
    NAIVE(),
    RW(Wealth ~ drift())
  )

fc <- fit |> forecast(h = 4)

fc |> autoplot(wealth)

fc |> accuracy(wealth)

# The drift method is better for all four countries.
country <- "Canada"

fit |>
  select(`RW(Wealth ~ drift())`) |>
  filter(Country == country) |>
  gg_tsresiduals()

# The residual ACF looks alright, mostly due to small sample size. However,
# all series have a big negative residual in 2007 or 2008 - the recession is
# not captured by these simple models. As such, the residual distributions are
# all negatively skewed.
