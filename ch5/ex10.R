library(fpp3)

takeaway <- aus_retail |>
  filter(Industry == "Takeaway food services") |>
  summarise(Turnover = sum(Turnover))

takeaway_train <- takeaway |> filter(year(Month) <= 2014)

takeaway_train |> gg_tsdisplay()

fit <- takeaway_train |>
  model(
    SNAIVE(Turnover),
    SNAIVE(Turnover ~ drift())
  )

fc <- fit |> forecast(h = 48)

fc |> autoplot(takeaway)

# The method with drift is clearly superior.

fc |> accuracy(takeaway)

fit |> select(`SNAIVE(Turnover ~ drift())`) |> gg_tsresiduals()

# The residuals have very strong autocorrelation - not even close to white noise
