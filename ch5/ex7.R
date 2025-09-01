library(fpp3)

set.seed(414961481)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries_train <- myseries |>
  filter(year(Month) < 2011)

autoplot(myseries, Turnover) +
  autolayer(myseries_train, Turnover, colour = "red")

fit <- myseries_train |>
  model(SNAIVE(Turnover))

fit |> gg_tsresiduals()

# Strong directional trend, heavy positive skew.

fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))

fc |> autoplot(myseries)

fit |> accuracy()
fc |> accuracy(myseries)
