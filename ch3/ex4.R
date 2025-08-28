library(fpp3)

set.seed(414961481)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries |> autoplot()

lambda <- myseries |>
  features(Turnover, features = guerrero) |>
  pull(lambda_guerrero)
lambda

myseries |> autoplot(box_cox(Turnover, lambda))
