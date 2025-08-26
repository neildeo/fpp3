install.packages("USgas")

USgas::us_total |> head(20)

usgas <- USgas::us_total |>
  as_tsibble(key = state, index = year)

usgas |> filter(state != "U.S.") |> autoplot()

negas <- usgas |>
  filter(
    state %in%
      c(
        "Connecticut",
        "Maine",
        "Massachusetts",
        "New Hampshire",
        "Rhode Island",
        "Vermont"
      )
  ) |>
  summarise(y = sum(y))

negas |> autoplot()
