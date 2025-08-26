library(fpp3)

dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE) |>
  mutate(diff = difference(Close))

# We have to re-index the tsibble to have a regular period, since trading days
# are not regularly spaced in time.

dgoog |> autoplot(diff)

dgoog |> ACF(diff) |> autoplot()

# It's white noise: only 1 out of 23 autocorrelations are outside of the white
# noise bands.
