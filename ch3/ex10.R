library(fpp3)

canadian_gas |> autoplot()
canadian_gas |> gg_season()
canadian_gas |> gg_subseries()

canadian_gas |>
  model(
    STL(Volume ~ trend() + season(window = 15))
  ) |>
  components() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Volume), colour = "grey") +
  geom_line(aes(y = trend), colour = "firebrick") +
  geom_line(aes(y = season_adjust), colour = "green4")

# It is very difficult to produce a plausible seasonally adjusted series,
# because of the high-volatility but medium value region in the middle of the
# series. Default STL parameters give a reasonable-looking seasonally adjusted
# series, but the remainder is clearly not white noise. We can also clearly see
# in the middle of the graph that the seasonality has not been completely
# removed.
