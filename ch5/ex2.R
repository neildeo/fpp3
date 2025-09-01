library(fpp3)

fb <- gafa_stock |>
  filter(Symbol == "FB") |>
  select(Close) |>
  mutate(index = row_number()) |>
  update_tsibble(index = index, regular = TRUE)

fb |> autoplot()

fb_extremes <- rbind(
  first(fb),
  last(fb)
)
last(fb)
# Drift method forecast
fb |>
  model(
    RW(Close ~ drift()),
    NAIVE(Close)
  ) |>
  forecast(h = 100) |>
  autoplot(fb, level = 80, alpha = 0.5) +
  annotate(
    "segment",
    x = first(fb$index),
    xend = last(fb$index),
    y = first(fb$Close),
    yend = last(fb$Close),
    colour = "grey",
    linetype = "dashed"
  )

# Naive may be better here, because the historical upward trend does not match
# the more recent downward trend. Stock prices are inherently unpredictable and
# the naive method concedes this.
