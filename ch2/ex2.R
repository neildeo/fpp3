library(fpp3)

close <- gafa_stock |> select(Close)

close |>
  filter(Close == max(Close), .by = Symbol)
