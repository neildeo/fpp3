library(fpp3)

# Between 1990 and 1995
aus_livestock |>
  filter(Animal == "Pigs", State == "Victoria") |>
  filter_index("1990" ~ "1995") |>
  autoplot()

aus_livestock |>
  filter(Animal == "Pigs", State == "Victoria") |>
  filter_index("1990" ~ "1995") |>
  ACF(Count) |>
  autoplot()

# Full duration
aus_livestock |>
  filter(Animal == "Pigs", State == "Victoria") |>
  autoplot()

aus_livestock |>
  filter(Animal == "Pigs", State == "Victoria") |>
  ACF(Count) |>
  autoplot()
