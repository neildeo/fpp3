library(fpp3)

?global_economy

global_economy |> autoplot(GDP) + theme(legend.position = "none")

global_economy |>
  mutate(GDPperCapita = GDP / Population) |>
  arrange(-GDPperCapita) |>
  autoplot(GDPperCapita) +
  theme(legend.position = "none")

global_economy |>
  mutate(GDPperCapita = GDP / Population) |>
  filter(GDPperCapita == max(GDPperCapita, na.rm = TRUE), .by = Year) |>
  arrange(Year) |>
  print(n = 70)
