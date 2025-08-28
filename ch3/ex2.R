library(fpp3)

# USA GDP ----
usgdp <- global_economy |>
  filter(Country == "United States")

usgdp |>
  mutate(GDP_10k = GDP / 10e4) |>
  autoplot(GDP_10k) +
  geom_line(aes(y = Population), colour = "firebrick") +
  labs(
    y = "GDP ($10,000s) / Population",
    title = "GDP (black) and population (red) of the USA"
  )

# Clearly we want a per capita transformation at the very least, possible also
# adjusting for inflation.

usgdp |>
  mutate(
    GDPperCapita = GDP / Population,
    InflationFactor = CPI / min(CPI, na.rm = TRUE),
    GDPinfadj = GDPperCapita / InflationFactor
  ) |>
  autoplot(GDPperCapita) +
  geom_line(aes(y = GDPinfadj), colour = "orchid4")

# We can see that indexed to 1960 PPP,the GDP growth in real terms is far less
# impressive.

# Steers slaughtered in Victoria ----
steers <- aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria")

steers |> autoplot()

# Could benefit from a Box-Cox transform - check the Guerrero feature
steers |> features(Count, features = guerrero)

# Close to 0, so we do a log transform
steers |>
  mutate(logCount = log(Count)) |>
  autoplot(logCount)

# Electricity demand in Victoria ----
vic_elec |>
  autoplot(Demand) +
  geom_point(aes(y = Holiday * 2000), colour = "green4")

# Demand is much lower on holiday days but this is probably information we want
# to capture...
#
# Check the Guerrero coefficient
vic_elec |> features(Demand, features = guerrero)

# Roughly 0.1 - again, we will just do a log transform
vic_elec |>
  mutate(logDemand = log(Demand)) |>
  autoplot(logDemand)

# Australian gas production ----
gas <- aus_production |> select(Gas)

gas |> autoplot()

# Clearly needs Box-Cox. We try a log-transform first
gas |>
  mutate(logGas = log(Gas)) |>
  autoplot(logGas)

# Looks overcompressed if anything. Check the Guerrero coefficient
lambda <- gas |> features(Gas, features = guerrero)
lambda
# 0.11 - log is reasonable but 0.11 is visibly more even.
gas |> autoplot(box_cox(Gas, lambda))
