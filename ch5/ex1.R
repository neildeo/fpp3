library(fpp3)

# Australian population ----
auspop <- global_economy |>
  filter(Country == "Australia") |>
  select(Population)

auspop |> autoplot()

# No seasonality and a clear linear drift: drift will be the best simple model

auspop |>
  model(
    RW(Population ~ drift())
  ) |>
  forecast(h = 10) |>
  autoplot(auspop) +
  labs(title = "Forecasting Australian population with the drift method")

# Australian brick production ----
ausbricks <- aus_production |>
  select(Bricks) |>
  filter_index(. ~ "2005 Q2") # The final 5 years are NA

ausbricks |> autoplot()

# Some seasonality is apparent. The trend-cycle is non-linear.
# We will use seasonal naive.

ausbricks |>
  model(
    SNAIVE(Bricks)
  ) |>
  forecast(h = 10) |>
  autoplot(ausbricks) +
  labs(title = "Forecasting brick production with the seasonal naive method")

# NSW lambs ----
nswlamb <- aus_livestock |>
  filter(Animal == "Lambs", State == "New South Wales")

nswlamb |> autoplot()
nswlamb |> gg_season()
nswlamb |> gg_subseries()

# There looks to be some seasonality, though the series is very noisy.
# We will try seasonal naive with and without drift.

nswlamb |>
  model(
    SNAIVE(Count)
  ) |>
  forecast(h = 24) |>
  autoplot(nswlamb) +
  labs(title = "Forecasting New South Wales lamb production")

# Household wealth ----
wealth <- hh_budget |> select(Wealth)

wealth |> autoplot()

# Drift is the best model here

# Australian takeaway food turnover
takeaway <- aus_retail |>
  filter(Industry == "Takeaway food services") |>
  summarise(Turnover = sum(Turnover))

takeaway |> autoplot()

# Seasonal with drift will be best

takeaway |>
  model(
    SNAIVE(Turnover ~ drift())
  ) |>
  forecast(h = 24) |>
  autoplot(takeaway) +
  labs(title = "Forecasting Australian takeaway turnover")
