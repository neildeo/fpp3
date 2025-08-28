library(fpp3)

# TIME SERIES GRAPHICS ----
# Tsibble basics ----
a10 <- PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6)

a10 |> head()

# Time plots ----
autoplot(a10, Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

# Seasonal plots ----
a10 |>
  gg_season(Cost, labels = "both") +
  labs(
    title = "Seasonal plot: Antidiabetic drug sales",
    y = "$ (millions)"
  )

# For multi-seasonal series, use the "period" argument to select the period
vic_elec |>
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec |>
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec |>
  gg_season(Demand, period = "year") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

# Seasonal subseries plots ----
a10 |>
  gg_subseries(Cost) +
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
  )

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
holidays

# All states have seasonal peaks - but their timing doesn't always match
autoplot(holidays, Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )

# A season plot makes this clearer
holidays |>
  gg_season(Trips) +
  labs(
    title = "Australian domestic holidays",
    y = "Overnight trips ('000)"
  )

# But a subseries plot is the most elucidating
holidays |>
  gg_subseries(Trips) +
  labs(
    y = "Overnight trips ('000)",
    titel = "Australian domestic holidays"
  )

# Scatterplots ----
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(
    y = "GW",
    title = "Half-hourly electricity demand: Victoria"
  )

vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "GW",
    title = "Half-hourly temperatures: Melbourne"
  )

# We can look at a scatterplot to see how these two are related
vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(
    title = "Electricity demand vs. Temperature",
    x = "Temperature (degrees C)",
    y = "Electricity demand (GW)"
  )

# Lag plots ----
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)

# Lag plots show scatter plots of the time series against a lagged version of
# itself. This helps identify autocorrelation (more on that later).
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# We see that lags 2 and 6 are negatively correlated, while 4 and 8 are
# positively correlated. This coincides with the regular peaks and troughs we
# see when we time plot the series:
recent_production |> autoplot(Beer)

# Autocorrelation ----
# Autocorrelation is the correlation between a series and the lagged version of
# itself. We compute it using the `ACF()` function.
recent_production |> ACF(Beer, lag_max = 9)

# Usually we view this in a correlogram plot
recent_production |>
  ACF(Beer) |>
  autoplot() +
  labs(title = "Australian beer production")

# We see what the lag plot showed us. Correlograms also have a dashed blue line
# which is a 95% confidence level for the autocorrelation of a white noise at
# any given lag.

# Trended data have linearly monotone ACFs, while seasonality generates
# periodicity.
a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title = "Australian antidiabetic drug sales")

# White noise ----
set.seed(30)
y <- tsibble(
  sample = 1:50,
  wn = rnorm(50),
  index = sample
)
y |> autoplot(wn) + labs(titel = "White noise", y = "")

# The ACF of white noise should be close to 0 with random fluctuations
y |>
  ACF(wn) |>
  autoplot() +
  labs(title = "White noise")
