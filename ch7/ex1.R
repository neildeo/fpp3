library(fpp3)
jan14_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

jan14_vic_elec |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(
    x = "Temperature",
    y = "Demand",
    title = "Electricity demand against max temperature"
  ) +
  geom_smooth(method = "lm", se = FALSE)

# The positive correlation makes sense: when it is hotter there is more demand
# on air conditioning and other cooling devices.

fit <- jan14_vic_elec |>
  model(lm = TSLM(Demand ~ Temperature))

report(fit)

fit |> gg_tsresiduals()
fit |>
  augment() |>
  ggplot(aes(.fitted, Demand)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "#666")

fit |>
  augment() |>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Demand, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Actual = "#555", Fitted = "firebrick")) +
  guides(colour = guide_legend(title = ""))

fit |>
  augment() |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()

# The residuals seem to have a pattern: at the medium fitted values, they are
# negative, while at the extreme fitted values they are large and positive.
# This suggests that extrapolation is unlikely to be successful.

temp_scenarios <- scenarios(
  Cold = new_data(jan14_vic_elec, 1) |> mutate(Temperature = 15),
  Hot = new_data(jan14_vic_elec, 1) |> mutate(Temperature = 35),
  names_to = "Scenario"
)

fit |>
  forecast(
    new_data(jan14_vic_elec, 1) |> mutate(Temperature = 15)
  ) |>
  autoplot(jan14_vic_elec)
fit |>
  forecast(
    new_data(jan14_vic_elec, 1) |> mutate(Temperature = 35)
  ) |>
  autoplot(jan14_vic_elec)

# The 35 degrees forecast looks more plausible as the temperature is within
# the range considered in the model. The 15 degrees forecast is based on a
# substantial extrapolation and as such is less plausible.

vic_elec |>
  index_by(Date = as_date(Time)) |>
  summarise(Demand = sum(Demand), Temperature = max(Temperature)) |>
  ggplot(aes(Temperature, Demand, colour = factor(month(Date)))) +
  geom_point()

# The relationship is non-linear. In the coldest months, the correlation is
# negative, while in the hottest months, the correlation is positive. Thus our
# model for January is unlikely to extrapolate beyond a time horizon of a few
# weeks.
