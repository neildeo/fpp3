library(fpp3)

afg_pop <- global_economy |>
  filter(Code == "AFG") |>
  select(Population)

afg_pop |> autoplot()

# We can see the drop in population starting at the outbreak of the
# Soviet-Afghan war in late 1979.

models <- afg_pop |>
  model(
    linear = TSLM(Population ~ trend()),
    knots = TSLM(Population ~ trend(knots = c(1980, 1989)))
  )

models |>
  augment() |>
  ggplot(aes(x = Year, y = .fitted, colour = .model)) +
  geom_line() +
  autolayer(afg_pop, Population)

models |>
  forecast(new_data(afg_pop, 5)) |>
  autoplot(afg_pop) +
  autolayer(models |> augment() |> select(.fitted))

# The linear model has a poor fit and, consequently, very wide prediction
# intervals. The model with knots fits the data much better and has very tight
# prediction intervals.
