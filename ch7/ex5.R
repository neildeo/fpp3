library(fpp3)

gas_hist <- us_gasoline |>
  filter(year(Week) <= 2004)

gas_hist |> autoplot()

orders <- c(2, 5, 7, 8, 10, 12, 15, 20, 25)

fourier_models <- gas_hist |>
  model(
    f1 = TSLM(Barrels ~ trend() + fourier(K = 1))
  )

for (ord in orders) {
  print(ord)
  fourier_models <- bind_cols(
    fourier_models,
    gas_hist |>
      model(
        temp = TSLM(Barrels ~ trend() + fourier(K = ord))
      )
  )
  fourier_models <- fourier_models |> rename("f{ord}" := temp)
}

fourier_models |>
  glance() |>
  select(.model, AIC, AICc, CV) |>
  mutate(Model = factor(.model, levels = unique(.model))) |>
  select(-.model) |>
  pivot_longer(-Model, names_to = "Metric", values_to = "Score") |>
  ggplot(aes(x = Model, y = Score, colour = Metric, group = Metric)) +
  geom_line() +
  facet_wrap(. ~ Metric, scales = "free_y")

# 7 Fourier coefficients have the best fit according to AICc and CV.

fbest <- fourier_models |> select(f7)
fbest |>
  augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Barrels, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Actual = "#444", Fitted = "firebrick")) +
  guides(colour = guide_legend(title = "Series"))

# The model is struggling to capture the extreme spikes in the data, thought the
# overall trend fit is good.

fbest |> gg_tsresiduals()
fbest |>
  augment() |>
  features(.resid, ljung_box, 30)

# The residual process is indeed significantly different (at 5%) from white noise, as is
# suggested by the ACF plot.

fbest |>
  forecast(new_data(gas_hist, 52)) |>
  autoplot(us_gasoline |> filter(year(Week) <= 2005))

# The model performs quite well for the first 8 months, but failed to anticipate
# the sharp downward spike. However, on balance the performance looks good.
