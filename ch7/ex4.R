library(fpp3)

souvenirs |>
  autoplot()

# There is a clear annual seasonality. The big spikes in December suggest that
# this is a popular travel time, and are probably boosted substantially by
# Christmas. There is a growing amplitude which appear to grow exponentially
# over time - a log transform is required to make the data (roughly)
# homoscedastic.
#
# We also see a spike in March from 1998 onwards due to the surfing festival

souvenirs |>
  autoplot(log(Sales))

# We see that the seasonal fluctuations are now roughly of equal amplitude. At
# the log level, there is a clear linear upward trend.

surf_souvenirs <- souvenirs |>
  mutate(
    Surf_festival = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0)
  )

lm_fit <- surf_souvenirs |>
  model(
    lm = TSLM(log(Sales) ~ trend() + season() + Surf_festival)
  )

lm_fit |> report()

lm_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Sales, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Actual = "#666", Fitted = "forestgreen")) +
  guides(colour = guide_legend(title = "Series"))

lm_fit |> gg_tsresiduals()

# The residuals have pretty high autocorrelation. There are long spells with
# exclusively positive and negative residuals.

lm_fit |>
  augment() |>
  ggplot(aes(x = .fitted, y = .innov)) +
  geom_point() +
  labs(x = "Fitted values", y = "Innovation residuals")

# The very high peaks are high-leverage points - they have an outsized influence
# over the coefficients.

lm_fit |>
  augment() |>
  ggplot(aes(x = factor(month(Month)), y = .resid)) +
  geom_boxplot(outlier.colour = "red")

lm_fit |> report()

# The trend coefficient is rather small compared to the others. The March
# seasonal dummy variable is not significantly different from 0. Introducing the
# surf festival dummy variable has introduced a collinearity which may be
# causing some problems. Otherwise, the overall model fit is quite good.

lm_fit |>
  augment() |>
  features(.innov, features = ljung_box, lag = 10)

# The Ljung-Box test is significant - the innovation residuals are not white
# noise. Thus there is some more structure to uncover in the model.

lm_fit |>
  forecast(
    new_data(surf_souvenirs, 36) |>
      mutate(
        Surf_festival = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0)
      )
  ) |>
  autoplot(surf_souvenirs)

# This would be nice for the shop owners but is a bit crazy.

# One improvement might be to ditch the festival indicator and just rely on the
# March season to capture the effect of the festival

new_lm_fit <- souvenirs |>
  model(
    lm = TSLM(log(Sales) ~ trend() + season())
  )

new_lm_fit |> report()

new_lm_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Sales, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Actual = "#666", Fitted = "forestgreen")) +
  guides(colour = guide_legend(title = "Series"))

# This model is not much better. It would be helpful to be able to dampen the
# trend.
