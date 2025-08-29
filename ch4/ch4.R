# TIME SERIES FEATURES ----
library(fpp3)

# Simple statistics
tourism |>
  features(Trips, quantile)

# ACF features
tourism |> features(Trips, feat_acf)

# STL features
tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(
    x = trend_strength,
    y = seasonal_strength_year,
    colour = Purpose
  )) +
  geom_point() +
  facet_wrap(vars(State))
