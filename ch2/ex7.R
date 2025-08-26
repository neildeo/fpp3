library(fpp3)

set.seed(414961481)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

# Clothing retail in Tasmania
myseries

# Increasing over time, though shallow dip around 2000
# Annual seasonality with spikes in December for Xmas
myseries |> autoplot(Turnover)

# December spike is becoming more pronounced over time
# In recent years, Jan is also a little higher
myseries |> gg_season()

# Sharp decline since 2015 peak in all months
myseries |> gg_subseries()

# Broadly positive autocorrelation - expected for a strongly trended series
myseries |> gg_lag(geom = "point")

# 12 month intervals are very highly autocorrelated. Typical strongly trended
# seasonal pattern: decreasing wedge with "scalloping"
myseries |> ACF(Turnover) |> autoplot()
