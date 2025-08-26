library(fpp3)

# Bricks ----
bricks <- aus_production |>
  select(Bricks)

# bricks is a quarterly time series of 218 observations (just over 54 years)
bricks

bricks |> autoplot(Bricks) + labs(title = "Bricks", y = "Bricks produced (mn)")

# There is an arcing overall trend, increasing from 1956 up to 1981 but then
# slowly decreasing. The series has very clear annual seasonality, as well as
# cyclicality reflecting broad macroeconomic trends (i.e. big dips at global
# recessions).

# Lynx ----
lynx <- pelt |>
  select(Lynx)

# lynx is an annual time series of 91 observations
lynx

lynx |> autoplot(Lynx) + labs(title = "Lynx", y = "Pelts")

# The trend is pretty flat. There is a seasonality of roughly ten years.
# Hard to identify any obvious cyclicality.

# Close ----
close <- gafa_stock |> select(Close)

# close is an irregular time series, with closing stock prices for 4 companies
# only on trading days.
close

close |> autoplot()
