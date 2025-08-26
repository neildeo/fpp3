library(fpp3)

# Quarterly time series for 4 origins
aus_arrivals

aus_arrivals |> autoplot(Arrivals)

# Observations:
# - Trend is linear increasing for UK, US and NZ, while for Japan it increases
#   until 1997, then declines.
# - All four series display annual seasonality.

aus_arrivals |> gg_season()

# Observations:
# - UK has the strongest seasonal effect: arrivals are much higher in Q4 and Q1.
# - US and Japan are fairly even across the four quarters.
# - NZ is the opposite of the UK though the effect is less pronounced.

aus_arrivals |> gg_subseries()

# NZ has maintained its upward trend over time. UK starts declining post '08.
# US flatlines after 2000. Japan has a sharp dip in the early noughties - dot
# com bubble? Big spike in 2000 for the US, probably due to the Olympics.
