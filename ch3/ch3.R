library(fpp3)

# TIME SERIES DECOMPOSITION ----
# Components ----
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

autoplot(us_retail_employment, Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# `model` produces a mable, or model table
dcmp <- us_retail_employment |>
  model(stl = STL(Employed))

# Display the mable as a decomposition table (dable)
components(dcmp)

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = trend), colour = "firebrick") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# The dable autoplots very nicely
components(dcmp) |> autoplot()

# Moving averages ----
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(
    y = "% of GDP",
    title = "Total Australian exports"
  )

aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 2,
      .after = 2,
      .complete = TRUE
    )
  )

aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "firebrick") +
  labs(
    y = "% of GDP",
    title = "Total Australian exports"
  )

# If the seasonal period is even, we need to use a 2xm-MA
us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(
      Employed,
      mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  )

us_retail_employment_ma |>
  autoplot(Employed, colour = "grey") +
  geom_line(aes(y = `2x12-MA`), colour = "firebrick") +
  labs(
    y = "% of GDP",
    title = "Total Australian exports"
  )

# Classical decomposition ----
us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of US retail employment")
# Note that the rigid seasonality means that the 2008 crash leaks into the
# random component.

# STL decomposition ----
# Default STL parameters: trend window = 11, season window = 21
# Again, the remainder captures the 08 crash,
# although the seasonal behaviour does now vary
us_retail_employment |>
  model(
    STL(Employed)
  ) |>
  components() |>
  autoplot() +
  labs(title = "Default STL decomposition of US retail employment")

# Using chosen parameters and robust mode
# This leads to a more irregular trend component, but the dip at 08 in the
# remainder is now gone.
# "periodic" forces the seasonal component to be unchanging over time.
us_retail_employment |>
  model(
    STL(
      Employed ~ trend(window = 7) + season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot() +
  labs(title = "STL decomposition of US retail employment")
