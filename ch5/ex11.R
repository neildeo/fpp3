library(fpp3)

bricks <- aus_production |> select(Bricks) |> filter(!is.na(Bricks))

bricks |> gg_tsdisplay()

bricks_train <- bricks |> filter(year(Quarter) < 2003)

bricks_stl <- bricks_train |> model(STL(Bricks))

bricks_stl |> components() |> autoplot()

bricks_sa <- bricks_stl |> components() |> select(season_adjust)

bricks_sa |> gg_tsdisplay()

sa_naive_fc <- bricks_sa |>
  model(NAIVE()) |>
  forecast(h = 10)

sa_naive_fc |> autoplot(bricks_sa)

naive_decomp <- bricks_train |>
  model(
    decomp = decomposition_model(
      STL(Bricks ~ trend(window = 5)),
      NAIVE(season_adjust)
    )
  )

naive_decomp |> forecast(h = 10) |> autoplot(bricks)

naive_decomp |> gg_tsresiduals()

# Residuals have a fair amount of autocorrelation. The model is not capturing
# certain abrupt downward spikes. Heavy negative skew in residual distribution.

naive_decomp_robust <- bricks_train |>
  model(
    robust = decomposition_model(
      STL(Bricks, robust = TRUE),
      NAIVE(season_adjust)
    )
  )

naive_decomp_robust |> forecast(h = 10) |> autoplot(bricks)

naive_decomp_robust |> gg_tsresiduals()

# Not a substantial difference from previous.

snaive_fc <- bricks_train |>
  model(SNAIVE()) |>
  forecast(h = 10)

naive_decomp_robust |>
  forecast(h = 10) |>
  bind_rows(snaive_fc) |>
  autoplot(bricks, level = NULL)
naive_decomp_robust |>
  forecast(h = 10) |>
  bind_rows(snaive_fc) |>
  accuracy(bricks)

# The decomposition model is a lot better!
