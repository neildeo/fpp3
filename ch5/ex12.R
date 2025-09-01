library(fpp3)

gc_tourism <- tourism |>
  filter(State == "Queensland") |>
  summarise(Trips = sum(Trips))

gc_tourism$Quarter |> max()
# 2017 is the most recent year

gc_train_1 <- gc_tourism |> filter(year(Quarter) <= 2016)
gc_train_2 <- gc_tourism |> filter(year(Quarter) <= 2015)
gc_train_3 <- gc_tourism |> filter(year(Quarter) <= 2014)

gc_fc_1 <- gc_train_1 |>
  model(m1 = SNAIVE()) |>
  forecast(h = 4)
gc_fc_2 <- gc_train_2 |>
  model(m2 = SNAIVE()) |>
  forecast(h = 4)
gc_fc_3 <- gc_train_3 |>
  model(m3 = SNAIVE()) |>
  forecast(h = 4)

gc_fc_1 |> accuracy(gc_tourism)
gc_fc_2 |> accuracy(gc_tourism)
gc_fc_3 |> accuracy(gc_tourism)

bind_rows(gc_fc_1, gc_fc_2, gc_fc_3) |> autoplot(gc_tourism, level = NULL)
bind_rows(gc_fc_1, gc_fc_2, gc_fc_3) |> accuracy(gc_tourism)
