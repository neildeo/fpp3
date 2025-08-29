library(fpp3)

?features

PBS_summary <- PBS |>
  features(Cost, features = list(mean = mean, sd = sd))

PBS_summary |>
  filter(mean == max(mean) | sd == min(sd))

# Highest mean cost - statins
PBS |>
  filter(
    Concession == "Concessional" & Type == "Co-payments" & ATC2 == "C10"
  ) |>
  autoplot(Cost)

# Lowest standard deviation of 0 - these are constant 0 series
PBS |>
  filter(
    Concession == "General" & Type == "Co-payments" & ATC2 %in% c("R", "S")
  ) |>
  autoplot(Cost)
