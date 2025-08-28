library(fpp3)

?canadian_gas

canadian_gas |> autoplot() + labs(title = "Canadian gas production")

# Box-Cox transform is not useful here because the period of highest variance
# does not coincide with the period of highest magnitude.
