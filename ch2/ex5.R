library(fpp3)
tour <- readxl::read_excel("ch2/tourism.xlsx")

# The tsibble we want to emulate
tourism

tour <- tour |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter, key = c(Region, State, Purpose))

tour

# What combination of Region and Purpose has the highest average number
# of trips?
tour |>
  group_by(Region, Purpose) |>
  summarise(Trips = mean(Trips)) |>
  ungroup() |>
  filter(Trips == max(Trips))

# Total trips by state
tour |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
