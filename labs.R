library(fpp3)
tsibble::tourism

tourism_tbl <- readr::read_csv(
  "https://workshop.nectric.com.au/fable-wombat2025/data/tourism.csv"
)

tourism <- tourism_tbl |>
  mutate(
    Quarter = yearquarter(Quarter)
  ) |>
  as_tsibble(
    index = Quarter,
    key = c(Region, State, Purpose)
  )


as_tsibble(
  tibble(
    time = 1:10,
    value = rnorm(10)
  ),
  index = time
)


tourism |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))

tourism |>
  filter(State == "Victoria") |>
  group_by(Purpose) |>
  summarise(Trips = sum(Trips))



tourism |>
  as_tibble() |>
  filter(State == "Victoria") |>
  group_by(Purpose) |>
  summarise(Trips = sum(Trips))


tourism |>
  filter(State == "Victoria") |>
  group_by(Purpose) |>
  index_by(Year = year(Quarter)) |>
  summarise(Trips = sum(Trips))

pedestrian |>
  mutate(wday(Date_Time, label = TRUE))


pedestrian |>
  index_by(Week = yearweek(Date_Time)) |>
  summarise(Count = sum(Count)) |>
  autoplot(Count)


tourism |>
  autoplot(Trips)

tourism |>
  ggplot() +
  geom_line(
    aes(
      x = Quarter,
      y = Trips,
      group = interaction(Region, State, Purpose)
    )
  )

tourism |>
  summarise(Trips = sum(Trips)) |>
  autoplot(Trips)


aus_production |>
  autoplot(Beer)


aus_production |>
  autoplot(Beer)
aus_production |>
  gg_season(Beer)
aus_production |>
  gg_subseries(Beer)



aus_production |>
  filter(Quarter > yearquarter("1992 Q1")) |>
  autoplot(Beer)
aus_production |>
  filter(Quarter > yearquarter("1992 Q1")) |>
  gg_subseries(Beer)

aus_production |>
  model(STL(Beer)) |>
  components() |>
  gg_subseries(season_year)


# 1. tidy
vic_tourism <- tourism |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))
# 2. visualise / explore
vic_tourism |>
  autoplot()

fit <- vic_tourism |>
  # 4. estimate model()
  model(
    # 3. specify
    # which model captures the patterns best?
    # in this case; trend and seasonality
    snaive_drift = SNAIVE(Trips ~ drift()),
    ets_aaa = ETS(Trips ~ error("A") + trend("A") + season("A"))
  )

fit
# Coefficients
tidy(fit) # coef(fit)
# Summary of the model
glance(fit)
# Augment the data
augment(fit)

fc <- fit |>
  # 5. forecast()
  forecast(h = "5 years")

fc |>
  autoplot(vic_tourism, level = c(50, 95))

# 6. evaluate accuracy()


# Exponential trend
tsibble(
  t = 1:100,
  yadd = 1:100,
  ymult = (1:100)^2.2,
  season = sin(1:100)+1.5,
  index = t
) |>
  autoplot(ymult * season)


# 1. Tidy + 2. Visualise
aus_production |>
  autoplot(Gas)

# 3. Specify
ETS(Gas ~ error("M") + trend("A") + season("M"))

# 4. model()

fit <- aus_production |>
  model(
    # ets_mam = ETS(Gas ~ error("M") + trend("A") + season("M"))
    ets = ETS(Gas)
  )

# 5. forecast()
fit |>
  forecast(h = "5 years", bootstrap = TRUE) |>
  autoplot(aus_production)


gg_tsresiduals(fit)


aus_production |>
  autoplot(Gas)
aus_production |>
  autoplot(log(Gas))
aus_production |>
  autoplot(box_cox(Gas, 0.2))




fit <- aus_production |>
  model(
    ets = ETS(Gas),

    arima = ARIMA(box_cox(Gas, guerrero(Gas)))
  )

# 5. forecast()
fit |>
  forecast(h = "5 years") |>print(n=100)
  autoplot(aus_production)


guerrero(aus_production$Gas)





library(fpp3)
vic_tourism <- tourism |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))
vic_tourism |>
  autoplot(Trips)
fit <- vic_tourism |>
  model(
    ETS(Trips),
    ARIMA(log(Trips))
  )
augment(fit)


accuracy(fit)

vic_tourism |>
  filter(
    Quarter < yearquarter("2016 Q1")
  ) |>
  model(
    ETS(Trips),
    ARIMA(log(Trips))
  ) |>
  forecast(h = "2 years") |>
  autoplot(vic_tourism)

vic_tourism |>
  filter(
    Quarter < yearquarter("2016 Q1")
  ) |>
  model(
    ETS(Trips),
    ARIMA(log(Trips))
  ) |>
  forecast(h = "2 years") |>
  accuracy(vic_tourism)

fc_cv <- vic_tourism |>
  # Keep some data for evaluating forecasts
  filter(Quarter < yearquarter("2016 Q1")) |>
  # Cross-validate the remaining data
  stretch_tsibble(.step = 4*2, .init = 4*10) |>
  model(
    ETS(Trips),
    ARIMA(log(Trips))
  ) |>
  forecast(h = "2 years")

fc_cv |>
  accuracy(vic_tourism)


fit <- vic_tourism |>
  model(
    # ETS(Trips),
    ARIMA(log(Trips))
  )
augment(fit) |>
  autoplot(.innov)

augment(fit) |>
  gg_season(.innov)


gg_tsresiduals(fit)
