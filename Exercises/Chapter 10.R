#* 1 ----
# This exercise uses data set LakeHuron giving the level of Lake Huron from 1875–1972.

#** 1.a ----
# Convert the data to a tsibble object using the as_tsibble() function.

LakeHuron_ts <- LakeHuron %>%
  as_tsibble() %>%
  rename(Year = index, Level = value)


#** 1.b ----
# Fit a piecewise linear trend model to the Lake Huron data with a knot at 1920 and an ARMA error structure.

LakeHuron_ts %>% autoplot(Level)

fit <- LakeHuron_ts %>%
  model(ARIMA(Level ~ pdq(d = 0) + trend(knots = 1920)))

fit %>%
  augment() %>%
  pivot_longer(Level:.fitted) %>%
  autoplot(value) +
  scale_color_manual(labels = c(".fitted", "Level"), values = c("red", "blue"))


#** 1.c ----
# Forecast the level for the next 30 years.
# Do you think the extrapolated linear trend is realistic?

fit %>%
  forecast(h = "30 years") %>%
  autoplot(LakeHuron_ts)

# The extrapolated linear trend doesn't seem realistic.
# There will be annual fluctuations in the water level due to multiple factors including the amount of rainfall in the year.


#* 2 ----
# Repeat Exercise 4 from Section 7.10, but this time adding in ARIMA errors to address the autocorrelations in the residuals.

souvenirs <- souvenirs %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0))


#** 2.a ----
# How much difference does the ARIMA error process make to the regression coefficients?

souvenirs_fit <- souvenirs %>%
  model(
    TSLM = TSLM(log(Sales) ~ trend() + season() + `Surfing Festival`),
    ARIMA = ARIMA(log(Sales) ~ trend() + season() + `Surfing Festival`)
  )

souvenirs_fit %>% select(TSLM) %>% coefficients()
souvenirs_fit %>% select(ARIMA) %>% coefficients()

# The parameters have changed slightly, with smaller coefficient values for the ARIMA model when compared to the TSLM model.


#** 2.b ----
# How much difference does the ARIMA error process make to the forecasts?

souvenirs_future_data <- new_data(souvenirs, 36) %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0))

souvenirs_fit %>%
  forecast(souvenirs_future_data) %>%
  autoplot(souvenirs %>% filter(Month >= yearmonth("1992 Jan")))

souvenirs_fit %>%
  forecast(souvenirs_future_data) %>%
  hilo(level = 95) %>%
  select(-"95%") %>%
  View()

# The ARIMA forecast, including the prediction interval, is similar to the TSLM model.


#** 2.c ----
# Check the residuals of the fitted model to ensure the ARIMA process has adequately addressed the autocorrelations seen in the TSLM model.

souvenirs_fit %>%
  select(ARIMA) %>%
  gg_tsresiduals()

souvenirs_fit %>%
  select(ARIMA) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 19, dof = 6)

# The residuals are white noise now.


#* 3 ----
# Repeat the daily electricity example, but instead of using a quadratic function of temperature, use a piecewise linear function with the “knot” around 25 degrees Celsius (use predictors Temperature & Temp2).
# How can you optimise the choice of knot?

# The data can be created as follows.
#   vic_elec_daily <- vic_elec %>%
#     filter(year(Time) == 2014) %>%
#     index_by(Date = date(Time)) %>%
#     summarise(
#       Demand = sum(Demand)/1e3,
#       Temperature = max(Temperature),
#       Holiday = any(Holiday)) %>%
#     mutate(
#       Temp2 = I(pmax(Temperature-20,0)),
#       Day_Type = case_when(
#         Holiday ~ "Holiday",
#         wday(Date) %in% 2:6 ~ "Weekday",
#         TRUE ~ "Weekend"))


vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(
    Temp2 = I(pmax(Temperature - 20, 0)),
    Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 25)))

fit %>% gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 9, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Temp2 = I(pmax(Temperature - 20, 0)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title = "Daily electricity demand: Victoria", y = "GW")

# To optimize the knots we can run multiple models with different knots and then compare which has the lowest AICc value.

fit <- vic_elec_daily %>%
  model(
    knot_21 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 21)),
    knot_22 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 22)),
    knot_23 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 23)),
    knot_24 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 24)),
    knot_25 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 25)),
    knot_26 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 26)),
    knot_27 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 27))
  )

fit %>% glance() %>% arrange(AICc)

# Although the AICc values for all the models are nearly the same, the value for Knot 23 is the lowest, and is therefore the optimum value.


#* 4 ----
# This exercise concerns aus_accommodation: the total quarterly takings from accommodation and the room occupancy level for hotels, motels, and guest houses in Australia, between January 1998 and June 2016.
# Total quarterly takings are in millions of Australian dollars.

#** 4.a ----
# Compute the CPI-adjusted takings and plot the result for each state

aus_accommodation <- aus_accommodation %>%
  mutate(Takings_CPI_adj = Takings / CPI * 100)

aus_accommodation %>%
  autoplot(Takings_CPI_adj) +
  facet_wrap(vars(State), scales = "free_y") +
  labs(x = "Quarter", y = "CPI adjusted Takings") +
  theme(legend.position = "none")


#** 4.b ----
# For each state, fit a dynamic regression model of CPI-adjusted takings with seasonal dummy variables, a piecewise linear time trend with one knot at 2008 Q1, and ARIMA errors.

fit <- aus_accommodation %>%
  model(Takings_CPI_adj_ARIMA = ARIMA(Takings_CPI_adj ~ trend(knots = yearquarter("2008 Q1")) + season()))


#** 4.c ----
# Check that the residuals of the model look like white noise.

for (i in fit$State) {
  fit %>%
    filter(State == i) %>%
    gg_tsresiduals() %>%
    print()
}

# The residuals for all models look like white noise.


#** 4.d ----
# Forecast the takings for each state to the end of 2017.
# (Hint: You will need to produce forecasts of the CPI first.)

#**** 4.d.1 ----
# Method 1: Forecasting mmodel in 4.b and CPI then combining to get Takings forecast.

future_data <- new_data(aus_accommodation, n = 18)

fit2 <- aus_accommodation %>%
  model(CPI_ARIMA = ARIMA(CPI))

fc2 <- fit2 %>%
  forecast(future_data) %>%
  rename(CPI_fc = .mean, CPI_model = .model)

fc <- fit %>%
  forecast(future_data) %>%
  rename(Takings_adj_fc = .mean, Takings_adj_model = .model)


# Combining done using below method.
# https://community.thinkwisesoftware.com/news-updates-21/time-series-forecasting-using-r-with-fable-651
fc_final <- fc %>%
  left_join(fc2, by = c("State", "Date")) %>%
  mutate(Takings = Takings_adj_fc * CPI_fc / 100) %>%
  select(State, Date, Takings) %>%
  mutate(
    .dist = distributional::dist_normal(
      mu = Takings,
      sigma = sqrt(
        purrr::map_dbl(fc$Takings_CPI_adj, function(.x) .x$sigma) +
        purrr::map_dbl(fc2$CPI, function(.x) .x$sigma)
      )
    ),
    .model = "Takings"
  ) %>%
  update_tsibble(key = c(State, .model)) %>%
  as_fable(response = .$Takings, distribution = .dist)

aus_accommodation %>%
  ggplot(aes(x = Date, y = Takings), scales = "free_y") +
  autolayer(fc_final %>% hilo(), .vars = Takings) +
  geom_line(aes(color = State)) +
  geom_line(aes(y = Takings), data = fc_final %>% hilo()) +
  geom_line(aes(y = `95%_lower`,alpha = 0.6), color = "lightblue",  data = fc_final %>% hilo() %>% unpack_hilo(c(`80%`, `95%`))) +
  geom_line(aes(y = `95%_upper`, alpha = 0.6), color = "lightblue", data = fc_final %>% hilo() %>% unpack_hilo(c(`80%`, `95%`))) +
  facet_wrap(vars(State), scales = "free_y") +
  theme(legend.position = "none")

# Note: Not confident about the method, although looking at the forecasts they look reasonable.


#**** 4.d.2 ----
# Method 2: Forecast Takings directly as a function of CPI.
#   This is how the author is forecasted using earlier packages.
#   https://robjhyndman.com/nyc2018/Day3.html
fit <- aus_accommodation %>%
  model(ARIMA(Takings ~ CPI + trend(knots = yearquarter("2008 Q1")) + season()))

future_data <- new_data(aus_accommodation, n = 18) %>%
  mutate(CPI = fc2$CPI_fc)

fc <- fit %>% forecast(future_data)

fc %>%
  autoplot(aus_accommodation) +
  facet_wrap(vars(State), scales = "free_y")

# Using CPI as a predictor, the forecast for Takings is similar to the previous forecast for most  States, but with much wider confidence interval.
#   Since Takings was adjusted using CPI in the previous forecast (resulting in a simpler trend) and the prediction was manually calculated, it is possible both reasons have resulted in a narrower prediction interval.

# Western Australia has a slightly different forecast.


#* 5 ----
# We fitted a harmonic regression model to part of the us_gasoline series in Exercise 5 in Section 7.10.
# We will now revisit this model, and extend it to include more data and ARMA errors.

#** 5.a ----
# Using TSLM(), fit a harmonic regression with a piecewise linear time trend to the full series.
# Select the position of the knots in the trend and the appropriate number of Fourier terms to include by minimising the AICc or CV value.

us_gasoline %>%
  # filter(between(year(Week), 1999, 2009)) %>%
  filter(year(Week) >= 2009) %>%
  autoplot(Barrels)

fit <- us_gasoline %>%
  model(
    K_5 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 5)),
    K_10 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 10)),
    K_15 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 15)),
    K_20 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 20)),
    K_26 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 26)),
    K_11 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 11)),
    K_12 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 12)),
    K_13 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 13)),
    K_14 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 14))
  )

glance(fit) %>%
  select(.model, AIC:CV) %>%
  arrange(AICc, CV)

# The best mode with the lowest AICc and CV value is K_12.


#** 5.b ----
# Now refit the model using ARIMA() to allow for correlated errors, keeping the same predictor variables as you used with TSLM().

fit <- us_gasoline %>%
  model(
    K_10 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 10) + PDQ(0,0,0)),
    K_15 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 15) + PDQ(0,0,0)),
    K_11 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 11) + PDQ(0,0,0)),
    K_12 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 12) + PDQ(0,0,0)),
    K_13 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 13) + PDQ(0,0,0)),
    K_14 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 14) + PDQ(0,0,0))
  )

glance(fit) %>%
  arrange(AICc)

# The best mode with the lowest AICc value is K_12.


#** 5.c ----
# Check the residuals of the final model using the gg_tsresiduals() function and a Ljung-Box test.
# Do they look sufficiently like white noise to continue?
# If not, try modifying your model, or removing the first few years of data.

fit %>%
  select(K_12) %>%
  gg_tsresiduals(lag = 105)

# lag
min(2*52, 1355/5)   # 104

# dof
fit %>%
  select(K_12) %>%
  coefficients() %>%
  # select(term) %>% print(n = 30)
  nrow()


fit %>%
  select(K_12) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 104, dof = 34)


# Although the ACF plot looks similar to white noise, ljung_box shows that the residuals are not white noise.

# Rerunning above test after creating new model by removing first few years of data.
fit <- us_gasoline %>%
  filter(year(Week) >= 2000) %>%
  model(K_12 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 12) + PDQ(0,0,0), stepwise = FALSE, approximation = FALSE))

# Even though the model fails the ljung_box test, the p-value is close to 0.05 and the residuals graph looks sufficiently like white noise, and therefore choosing this model (<LM w/ ARIMA(2,0,4) errors>).


#** 5.d ----
# Once you have a model with white noise residuals, produce forecasts for the next year.

fit %>%
  forecast(h = "1 year") %>%
  autoplot(us_gasoline)


#* 6 ----
# Electricity consumption is often modelled as a function of temperature.
# Temperature is measured by daily heating degrees and cooling degrees.
# Heating degrees is 18∘C minus the average daily temperature when the daily average is below 18∘C; otherwise it is zero.
# This provides a measure of our need to heat ourselves as temperature falls.
# Cooling degrees measures our need to cool ourselves as the temperature rises.
# It is defined as the average daily temperature minus 18∘C when the daily average is above 18∘C; otherwise it is zero.
# Let yt denote the monthly total of kilowatt-hours of electricity used, let x1,t denote the monthly total of heating degrees, and let x2,t denote the monthly total of cooling degrees.

# An analyst fits the following model to a set of such data: 
# y∗[t] = β[1]x∗[1,t] + β[2]x∗[2,t] + η[t],
#   where
#   (1 − Φ[1]B^12 − Φ[2]B^24)(1 − B)(1 − B^12)η[t] = (1 + θ[1]B)ε[t] and
#   y∗[t] = log(y[t]), x∗[1,t] = √x[1,t]    and     x∗[2,t] = √x[2,t].


#** 6.a ----
# What sort of ARIMA model is identified for ηt?

#   Model - (0,1,1)(2,1,0)[12]


#** 6.b ----
# The estimated coefficients are
# Parameter 	Estimate 	s.e. 	    Z 	        P -value
# β1 	        0.0077 	    0.0015 	    4.98 	    0.000
# β2 	        0.0208 	    0.0023 	    9.23 	    0.000
# θ1 	        -0.5830 	0.0720 	    8.10 	    0.000
# Φ1 	        -0.5373 	0.0856 	    -6.27 	    0.000
# Φ2 	        -0.4667 	0.0862 	    -5.41 	    0.000
# Explain what the estimates of β1 and β2 tell us about electricity consumption.


#   β1 = 0.0077 increase in the electricity demand for each unit increase in heating degrees.
#   β2 = 0.0208 increase in the electricity demand for each unit increase in cooling degrees.


#** 6.c ----
# Write the equation in a form more suitable for forecasting.

#   log(y[t]) = 0.0077*√x[1,t] + 0.0208*√x[2,t] + η[t]
    
#   (1 − Φ[1]B[^12] − Φ[2]B[^24])(1 − B)(1 − B[^12]) η[t] = (1 + θ[1]B) ε[t]
#   (1 − Φ[1]B[^12] − Φ[2]B[^24] − B + Φ[1]B[^13] + Φ[2]B[^25])(1 − B[^12]) η[t] = (1 + θ[1]B) ε[t]
#   (1 − Φ[1]B[^12] − Φ[2]B[^24] − B + Φ[1]B[^13] + Φ[2]B[^25] − B[^12] + Φ[1]B[^24] + Φ[2]B[^36] + B[^13] - Φ[1]B[^25] - Φ[2]B[^37]) η[t] = (1 + θ[1]B) ε[t]
#   (1 − B − B[^12](Φ[1] + 1) - B[^24](Φ[2] - Φ[1]) + B[^13](Φ[1] + 1) + B[^25](Φ[2] - Φ[1]) + Φ[2]B[^36] - Φ[2]B[^37]) η[t] = (1 + θ[1]B) ε[t]
#   (1 − B - (Φ[1] + 1)(B[^12] -  B[^13]) - (Φ[2] - Φ[1])(B[^24] - B[^25]) + Φ[2](B[^36] - B[^37])) η[t] = (1 + θ[1]B) ε[t]
#   η[t] = η[t-1] + (Φ[1] + 1)(η[t-12] -  η[t-13]) + (Φ[2] - Φ[1])(η[t-24] - η[t-25]) - Φ[2](η[t-36] - η[t-37]) + θ[1]ε[t-1] + ε[t]
    
#   η[t] = η[t-1] + (1 - 0.5373)(η[t-12] -  η[t-13]) + (-0.4667 + 0.5373)(η[t-24] - η[t-25]) + 0.4667(η[t-36] - η[t-37]) - 0.5830ε[t-1] + ε[t]
#   η[t] = η[t-1] + 0.4627(η[t-12] -  η[t-13]) + 0.0706(η[t-24] - η[t-25]) + 0.4667(η[t-36] - η[t-37]) - 0.5830ε[t-1] + ε[t]


#** 6.d ----
# Explain why the ηt term should be modelled with an ARIMA model rather than modelling the data using a standard regression package.
# In your discussion, comment on the properties of the estimates, the validity of the standard regression results, and the importance of the ηt model in producing forecasts.

#   When we estimate the parameters from the model, we need to minimise the sum of squared ε[t] values.
#   If we minimise the sum of squared η[t] values instead (which is what would happen if we estimated the regression model ignoring the autocorrelations in the errors), then several problems arise.
#       1. The estimated coefficients ^β[0], …, ^β[k] are no longer the best estimates, as some information has been ignored in the calculation;
#       2. Any statistical tests associated with the model (e.g., t-tests on the coefficients) will be incorrect.
#       3. The AICc values of the fitted models are no longer a good guide as to which is the best model for forecasting.
#       4. In most cases, the p-values associated with the coefficients will be too small, and so some predictor variables will appear to be important when they are not. This is known as “spurious regression.”

#   Minimising the sum of squared ε[t] values avoids these problems.


#* 7 ----
# For the retail time series considered in earlier chapters:

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)


#** 7.a ----
# Develop an appropriate dynamic regression model with Fourier terms for the seasonality.
# Use the AICc to select the number of Fourier terms to include in the model.
# (You will probably need to use the same Box-Cox transformation you identified previously.)

lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

fit <- myseries_train %>%
  model(
    K_1 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 1) + PDQ(0,0,0)),
    K_2 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 2) + PDQ(0,0,0)),
    K_3 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 3) + PDQ(0,0,0)),
    K_4 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 4) + PDQ(0,0,0)),
    K_5 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 5) + PDQ(0,0,0)),
    K_6 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0))
  )

fit %>%
  glance() %>%
  arrange(AICc)

# The best model with lowest AICc is K_6.


#** 7.b ----
# Check the residuals of the fitted model.
# Does the residual series look like white noise?

fit %>%
  select(K_6) %>%
  gg_tsresiduals()

# dof
fit %>%
  select(K_6) %>%
  coefficients() %>%
  nrow()

fit %>%
  select(K_6) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24, dof = 18)

# The spikes for lags 12, 18 and 24 are huge. It also fails the ljung-box test.


fit <- myseries_train %>%
  model(K_6 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0), stepwise = FALSE, approximation = FALSE))

# Even though the model fails the ljung-box test, this is the best model selected and the residual plot and the histogram look closer to white noise than for the first model.


#** 7.c ----
# Compare the forecasts with those you obtained earlier using alternative models.

fit <- myseries_train %>%
  model(
    ARIMA_K_6_313 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0) + pdq(3,1,3)),
    ARIMA_K_6_214 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0) + pdq(2,1,4)),
    Arima = ARIMA(box_cox(Turnover, lambda)),
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(myseries_test)

fc %>%
  filter(.model %in% c("ARIMA_K_6_313", "ARIMA_K_6_214", "ARIMA", "AM")) %>%
  autoplot(myseries, level = NULL)

fc %>% accuracy(myseries) %>% arrange(RMSE)

# Both, AM and ARIMA models, are better than the K_6 model.
