#* 1 ----
# Figure 9.32 shows the ACFs for 36 random numbers, 360 random numbers and 1,000 random numbers.
# Figure 9.32: Left: ACF for a white noise series of 36 numbers. Middle: ACF for a white noise series of 360 numbers. Right: ACF for a white noise series of 1,000 numbers. 

#** 1.a ----
# Explain the differences among these figures. Do they all indicate that the data are white noise?
#   Since for all 3 series the lagged values are within the limits, all 3 are white noises.


#** 1.b ----
# Why are the critical values at different distances from the mean of zero?
# Why are the autocorrelations different in each figure when they each refer to white noise?

#   Depending on the range T of all Time series, the limits are calculated as ±2/√T.
#   Therefore, the range for the 3 series are:
#     36 = ±0.3333333;        360 = ±0.1054093;         1000 = ±0.06324555
    
#   The autocorrelations are different for each series, since that is dependent on the data in the series.
#   Different white noise series can have different autocorrelations depending on their data.


#* 2 ----
# A classic example of a non-stationary series are stock prices.
# Plot the daily closing prices for Amazon stock (contained in gafa_stock), along with the ACF and PACF.
# Explain how each plot shows that the series is non-stationary and should be differenced.

gafa_stock %>%
  filter(Symbol == "AMZN") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day) %>%
  gg_tsdisplay(Close, plot_type = "partial", lag_max = 60)

# ACF plot shows that the first 60 lagged values have autocorrelations higher than the critical values, and are decreasing slowly.
# PACF plot shows a strong spike at lag 1 (near 1), meaning that stock price has strong correlation with the previous days price. There are also spikes at other lagged values, although these have no visible pattern.
# This shows that the series is non-stationary and should be differenced at least once, to remove effects of lagged values and stabilize the series.


#* 3 ----
# For the following series, find an appropriate Box-Cox transformation and order of differencing in order to obtain stationary data.

#** 3.a ----
# Turkish GDP from global_economy.

global_economy %>%
  filter(Code == "TUR") %>%
  autoplot(GDP)

# The presence of trend, no seasonality and no large variances means that first difference should give a stationary series.

global_economy %>%
  filter(Code == "TUR") %>%
  mutate(diff_gdp = difference(GDP)) %>%
  gg_tsdisplay(diff_gdp, plot_type = "partial")

# Except the spike at lag 11 in the ACF and PACF plot, the rest of the plot shows that the first differenced data is a white noise.

global_economy %>%
  filter(Code == "TUR") %>%
  features(GDP, unitroot_ndiffs)
  
# Performing unit root test also tells us that one difference is required.


#** 3.b ----
# Accommodation takings in the state of Tasmania from aus_accommodation.

aus_accommodation %>%
  filter(State == "Tasmania") %>%
  autoplot(Takings)

# There is trend and seasonality present, with increasing variance in the seasonality.
# Therefore, a first difference for both seasonal and non-seasonal might be required, along with Box-Cox transformations.

lambda <- aus_accommodation %>%
  filter(State == "Tasmania") %>%
  features(Takings, guerrero) %>%
  pull(lambda_guerrero)


aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda)) %>%
  gg_tsdisplay(Takings, plot_type = "partial")

# ACF plot shows decreasing spikes with seasonal variation every 4 quarters.

aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda)) %>%
  features(Takings, unitroot_nsdiffs)

aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda) %>% difference(4)) %>%
  features(Takings, unitroot_ndiffs)


aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda) %>% difference(4)) %>%
  gg_tsdisplay(Takings, plot_type = "partial")

# Both ACF and PACF plots show there is still lags at 1, 3 and 4.
# Therefore, maybe one more difference is required.  


aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda) %>% difference(4)) %>%
  features(Takings, ljung_box)

# ljung_box test also has given significant p-value meaning that the data is still not white noise.

# However, since the KPSS test doesn't give further differences, it means that though the data is not white noise, it is stationary.


#** 3.c ----
# Monthly sales from souvenirs.

souvenirs %>% autoplot(Sales)

# Increasing trend and seasonality with changing variance.
# Box-Cox transformation followed by seasonal differencing and first differencing might be required.

lambda <- souvenirs %>% features(Sales, guerrero) %>% pull(lambda_guerrero)

souvenirs %>%
  transmute(Sales = box_cox(Sales, lambda)) %>%
  features(Sales, unitroot_nsdiffs)

souvenirs %>%
  transmute(Sales = box_cox(Sales, lambda)) %>%
  features(Sales, unitroot_ndiffs)

souvenirs %>%
  transmute(Sales = box_cox(Sales, lambda) %>% difference(12)) %>%
  gg_tsdisplay(Sales, plot_type = "partial")


# There are rapidly failing spikes at lag 1 and 2.
# However, unit test doesn't give suggest a first difference is required. 


#* 4 ----
# For the souvenirs data, write down the differences you chose above using backshift operator notation.

#   Data needs 1 seasonal difference after Box-Cox transformation (lambda = 0.00213)
#   Model equation is:
#     w[t]  = (sign(y[t])|y[t]|^0.00213 - 1)/0.00213
#           = (y[t]^0.00213 - 1)/0.00213      # since sales values are positive we can drop sign and ||
#     y'[t] = w[t] - w[t-4]
#           = w[t] - B^4w[t]
#           = (1 - B^4)w[t]


#* 5 ----
# For your retail data (from Exercise 8 in Section 2.10), find the appropriate order of differencing (after transformation if necessary) to obtain stationary data.

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries %>% autoplot(Turnover)

# Trend, seasonality with changing variance.
# Might need seasonal and first difference after Box-Cox transformation.

lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

myseries %>%
  transmute(Turnover = box_cox(Turnover, lambda)) %>%
  features(Turnover, unitroot_nsdiffs)

myseries %>%
  transmute(Turnover = box_cox(Turnover, lambda) %>% difference(12)) %>%
  features(Turnover, unitroot_ndiffs)


myseries %>%
  transmute(Turnover = box_cox(Turnover, lambda) %>% difference(12) %>% difference(1)) %>%
  gg_tsdisplay(Turnover, plot_type = "partial")

# 1 seasonal difference after Box-Cox transformation.


#* 6 ----
# Simulate and plot some data from simple ARIMA models.

#** 6.a ----
# Use the following R code to generate data from an AR(1) model with ϕ1=0.6 and σ2=1.
# The process starts with y1=0.
#   y <- numeric(100)
#   e <- rnorm(100)
#   for(i in 2:100)
#     y[i] <- 0.6*y[i-1] + e[i]
#   sim <- tsibble(idx = seq_len(100), y = y, index = idx)


y <- numeric(100)
e <- rnorm(100)       # since σ[^2] = 1

# since y1 = 0, so c = 0
for (i in 2:100) {
  y[i] <- 0.6*y[i-1] + e[i]
}

sim <- tsibble(idx = seq_len(100), y = y, index = idx)


#** 6.b ----
# Produce a time plot for the series. How does the plot change as you change ϕ1?

autoplot(sim, y)

ar1generator <- function(phi1 = 0.6){
  y <- numeric(100)
  e <- rnorm(100)
  
  for (i in 2:100) {
    y[i] <- phi1*y[i-1] + e[i]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, phi = phi1, index = idx, key = phi)
  
  return(sim)
}

bind_rows(
  ar1generator(),
  ar1generator(0.3),
  ar1generator(0.9)
) %>%
  autoplot(y)

# As phi increases, the variation in y increases.


#** 6.c ----
# Write your own code to generate data from an MA(1) model with θ1=0.6 and σ2=1.

ma1generator <- function(theta1 = 0.6){
  y <- numeric(100)
  e <- rnorm(100)
  
  for (i in 2:100) {
    y[i] <- e[i] + theta1*e[i-1]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, theta1 = theta1, index = idx, key = theta1)
  
  return(sim)
}


#** 6.d ----
# Produce a time plot for the series.
# How does the plot change as you change θ1?

bind_rows(
  ma1generator(),
  ma1generator(0.3),
  ma1generator(0.9)
) %>%
  autoplot(y)

# As theta increases, the variation in y increases.


#** 6.e ----
# Generate data from an ARMA(1,1) model with ϕ1=0.6, θ1=0.6 and σ2=1.

arma11generator <- function(phi1 = 0.6, theta1 = 0.6){
  y <- numeric(100)
  e <- rnorm(100)
  
  for (i in 2:100) {
    y[i] <- phi1*y[i-1] + e[i] + theta1*e[i-1]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, phi1 = phi1, theta1 = theta1, index = idx, key = c(phi1, theta1))
  
  return(sim)
}

autoplot(arma11generator(), y)


#** 6.f ----
# Generate data from an AR(2) model with ϕ1=−0.8, ϕ2=0.3 and σ2=1.
# (Note that these parameters will give a non-stationary series.)

ar2generator <- function(phi1 = -0.8, phi2 = 0.3){
  y <- numeric(100)
  e <- rnorm(100)
  
  y[2] <- phi1*y[i] + e[2]
  
  for (i in 3:100) {
    y[i] <- phi1*y[i-1] + phi2*y[i-2] + e[i]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, phi1 = phi1, phi2 = phi2, index = idx, key = c(phi1, phi2))
  
  return(sim)
}

autoplot(ar2generator(), y)


#** 6.g ----
# Graph the latter two series and compare them.

p1 <- autoplot(arma11generator(), y)
p2 <- autoplot(ar2generator(), y)

p1 / p2

# data from AR(2) increased with oscelations, and is non-stationary.
# AR(1) model is stationary.


#* 7 ----
# Consider aus_airpassengers, the total number of passengers (in millions) from Australian air carriers for the period 1970-2011.

#** 7.a ----
# Use ARIMA() to find an appropriate ARIMA model.
# What model was selected.
# Check that the residuals look like white noise.
# Plot forecasts for the next 10 periods.

aus_airpassengers %>% autoplot(Passengers)
# Annual data with increasing trend.

fit <- aus_airpassengers %>%
  model(ARIMA(Passengers))

fit
# Model chosen (0,2,1)

fit %>% gg_tsresiduals()
# residuals look like white noise

fit %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)


#** 7.b ----
# Write the model in terms of the backshift operator.

#   y''[t]  = c + ε[t] + θ[1]ε[t−1] + θ[2]ε[t−2]
#           = c + ε[t] + θ[1]Bε[t] + θ[2]B[^2]ε[t]
#           = c + ε[t] (1 + θ[1]B + θ[2]B[^2])
    

#** 7.c ----
# Plot forecasts from an ARIMA(0,1,0) model with drift and compare these to part a.

aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima010 = ARIMA(Passengers ~ 1 + pdq(0,1,0))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# ARIMA(0,1,0) gives a damped forecast compared to ARIMA.


#** 7.d ----
# Plot forecasts from an ARIMA(2,1,2) model with drift and compare these to parts a and c.
# Remove the constant and see what happens.

aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima010 = ARIMA(Passengers ~ 1 + pdq(0,1,0)),
    arima212 = ARIMA(Passengers ~ 1 + pdq(2,1,2))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# ARIMA(2,1,2) gives a forecast similar to ARIMA, but with a narrower confidence interval.


aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima010 = ARIMA(Passengers ~ 1 + pdq(0,1,0)),
    arima212 = ARIMA(Passengers ~ 0 + pdq(2,1,2))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# Removing the constant gives an error as the AR becomes non-stationary.


#** 7.e ----
# Plot forecasts from an ARIMA(0,2,1) model with a constant.
# What happens?

aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima_1 = ARIMA(Passengers ~ 1 + pdq(0,2,1))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# Introducing constant causes the model to give a quadratic or higher order polynomial trend.
#   This is generally discouraged.
# The trend is higher than for without constant, but the confidence interval is narrower.


#* 8 ----
# For the United States GDP series (from global_economy):

gdp <- global_economy %>% filter(Code == "USA") %>% select(GDP)


#** 8.a ----
# if necessary, find a suitable Box-Cox transformation for the data;

gdp %>% autoplot(GDP)

# The data has increasing trend, with little to no variance.
# Box-Cox transformation is not required.


#** 8.b ----
# fit a suitable ARIMA model to the transformed data using ARIMA();

gdp %>%
  model(ARIMA(GDP))
# ARIMA(0,2,2)


#** 8.c ----
# try some other plausible models by experimenting with the orders chosen;

gdp %>%
  gg_tsdisplay(GDP, plot_type = "partial")

gdp %>%
  gg_tsdisplay(difference(GDP), plot_type = "partial")

gdp %>%
  gg_tsdisplay(difference(difference(GDP)), plot_type = "partial")

gdp %>%
  model(
    arima = ARIMA(GDP),
    arima122 = ARIMA(GDP ~ pdq(1,2,2)),
    arima021 = ARIMA(GDP ~ pdq(0,2,1)),
    arima012 = ARIMA(GDP ~ pdq(0,1,2)),
    arima222 = ARIMA(GDP ~ pdq(2,2,2))
  ) %>%
  # accuracy() %>% arrange(RMSE)
  augment() %>%
  autoplot(.fitted) +
  autolayer(gdp, GDP)


#** 8.d ----
# choose what you think is the best model and check the residual diagnostics;

# ARIMA(2,2,2) looks best.

gdp %>%
  model(ARIMA(GDP ~ pdq(2,2,2))) %>%
  gg_tsresiduals()

# The residuals show white noise, although the histogram shows it is not normal.


#** 8.e ----
# produce forecasts of your fitted model.
# Do the forecasts look reasonable?

gdp %>%
  model(ARIMA(GDP ~ pdq(2,2,2))) %>%
  forecast(h = 10) %>%
  autoplot(gdp)

# The forecast looks reasonable.


#** 8.f ----
# compare the results with what you would obtain using ETS() (with no transformation).

gdp %>%
  model(
    arima = ARIMA(GDP),
    arima222 = ARIMA(GDP ~ pdq(2,2,2)),
    ets = ETS(GDP)
  ) %>%
  forecast(h = 10) %>%
  autoplot(gdp, level = NULL)

# ARIMA(2,2,2) seems to give a more reasonable forecast than ETS.
# ETS has a higher trend increase than the default ARIMA model.


#* 9 ----
# Consider aus_arrivals, the quarterly number of international visitors to Australia from several countries for the period 1981 Q1 – 2012 Q3.

#** 9.a ----
# Select one country and describe the time plot. 

aus_arrivals_jp <- aus_arrivals %>% filter(Origin == "Japan")

aus_arrivals_jp %>% autoplot(Arrivals)

# Increasing trend till 1995 before decreasing.
# Possible seasonality with changing variances.


#** 9.b ----
# Use differencing to obtain stationary data.

aus_arrivals_jp %>% features(Arrivals, unitroot_nsdiffs)

aus_arrivals_jp %>% features(Arrivals %>% difference(lag = 4), unitroot_ndiffs)

aus_arrivals_jp <- aus_arrivals_jp %>%
  mutate(
    double_diff_arrivals = difference(Arrivals, lag = 4) %>% difference(lag = 1)
  )

# 1 seasonal + 1 first difference.


#** 9.c ----
# What can you learn from the ACF graph of the differenced data?

aus_arrivals_jp %>% gg_tsdisplay(double_diff_arrivals, plot_type = "partial")

# There are spikes at lags 1, 4, 6 and 10, with the spikes at lag 4 being extreme.


#** 9.d ----
# What can you learn from the PACF graph of the differenced data?

#   There are decreasing spikes at multiples of 4. Additional spikes at lags 1 and 5.


#** 9.e ----
# What model do these graphs suggest?

#   Possible model = ARIMA(1,1,1)(1,1,1)[4]


#** 9.f ----
# Does ARIMA() give the same model that you chose?
# If not, which model do you think is better?

fit <- aus_arrivals_jp %>%
  model(
    arima = ARIMA(Arrivals),
    arima_111111 = ARIMA(Arrivals ~ pdq(1,1,1) + PDQ(1,1,1))
  )

fit %>% glance() %>% arrange(AICc)
# Both models have similar values, although the arima model is slightly better.


#** 9.g ----
# Write the model in terms of the backshift operator, then without using the backshift operator.

#   Backshift operator
#     (1 − Φ[1]B[^4]) * (1 − B)(1 − B[^4]) * y[t] = (1 + θ[1]B) (1 + Θ[1]B[^4]) * ε[t]
    
#     (1 − Φ[1]B[^4]) * (1 − B[^4] − B + B[^4]) * y[t] = (1 + θ[1]B + Θ[1]B[^4] + Θ[1][^2]B[^5]) * ε[t]
#     (1 − Φ[1]B[^4] − B[^4] + Φ[1]B[^8] − B + Φ[1]B[^5] + B[^4] - Φ[1]B[^8]) * y[t] = (1 + θ[1]B + Θ[1]B[^4] + Θ[1][^2]B[^5]) * ε[t]
#     (1 − Φ[1]B[^4] − B + Φ[1]B[^5]) * y[t] = (1 + θ[1]B + Θ[1]B[^4] + Θ[1][^2]B[^5]) * ε[t]
#     y[t] - Φ[1]y[t-4] - y[t-1] + Φ[1]y[t-5] = ε[t] + θ[1]ε[t-1] + Θ[1]ε[t-4] + Θ[1][^2]ε[t-5]
#     y[t] = y[t-1] + Φ[1](y[t-4] - y[t-5]) + ε[t] + θ[1](ε[t-1] + ε[t-4]) + Θ[1][^2]ε[t-5]
#   Without Backshift operator


#* 10 ----
# Choose a series from us_employment, the total employment in different industries in the United States.

us_emp_TP <- us_employment %>%
  filter(Title == "Total Private") %>%
  select(Month, Employed)

us_emp_TP %>% autoplot(Employed)


#** 10.a ----
# Produce an STL decomposition of the data and describe the trend and seasonality.

us_emp_TP %>%
  model(STL(Employed)) %>%
  components() %>%
  autoplot()

# Increasing trend with changing seasonal variation.


#** 10.b ----
# Do the data need transforming?
# If so, find a suitable transformation.

#   The data needs transformation.

lambda <- us_emp_TP %>% features(Employed, guerrero) %>% pull(lambda_guerrero)

us_emp_TP <- us_emp_TP %>% mutate(Employed_bc = box_cox(Employed, lambda))


#** 10.c ----
# Are the data stationary?
# If not, find an appropriate differencing which yields stationary data.

#   Not stationary.

us_emp_TP %>% features(Employed_bc, unitroot_nsdiffs)

us_emp_TP %>% features(Employed_bc %>% difference(lag = 12), unitroot_ndiffs)


us_emp_TP %>% autoplot(difference(Employed_bc, lag = 12))

# 1 seasonal difference.

us_emp_TP <- us_emp_TP %>%
  mutate(Employed_bc_sdiff = difference(Employed_bc, lag = 12))


#** 10.d ----
# Identify a couple of ARIMA models that might be useful in describing the time series.
# Which of your models is the best according to their AICc values?

us_emp_TP %>% gg_tsdisplay(Employed_bc_sdiff, plot_type = "partial", lag_max = 36)

fit <- us_emp_TP %>%
  model(
    arima = ARIMA(box_cox(Employed, lambda)),
    arima_100010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0)),
    arima_200010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0)),
    arima_300010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0)),
    arima_400010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0))
  )

fit %>% pivot_longer(everything())

fit %>% glance() %>% arrange(AICc)

# arima model is the best.


#** 10.e ----
# Estimate the parameters of your best model and do diagnostic testing on the residuals.
# Do the residuals resemble white noise?
# If not, try to find another ARIMA model which fits better.

fit %>%
  select(arima) %>%
  gg_tsresiduals()

# The arima model has residuals which resemble white noise.
# However, there is a spike at lag 5, 7 and 18.


fit %>%
  select(arima_400010) %>%
  gg_tsresiduals()

# The next best model has spikes at multiple lags.


#** 10.f ----
# Forecast the next 3 years of data.
# Get the latest figures from https://fred.stlouisfed.org/categories/11 to check the accuracy of your forecasts.

fit %>%
  select(arima) %>%
  forecast(h = "3 years") %>%
  hilo(level = 95)

fit %>%
  select(arima) %>%
  forecast(h = "3 years") %>%
  autoplot(us_emp_TP %>% filter(year(Month) >= 2018))

# The mean forecasted values are close to the actual values, and are in the range.
# Due to Covid 19 (black swan event), the data starting from Mar 2020 is different from the forecasted values.


#** 10.g ----
# Eventually, the prediction intervals are so wide that the forecasts are not particularly useful.
# How many years of forecasts do you think are sufficiently accurate to be usable?

fit %>%
  select(arima) %>%
  forecast(h = "10 years") %>%
  autoplot(us_emp_TP %>% filter(year(Month) >= 2018))

# Due to the black swan event, it is not possible to check the forecast for long ranges.
# 2-3 years is a decent range, especially as this gives a decent near forecast.
# If the historical pattern is consistant and is expected to continue in the future then even 5-6 years might be considered.


#* 11 ----
# Choose one of the following seasonal time series: the Australian production of electricity, cement, or gas (from aus_production).

#** 11.a ----
# Do the data need transforming?
# If so, find a suitable transformation.

aus_production %>% autoplot(Electricity)
aus_production %>% model(STL(Electricity)) %>% components() %>% autoplot()
# There is changing variation in the seasonal pattern, so transformation is required.

lambda <- aus_production %>%
  features(Electricity, guerrero) %>%
  pull(lambda_guerrero)

aus_electricity <- aus_production %>%
  select(Quarter, Electricity) %>%
  mutate(Electricity_bc = box_cox(Electricity, lambda))

aus_electricity %>% autoplot(Electricity_bc)
aus_electricity %>% model(STL(Electricity_bc)) %>% components() %>% autoplot()


#** 11.b ----
# Are the data stationary?
# If not, find an appropriate differencing which yields stationary data.

#   The data is not stationary as it has trend and seasonality.

aus_electricity %>% features(Electricity_bc, unitroot_nsdiffs)
aus_electricity %>% features(Electricity_bc %>% difference(lag = 4), unitroot_ndiffs)

aus_electricity <- aus_electricity %>%
  mutate(Electricity_bc_ddiff = Electricity_bc %>% difference(lag = 3) %>% difference(lag = 1))

aus_electricity %>% autoplot(Electricity_bc_ddiff)

aus_electricity %>%
  filter(!is.na(Electricity_bc_ddiff)) %>%
  model(STL(Electricity_bc_ddiff)) %>%
  components() %>% autoplot()


#** 11.c ----
# Identify a couple of ARIMA models that might be useful in describing the time series.
# Which of your models is the best according to their AIC values?

aus_electricity %>%
  gg_tsdisplay(Electricity_bc_ddiff, plot_type = "partial", lag_max = 36)

# Possible Model = (1,1,1)(1,1,1)[4]

fit <- aus_electricity %>%
  model(
    arima = ARIMA(box_cox(Electricity, lambda)),
    arima_111111 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(1,1,1)),
    arima_111011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(0,1,1)),
    arima_011111 = ARIMA(box_cox(Electricity, lambda) ~ pdq(0,1,1) + PDQ(1,1,1)),
    arima_011011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima_111110 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(1,1,0)),
    arima_110111 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,0) + PDQ(1,1,1)),
    arima_110110 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,0) + PDQ(1,1,0)),
    arima_110112 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,0) + PDQ(1,1,2)),
    arima_112011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,2) + PDQ(0,1,1))
  )

fit %>% pivot_longer(everything())

fit %>% glance() %>% arrange(AIC)
# The best model is 111011.


#** 11.d ----
# Estimate the parameters of your best model and do diagnostic testing on the residuals.
# Do the residuals resemble white noise?
# If not, try to find another ARIMA model which fits better.

fit %>%
  select(arima_111011) %>%
  gg_tsresiduals(lag = 36)

# The residuals resemble white noise. There are spikes at lag 6 and 22 but they are far back that they can be ignored.


#** 11.e ----
# Forecast the next 24 months of data using your preferred model.

fit %>%
  select(arima_111011) %>%
  forecast(h = "24 months") %>%
  autoplot(aus_electricity %>% filter(year(Quarter) >= 2000))


#** 11.f ----
# Compare the forecasts obtained using ETS().

aus_electricity %>%
  model(
    arima_111011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(0,1,1)),
    ETS = ETS(box_cox(Electricity, lambda))
  ) %>%
  forecast(h = "24 months") %>%
  autoplot(aus_electricity %>% filter(year(Quarter) >= 2000))

# The arima model shows a slightly damped trend compared to the ETS model.


#* 12 ----
# For the same time series you used in the previous exercise, try using a non-seasonal model applied to the seasonally adjusted data obtained from STL.
# Compare the forecasts with those obtained in the previous exercise.
# Which do you think is the best approach?

aus_electricity %>%
  model(
    arima_111011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(0,1,1)),
    ETS = ETS(box_cox(Electricity, lambda)),
    arima_bc_sa = decomposition_model(
      STL(box_cox(Electricity, lambda)),
      ARIMA(season_adjust)
    ),
    ETS_bc_sa = decomposition_model(
      STL(box_cox(Electricity, lambda)),
      ETS(season_adjust)
    ),
    arima_sa = decomposition_model(
      STL(Electricity),
      ARIMA(season_adjust)
    ),
    ETS_sa = decomposition_model(
      STL(Electricity),
      ETS(season_adjust)
    )
  ) %>%
  forecast(h = "24 months") %>%
  autoplot(aus_electricity %>% filter(year(Quarter) >= 2000), level = NULL)

# Except arima_bc_sa and ETS_sa the remaining models gives similar forecasts and all look reasonable.
# Non seasonally adjusted models seems to give a slightly better result, although either approach can work.


#* 13 ----
# For the Australian tourism data (from tourism):

#** 13.a ----
# Fit ARIMA models for each time series.

fit <- tourism %>%
  filter(Region %in% c("Snowy Mountains", "Melbourne")) %>%
  model(ARIMA(Trips))


#** 13.b ----
# Produce forecasts of your fitted models.

fc <- fit %>% forecast(h = 12)


#** 13.c ----
# Check the forecasts for the “Snowy Mountains” and “Melbourne” regions.
# Do they look reasonable?

fc %>%
  autoplot(tourism) +
  facet_grid(Region ~ Purpose, scales = "free_y")

# Snowy Mountains - Business, Other & Visiting have naive forecasts.
# Melbourne - Holiday & Other have MA(1) models and the forecasts are similar to naive forecasts.
# These do not capture any seasonal fluctuations, but the forecasts ranges look reasonable.

# The other forecasts look reasonable.


#* 14 ----
# For your retail time series (Exercise 5 above):

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

# 1 seasonal difference after Box-Cox transformation.

myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)


#** 14.a ----
# develop an appropriate seasonal ARIMA model;

fit <- myseries_train %>%
  model(
    Arima = ARIMA(box_cox(Turnover, lambda)),
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )


#** 14.b ----
# compare the forecasts with those you obtained in earlier chapters;

fc <- fit %>% forecast(new_data = myseries_test)

fc %>% autoplot(myseries, level = NULL)

fc %>% accuracy(myseries) %>% arrange(RMSE)


# ETS AM model is better than ARIMA model.
# However, ARIMA is much better than the AdM and SNaive models.


#** 14.c ----
# Obtain up-to-date retail data from the ABS website (Cat 8501.0, Table 11), and compare your forecasts with the actual numbers.
# How good were the forecasts from the various models?

fc <- fit %>% forecast(h = "10 years")

fc %>%
  hilo(level = 95) %>%
  select(.model, Month, .mean, "95%") %>%
  filter(year(Month) == 2019)

# The RMSE for year 2019 = 115.6, which is higher than for test set (87).
# The forecasts are all within the 95% confidence interval.
# However, considering that the model has built using data till 2010, this error is reasonable and it shows that the model is still good even after nearly a decade.


#* 15 ----
# Consider the number of Snowshoe Hare furs traded by the Hudson Bay Company between 1845 and 1935 (data set pelt).

#** 15.a ----
# Produce a time plot of the time series.

pelt %>% autoplot(Hare)


#** 15.b ----
# Assume you decide to fit the following model: 
# yt = c + ϕ[1]y[t−1] + ϕ[2]y[t−2] + ϕ[3]y[t−3] + ϕ[4]y[t−4] + ε[t],
# where εt is a white noise series.
# What sort of ARIMA model is this (i.e., what are p, d, and q)?

#   Model - ARIMA(4,0,0)


#** 15.c ----
# By examining the ACF and PACF of the data, explain why this model is appropriate.

pelt %>% gg_tsdisplay(Hare, plot_type = "partial")

# The ACF plot is sinusoidal and the PACF has spike at lag 4 but none after.
# Therefore the plot is correct.


#** 15.d ----
# The last five values of the series are given below:
# Year 	                  1931 	        1932 	  1933 	        1934 	  1935
# Number of hare pelts 	  19520 	  82110 	  89760 	  81660 	  15760
# The estimated parameters are c=30993, ϕ1=0.82, ϕ2=−0.29, ϕ3=−0.01, and ϕ4=−0.22.
# Without using the forecast() function, calculate forecasts for the next three years (1936–1939).


#   c=30993, ϕ1=0.82, ϕ2=−0.29, ϕ3=−0.01, and ϕ4=−0.22
#   ε[t] = 0 (while forecasting future errors are replaced with 0).
    
    
#   y[t]  = c + ϕ[1]y[t−1] + ϕ[2]y[t−2] + ϕ[3]y[t−3] + ϕ[4]y[t−4] + ε[t]
#         = 30993 + 0.82y[t−1] − 0.29y[t−2] − 0.01y[t−3] − 0.22y[t−4]
    
#   y[1936] = 30993 + 0.82y[1935] − 0.29y[1934] − 0.01y[1933] − 0.22y[1932]
#           = 30993 + 0.82*15760 − 0.29*81660 − 0.01*89760 − 0.22*82110
#           = 1273
    
#   Similarly 1937 = 6903, 1938 = 18161, 1939 = 40403


#** 15.e ----
# Now fit the model in R and obtain the forecasts using forecast().
# How are they different from yours?
# Why?

pelt %>%
  model(ARIMA(Hare ~ pdq(4,0,0))) %>%
  report()

# The forecasted numbers are slightly different as the manually calculated numbers are using rounded off coefficients rather than the actuals ones.
# Also, as the future forecasted numbers are based on the past forecasted values, the difference in the past values get compounded as the future values are calculated.


#* 16 ----
# The population of Switzerland from 1960 to 2017 is in data set global_economy.

popu_switz <- global_economy %>%
  filter(Country == "Switzerland") %>%
  select(Year, Population) %>%
  mutate(Population = Population/1e6)


#** 16.a ----
# Produce a time plot of the data.

popu_switz %>% autoplot(Population)


#** 16.b ----
# You decide to fit the following model to the series: 
# y[t] = c + y[t−1] + ϕ[1](y[t−1] − y[t−2]) + ϕ[2](y[t−2] − y[t−3]) + ϕ[3](y[t−3] − y[t−4]) + ε[t]
# where yt is the Population in year t and εt is a white noise series.
# What sort of ARIMA model is this (i.e., what are p, d, and q)?

#   Model - ARIMA(3,1,0)


#** 16.c ----
# Explain why this model was chosen using the ACF and PACF of the differenced series.

popu_switz %>% gg_tsdisplay(Population %>% difference(), plot_type = "partial")

# ACF plot is sinusoidal and the PACF has a spike at lag 3 and none after.
# Therefore the model is appropriate.


#** 16.d ----
# The last five values of the series are given below.
# Year 	                    2013 	      2014 	      2015 	      2016 	      2017
# Population (millions)     8.09 	      8.19 	      8.28 	      8.37 	      8.47
# The estimated parameters are c=0.0053, ϕ1=1.64, ϕ2=−1.17, and ϕ3=0.45.
# Without using the forecast() function, calculate forecasts for the next three years (2018–2020).


# c=0.0053, ϕ1=1.64, ϕ2=−1.17, and ϕ3=0.45

# y[t]  = c + y[t−1] + ϕ[1](y[t−1] − y[t−2]) + ϕ[2](y[t−2] − y[t−3]) + ϕ[3](y[t−3] − y[t−4]) + ε[t]
#       = 0.0053 + y[t−1] + 1.64*(y[t−1] − y[t−2]) − 1.17(y[t−2] − y[t−3]) + 0.45(y[t−3] − y[t−4])

# y[2018] = 0.0053 + y[2017] + 1.64*(y[2017] − y[2016]) − 1.17(y[2016] − y[2015]) + 0.45(y[2015] − y[2014])
#         = 0.0053 + 8.47 + 1.64*(8.47 − 8.37) − 1.17(8.37 − 8.28) + 0.45(8.28 − 8.19)
#         = 8.5745

# Similarly 2019 = 8.6747, 2020 = 8.7670


#** 16.e ----
# Now fit the model in R and obtain the forecasts from the same model.
# How are they different from yours?
# Why?

popu_switz %>%
  model(ARIMA(Population ~ pdq(3,1,0))) %>%
  forecast(h = 3) %>%
  hilo() %>%
  select(Year, .mean, "95%")

# 2018 = 8.5585, 2019 = 8.6475, 2020 = 8.7317

# The forecasted numbers are slightly lower as the manually calculated numbers are using rounded off coefficients rather than the actual ones.
# Also, as the future forecasted numbers are based on the past forecasted values, the differences in the past values get compounded as the future values are calculated.


#* 17 ----
# Before doing this exercise, you will need to install the Quandl package in R using
#   install.packages("Quandl")

#** 17.a ----
# Select a time series from Quandl. Then copy its short URL and import the data using
# y <- as_tsibble(Quandl::Quandl("?????"), index = Date)
# (Replace ????? with the appropriate code.)

y <- as_tsibble(Quandl::Quandl("PSE/ANNINC992I_PALL_IN"))

y <- y %>% mutate(Date = year(Date))


#** 17.b ----
# Plot graphs of the data, and try to identify an appropriate ARIMA model.

y %>% autoplot(Value)
# This is annual data, with increasing trend, which exponentially increases after 2000.
# Log transform might help stabalize the series.
# Also, first difference might be required.

y %>% autoplot(log(Value))
y %>% autoplot(log(Value) %>% difference())
# There is a slight trend still visible, so a second difference might be required.

y %>% features(log(Value), unitroot_ndiffs)

y %>%
  gg_tsdisplay(log(Value) %>% difference(differences = 2), plot_type = "partial")

# The PACF has a spike at lag 2 followed by a small spike at lag 5 and 8. A MA(2) might work.
# The ACF has a spike at lag 1 followed by a spike at lag 9. An AR(1) might work.

# Model - ARIMA(1,2,2)


#** 17.c ----
# Do residual diagnostic checking of your ARIMA model.
# Are the residuals white noise?

fit <- y %>% model(ARIMA(log(Value) ~ pdq(1,2,2)))

fit %>% gg_tsresiduals()
# The residuals look like white noise.

augment(fit) %>% features(.innov, ljung_box, lag = 10, dof = 3)
# The test returns a large p-value proving that the residuals are white noise.


#** 17.d ----
# Use your chosen ARIMA model to forecast the next four years.

fit %>%
  forecast(h = 4) %>%
  autoplot(y)

# The forecast looks reasonable.


#** 17.e ----
# Now try to identify an appropriate ETS model.

fit <- y %>% model(ETS(log(Value)))


#** 17.f ----
# Do residual diagnostic checking of your ETS model.
# Are the residuals white noise?

fit %>% gg_tsresiduals()
# The residuals look like white noise.

augment(fit) %>% features(.innov, ljung_box, lag = 10, dof = 2)
# The test returns a large p-value proving that the residuals are white noise.


#** 17.g ----
# Use your chosen ETS model to forecast the next four years.

fit %>%
  forecast(h = 4) %>%
  autoplot(y)

# The forecast looks reasonable.


#** 17.h ----
# Which of the two models do you prefer?

y %>%
  model(
    ARIMA(log(Value) ~ pdq(1,2,2)),
    ETS(log(Value))
  ) %>%
  forecast(h = 4) %>%
  autoplot(y %>% filter(Date >= 2010))

# The ETS model gives a slightly damped forecast compared to the ARIMA model, and therefore seems more reasonable.
