#* 1 ----
# Consider the the number of pigs slaughtered in Victoria, available in the aus_livestock dataset.

aus_pigs <- aus_livestock %>%
  filter(Animal == "Pigs", State == "Victoria")


#** 1.a ----
# Use the ETS() function to estimate the equivalent model for simple exponential smoothing.
# Find the optimal values of α and ℓ0, and generate forecasts for the next four months.

fit <- aus_pigs %>%
  model(ETS(Count ~ error() + trend("N") + season("N")))

tidy(fit)

fc <- fit %>%
  forecast(h = 4)

fc %>%
  autoplot(aus_pigs %>% filter(year(Month) >= 2010))


#** 1.b ----
# Compute a 95% prediction interval for the first forecast using ^y±1.96s where s is the standard deviation of the residuals.
# Compare your interval with the interval produced by R.

clipr::write_clip(augment(fit))
# std in excel(using STDEV.S()) == 9344.665793

s <- glance(fit)$sigma2 %>% sqrt()  # 9353.115

level_max_month <- components(fit) %>%
  filter(Month == max(Month)) %>%
  pull(level)

# first forecast
#   Excel std
level_max_month - 1.96*9344.665793  # 76871.01
level_max_month + 1.96*9344.665793  # 113502.1


#   R std
level_max_month - 1.96*s  # 76854.45
level_max_month + 1.96*s  # 113518.7


#   R fc
fc %>% filter(Month == min(Month)) %>% hilo() %>% pull(`95%`)   # [76854.79, 113518.3]95


# There is a slight difference between the 3 results.
# R std result might be different because 1.96 is a rounded figure rather than the full number.
# Excel std result might be different because STD calculation is different and also the above reason.


#* 2 ----
# Write your own function to implement simple exponential smoothing.
# The function should take arguments y (the time series), alpha (the smoothing parameter α) and level (the initial level ℓ0).
# It should return the forecast of the next observation in the series.
# Does it give the same forecast as ETS()?

ses <- function(y, alpha, level) {
  level0 <- level
  rows <- nrow(y)
  level <- NA
  for (t in 1:rows) {
    if(t == 1)
      pre_level <- level0
    else
      pre_level <- level[t-1]
  
    level[t] <- (alpha * y[[t,2]]) + ((1 - alpha) * pre_level)
  }
  level <- level[rows]
  return(level)
}


# aus_pigs from Q1
aus_pigs %>%
  select(Month, Count) %>%
  ses(alpha = 0.3221247, level = 100646.6)

# Yes. The forecast is the same.


#* 3 ----
# Modify your function from the previous exercise to return the sum of squared errors rather than the forecast of the next observation.
# Then use the optim() function to find the optimal values of α and ℓ0.
# Do you get the same values as the ETS() function?

sse <- function(y, par) {
  # par1 is alpha; par2 is level0
  alpha <- par[1]
  level0 <- par[2]
  rows <- nrow(y)
  level <- yhat <- error <- NA
  for (t in 1:rows) {
    if(t == 1)
      pre_level <- level0
    else
      pre_level <- level[t-1]
    
    yhat[t] <- pre_level
    level[t] <- (alpha * y[[t,2]]) + ((1 - alpha) * pre_level)
    error[t] <- y[[t,2]] - yhat[t]
  }
  level <- level[rows]
  sse <- sum(error^2)
  return(sse)
}


# aus_pigs from Q1
aus_pigs %>%
  select(Month, Count) %>%
  ses(par = c(0.3221247, 100646.6))

optim_par <- optim(
  par = c(0, mean(range(aus_pigs$Count))), 
  fn = sse, 
  y = aus_pigs %>% select(Month, Count)
)


# The optim value varies from the values forecasted using ETS().
# This might be possibly because of the large range for aus_pigs$count.


#* 4 ----
# Combine your previous two functions to produce a function that both finds the optimal values of α and ℓ0, and produces a forecast of the next observation in the series.

ets_custom <- function(y){
  optim_par <- optim(
    par = c(0, mean(range(y[,2]))), 
    fn = sse, 
    y = y
  )
  fc <- ses(y = y, alpha = optim_par$par[1], level = optim_par$par[2])
  return(list(fc = fc, alpha = optim_par$par[1], level = optim_par$par[2]))
}

aus_pigs %>%
  select(Month, Count) %>%
  ets_custom()


# compare with ETS model
fc$.mean[1]; tidy(fit)$estimate[1]; tidy(fit)$estimate[2]


#* 5 ----
# Data set global_economy contains the annual Exports from many countries.
# Select one country to analyse.

uk_exports <- global_economy %>%
  filter(Country == "United Kingdom")


#** 5.a ----
# Plot the Exports series and discuss the main features of the data.

uk_exports %>% autoplot(Exports)
# Rising trend, no clear seasonality, possible cyclicity present

uk_exports %>% gg_lag(Exports, geom = "point")
# Strong correlation at lag 1 which reduces to lag 2 and 3. No clear correlation after lag 3.

uk_exports %>% ACF(Exports) %>% autoplot()
# Large autocorrelations for nearby lags slowly reducing confirms the presence of trend.
# No seasonal spikes show lack of seasonality in the data.


#** 5.b ----
# Use an ETS(A,N,N) model to forecast the series, and plot the forecasts.

fit <- uk_exports %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

fc <- fit %>% forecast(h = 5)

fc %>% autoplot(uk_exports)


#** 5.c ----
# Compute the RMSE values for the training data.

fit_RMSE <- accuracy(fit)$RMSE


#** 5.d ----
# Compare the results to those from an ETS(A,A,N) model.
# (Remember that the trended model is using one more parameter than the simpler model.)
# Discuss the merits of the two forecasting methods for this data set.

fit2 <- uk_exports %>%
  model(ETS(Exports ~ error("A") + trend("A") + season("N")))

fit2_RMSE <- accuracy(fit2)$RMSE

fit_RMSE; fit2_RMSE
# The acccuracy of the 2 models using RMSE is nearly the same.
# Based on the fit accuracy, either model can be used.


#** 5.e ----
# Compare the forecasts from both methods.
# Which do you think is best?

fc2 <- fit2 %>% forecast(h = 5)

p1 <- fc %>% autoplot(uk_exports) + theme(legend.position = "none")

p2 <- fc2 %>% autoplot(uk_exports) + labs(y = element_blank())

p1 + p2

# The forecasts also provide a similar range, although there is a clear presence of trend in the trended model.
# For short term forecasts(< 5 years), the trended model gives a slightly better forecast.


#** 5.f ----
# Calculate a 95% prediction interval for the first forecast for each model, using the RMSE values and assuming normal errors.
# Compare your intervals with those produced using R.

fc %>% filter(Year == min(Year)) %>% hilo() %>% pull(`95%`); fc$.mean[1] - 1.96*fit_RMSE; fc$.mean[1] + 1.96*fit_RMSE

fc2 %>% filter(Year == min(Year)) %>% hilo() %>% pull(`95%`); fc2$.mean[1] - 1.96*fit2_RMSE; fc2$.mean[1] + 1.96*fit2_RMSE

# The ranges for the manually calculated intervals are slightly narrower than the range produced from R.
# This might be due to the rounding of 1.96.


#* 6 ----
# Forecast the Chinese GDP from the global_economy data set using an ETS model.
# Experiment with the various options in the ETS() function to see how much the forecasts change with damped trend, or with a Box-Cox transformation.
# Try to develop an intuition of what each is doing to the forecasts.
# [Hint: use a relatively large value of h when forecasting, so you can clearly see the differences between the various options when plotting the forecasts.]

gdp_china <- global_economy %>%
  filter(Country == "China")

gdp_china %>% autoplot(GDP)

gdp_china %>% gg_lag(GDP, geom = "point")

gdp_china %>% ACF(GDP) %>% autoplot()


# Clear trend is visible, which starts exponentially increasing after 2020.
# No seasonality seems to be visible.


lambda <- gdp_china %>% features(GDP, features = guerrero) %>% pull(lambda_guerrero)

gdp_china %>% autoplot(GDP) + labs(title = "Original scale") +
  gdp_china %>% autoplot(box_cox(GDP, lambda)) + labs(title = "Box-Cox Transformation", y = element_blank())

fit <- gdp_china %>%
  model(
    Auto = ETS(GDP),
    AAN = ETS(GDP ~ error("A") + trend("A") + season("N")),
    AAdN = ETS(GDP ~ error("A") + trend("Ad") + season("N")),
    Box_Cox = ETS(box_cox(GDP, lambda)),
    Box_Cox_Ad = ETS(box_cox(GDP, lambda) ~ error("A") + trend("Ad") + season("N"))
  )

fc <- fit %>% forecast(h = 100)

fc %>%
  filter(Year <= 2025) %>%
  autoplot(gdp_china) +
  # autoplot(gdp_china, level = NULL) +
  facet_wrap(vars(.model), scales = "free_y")

# Forecast Analysis
# Auto, AAN, and AAdN give negative intervals after 2075. Negative values for GDP are not possible.
# The forecast intervals for all forecast are very wide, caused due to the exponential increase in the GDP post 2020.
#   Auto(MAN) - the point forecast is the trend continued in the future. Has the widest interval from the start, with the lower interval quickly moving towards the negative.
#   AAN - point forecast is similar to Auto.
#   AAdN - the forecast is slowly dampened.
#   Box_Cox - the point forecast is an exponential increase.
#   Box_Cox_Ad - the point forecast is an exponential increase, but the increase is slightly dampened. So the rate of increase is not as high as for Box_Cox.


accuracy(fit)
# All have similar error terms.
# The Box_Cox methods seem to have the lowest AIC values showing they are better models.
# However, over the 100 years forecast period their forecast doesn't seem possible. The AAdN model seems to give a better forecast over large forecast range.


#* 7 ----
# Find an ETS model for the Gas data from aus_production and forecast the next few years.
# Why is multiplicative seasonality necessary here? Experiment with making the trend damped.
# Does it improve the forecasts?

aus_production %>% autoplot(Gas)
# Increasing trend with precense of seasonality. The increase in seasonal component YoY is clearly visible.

aus_production %>% gg_season(Gas)
# Seasonality increases from Q1 to Q3 and then dips in Q4.

aus_production %>% gg_subseries(Gas)
# Rates are increasing YoY each quarter.

aus_production %>% gg_lag(Gas)
aus_production %>% ACF(Gas) %>% autoplot()
# Strong lag clearly visible. Decreasing bar heights confirm presence of trend. Spikes every 4 quarters confirms presence of seasonality.


# Due to  increasing seasonal component YoY the multiplicative seasonality is necessary.

fit <- aus_production %>%
  model(
    AM = ETS(Gas ~ error() + trend("A") + season("M")),
    AdM = ETS(Gas ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(h = "2 years")

fc %>%
  autoplot(aus_production) +
  facet_wrap(vars(.model))

# Damping the trend seems to have no visible impact on the forecast.


#* 8 ----
# Recall your retail time series data (from Exercise 8 in Section 2.10).

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries %>% autoplot(Turnover)

# Comments from 2.10.6
# increasing trend with seasonality
# decreases in Feb, peaks in Dec
# peaks in Dec, starts increasing from Jan
# data is not random, with increasing trend showcasing high correlation with historical values
# trend and seasonlity are confirmed
# correlation with lag = 1, 12


#** 8.a ----
# Why is multiplicative seasonality necessary for this series?

#   Due to presence of increasing seasonal component YoY multiplicative seasonality is necessary.


#** 8.b ----
# Apply Holt-Winters’ multiplicative method to the data.
# Experiment with making the trend damped.

fit <- myseries %>%
  model(
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(h = "2 years")

fc %>% autoplot(myseries)

# Trend damping results in slightly lower point forecasts and wider prediction intervals.


#** 8.c ----
# Compare the RMSE of the one-step forecasts from the two methods.
# Which do you prefer?

fit %>% accuracy()

# The RMSE is similar for both models, with the value for AdM being slightly lower. Hence the AdM model is preferred.


#** 8.d ----
# Check that the residuals from the best method look like white noise.

fit %>% select(AdM) %>% gg_tsresiduals()

fit %>% augment() %>% filter(.model == "AdM") %>% features(.resid, ljung_box, lag = 24, dof = 0)
# Since this is seasonal data taking lag = 2m = 2*12 = 24

# There is clear visibility of autocorrelation visible in the lag plot.
# This is confirmed by the ljung_box test.


#** 8.e ----
# Now find the test set RMSE, while training the model to the end of 2010.
# Can you beat the seasonal naïve approach from Exercise 7 in Section 5.11?

myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)

fit <- myseries_train %>%
  model(
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(new_data = myseries_test)

fc %>% accuracy(myseries)

fc %>% autoplot(myseries, level = NULL)

# Both AM and AdM beat the SNaive model.
#   This is because the SNaive model assumes no trend is present in the model.
# However, between AM and AdM, AM model performs better on the test set.
#   This is because the actual data continues following historical trends rather than plateauing out.

# This case shows that even though a model may perform better on the train set, all models should still be checked against the test set, and only the model which performs best on the test set should be taken, no matter it's performance on the train set.


#* 9 ----
# For the same retail data, try an STL decomposition applied to the Box-Cox transformed series, followed by ETS on the seasonally adjusted data.
# How does that compare with your best previous forecasts on the test set?

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries %>% autoplot(Turnover)


myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)

lambda <- myseries_train %>% features(Turnover, features = guerrero) %>% pull(lambda_guerrero)

myseries_train %>%
  model(STL(box_cox(Turnover, lambda = lambda))) %>%
  components() %>%
  autoplot()

fit <- myseries_train %>%
  model(
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    STL = decomposition_model(
      STL(Turnover),
      ETS(season_adjust)
    )
  )

fc <- forecast(fit, new_data = myseries_test)

fc %>% accuracy(myseries)

fc %>% autoplot(myseries, level = NULL)


# For periods to close to start of test set, the forecasts from all 3 models are similar.
# For periods close to the end of the test set, the forecasts for STL model are lower than the actual Turnover, Box_Cox forecasts higher, while AM forecasts closest to the actual.
# Therefore, AM model is still better than STL decomposition (with or without Box_Cox transformation).


#* 10 ----
# Compute the total domestic overnight trips across Australia from the tourism dataset.

total_trips <- tourism %>%
  summarise(Trips = sum(Trips))


#** 10.a ----
# Plot the data and describe the main features of the series.

total_trips %>% autoplot(Trips)
# No visible trend or seasonality before 2010.
# Increasing trend starting from 2010 with possible seasonal pattern.

total_trips %>% gg_season(Trips)
# Highest Trips in Q1, before dropping in Q2 and Q3, and rising in Q4.

total_trips %>% gg_subseries(Trips)
# There is a clear increase YoY in the number of Trips in each quarter starting from 2010.


total_trips %>% gg_lag(Trips, geom = "point")
total_trips %>% ACF(Trips) %>% autoplot()
# Both Trend and Seasonality is present, with seasonal spikes at 4, 8 and 12 lags.
# Since this is quarterly data it essentially means that the Trips data has a decreasing (but significant) correlation with last 3 years.


#** 10.b ----
# Decompose the series using STL and obtain the seasonally adjusted data.

total_trips %>%
  model(STL(Trips)) %>%
  components() %>%
  autoplot(season_adjust)


#** 10.c ----
# Forecast the next two years of the series using an additive damped trend method applied to the seasonally adjusted data.
# (This can be specified using decomposition_model().)

fit <- total_trips %>%
  model(
    decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("Ad"))
    )
  )

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)


#** 10.d ----
# Forecast the next two years of the series using an appropriate model for Holt’s linear method applied to the seasonally adjusted data (as before but without damped trend).

fit <- total_trips %>%
  model(
    decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("A"))
    )
  )

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)


#** 10.e ----
# Now use ETS() to choose a seasonal model for the data.

fit <- total_trips %>%
  model(
    decomposition_model(
      STL(Trips),
      ETS(season_adjust)
    )
  )

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)


#** 10.f ----
# Compare the RMSE of the ETS model with the RMSE of the models you obtained using STL decompositions.
# Which gives the better in-sample fits?

fit <- total_trips %>%
  model(
    Ad = decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("Ad"))
    ),
    A = decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("A"))
    ),
    ETS = decomposition_model(
      STL(Trips),
      ETS(season_adjust)
    )
  )

fit %>% select(ETS) %>% report()

accuracy(fit)

# The A and ETS models are the same, and give slightly better result than Ad on MAPE and RMSSE.


#** 10.g ----
# Compare the forecasts from the three approaches?
# Which seems most reasonable?

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)

# Model A/ETS seem to be reasonable based on past trends.


#** 10.h ----
# Check the residuals of your preferred model.

fit %>% select(A) %>% gg_tsresiduals()

# The residual plot seems to show no visible pattern, and for the most part seem to be centered around 0.
# The histogram also shows a near normal plot.
# The correlogram shows a spike at lag 14. However, since there is only one such spike and it is far back, we can say that the residuals are not correlated.


#* 11 ----
# For this exercise use the quarterly number of arrivals to Australia from New Zealand, 1981 Q1 – 2012 Q3, from data set aus_arrivals.

aus_arrivals_nz <- aus_arrivals %>% filter(Origin == "NZ")


#** 11.a ----
# Make a time plot of your data and describe the main features of the series.

aus_arrivals_nz %>% autoplot(Arrivals)
# Increasing trend with seasonal patterns. Seasonal amplitude seems to be increasing.

aus_arrivals_nz %>% gg_season(Arrivals)
# Lowest arrivals in Q1, which decreases in Q2 and peaks in Q3, before falling in Q4.

aus_arrivals_nz %>% gg_subseries(Arrivals)
# The mean arrivals in each quarter validate above observation.
# There is YoY increase in each quarter, although the increase in Q1 is less than for other quarters.

aus_arrivals_nz %>% gg_lag(Arrivals, geom = "point")
aus_arrivals_nz %>% ACF(Arrivals, lag_max = 50) %>% autoplot()
# The lag plot and the correlogram shows a strong presence of autocorrelation.
# The trend and seasonal patterns are clearly visible.
# The seasonal patterns repeat every 4 quarters (1 year).
# The strong correlation goes back almost 7-8 years.


#** 11.b ----
# Create a training set that withholds the last two years of available data.
# Forecast the test set using an appropriate model for Holt-Winters’ multiplicative method.

aus_arrivals_nz_test <- aus_arrivals_nz %>% slice(n() - 8:0)

aus_arrivals_nz_train <- aus_arrivals_nz %>% anti_join(aus_arrivals_nz_test)

fit <- aus_arrivals_nz_train %>%
  model(
    AM = ETS(Arrivals ~ error() + trend("A") + season("M")),
    AdM = ETS(Arrivals ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(new_data = aus_arrivals_nz_test)

fc %>% autoplot(aus_arrivals_nz, level = NULL)

accuracy(fc, aus_arrivals_nz)
# AM gives better results on all accuracy measures.

fit %>% select(AdM) %>% gg_tsresiduals()
# Residuals are centered around the mean, histogram is near normal, and the lag plot shows white noise.


#** 11.c ----
# Why is multiplicative seasonality necessary here?

#   Due to presence of incresing seasonal component YoY multiplicative seasonality is necessary.


#** 11.d ----
# Forecast the two-year test set using each of the following methods:
#    an ETS model;
#    an additive ETS model applied to a log transformed series;
#    a seasonal naïve method;
#    an STL decomposition applied to the log transformed data followed by an ETS model applied to the seasonally adjusted (transformed) data.


fit <- aus_arrivals_nz_train %>%
  model(
    ETS = ETS(Arrivals),
    ETS_A = ETS(log(Arrivals) ~ error() + trend() + season("M")),
    SNaive = SNAIVE(Arrivals),
    STL = decomposition_model(
      STL(log(Arrivals)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = aus_arrivals_nz_test)

fc %>% autoplot(aus_arrivals_nz, level = NULL)


#** 11.e ----
# Which method gives the best forecasts?
# Does it pass the residual tests?

accuracy(fc, aus_arrivals_nz)
# The ETS model gives the best forecast.

fit %>% select(ETS) %>% gg_tsresiduals()
# Residuals are almost centered around the mean, histogram has a slight night skew, and the lag plot shows white noise.


#** 11.f ----
# Compare the same four methods using time series cross-validation instead of using a training and test set.
# Do you come to the same conclusions?

fit <- aus_arrivals_nz %>%
  stretch_tsibble(.init = 4) %>%
  model(
    ETS = ETS(Arrivals),
    ETS_A = ETS(log(Arrivals) ~ error() + trend() + season("M")),
    SNaive = SNAIVE(Arrivals),
    STL = decomposition_model(
      STL(log(Arrivals)),
      ETS(season_adjust)
    )
  )

fit %>% forecast(h = 1) %>% accuracy(aus_arrivals_nz)

# Using CV, ETS_A gives the best forecast.


#** 12 ----
aus_production %>% autoplot(Cement)

aus_production %>% gg_season(Cement)

aus_production %>% gg_subseries(Cement)

aus_production %>% gg_lag(Cement, geom = "point")

aus_production %>% ACF(Cement, lag_max = 70) %>% autoplot()

# Trend present, increasing for most part.
# Minor seasonality with changing amplitude.
# lag present up to 64 quarters.

# Therefore, an ETS model with (A,M) or (Ad,M) might be good.


#** 12.a ----
# Apply cross-validation techniques to produce 1 year ahead ETS and seasonal naïve forecasts for Portland cement production (from aus_production).
# Use a stretching data window with initial size of 5 years, and increment the window by one observation.

fit <- aus_production %>%
  stretch_tsibble(.init = 5*4, .step = 1) %>%
  model(
    ETS_AM = ETS(Cement ~ error() + trend("A") + season("M")),
    ETS_AdM = ETS(Cement ~ error() + trend("Ad") + season("M")),
    SNaive = SNAIVE(Cement)
  )

fc <- fit %>% forecast(h = 4)


#** 12.b ----
# Compute the MSE of the resulting 4-step-ahead errors.
# Comment on which forecasts are more accurate.
# Is this what you expected?

fc %>%
  accuracy(aus_production) %>%
  select(.model, RMSE) %>%
  mutate(MSE = RMSE*2) %>%
  print.data.frame()

# ETS_AM is a better model. This is expected based on the initial analysis done on the data.

 
#* 13 ----
# Compare ETS(), SNAIVE() and decomposition_model(STL, ???) on the following five time series.
# You might need to use a Box-Cox transformation for the STL decomposition forecasts.
# Use a test set of three years to decide what gives the best forecasts.
#    Beer and bricks production from aus_production
#    Cost of drug subsidies for diabetes (ATC2 == "A10") and corticosteroids (ATC2 == "H02") from PBS
#    Total food retailing turnover for Australia from aus_retail.


#*** Beer and bricks production ----
# Beer
aus_production %>% autoplot(Beer)

test <- aus_production %>% slice(n() -  11:0)
train <- aus_production %>% anti_join(test)

lambda <- aus_production %>%
  features(Beer, features = guerrero) %>%
  pull(lambda_guerrero)

aus_production %>%
  model(STL(box_cox(Beer, lambda = lambda))) %>%
  components() %>%
  autoplot()

fit <- train %>%
  model(
    ETS = ETS(Beer),
    SNaive = SNAIVE(Beer),
    STL = decomposition_model(
      STL(box_cox(Beer, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(aus_production, level = NULL)

accuracy(fc, aus_production)
# ETS model is the best based on RMSE, RMSSE.


# Bricks
aus_production %>% autoplot(Bricks)

test <- aus_production %>% filter(!is.na(Bricks)) %>% slice(n() - 11:0)
train <- aus_production %>% filter(!is.na(Bricks)) %>% anti_join(test)

lambda <- aus_production %>%
  features(Bricks, features = guerrero) %>%
  pull(lambda_guerrero)

fit <- train %>%
  filter(!is.na(Bricks)) %>%
  model(
    ETS = ETS(Bricks),
    SNaive = SNAIVE(Bricks),
    STL = decomposition_model(
      STL(box_cox(Bricks, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(aus_production, level = NULL)

accuracy(fc, aus_production)
# ETS model is the best based on all parameters.


#*** diabetes(ATC2 == "A10") and corticosteroids (ATC2 == "H02") ----
PBS_A10_H02 <- PBS %>%
  filter(ATC2 %in% c("A10", "H02")) %>%
  group_by(ATC2) %>%
  summarise(Cost = sum(Cost))

PBS_A10_H02_test <- PBS_A10_H02 %>% filter(yearmonth(Month) >= max(yearmonth(Month)) - 35)
PBS_A10_H02_train <- PBS_A10_H02 %>% anti_join(PBS_A10_H02_test)

# diabetes (ATC2 == "H02")
PBS_A10_H02 %>% filter(ATC2 == "A10") %>% autoplot(Cost)

lambda <- PBS_A10_H02 %>% filter(ATC2 == "A10") %>% features(Cost, features = guerrero) %>% pull(lambda_guerrero)

PBS_A10_H02 %>% filter(ATC2 == "A10") %>%
  model(STL(Cost)) %>%
  components() %>%
  autoplot()

fit <- PBS_A10_H02_train %>%
  filter(ATC2 == "A10") %>%
  model(
    ETS = ETS(Cost),
    SNaive = SNAIVE(Cost),
    STL = decomposition_model(
      STL(box_cox(Cost, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = PBS_A10_H02_test %>% filter(ATC2 == "A10"))

fc %>% autoplot(PBS_A10_H02 %>% filter(ATC2 == "A10"), level = NULL)

accuracy(fc, PBS_A10_H02 %>% filter(ATC2 == "A10"))
# STL model is better.
# Probably because of the changing seasonal amplitude, first performing STL on the Box-Cox transformed data, and then using ETS only on the seasonally adjusted data will reduce the impact of the varying seasonal amplitude resulting in better forecast.


# corticosteroids (ATC2 == "H02")
PBS_A10_H02 %>% filter(ATC2 == "H02") %>% autoplot(Cost)

lambda <- PBS_A10_H02 %>% filter(ATC2 == "H02") %>% features(Cost, features = guerrero) %>% pull(lambda_guerrero)

PBS_A10_H02 %>% filter(ATC2 == "H02") %>%
  model(STL(Cost)) %>%
  components() %>%
  autoplot()

fit <- PBS_A10_H02_train %>%
  filter(ATC2 == "H02") %>%
  model(
    ETS = ETS(Cost),
    SNaive = SNAIVE(Cost),
    STL = decomposition_model(
      STL(box_cox(Cost, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = PBS_A10_H02_test %>% filter(ATC2 == "H02"))

fc %>% autoplot(PBS_A10_H02 %>% filter(ATC2 == "H02"), level = NULL)

accuracy(fc, PBS_A10_H02 %>% filter(ATC2 == "H02"))

# STL model is better. Probably same reason as above.


#*** Total food retailing turnover ----
food_turnover <- aus_retail %>% summarise(Turnover = sum(Turnover))

food_turnover %>% autoplot(Turnover)

test <- food_turnover %>% slice(n() - 35:0)
train <- food_turnover %>% anti_join(test)

lambda <- train %>% features(Turnover, features = guerrero) %>% pull(lambda_guerrero)

food_turnover %>%
  model(STL(box_cox(Turnover, lambda = lambda))) %>%
  components() %>%
  autoplot()

fit <- train %>%
  model(
    ETS = ETS(Turnover),
    SNaive = SNAIVE(Turnover),
    STL = decomposition_model(
      STL(box_cox(Turnover, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(food_turnover, level = NULL)

accuracy(fc, food_turnover)
# ETS is the best model.


#* 14 ----
#** 14.a ----
# Use ETS() to select an appropriate model for the following series:
#   total number of trips across Australia using tourism,
#   the closing prices for the four stocks in gafa_stock, and
#   the lynx series in pelt.
# Does it always give good forecasts?


#**** total number of trips across Australia ----
total_trips <- tourism %>% summarise(Trips = sum(Trips))

total_trips %>% autoplot(Trips)

total_trips %>%
  stretch_tsibble(.init = 4) %>%
  model(ETS(Trips)) %>%
  forecast(h = 4) %>%
  accuracy(total_trips)

test <- total_trips %>% slice(n() - 7:0)
train <- total_trips %>% anti_join(test)

fc <- train %>%
  model(ETS(Trips)) %>%
  forecast(new_data = test)

fc %>% autoplot(total_trips, level = NULL)
fc %>% accuracy(total_trips)

# The forecast is not very great, especially as the forecast range increases.


#**** closing prices for the four stocks ----
stock <- gafa_stock %>%
  group_by(Symbol) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE) %>%
  ungroup()

stock %>% autoplot(Close) +
  facet_wrap(vars(Symbol), scales = "free_y")

test <- stock %>% filter(day >= max(day) - 99)
train <- stock %>% anti_join(test)

fit <- train %>% model(ETS(Close))

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(stock, level = NULL) +
  facet_wrap(vars(Symbol), scales = "free_y")
fc %>% accuracy(stock)

# ETS gives a flat forecast which is incorrect.


#**** lynx series ----
pelt %>% autoplot(Lynx)

test <- pelt %>% slice(n() - 4:0)
train <- pelt %>% anti_join(test)

fit <- train %>% model(ETS(Lynx))

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(pelt, level = NULL)
fc %>% accuracy(pelt)

# ETS gives a flat forecast which is incorrect.


#** 14.b ----
# Find an example where it does not work well.
# Can you figure out why?

# ETS does not always give a food forecasts.
# None of the forecsats above work well.

# total number of trips across Australia
# Model - (M,N,M)
#   due to no clear trend, ETS sets the Trend parameter as None. However, after 2010 there seems to be an increasing trend.
#   the seasonal patterns are also not clear and the amplitude seems to be changing. Therefore, even if we take only data after 2010, then the ETS model sets the seasonal parameter as None (see below).

total_trips <- tourism %>%
  summarise(Trips = sum(Trips)) %>%
  filter(year(Quarter) >= 2010)

test <- total_trips %>% slice(n() - 7:0)
train <- total_trips %>% anti_join(test)

fc <- train %>% model(ETS(Trips)) %>% forecast(new_data = test)

fc %>% autoplot(total_trips, level = NULL)
fc %>% accuracy(total_trips)


# closing prices for the four stocks
# Model -
#   AAPL   <ETS(M,N,N)>
#   AMZN   <ETS(M,A,N)>
#   FB     <ETS(M,N,N)>
#   GOOG   <ETS(M,N,N)>
# Since stocks trading does not take place on all 7 days, and there is no clear seasonal patterns, the ETS model selects (N,N) for all stocks except AMZN, where it selects additive trend.
#   However, a similar trend seem to be visible in the other 4 stocks so not sure why it didn't use the same model for the other 3.


# lynx series
# Model - (A,N,N)
# Since this is a annual data, there are no seasonal patterns. Also, there is no trend even though is cyclicity.
# Hence, while the model chosen by ETS is correct, it fails to capture the data trends.
#   This is because ETS is meant to capture trend and seasonality only, and fails when there is only cyclical data with no trend.


#* 15 ----
# Show that the point forecasts from an ETS(M,A,M) model are the same as those obtained using Holt-Winters’ multiplicative method.

# Holt-Winters’ multiplicative method
# The component form for the multiplicative method is:
#   ^y[t+h|t] = (ℓ[t] + hb[t]) * s[t+h−m(k+1)]
#   ℓ[t]      = α(y[t] / s[t−m]) + (1 − α)(ℓ[t−1] + b[t−1])
#   b[t]      = β∗ (ℓ[t] − ℓ[t−1]) + (1 − β∗)b[t−1]
#   s[t]      = γ(y[t] / (ℓ[t−1] + b[t−1])) + (1 − γ)s[t−m]

# 1 step ahead model
#   ^y[t+1|t] = (ℓ[t] + b[t]) * s[t+1−m]

# Specifying one-step-ahead training errors as relative errors such that
#   ε[t]      = ( ^y[t|t-1] - (ℓ[t-1] + b[t-1]) * s[t−m] ) / ( (ℓ[t-1] + b[t-1]) * s[t−m] )

# Substituting this into the error correction equations for Holt-Winters' linear method we obtain
#   y[t]      = ( (ℓ[t-1] + b[t-1]) * s[t−m] ) (1 + ε[t])

#   ℓ[t]      = α(y[t] / s[t−m]) + (1 − α)(ℓ[t−1] + b[t−1])
#             = α( ( ( (ℓ[t-1] + b[t-1]) * s[t−m] ) (1 + ε[t]) ) / s[t−m] ) + (1 − α)(ℓ[t−1] + b[t−1])
#             = α( ℓ[t-1] + b[t-1] )  (1 + ε[t]) + (1 − α)(ℓ[t−1] + b[t−1])
#             = α( ℓ[t-1] + b[t-1] ) + α( ℓ[t-1] + b[t-1] ) ε[t] + ( ℓ[t-1] + b[t-1] ) - α( ℓ[t-1] + b[t-1] )
#             = (ℓ[t-1] + b[t-1])(1 + αε[t])

#   b[t]      = β∗ (ℓ[t] − ℓ[t−1]) + (1 − β∗)b[t−1]
#             = β∗ ( (ℓ[t-1] + b[t-1])(1 + αε[t]) - ℓ[t−1] ) + (1 - β∗)b[t-1]
#             = β∗ ( (ℓ[t-1] + b[t-1]) + (ℓ[t-1] + b[t-1])αε[t] - ℓ[t−1] ) + (1 - β∗)b[t-1]
#             = β∗ ( b[t-1] + (ℓ[t-1] + b[t-1])αε[t] ) + b[t-1] - β∗b[t-1]
#             = β∗ ( ℓ[t-1] + b[t-1] )αε[t] + b[t-1]
#             = b[t-1] + β(ℓ[t-1] + b[t-1])ε[t]
# for simplicity we have set β = αβ*

#   s[t]      = γ(y[t] / (ℓ[t−1] + b[t−1])) + (1 − γ)s[t−m]
#             = γ( ( ( (ℓ[t-1] + b[t-1]) * s[t−m] ) (1 + ε[t]) ) / (ℓ[t-1] + b[t-1]) ) + (1 − γ)s[t−m]
#             = γs[t−m](1 + ε[t]) + (1 − γ)s[t−m]
#             = γs[t−m] + γs[t−m]ε[t] + s[t−m] - γs[t−m]
#             = s[t−m](1 + γε[t])

# After substituting we get the ETS(M,A,M) model.


#* 16 ----
# Show that the forecast variance for an ETS(A,N,N) model is given by 
# σ^2[1 + α^2(h−1)].
#   Skipped as this proof is given in a separate book.


#* 17 ----
# Write down 95% prediction intervals for an ETS(A,N,N) model as a function of ℓT, α, h and σ, assuming normally distributed errors.
#   prediction interval can be written as
#     y[T+h|T] ± c^σh
#     ℓ[t+h-1] + ε[t] ± 1.96(σ[^2][1 + α[^2](h-1)])[^0.5]
