#* 1 ----
# Compare STL and Dynamic Harmonic Regression forecasts for one of the series in the pedestrian data set.

sth_cross_ped <- pedestrian %>%
  filter(Sensor == "Southern Cross Station")

sth_cross_ped <- sth_cross_ped %>%
  fill_gaps() %>%
  fill(Count, .direction = "down")
  # filter(us.na(Date))
# There is missing data, so using previous value of Count to fill.

sth_cross_ped %>%
  # filter(yearweek(Date) == yearweek("2015 W10")) %>%
  filter(yearmonth(Date) == yearmonth("2015 Aug")) %>%
  autoplot(Count)

# Hourly data so 3 seasonal periods - daily(24), weekly(7 * 24), annual(365.25 * 24)
#   Order matters so - STL = year + week + day, robust = TRUE
# Since no re-index required, not explicitly setting the season.

sth_cross_ped %>%
  model(STL(sqrt(Count))) %>%
  components() %>%
  autoplot()

# In this case, the trend and the daily seasonality have wider bars (and therefore relatively narrower ranges) compared to the other components, because there is little trend seen in the data, and the daily seasonality is weak.


#** 1.a ----
# Try modifying the order of the Fourier terms to minimize the AICc value.

fit <- sth_cross_ped %>%
  model(
    dhr_K1053 = ARIMA(
      log(Count + 1) ~ PDQ(0,0,0) + pdq(d = 0) +
        fourier(period = "day", K = 10) +
        fourier(period = "week", K = 5) +
        fourier(period = "year", K = 3)
    ),
    dhr_K1076 = ARIMA(
      log(Count + 1) ~ PDQ(0,0,0) + pdq(d = 0) +
        fourier(period = "day", K = 11) +
        fourier(period = "week", K = 6) +
        fourier(period = "year", K = 6)
    ),
    dhr_K1253 = ARIMA(
      log(Count + 1) ~ PDQ(0,0,0) + pdq(d = 0) +
        fourier(period = "day", K = 9) +
        fourier(period = "week", K = 4) +
        fourier(period = "year", K = 3)
    )
  )

# sqrt() transformation doesn't work as the distribution generated is with negative mean, which results in the lower prediction limit being higher than the upper limit which is not possible.
# adding +1 in the log() transformation to handle 0's in the data.

glance(fit) %>% arrange(BIC)
# Using AICc we get K_1076 as the best model.
# However, if we look at BIC it is comparable across the 3 models.


#** 1.b ----
# Check the residuals for each model.
# Do they capture the available information in the data?

for(i in fit %>% select(-Sensor) %>% names()){
  fit %>%
    select(i) %>%
    gg_tsresiduals() %>%
    print()
}

fit2 <- sth_cross_ped %>%
  model(
    stl = decomposition_model(
      STL(sqrt(Count)),
      ETS(season_adjust ~ season("N"))
    )
  )

fit2 %>% gg_tsresiduals()

# All models have significant autocorrelations, which means here is additional information left in the data.


#** 1.c ----
# Which of the two sets of forecasts are best?
# Explain.

fc <- bind_rows(
  fit %>% forecast(h = "1 week"),
  fit2 %>% forecast(h = "1 week")
)

fc %>%
  filter(.model %in% c("dhr_K1053", "stl")) %>%
  autoplot(sth_cross_ped %>% filter(yearmonth(Date_Time) >= yearmonth("2016 Dec")))

# Both models give comparable forecasts, although the mean forecasts give lower values than the historical data.
# There is room for improving the model, as neither model is very accurate.


#* 2 ----
# Consider the weekly data on US finished motor gasoline products supplied (millions of barrels per day) (series us_gasoline):

us_gasoline_2004 <- us_gasoline %>%
  filter(year(Week) <= 2004)

us_gasoline_2004 %>%
  filter(year(Week) <= 1995) %>%
  autoplot(Barrels)


#** 2.a ----
# Fit a dynamic harmonic regression model to these data.
# How does it compare to the regression model you fitted in Exercise 5 in Section 7.10?

fit <- us_gasoline_2004 %>%
  model(
    dhr_K_26 = ARIMA( sqrt(Barrels) ~ PDQ(0,0,00) + pdq(d = 0) + fourier(period = "year", K = 26) ),
    dhr_K_11 = ARIMA( sqrt(Barrels) ~ PDQ(0,0,00) + pdq(d = 0) + fourier(period = "year", K = 11) ),
    dhr_K_7 = ARIMA( sqrt(Barrels) ~ PDQ(0,0,00) + pdq(d = 0) + fourier(period = "year", K = 7) ),
    K_26 = ARIMA( sqrt(Barrels) ~ trend() + fourier(K = 26) ),
    K_11 = ARIMA( sqrt(Barrels) ~ trend() + fourier(K = 11) ),
    K_7 = ARIMA( sqrt(Barrels) ~ trend() + fourier(K = 7) )
  )

glance(fit) %>% arrange(AICc) %>% select(.model, AIC:BIC)

# dhr_K_7 is he best model when comparing AICc.


us_gasoline_test <- us_gasoline %>%
  filter(year(Week) == 2005)

us_gasoline_fc <- fit %>%
  forecast(us_gasoline_test)

us_gasoline_fc %>%
  filter(.model %in% c("dhr_K_7", "K_7", "K_11")) %>%
  autoplot(us_gasoline %>% filter(between(year(Week), 2004, 2005)), level = NULL)

accuracy(us_gasoline_fc, us_gasoline) %>% arrange(RMSE)

# While the model for the dhr models are better, the regression models are better at forecasting.
# The dhr models give a lower forecast than the actual data, while the regression models give more inline forecasts.


#** 2.b ----
# Check the residuals from both models and comment on what you see.

for (i in fit %>% names()) {
  fit %>%
    select(i) %>%
    gg_tsresiduals() %>%
    print()
}

fit %>%
  augment() %>%
  features(.innov, ljung_box, lag = 2 * 52, dof = fit %>% select(i) %>% coefficients() %>% nrow()) %>%
  arrange(desc(lb_pvalue))

# All models fail the portmanteau test, although the regression models give values closer to 0.05.


#** 2.c ----
# Could you model these data using any of the other methods we have considered in this book?
# Explain why/why not.

#   Due to presence of varying cyclicity along with the trend, the regression model is the best option.
#   We can try modeling using Neural Network, as it can run non-linear regression model which might capture the cyclicity better.


#* 3 ----
# Experiment with using NNETAR() on your retail data and other data we have considered in previous chapters.

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

# 1 seasonal difference after Box-Cox transformation.

myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)

fit <- myseries_train %>%
  model(
    nn = NNETAR(box_cox(Turnover, lambda)),
    Arima_K_6_313 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0) + pdq(3,1,3)),
    Arima_K_6_214 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0) + pdq(2,1,4)),
    Arima = ARIMA(box_cox(Turnover, lambda)),
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(new_data = myseries_test)

fc %>% filter(!.model %in% c("SNaive", "AdM")) %>% autoplot(myseries, level = NULL)

fc %>% accuracy(myseries) %>% arrange(MASE)

# NNETAR gives a decent forecast eacpecially in the early period, but it starts damping and therefore deviating from the actuals.
# The best model is still AM.
