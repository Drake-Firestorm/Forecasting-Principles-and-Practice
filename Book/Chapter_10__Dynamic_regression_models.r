# Chapter 10 Dynamic regression models --------
# The time series models in the previous two chapters allow for the inclusion of information from past observations of a series, but not for the inclusion of other information that may also be relevant.
#   example, the effects of holidays, competitor activity, changes in the law, the wider economy, or other external variables, may explain some of the historical variation and may lead to more accurate forecasts.
# On the other hand, the regression models in Chapter 7 allow for the inclusion of a lot of relevant information from predictor variables, but do not allow for the subtle time series dynamics that can be handled with ARIMA models.
# In this chapter, we consider how to extend ARIMA models in order to allow other information to be included in the models.

# In Chapter 7 we considered regression models of the form
#   y[t] = β[0] + β[1]x[1,t] + ⋯ + β[k]x[k,t] + ε[t],
#     where
#       y[t] is a linear function of the k predictor variables (x[1,t], …, x[k,t]), and
#       ε[t] is usually assumed to be an uncorrelated error term (i.e., it is white noise).
# We considered tests such as the Ljung-Box test for assessing whether the resulting residuals were significantly correlated.

# In this chapter, we will allow the errors from a regression to contain autocorrelation.
# To emphasise this change in perspective, we will replace ε[t] with η[t] in the equation.
# The error series η[t] is assumed to follow an ARIMA model.
#   example, if η[t] follows an ARIMA(1,1,1) model, we can write
#     y[t] = β[0] + β[1]x[1,t] + ⋯ + β[k]x[k,t] + η[t],
#     (1 − ϕ[1]B)(1 − B)η[t] = (1 + θ[1]B)ε[t], where ε[t] is a white noise series.

# Notice that the model has two error terms here —
#   the error from the regression model, which we denote by η[t],
#   and the error from the ARIMA model, which we denote by ε[t].
# Only the ARIMA model errors are assumed to be white noise.


# _----


#* 10.1 Estimation ---------------------------------------------------------
# When we estimate the parameters from the model, we need to minimise the sum of squared ε[t] values.
# If we minimise the sum of squared η[t] values instead (which is what would happen if we estimated the regression model ignoring the autocorrelations in the errors), then several problems arise.
#   1. The estimated coefficients ^β[0], …, ^β[k] are no longer the best estimates, as some information has been ignored in the calculation;
#   2. Any statistical tests associated with the model (e.g., t-tests on the coefficients) will be incorrect.
#   3. The AICc values of the fitted models are no longer a good guide as to which is the best model for forecasting.
#   4. In most cases, the p-values associated with the coefficients will be too small, and so some predictor variables will appear to be important when they are not. This is known as “spurious regression.”

# Minimising the sum of squared ε[t] values avoids these problems.
# Alternatively, maximum likelihood estimation can be used; this will give similar estimates of the coefficients.

# An important consideration when estimating a regression with ARMA errors is that all of the variables in the model must first be stationary.
# Thus, we first have to check that y[t] and all of the predictors (x[1,t], …, x[k,t]) appear to be stationary.
# If we estimate the model when any of these are non-stationary, the estimated coefficients will not be consistent estimates (and therefore may not be meaningful).
# One exception to this is the case where non-stationary variables are co-integrated.
# If there exists a linear combination of the non-stationary y[t] and the predictors that is stationary, then the estimated coefficients will be consistent.

# We therefore first difference the non-stationary variables in the model.
# It is often desirable to maintain the form of the relationship between y[t] and the predictors, and consequently it is common to difference all of the variables if any of them need differencing.
#   The resulting model is then called a “model in differences,” as distinct from a
#   “model in levels,” which is what is obtained when the original data are used without differencing.

# If all of the variables in the model are stationary, then we only need to consider an ARMA process for the errors.
# It is easy to see that a regression model with ARIMA errors is equivalent to a regression model in differences with ARMA errors.
#   example, if the above regression model with ARIMA(1,1,1) errors is differenced we obtain the model
#     y′[t] = β[1]x′[1,t] + ⋯ + β[k]x′[k,t] + η′[t],
#     (1 − ϕ[1]B)η′[t] = (1 + θ[1]B)ε[t],
#     where
#       y′[t] = y[t] − y[t−1],
#       x′[t,i] = x[t,i] − x[t−1,i] and
#       η′[t] = η[t] − η[t−1],
# which is a regression model in differences with ARMA errors.


# _----


#* 10.2 Regression with ARIMA errors using fable ---------------------------
# The function ARIMA() will fit a regression model with ARIMA errors if exogenous regressors are included in the formula.
# As introduced in Section 9.5, the pdq() special specifies the order of the ARIMA error model.
# If differencing is specified, then the differencing is applied to all variables in the regression model before the model is estimated.
#   example, the command
#     ARIMA(y ~ x + pdq(1,1,0))
#   will fit the model y′[t] = β[1]x′[t] + η′[t], where η′[t] = ϕ[1]η′[t−1] + ε[t] is an AR(1) error.
#   This is equivalent to the model y[t] = β[0] + β[1]x[t] + η[t], where η[t] is an ARIMA(1,1,0) error.
# Notice that the constant term disappears due to the differencing.
# To include a constant in the differenced model, we would add 1 to the model formula.

# The ARIMA() function can also be used to select the best ARIMA model for the errors.
# This is done by not specifying the pdq() special.
# If differencing is required, then all variables are differenced during the estimation process, although the final model will be expressed in terms of the original variables.

# The AICc is calculated for the final model, and this value can be used to determine the best predictors.
# That is, the procedure should be repeated for all subsets of predictors to be considered, and the model with the lowest AICc value selected.


#** Example: US Personal Consumption and Income ----
# Figure below shows the quarterly changes in personal consumption expenditure and personal disposable income from 1970 to 2016 Q3.
# We would like to forecast changes in expenditure based on changes in income.
# A change in income does not necessarily translate to an instant change in consumption (e.g., after the loss of a job, it may take a few months for expenses to be reduced to allow for the new circumstances).
# However, we will ignore this complexity in this example and try to measure the instantaneous effect of the average change of income on the average change of consumption expenditure.

us_change %>%
  pivot_longer(c(Consumption, Income), names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income", y = "Quarterly % change")

fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))

report(fit)

# The data are clearly already stationary (as we are considering percentage changes rather than raw expenditure and income), so there is no need for any differencing.
# The fitted model is
#   y[t] = 0.595 + 0.198x[t] + η[t],
#   η[t] = 0.707η[t−1] + ε[t] − 0.617ε[t−1] + 0.207ε[t−2],
#   ε[t]∼NID(0,0.311).

# We can recover estimates of both the η[t] and εt series using the residuals() function.

bind_rows(
  `Regression residuals` = as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` = as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) %>%
  mutate(
    type = factor(type, levels=c(
      "Regression residuals", "ARIMA residuals"))
  ) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

# It is the ARIMA estimated errors (the innovation residuals) that should resemble a white noise series.

fit %>% gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 8)


# _----


#* 10.3 Forecasting --------------------------------------------------------
# To forecast using a regression model with ARIMA errors, we need to forecast the regression part of the model and the ARIMA part of the model, and combine the results.
# As with ordinary regression models, in order to obtain forecasts we first need to forecast the predictors.
# When the predictors are known into the future (e.g., calendar-related variables such as time, day-of-week, etc.), this is straightforward.
# But when the predictors are themselves unknown, we must either model them separately, or use assumed future values for each predictor.


#** Example: US Personal Consumption and Income ----
# We will calculate forecasts for the next eight quarters assuming that the future percentage changes in personal disposable income will be equal to the mean percentage change from the last forty years.

us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")

# The prediction intervals for this model are narrower than if we had fitted an ARIMA model without covariates, because we are now able to explain some of the variation in the data using the income predictor.

# It is important to realise that the prediction intervals from regression models (with or without ARIMA errors) do not take into account the uncertainty in the forecasts of the predictors.
# So they should be interpreted as being conditional on the assumed (or estimated) future values of the predictor variables.


#** Example: Forecasting electricity demand ----
# Daily electricity demand can be modelled as a function of temperature.
# As can be observed on an electricity bill, more electricity is used on cold days due to heating and hot days due to air conditioning.
# The higher demand on cold and hot days is reflected in the U-shape of Figure below, where daily demand is plotted versus daily maximum temperature.

vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)", x = "Maximum daily temperature")

# The data stored as vic_elec_daily includes total daily demand, daily maximum temperatures, and an indicator variable for if that day is a public holiday.
# Figure below shows the time series of both daily demand and daily maximum temperatures.
# The plots highlight the need for both a non-linear and a dynamic model.

vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature)) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + ylab("")

# In this example, we fit a quadratic regression model with ARMA errors using the ARIMA() function.
# The model also includes an indicator variable for if the day was a working day or not.

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type == "Weekday")))

fit %>% gg_tsresiduals()

augment(fit) %>% features(.innov, ljung_box, dof = 9, lag = 14)

# There is clear heteroscedasticity in the residuals, with higher variance in January and February, and lower variance in May.
# The model also has some significant autocorrelation in the residuals, and the histogram of the residuals shows long tails.
# All of these issues with the residuals may affect the coverage of the prediction intervals, but the point forecasts should still be ok.

# Using the estimated model we forecast 14 days ahead starting from Thursday 1 January 2015 (a non-work-day being a public holiday for New Years Day).
# In this case, we could obtain weather forecasts from the weather bureau for the next 14 days.
# But for the sake of illustration, we will use scenario based forecasting (as introduced in Section 7.6) where we set the temperature for the next 14 days to a constant 26 degrees.

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title="Daily electricity demand: Victoria", y="GW")

# The point forecasts look reasonable for the first two weeks of 2015.
# The slow down in electricity demand at the end of 2014 (due to many people taking summer vacations) has caused the forecasts for the next two weeks to show similarly low demand values.


# _----


#* 10.4 Stochastic and deterministic trends --------------------------------
# There are two different ways of modelling a linear trend.
#   A deterministic trend is obtained using the regression model
#     y[t] = β[0] + β[1]t + η[t],     where η[t] is an ARMA process.
#   A stochastic trend is obtained using the model
#     y[t] = β[0] + β[1]t + η[t],     where η[t] is an ARIMA process with d=1.
# In the latter case, we can difference both sides so that
#   y′[t] = β[1] + η′[t],   where η′[t] is an ARMA process.
# In other words,
#   y[t] = y[t−1] + β[1] + η′[t].

# This is similar to a random walk with drift (introduced in Section 9.1), but here the error term is an ARMA process rather than simply white noise.

# Although these models appear quite similar (they only differ in the number of differences that need to be applied to η[t]), their forecasting characteristics are quite different.


#** Example: Air transport passengers Australia ----
aus_airpassengers %>%
  autoplot(Passengers) +
  labs(y = "Passengers (millions)", title = "Total annual air passengers")

# Figure above shows the total number of passengers for Australian air carriers each year from 1970 to 2016.
# We will fit both a deterministic and a stochastic trend model to these data.

# The deterministic trend model is obtained as follows:
fit_deterministic <- aus_airpassengers %>%
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))

report(fit_deterministic)

# This model can be written as
#   y[t] = 0.901 + 1.415t + η[t]
#   η[t] = 0.956η[t−1] + ε[t]
#   ε[t]∼NID(0,4.343).

# The estimated growth in visitor numbers is 1.42 million people per year.

# Alternatively, the stochastic trend model can be estimated.
fit_stochastic <- aus_airpassengers %>%
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))

report(fit_stochastic)

# This model can be written as
#   y[t] − y[t−1] = 1.419 + ε[t], or equivalently
#   y[t] = y[0] + 1.419t + η[t]
#   η[t] = η[t−1] + ε[t]
#   ε[t]∼NID(0,4.271).

# In this case, the estimated growth in visitor numbers is also 1.42 million people per year.
# Although the growth estimates are similar, the prediction intervals are not, as Figure below shows.
# In particular, stochastic trends have much wider prediction intervals because the errors are non-stationary.

aus_airpassengers %>%
  autoplot(Passengers) +
  autolayer(fit_stochastic %>% forecast(h = 20), colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic %>% forecast(h = 20), colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)", title = "Forecasts from trend models")


# There is an implicit assumption with deterministic trends that the slope of the trend is not going to change over time.
# On the other hand, stochastic trends can change, and the estimated growth is only assumed to be the average growth over the historical period, not necessarily the rate of growth that will be observed into the future.
# Consequently, it is safer to forecast with stochastic trends, especially for longer forecast horizons, as the prediction intervals allow for greater uncertainty in future growth.


# _----


#* 10.5 Dynamic harmonic regression ----------------------------------------
# When there are long seasonal periods, a dynamic regression with Fourier terms is often better than other models we have considered in this book.
#   (The term “dynamic harmonic regression” is also used for a harmonic regression with time-varying parameters)

#   example,
#     daily data can have annual seasonality of length 365,
#     weekly data has seasonal period of approximately 52,
#     while half-hourly data can have several seasonal periods, the shortest of which is the daily pattern of period 48.

# Seasonal versions of ARIMA and ETS models are designed for shorter periods such as 12 for monthly data or 4 for quarterly data.
# The ETS() model restricts seasonality to be a maximum period of 24 to allow hourly data but not data with a larger seasonal period.
# The problem is that there are m−1 parameters to be estimated for the initial seasonal states where m is the seasonal period.
# So for large m, the estimation becomes almost impossible.

# The ARIMA() function will allow a seasonal period up to m=350, but in practice will usually run out of memory whenever the seasonal period is more than about 200.
# In any case, seasonal differencing of high order does not make a lot of sense —
#   for daily data it involves comparing what happened today with what happened exactly a year ago and there is no constraint that the seasonal pattern is smooth.

# So for such time series, we prefer a harmonic regression approach where the seasonal pattern is modelled using Fourier terms with short-term time series dynamics handled by an ARMA error.

# The advantages of this approach are:
#   it allows any length seasonality;
#   for data with more than one seasonal period, Fourier terms of different frequencies can be included;
#   the smoothness of the seasonal pattern can be controlled by K, the number of Fourier sin and cos pairs – the seasonal pattern is smoother for smaller values of K;
#   the short-term dynamics are easily handled with a simple ARMA error.

# The only real disadvantage (compared to a seasonal ARIMA model) is that the seasonality is assumed to be fixed — the seasonal pattern is not allowed to change over time.
# But in practice, seasonality is usually remarkably constant so this is not a big disadvantage except for long time series.


#** Example: Australian eating out expenditure ----
# In this example we demonstrate combining Fourier terms for capturing seasonality with ARIMA errors capturing other dynamics in the data.
# For simplicity, we will use an example with monthly data.
#   The same modelling approach using weekly data is discussed in Section 13.1.

# We use the total monthly expenditure on cafes, restaurants and takeaway food services in Australia ($billion) from 2004 up to 2018 and forecast 24 months ahead.
# We vary K, the number of Fourier sin and cos pairs, from K=1 to K=6 (which is equivalent to including seasonal dummies).
# Figure below shows the seasonal pattern projected forward as K increases.
# Notice that as K increases the Fourier terms capture and project a more “wiggly” seasonal pattern and simpler ARIMA models are required to capture other dynamics.
# The AICc value is minimised for K=6, with a significant jump going from K=4 to K=5, hence the forecasts generated from this model would be the ones used.

aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>%
  summarise(Turnover = sum(Turnover))

fit <- model(aus_cafe,
             `K = 1` = ARIMA(log(Turnover) ~ fourier(K=1) + PDQ(0,0,0)),
             `K = 2` = ARIMA(log(Turnover) ~ fourier(K=2) + PDQ(0,0,0)),
             `K = 3` = ARIMA(log(Turnover) ~ fourier(K=3) + PDQ(0,0,0)),
             `K = 4` = ARIMA(log(Turnover) ~ fourier(K=4) + PDQ(0,0,0)),
             `K = 5` = ARIMA(log(Turnover) ~ fourier(K=5) + PDQ(0,0,0)),
             `K = 6` = ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0))
)

fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = FALSE, fill = FALSE, level = FALSE) +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc = ", format(AICc, digits = 3, nsmall = 2))),
    data = glance(fit)
  ) +
  labs(title= "Total monthly eating-out expenditure", y="$ billions")


# _----


#* 10.6 Lagged predictors --------------------------------------------------
# Sometimes, the impact of a predictor that is included in a regression model will not be simple and immediate.
#   example,
#     an advertising campaign may impact sales for some time beyond the end of the campaign, and sales in one month will depend on the advertising expenditure in each of the past few months.
#     Similarly, a change in a company’s safety policy may reduce accidents immediately, but have a diminishing effect over time as employees take less care when they become familiar with the new working conditions.

# In these situations, we need to allow for lagged effects of the predictor.
# Suppose that we have only one predictor in our model.
# Then a model which allows for lagged effects can be written as
#   y[t] = β[0] + γ[0]x[t] + γ[1]x[t−1] + ⋯ + γ[k]x[t−k] + η[t],
#     where η[t] is an ARIMA process.
# The value of k can be selected using the AICc, along with the values of p and q for the ARIMA error.


#** Example: TV advertising and insurance quotations ----
# A US insurance company advertises on national television in an attempt to increase the number of insurance quotations provided (and consequently the number of new policies).
# Figure below shows the number of quotations and the expenditure on television advertising for the company each month from January 2002 to April 2005.

insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")


# We will consider including advertising expenditure for up to four months; that is, the model may include advertising expenditure in the current month, and the three months before that.
# When comparing models, it is important that they all use the same training set.
# In the following code, we exclude the first three months in order to make fair comparisons.

fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2) + lag(TVadverts, 3))
  )

# Next we choose the optimal lag length for advertising based on the AICc.

glance(fit)

# The best model (with the smallest AICc value) is lag1 with two predictors; that is, it includes advertising only in the current month and the previous month.
# So we now re-estimate that model, but using all the available data.

fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)))

report(fit_best)

# The chosen model has ARIMA(1,0,2) errors.
# The model can be written as
#   y[t] = 2.155 + 1.253x[t] + 0.146x[t−1] + η[t],
#     where
#       y[t] is the number of quotations provided in month t,
#       x[t] is the advertising expenditure in month t,
#       η[t] = 0.512η[t−1] + ε[t] + 0.917ε[t−1] + 0.459ε[t−2], and ε[t] is white noise.

# We can calculate forecasts using this model if we assume future values for the advertising variable.
# If we set the future monthly advertising to 8 units, we get the forecasts in Figure below.

insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)

fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )


# ____----