# Chapter 9 ARIMA models --------------------------------------------------
# ARIMA models provide another approach to time series forecasting.
# Exponential smoothing and ARIMA models are the two most widely used approaches to time series forecasting, and provide complementary approaches to the problem.
# While
#   exponential smoothing models are based on a description of the trend and seasonality in the data,
#   ARIMA models aim to describe the autocorrelations in the data.

# Before we introduce ARIMA models, we must first discuss the concept of stationarity and the technique of differencing time series.


# _----


#* 9.1 Stationarity and differencing ---------------------------------------
# A stationary time series is one whose statistical properties do not depend on the time at which the series is observed.
#   More precisely, if {yt} is a stationary time series, then for all s, the distribution of (yt,…,yt+s) does not depend on t.
# Thus, time series with trends, or with seasonality, are not stationary —
#   the trend and seasonality will affect the value of the time series at different times.
# On the other hand, a white noise series is stationary —
#   it does not matter when you observe it, it should look much the same at any point in time.

# Some cases can be confusing —
#   a time series with cyclic behaviour (but with no trend or seasonality) is stationary.
#   This is because the cycles are not of a fixed length, so before we observe the series we cannot be sure where the peaks and troughs of the cycles will be.

# In general, a stationary time series will have no predictable patterns in the long-term.
# Time plots will show the series to be roughly horizontal (although some cyclic behaviour is possible), with constant variance.

a <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  autoplot(Close) +
  labs(x = "Day", y = "$US", title = "(a) Google closing price")

b <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  transmute(change = difference(Close)) %>%
  autoplot(change) +
  labs(x = "Day", y = "$US", title = "(b) Change in Google price")

c <- strikes %>%
  as_tsibble() %>%
  rename(Year = index, Count = value) %>%
  autoplot(Count) +
  labs(x = "Year", y = "Number of strikes", title = "(c) Strikes: US")

d <- hsales %>%
  as_tsibble() %>%
  rename(Month = index, Count = value) %>%
  autoplot(Count) +
  labs(x = "Month", y = "Number of houses", title = "(d) House sales: US")

e <- eggs %>%
  as_tsibble() %>%
  rename(Year = index, Price = value) %>%
  autoplot(Price) +
  labs(x = "Year", y = "$US (constant prices)", title = "(e) Egg prices: US")

f <- aus_livestock %>%
  filter(Animal == "Pigs", State == "Victoria") %>%
  autoplot(Count) +
  labs(x = "Month", y = "Number of pigs", title = "(f) Pigs slaughtered: Victoria, Australia")

g <- lynx %>%
  as_tsibble() %>%
  rename(Year = index, Count = value) %>%
  filter(Year > 1840) %>%
  autoplot(Count) +
  labs(x = "Year", y = "Number of lynx", title = "(g) Lynx trapped: Canada")

h <- aus_production %>%
  filter(year(Quarter) %in% c(1991:1995)) %>%
  autoplot(Beer) +
  labs(x = "Quarter", y = "Megalitres", title = "(h) Beer production: Australia")

i <- aus_production %>%
  autoplot(Gas) +
  labs(x = "Quarter", y = "Petajoules", title = "(i) Gas production: Australia")

(a + b + c) / (d + e + f) / (g + h + i)


# Consider the nine series plotted in Figure above.
# Which of these do you think are stationary?
#   Obvious seasonality rules out series (d), (h) and (i).
#   Trends and changing levels rules out series (a), (c), (e), (f) and (i).
#   Increasing variance also rules out (i).
#   That leaves only (b) and (g) as stationary series.

# At first glance, the strong cycles in series (g) might appear to make it non-stationary.
# But these cycles are aperiodic —
#   they are caused when the lynx population becomes too large for the available feed, so that they stop breeding and the population falls to low numbers, then the regeneration of their food sources allows the population to grow again, and so on.
# In the long-term, the timing of these cycles is not predictable.
#   Hence the series is stationary.


#** Differencing ----
# In Figure above, note that the Google stock price was non-stationary in panel (a), but the daily changes were stationary in panel (b).

# differencing
#   compute the differences between consecutive observations.
# This shows one way to make a non-stationary time series stationary.
   
# Transformations such as logarithms can help to stabilise the variance of a time series.
# Differencing can help stabilise the mean of a time series by removing changes in the level of a time series, and therefore eliminating (or reducing) trend and seasonality.

# As well as the time plot of the data, the ACF plot is also useful for identifying non-stationary time series.
# For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly.
# Also, for non-stationary data, the value of r[1] is often large and positive.

p1 <- google_2015 %>%
  ACF(Close) %>%
  autoplot() +
  labs(title = "Google closing stock price")

p2 <- google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  ACF(diff_close) %>%
  autoplot() +
  labs(title = "Changes in Google closing price")

p1 + p2


google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10)

# The ACF of the differenced Google stock price looks just like that of a white noise series.
# Only one autocorrelation is outside of the 95% limits, and the Ljung-Box Q∗ statistic has a p-value of 0.637 (for h=10).
# This suggests that the daily change in the Google stock price is essentially a random amount which is uncorrelated with that of previous days.


#** Random walk model ----
# The differenced series is the change between consecutive observations in the original series, and can be written as 
#   y′[t] = y[t] − y[t−1].
# The differenced series will have only T−1 values, since it is not possible to calculate a difference y′[1] for the first observation.

# When the differenced series is white noise, the model for the original series can be written as
#   y[t] − y[t−1] = ε[t],
#     where ε[t] denotes white noise.
# Rearranging this leads to the “random walk” model
#   y[t] = y[t−1] + ε[t].

# Random walk models are widely used for non-stationary data, particularly financial and economic data.
# Random walks typically have:
#   long periods of apparent trends up or down
#   sudden and unpredictable changes in direction.

# The forecasts from a random walk model are equal to the last observation, as future movements are unpredictable, and are equally likely to be up or down.
# Thus, the random walk model underpins naïve forecasts, first introduced in Section 5.2.

# A closely related model allows the differences to have a non-zero mean.
# Then
#   y[t] − y[t−1] = c + ε[t]    or    y[t] = c + y[t−1] + ε[t].
# The value of c is the average of the changes between consecutive observations.
#   If c is positive, then the average change is an increase in the value of y[t].
#     Thus, y[t] will tend to drift upwards.
#   However, if c is negative, y[t] will tend to drift downwards.

# This is the model behind the Drift method, also discussed in Section 5.2.


#** Second-order differencing ----
# Occasionally the differenced data will not appear to be stationary and it may be necessary to difference the data a second time to obtain a stationary series:
#   y′′[t]  = y′[t] − y′[t−1]
#           = (y[t] − y[t−1]) − (y[t−1] − y[t−2])
#           = y[t] − 2y[t−1] + y[t−2].
# In this case, y′′[t] will have T−2 values.
# Then, we would model the “change in the changes” of the original data.
# In practice, it is almost never necessary to go beyond second-order differences.


#** Seasonal differencing ----
# A seasonal difference is the difference between an observation and the previous observation from the same season. 
# So
#   y′[t] = y[t] − y[t−m],
#     where m = the number of seasons.
# These are also called “lag-m differences,” as we subtract the observation after a lag of m periods.

# If seasonally differenced data appear to be white noise, then an appropriate model for the original data is
#   y[t] = y[t−m] + ε[t].

# Forecasts from this model are equal to the last observation from the relevant season.
#   That is, this model gives seasonal naïve forecasts, introduced in Section 5.2.

# The bottom panel in Figure below shows the seasonal differences of the logarithm of the monthly scripts for A10 (antidiabetic) drugs sold in Australia.
# The transformation and differencing have made the series look relatively stationary.

# Figure 9.3
PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12)
  ) %>%
  pivot_longer(-Month, names_to = "Type", values_to = "Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales"
      )
    )
  ) %>%
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Antidiabetic drug sales", y = NULL)


# To distinguish seasonal differences from ordinary differences, we sometimes refer to ordinary differences as “first differences,” meaning differences at lag 1.

# Sometimes it is necessary to take both a seasonal difference and a first difference to obtain stationary data. 
# Figure below plots Australian corticosteroid drug sales ($AUD) (top panel).
# Here, the data are first transformed using logarithms (second panel), then seasonal differences are calculated (third panel).
# The data still seem somewhat non-stationary, and so a further lot of first differences are computed (bottom panel).

# Figure 9.4
PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` = difference(difference(log(Cost), 12), 1)
  ) %>%
  pivot_longer(-Month, names_to = "Type", values_to = "Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"
      )
    )
  ) %>%
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)


# There is a degree of subjectivity in selecting which differences to apply.
# The seasonally differenced data in Figure 9.3 do not show substantially different behaviour from the seasonally differenced data in Figure 9.4.
# In the latter case, we could have decided to stop with the seasonally differenced data, and not done an extra round of differencing.
# In the former case, we could have decided that the data were not sufficiently stationary and taken an extra round of differencing.
# Some formal tests for differencing are discussed below, but there are always some choices to be made in the modelling process, and different analysts may make different choices.

# If y′[t] = y[t] − y[t−m] denotes a seasonally differenced series, then the twice-differenced series is
#   y′′[t]  = y′[t] − y′[t−1]
#           = (y[t] − y[t−m]) − (y[t−1] − y[t−m−1])
#           = y[t] − y[t−1] − y[t−m] + y[t−m−1]

# When both seasonal and first differences are applied, it makes no difference which is done first—the result will be the same.
# However, if the data have a strong seasonal pattern, we recommend that seasonal differencing be done first, because the resulting series will sometimes be stationary and there will be no need for a further first difference.
# If first differencing is done first, there will still be seasonality present.

# Beware that applying more differences than required will induce false dynamics or autocorrelations that do not really exist in the time series.
# Therefore, do as few differences as necessary to obtain a stationary series.

# It is important that if differencing is used, the differences are interpretable.
#   First differences are the change between one observation and the next.
#   Seasonal differences are the change between one year to the next.
# Other lags are unlikely to make much interpretable sense and should be avoided.


#** Unit root tests ----
# One way to determine more objectively whether differencing is required is to use a unit root test.
# These are statistical hypothesis tests of stationarity that are designed for determining whether differencing is required.

# A number of unit root tests are available, which are based on different assumptions and may lead to conflicting answers.
# In our analysis, we use the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test (Kwiatkowski et al., 1992).
# In this test, the null hypothesis is that the data are stationary, and we look for evidence that the null hypothesis is false.
# Consequently, small p-values (e.g., less than 0.05) suggest that differencing is required.
# unitroot_kpss() -
#   The test can be computed.

# For example, let us apply it to the Google stock price data.
google_2015 %>%
  features(Close, unitroot_kpss)
unitroot_kpss(google_2015$Close)
# The p-value is reported as 0.01 if it is less than 0.01, and as 0.1 if it is greater than 0.1.
# In this case, the test statistic (3.56) is bigger than the 1% critical value, so the p-value is less than 0.01, indicating that the null hypothesis is rejected.
# That is, the data are not stationary.
# We can difference the data, and apply the test again.

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)
# This time, the test statistic is tiny, and well within the range we would expect for stationary data, so the p-value is greater than 0.1.
# We can conclude that the differenced data appear stationary.

# unitroot_ndiffs()
#   This process of using a sequence of KPSS tests to determine the appropriate number of first differences is carried out.
google_2015 %>%
  features(Close, unitroot_ndiffs)

# As we saw from the KPSS tests above, one difference is required to make the google_2015 data stationary.

# unitroot_nsdiffs() -
#   A similar feature for determining whether seasonal differencing is required is unitroot_nsdiffs(),
#     which uses the measure of seasonal strength introduced in Section 4.3 to determine the appropriate number of seasonal differences required.
#   No seasonal differences are suggested if FS<0.64, otherwise one seasonal difference is suggested.

# We can apply unitroot_nsdiffs() to the monthly total Australian retail turnover.
aus_total_retail <- aus_retail %>%
  summarise(Turnover = sum(Turnover))

aus_total_retail %>%
  mutate(log_turnover = log(Turnover)) %>%
  features(log_turnover, unitroot_nsdiffs)


aus_total_retail %>%
  mutate(log_turnover = difference(log(Turnover), 12)) %>%
  features(log_turnover, unitroot_ndiffs)
# Because unitroot_nsdiffs() returns 1 (indicating one seasonal difference is required), we apply the unitroot_ndiffs() function to the seasonally differenced data.
# These functions suggest we should do both a seasonal difference and a first difference.


# _----


#* 9.2 Backshift notation --------------------------------------------------
# The backward shift operator B is a useful notational device when working with time series lags:
#   By[t] = y[t−1].
#   (Some references use L for “lag” instead of B for “backshift.”)
# In other words, B, operating on y[t], has the effect of shifting the data back one period.
# Two applications of B to y[t] shifts the data back two periods:
#   B(By[t]) = B[^2]y[t] = y[t−2].
# For monthly data, if we wish to consider “the same month last year,” the notation is
#   B[^12]y[t] = y[t−12].

# The backward shift operator is convenient for describing the process of differencing.
# A first difference can be written as
#   y′[t] = y[t] − y[t−1] = y[t] − By[t] = (1−B)y[t].
# So a first difference can be represented by (1−B).
# Similarly, if second-order differences have to be computed, then:
#   y′′[t] = y[t] − 2y[t−1] + y[t−2] = (1 − 2B + B[^2])y[t] = (1−B)[^2]y[t].
# In general, a dth-order difference can be written as
#   (1−B)[^d]y[t].

# Backshift notation is particularly useful when combining differences, as the operator can be treated using ordinary algebraic rules.
# In particular, terms involving B can be multiplied together.

# For example, a seasonal difference followed by a first difference can be written as
#   (1−B)(1−B[^m])y[t]  = (1 − B − B[^m] + B[^m+1])y[t]
#                       = y[t] − y[t−1] − y[t−m] + y[t−m−1],
# the same result we obtained earlier.


# _----


#* 9.3 Autoregressive models -----------------------------------------------
# In a multiple regression model, introduced in Chapter 7, we forecast the variable of interest using a linear combination of predictors.
# In an autoregression model, we forecast the variable of interest using a linear combination of past values of the variable.
# The term autoregression indicates that it is a regression of the variable against itself.

# Thus, an autoregressive model of order p can be written as
#   y[t] = c + ϕ[1]y[t−1] + ϕ[2]y[t−2] + ⋯ + ϕ[p]y[t−p] + ε[t], 
#     where ε[t] is white noise.
# This is like a multiple regression but with lagged values of yt as predictors.

# We refer to this as an AR(p) model,
#   an autoregressive model of order p.

# Autoregressive models are remarkably flexible at handling a wide range of different time series patterns.
#   The two series in Figure 9.5 show series from an AR(1) model and an AR(2) model.
# Changing the parameters ϕ1,…,ϕp results in different time series patterns.
# The variance of the error term εt will only change the scale of the series, not the patterns.

# For an AR(1) model:
#   when ϕ1=0 and c=0, yt is equivalent to white noise;
#   when ϕ1=1 and c=0, yt is equivalent to a random walk;
#   when ϕ1=1 and c≠0, yt is equivalent to a random walk with drift;
#   when ϕ1<0, yt tends to oscillate around the mean.

# We normally restrict autoregressive models to stationary data, in which case some constraints on the values of the parameters are required.
#   For an AR(1) model: −1<ϕ1<1.
#   For an AR(2) model: −1<ϕ2<1, ϕ1+ϕ2<1, ϕ2−ϕ1<1.

# When p≥3, the restrictions are much more complicated.
# The fable package takes care of these restrictions when estimating a model.


# _----


#* 9.4 Moving average models -----------------------------------------------
# Rather than using past values of the forecast variable in a regression, a moving average model uses past forecast errors in a regression-like model,
#   y[t] = c + ε[t] + θ[1]ε[t−1] + θ[2]ε[t−2] + ⋯ + θ[q]ε[t−q],
#     where ε[t] is white noise.

# We refer to this as an MA(q) model,
#   a moving average model of order q.

# Of course, we do not observe the values of ε[t], so it is not really a regression in the usual sense.

# Notice that each value of y[t] can be thought of as a weighted moving average of the past few forecast errors (although the coefficients will not normally sum to one).
# However, moving average models should not be confused with the moving average smoothing we discussed in Chapter 3. #   A moving average model is used for forecasting future values, while 
#   moving average smoothing is used for estimating the trend-cycle of past values.

# Figure 9.6 shows some data from an MA(1) model and an MA(2) model.
# Changing the parameters θ1,…,θq results in different time series patterns.
# As with autoregressive models, the variance of the error term ε[t] will only change the scale of the series, not the patterns.

# It is possible to write any stationary AR(p) model as an MA(∞) model.
#   example, using repeated substitution, we can demonstrate this for an AR(1) model:
#     y[t]  = ϕ[1]y[t−1] + ε[t]
#           = ϕ[1](ϕ[1]y[t−2] + ε[t−1]) + ε[t]
#           = ϕ[^2][1]y[t−2] + ϕ[1]ε[t−1] + ε[t]
#           = ϕ[^3][1]y[t−3] + ϕ[^2][1]ε[t−2] + ϕ[1]ε[t−1] + ε[t]
#           etc.
# Provided −1<ϕ[1]<1, the value of ϕ[^k][1] will get smaller as k gets larger.
# So eventually we obtain
#   y[t] = ε[t] + ϕ[1]ε[t−1] + ϕ[^2][1]ε[t−2] + ϕ[^3][1]ε[t−3] + ⋯, an MA(∞) process.

# The reverse result holds if we impose some constraints on the MA parameters.

# Then the MA model is called invertible.
#   That is, we can write any invertible MA(q) process as an AR(∞) process.

# Invertible models are not simply introduced to enable us to convert from MA models to AR models.
# They also have some desirable mathematical properties.

#   example, consider the MA(1) process, y[t] = ε[t] + θ[1]ε[t−1].
#   In its AR(∞) representation, the most recent error can be written as a linear function of current and past observations: 
#     ε[t] = [∞∑j=0](−θ[1])[^j]y[t−j].
#       When |θ1|>1, the weights increase as lags increase, so the more distant the observations the greater their influence on the current error.
#       When |θ1|=1, the weights are constant in size, and the distant observations have the same influence as the recent observations.
#       As neither of these situations make much sense, we require |θ1|<1, so the most recent observations have higher weight than observations from the more distant past.
#       Thus, the process is invertible when |θ1|<1.

# The invertibility constraints for other models are similar to the stationarity constraints.
#   For an MA(1) model: −1<θ1<1.
#   For an MA(2) model: −1<θ2<1, θ2+θ1>−1,  θ1−θ2<1.

# More complicated conditions hold for q≥3.
#   Again, the fable package will take care of these constraints when estimating the models.


# _----


#* 9.5 Non-seasonal ARIMA models -------------------------------------------
# If we combine differencing with autoregression and a moving average model, we obtain a non-seasonal ARIMA model. 

# ARIMA is an acronym for
#   AutoRegressive Integrated Moving Average
#   (in this context, “integration” is the reverse of differencing).

# The full model can be written as
#   y′[t] = c + ϕ[1]y′[t−1] + ⋯ + ϕ[p]y′[t−p] + θ[1]ε[t−1] + ⋯+ θ[q]ε[t−q] + ε[t],        (9.1)
#     where y′[t] is the differenced series (it may have been differenced more than once).
# The “predictors” on the right hand side include both lagged values of y[t] and lagged errors.
# We call this an ARIMA(p,d,q) model, where
#   p=  order of the autoregressive part;
#   d=  degree of first differencing involved;
#   q=  order of the moving average part. 

# The same stationarity and invertibility conditions that are used for autoregressive and moving average models also apply to an ARIMA model.

# Many of the models we have already discussed are special cases of the ARIMA model, as shown in Table 9.1.
#   Table 9.1: Special cases of ARIMA models.
#     White noise 	              ARIMA(0,0,0) with no constant
#     Random walk 	              ARIMA(0,1,0) with no constant
#     Random walk with drift 	    ARIMA(0,1,0) with a constant
#     Autoregression 	            ARIMA(p,0,0)
#     Moving average 	            ARIMA(0,0,q)

# Once we start combining components in this way to form more complicated models, it is much easier to work with the backshift notation.
#   example, Equation (9.1) can be written in backshift notation as 
#     (1 − ϕ[1]B − ⋯ − ϕ[p]B[p]) (1−B)[^d]y[t] = c + (1 + θ[1]B + ⋯ + θ[q]B[q])ε[t]
#             ↑AR(p)               ↑d differences            ↑MA(q)

# Selecting appropriate values for p, d and q can be difficult.
#   However, the ARIMA() function from the fable package will do it for you automatically.
# In Section 9.7, we will learn how this function works, along with some methods for choosing these values yourself.


#** Egyptian exports ----
# Figure below shows Egyptian exports as a percentage of GDP from 1960 to 2017.
global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

# The following R code selects a non-seasonal ARIMA model automatically.
fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))

report(fit)


# This is an ARIMA(2,0,1) model:
#   y[t] = 2.56 + 1.68y[t−1] − 0.80y[t−2] − 0.69ε[t−1] + ε[t],
#     where ε[t] is white noise with a standard deviation of 2.837=√8.046.
# Forecasts from the model are shown in Figure below.
# Notice how they have picked up the cycles evident in the Egyptian economy over the last few decades.
fit %>% forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")


#** Understanding ARIMA models ----
# The ARIMA() function is useful, but anything automated can be a little dangerous, and it is worth understanding something of the behaviour of the models even when you rely on an automatic procedure to choose the model for you.

# The constant c has an important effect on the long-term forecasts obtained from these models.
#   If c=0 and d=0, the long-term forecasts will go to zero.
#   If c=0 and d=1, the long-term forecasts will go to a non-zero constant.
#   If c=0 and d=2, the long-term forecasts will follow a straight line.
#   If c≠0 and d=0, the long-term forecasts will go to the mean of the data.
#   If c≠0 and d=1, the long-term forecasts will follow a straight line.
#   If c≠0 and d=2, the long-term forecasts will follow a quadratic trend. (This is not recommended, and fable will not permit it.)

# The value of d also has an effect on the prediction intervals — the higher the value of d, the more rapidly the prediction intervals increase in size.
# For d=0, the long-term forecast standard deviation will go to the standard deviation of the historical data, so the prediction intervals will all be essentially the same.

#   This behaviour is seen in Figure above where d=0 and c≠0.
#   In this figure, the prediction intervals are almost the same width for the last few forecast horizons, and the final point forecasts are close to the mean of the data.

# The value of p is important if the data show cycles.
# To obtain cyclic forecasts, it is necessary to have p≥2, along with some additional conditions on the parameters.
# For an AR(2) model, cyclic behaviour occurs if ϕ[^2][1] + 4ϕ[2] <0 (as is the case for the Egyptian Exports model).
# In that case, the average period of the cycles is
#   2π / ( arc cos(−ϕ[1](1 − ϕ[2]) / (4ϕ[2])) )
#     arc cos is the inverse cosine function. (also labelled acos or cos[^−1])


#** ACF and PACF plots ----
# It is usually not possible to tell, simply from a time plot, what values of p and q are appropriate for the data. 
# However, it is sometimes possible to use the ACF plot, and the closely related PACF plot, to determine appropriate values for p and q.

# Recall that an ACF plot shows the autocorrelations which measure the relationship between y[t] and y[t−k] for different values of k.
# Now if y[t] and y[t−1] are correlated, then y[t−1] and y[t−2] must also be correlated.
# However, then y[t] and y[t−2] might be correlated, simply because they are both connected to y[t−1], rather than because of any new information contained in y[t−2] that could be used in forecasting y[t].

# To overcome this problem, we can use partial autocorrelations.
# These measure the relationship between y[t] and y[t−k] after removing the effects of lags 1,2,3,…,k−1.
# So the first partial autocorrelation is identical to the first autocorrelation, because there is nothing between them to remove.
# Each partial autocorrelation can be estimated as the last coefficient in an autoregressive model.
# Specifically, αk, the kth partial autocorrelation coefficient, is equal to the estimate of ϕ[k] in an AR(k) model. # In practice, there are more efficient algorithms for computing α[k] than fitting all of these autoregressions, but they give the same results.

# Figures below and below2 shows the ACF and PACF plots for the Egyptian exports data shown in Figure above.
# The partial autocorrelations have the same critical values of ±1.96/√T as for ordinary autocorrelations, and these are typically shown on the plot as in Figure below2.

global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot()

global_economy %>%
  filter(Code == "EGY") %>%
  PACF(Exports) %>%
  autoplot()

# A convenient way to produce a time plot, ACF plot and PACF plot in one command is to use the gg_tsdisplay() function with plot_type = "partial".

# If the data are from an ARIMA(p,d,0) or ARIMA(0,d,q) model, then the ACF and PACF plots can be helpful in determining the value of p or q.
# If p and q are both positive, then the plots do not help in finding suitable values of p and q.

# The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:
#   the ACF is exponentially decaying or sinusoidal;
#   there is a significant spike at lag p in the PACF, but none beyond lag p.

# The data may follow an ARIMA(0,d,q) model if the ACF and PACF plots of the differenced data show the following patterns:
#   the PACF is exponentially decaying or sinusoidal;
#   there is a significant spike at lag q in the ACF, but none beyond lag q.

# In Figure above2, we see that there is a decaying sinusoidal pattern in the ACF, and in Figure above the PACF shows the last significant spike at lag 4.
# This is what you would expect from an ARIMA(4,0,0) model.

fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))

report(fit2)

# This model is only slightly worse than the ARIMA(2,0,1) model identified by ARIMA() (with an AICc value of 294.70 compared to 294.29).

# We can also specify particular values of pdq() that ARIMA() can search for.
#   example, to find the best ARIMA model with p∈{1,2,3}, q∈{0,1,2} and d=1, you could use ARIMA(y ~ pdq(p=1:3, d=1, q=0:2)).


# _----


#* 9.6 Estimation and order selection --------------------------------------
#** Maximum likelihood estimation ----
# Once the model order has been identified (i.e., the values of p, d and q), we need to estimate the parameters c, ϕ1,…,ϕp, θ1,…,θq.

# When fable estimates the ARIMA model, it uses maximum likelihood estimation (MLE).
#   This technique finds the values of the parameters which maximise the probability of obtaining the data that we have observed.

# For ARIMA models, MLE is similar to the least squares estimates that would be obtained by minimising
#   [T∑t=1]ε[^2][t].
# (For the regression models considered in Chapter 7, MLE gives exactly the same parameter estimates as least squares estimation.)
# Note that ARIMA models are much more complicated to estimate than regression models, and different software will give slightly different answers as they use different methods of estimation, and different optimisation algorithms.

# In practice, the fable package will report the value of the log likelihood of the data;
#   that is, the logarithm of the probability of the observed data coming from the estimated model.
# For given values of p, d and q, ARIMA() will try to maximise the log likelihood when finding parameter estimates.


#** Information Criteria ----
# Akaike’s Information Criterion (AIC), which was useful in selecting predictors for regression (see Section 7.5), is also useful for determining the order of an ARIMA model.
# It can be written as
#   AIC = −2log(L) + 2(p+q+k+1),
#     where
#       L is the likelihood of the data,
#       k=1 if c≠0      and     k=0 if c=0.
#   Note that the last term in parentheses is the number of parameters in the model (including σ[^2], the variance of the residuals).

# For ARIMA models, the corrected AIC can be written as
#   AICc = AIC + 2(p+q+k+1)(p+q+k+2) / (T−p−q−k−2),       and
# the Bayesian Information Criterion can be written as
#   BIC = AIC + [log(T) − 2](p+q+k+1).

# Good models are obtained by minimising the AIC, AICc or BIC.
#   Our preference is to use the AICc.

# It is important to note that these information criteria tend not to be good guides to selecting the appropriate order of differencing (d) of a model, but only for selecting the values of p and q.
# This is because the differencing changes the data on which the likelihood is computed, making the AIC values between models with different orders of differencing not comparable.
# So we need to use some other approach to choose d, and then we can use the AICc to select p and q.


# _----


#* 9.7 ARIMA modelling in fable --------------------------------------------
#** How does ARIMA() work? ----
# The ARIMA() function in the fable package uses a variation of the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008), which combines unit root tests, minimisation of the AICc and MLE to obtain an ARIMA model.
# The arguments to ARIMA() provide for many variations on the algorithm.
# What is described here is the default behaviour.

# Hyndman-Khandakar algorithm for automatic ARIMA modelling
# 1. The number of differences 0≤d≤2 is determined using repeated KPSS tests.
# 2. The values of p and q are then chosen by minimising the AICc after differencing the data d times.
#   Rather than considering every possible combination of p and q, the algorithm uses a stepwise search to traverse the model space.
#   2.a. Four initial models are fitted:
#         ARIMA(0,d,0),
#         ARIMA(2,d,2),
#         ARIMA(1,d,0),
#         ARIMA(0,d,1).
#     A constant is included unless d=2.
#     If d≤1, an additional model is also fitted:
#         ARIMA(0,d,0) without a constant.
#   2.b. The best model (with the smallest AICc value) fitted in step (a) is set to be the “current model.”
#   2.c. Variations on the current model are considered:
#         vary p and/or q from the current model by ±1;
#         include/exclude c from the current model.
#       The best model considered so far (either the current model or one of these variations) becomes the new current model.
#   2.d. Repeat Step 2(c) until no lower AICc can be found.

# Figure 9.11: An illustrative example of the Hyndman-Khandakar stepwise search process (Image) illustrates diagrammatically how the Hyndman-Khandakar algorithm traverses the space of the ARMA orders, through an example.
# The grid covers combinations of ARMA(p,q) orders starting from the top-left corner with an ARMA(0,0), with the AR order increasing down the vertical axis, and the MA order increasing across the horizontal axis.

# The orange cells show the initial set of models considered by the algorithm.
# In this example, the ARMA(2,2) model has the lowest AICc value amongst these models.
#   This is called the “current model” and is shown by the black circle.
# The algorithm then searches over neighbouring models as shown by the blue arrows.
# If a better model is found then this becomes the new “current model.”
#   In this example, the new “current model” is the ARMA(3,3) model.
# The algorithm continues in this fashion until no better model can be found.
#   In this example the model returned is an ARMA(4,2) model.

# The default procedure will switch to a new “current model” as soon as a better model is identified, without going through all the neighbouring models.
#   The full neighbourhood search is done when greedy=FALSE.

# The default procedure also uses some approximations to speed up the search.
#   These approximations can be avoided with the argument approximation=FALSE.
# It is possible that the minimum AICc model will not be found due to these approximations, or because of the use of the stepwise procedure.
#   A much larger set of models will be searched if the argument stepwise=FALSE is used.
# See the help file for a full description of the arguments.


#** Modelling procedure ----
# When fitting an ARIMA model to a set of (non-seasonal) time series data, the following procedure provides a useful general approach.

#   1. Plot the data and identify any unusual observations.
#   2. If necessary, transform the data (using a Box-Cox transformation) to stabilise the variance.
#   3. If the data are non-stationary, take first differences of the data until the data are stationary.
#   4. Examine the ACF/PACF: Is an ARIMA(p,d,0) or ARIMA(0,d,q) model appropriate?
#   5. Try your chosen model(s), and use the AICc to search for a better model.
#   6. Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals.
#     If they do not look like white noise, try a modified model.
#   7. Once the residuals look like white noise, calculate forecasts.

# The Hyndman-Khandakar algorithm only takes care of steps 3–5.
# So even if you use it, you will still need to take care of the other steps yourself.

# The process is summarised in Figure 9.12: (General process for forecasting using an ARIMA model) (Image).


#** Example: Central African Republic exports ----
# We will apply this procedure to the exports of the Central African Republic
global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title = "Central African Republic exports", y="% of GDP")

# 1. The time plot shows some non-stationarity, with an overall decline.
#   The improvement in 1994 was due to a new government which overthrew the military junta and had some initial success, before unrest caused further economic decline.
# 2. There is no evidence of changing variance, so we will not do a Box-Cox transformation.
# 3. To address the non-stationarity, we will take a first difference of the data.

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type = 'partial')

# These now appear to be stationary.

# 4. The PACF shown in Figure above is suggestive of an AR(2) model; so an initial candidate model is an ARIMA(2,1,0).
#   The ACF suggests an MA(3) model; so an alternative candidate is an ARIMA(0,1,3).
# 5. We fit both an ARIMA(2,1,0) and an ARIMA(0,1,3) model along with two automated model selections, one using the default stepwise procedure, and one working harder to search a larger model space.

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(
    arima210 = ARIMA(Exports ~ pdq(2,1,0)),
    arima013 = ARIMA(Exports ~ pdq(0,1,3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise=FALSE)
  )

caf_fit %>% pivot_longer(!Country, names_to = "Model name", values_to = "Orders")

glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)

# The four models have almost identical AICc values.
# Of the models fitted, the full search has found that an ARIMA(3,1,0) gives the lowest AICc value, closely followed by the ARIMA(2,1,0) and ARIMA(0,1,3) — the latter two being the models that we guessed from the ACF and PACF plots.
# The automated stepwise selection has identified an ARIMA(2,1,2) model, which has the highest AICc value of the four models.

# 6. The ACF plot of the residuals from the ARIMA(3,1,0) model shows that all autocorrelations are within the threshold limits, indicating that the residuals are behaving like white noise.

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

# A portmanteau test returns a large p-value, also suggesting that the residuals are white noise.

augment(caf_fit) %>%
  filter(.model == 'search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

# 7. Forecasts from the chosen model are shown in Figure below
caf_fit %>%
  forecast(h = 5) %>%
  filter(.model == 'search') %>%
  autoplot(global_economy)

# Note that the mean forecasts look very similar to what we would get with a random walk (equivalent to an ARIMA(0,1,0)).
# The extra work to include AR and MA terms has made little difference to the point forecasts in this example, although the prediction intervals are much narrower than for a random walk model.


#** Understanding constants in R ----
# A non-seasonal ARIMA model can be written as
#   (1 − ϕ[1]B − ⋯ − ϕ[p]B[^p])(1 − B)[^d]y[t] = c + (1 + θ[1]B + ⋯ + θ[q]B[^q])ε[t],                 (9.3)
# or equivalently as
#   (1 − ϕ[1]B − ⋯ − ϕ[p]B[^p])(1 − B)[^d](y[t] − μt[^d]/d!) = (1 + θ[1]B + ⋯ + θ[q]B[^q])ε[t],       (9.4)
#     where
#       c = μ(1 − ϕ[1] − ⋯ − ϕ[p]) and
#       μ is the mean of (1 − B)[^d]y[t].
# The fable package uses the parameterisation of Equation (9.3) and most other R implementations use Equation (9.4).

# Thus, the inclusion of a constant in a non-stationary ARIMA model is equivalent to inducing a polynomial trend of order d in the forecasts.
#   (If the constant is omitted, the forecasts include a polynomial trend of order d−1.)
# When d=0, we have the special case that μ is the mean of y[t].

# By default, the ARIMA() function will automatically determine if a constant should be included.
# For d=0 or d=1, a constant will be included if it improves the AICc value.
# If d>1 the constant is always omitted as a quadratic or higher order trend is particularly dangerous when forecasting.

# The constant can be specified by including 0 or 1 in the model formula (like the intercept in lm()).
#     example,
#       to automatically select an ARIMA model with a constant, you could use ARIMA(y ~ 1 + ...).
#       Similarly, a constant can be excluded with ARIMA(y ~ 0 + ...).


#** Plotting the characteristic roots ----
# (This is a more advanced section and can be skipped if desired.)

# We can re-write Equation (9.3) as
#   ϕ(B)(1 − B)[^d]y[t] = c + θ(B)ε[t]
#     where
#       ϕ(B) = (1 − ϕ[1]B − ⋯ − ϕ[p]B[^p]) is a pth order polynomial in B and 
#       θ(B) = (1 + θ[1]B + ⋯ + θ[q]B[^q]) is a qth order polynomial in B.

# The stationarity conditions for the model are that the p complex roots of ϕ(B) lie outside the unit circle, and 
#   the invertibility conditions are that the q complex roots of θ(B) lie outside the unit circle.
# So we can see whether the model is close to invertibility or stationarity by a plot of the roots in relation to the complex unit circle.

# It is easier to plot the inverse roots instead, as they should all lie within the unit circle.
# This is easily done in R.
#   For the ARIMA(3,1,0) model fitted to the Central African Republic Exports, we obtain Figure below.

gg_arma(caf_fit %>% select(Country, search))

# The three orange dots in the plot correspond to the roots of the polynomials ϕ(B).
# They are all inside the unit circle, as we would expect because fable ensures the fitted model is both stationary and invertible.
#   Any roots close to the unit circle may be numerically unstable, and the corresponding model will not be good for forecasting.

# The ARIMA() function will never return a model with inverse roots outside the unit circle.
# Models automatically selected by the ARIMA() function will not contain roots close to the unit circle either. 
# Consequently, it is sometimes possible to find a model with better AICc value than ARIMA() will return, but such models will be potentially problematic.


# _----


#* 9.8 Forecasting ---------------------------------------------------------
#** Point forecasts ----
# Although we have calculated forecasts from the ARIMA models in our examples, we have not yet explained how they are obtained.
# Point forecasts can be calculated using the following three steps.
#   1. Expand the ARIMA equation so that y[t] is on the left hand side and all other terms are on the right.
#   2. Rewrite the equation by replacing t with T+h .
#   3. On the right hand side of the equation, replace future observations with their forecasts, future errors with zero, and past errors with the corresponding residuals.

# Beginning with h=1, these steps are then repeated for h=2,3,… until all forecasts have been calculated.

# The procedure is most easily understood via an example.
# We will illustrate it using a ARIMA(3,1,1) model which can be written as follows:
#   (1 − ^ϕ[1]B − ^ϕ[2]B[^2] − ^ϕ[3]B[^3])(1 − B)y[t] = (1 + ^θ[1]B)ε[t].

# Then we expand the left hand side to obtain
#   [1 − (1 + ^ϕ[1])B + (^ϕ[1] − ^ϕ[2])B[^2] + (^ϕ[2] − ^ϕ[3])B[^3] + ^ϕ[3]B[^4]]y[t] = (1 + ^θ[1]B)ε[t],

# and applying the backshift operator gives
#   y[t] − (1 + ^ϕ[1])y[t−1] + (^ϕ[1] − ^ϕ[2])y[t−2] + (^ϕ[2] − ^ϕ[3])y[t−3] + ^ϕ[3]y[t−4] = ε[t] + ^θ[1]ε[t−1].

# Finally, we move all terms other than yt to the right hand side:
#   y[t] = (1 + ^ϕ[1])y[t−1] − (^ϕ[1] − ^ϕ[2])y[t−2] − (^ϕ[2] − ^ϕ[3])y[t−3] − ^ϕ[3]y[t−4] + ε[t] + ^θ[1]ε[t−1].        (9.5)

# This completes the first step.
# While the equation now looks like an ARIMA(4,0,1), it is still the same ARIMA(3,1,1) model we started with.
# It cannot be considered an ARIMA(4,0,1) because the coefficients do not satisfy the stationarity conditions.


# For the second step, we replace t with T+1 in (9.5):
#   y[T+1] = (1 + ^ϕ[1])y[T] − (^ϕ[1] − ^ϕ[2])y[T−1] − (^ϕ[2] − ^ϕ[3])y[T−2] − ^ϕ[3]y[T−3] + ε[T+1] + ^θ[1]ε[T].

# Assuming we have observations up to time T, all values on the right hand side are known except for ε[T+1], which we replace with zero, and ε[T], which we replace with the last observed residual e[T]:
#   ^y[T+1|T] = (1 + ^ϕ[1])y[T] − (^ϕ[1] − ^ϕ[2])y[T−1] − (^ϕ[2] − ^ϕ[3])y[T−2] − ^ϕ[3]y[T−3] + ^θ[1]e[T].

# A forecast of y[T+2] is obtained by replacing t with T+2 in (9.5).
# All values on the right hand side will be known at time T except y[T+1] which we replace with ^y[T+1|T], and ε[T+2] and ε[T+1], both of which we replace with zero:
#   ^y[T+2|T] = (1 + ^ϕ[1])^y[T+1|T] − (^ϕ[1] − ^ϕ[2])y[T] − (^ϕ[2] − ^ϕ[3])y[T−1] − ^ϕ[3]y[T−2].

# The process continues in this manner for all future time periods.
# In this way, any number of point forecasts can be obtained.


#** Prediction intervals ----
# The calculation of ARIMA prediction intervals is more difficult, and the details are largely beyond the scope of this book.
# We will only give some simple examples.

# The first prediction interval is easy to calculate.
# If ^σ is the standard deviation of the residuals, then a 95% prediction interval is given by ^y[T+1|T] ± 1.96^σ.
# This result is true for all ARIMA models regardless of their parameters and orders.

# Multi-step prediction intervals for ARIMA(0,0,q) models are relatively easy to calculate.
# We can write the model as
#   y[t] = ε[t] + [q∑i=1]θ[i]ε[t−i].

# Then, the estimated forecast variance can be written as
#   ^σ[^2][h] = ^σ[^2][1 + [h−1∑i=1]^θ[^2][i]],         for h=2,3,…,
# and a 95% prediction interval is given by ^y[T+h|T] ± 1.96^σ[h].

# In Section 9.4, we showed that an AR(1) model can be written as an MA(∞) model.
# Using this equivalence, the above result for MA(q) models can also be used to obtain prediction intervals for AR(1) models.

# More general results, and other special cases of multi-step prediction intervals for an ARIMA(p,d,q) model, are given in more advanced textbooks such as Brockwell & Davis (2016).

# The prediction intervals for ARIMA models are based on assumptions that the residuals are uncorrelated and normally distributed.
# If either of these assumptions does not hold, then the prediction intervals may be incorrect.
# For this reason, always plot the ACF and histogram of the residuals to check the assumptions before producing prediction intervals.

# If the residuals are uncorrelated but not normally distributed, then bootstrapped intervals can be obtained instead, as discussed in Section 5.5.
# This is easily achieved by simply adding bootstrap=TRUE in the forecast() function.

# In general, prediction intervals from ARIMA models increase as the forecast horizon increases.
#   For stationary models (i.e., with d=0) they will converge, so that prediction intervals for long horizons are all essentially the same.
#   For d≥1, the prediction intervals will continue to grow into the future.

# As with most prediction interval calculations, ARIMA-based intervals tend to be too narrow.
# This occurs because only the variation in the errors has been accounted for.
# There is also variation in the parameter estimates, and in the model order, that has not been included in the calculation.
# In addition, the calculation assumes that the historical patterns that have been modelled will continue into the forecast period.


# _----


#* 9.9 Seasonal ARIMA models -----------------------------------------------
# So far, we have restricted our attention to non-seasonal data and non-seasonal ARIMA models.
# However, ARIMA models are also capable of modelling a wide range of seasonal data.

# A seasonal ARIMA model is formed by including additional seasonal terms in the ARIMA models we have seen so far.
# It is written as follows:
#   ARIMA 	   (p,d,q)               (P,D,Q)[m]
#                 ↑                     ↑
#           Non-seasonal part       Seasonal part
#             of the model          of the model
#     where m=  the seasonal period (e.g., number of observations per year).
# We use
#   uppercase notation for the seasonal parts of the model, and 
#   lowercase notation for the non-seasonal parts of the model.

# The seasonal part of the model consists of terms that are similar to the non-seasonal components of the model, but involve backshifts of the seasonal period.
#   example, an ARIMA(1,1,1)(1,1,1)[4] model (without a constant) is for quarterly data (m=4), and can be written as
#     (1 − ϕ[1]B) (1 − Φ[1]B[^4])(1 − B)(1 − B[^4])y[t] = (1 + θ[1]B) (1 + Θ[1]B[^4])ε[t].

# The additional seasonal terms are simply multiplied by the non-seasonal terms.


#** ACF/PACF ----
# The seasonal part of an AR or MA model will be seen in the seasonal lags of the PACF and ACF.
#   example, an ARIMA(0,0,0)(0,0,1)[12] model will show:
#     a spike at lag 12 in the ACF but no other significant spikes;
#     exponential decay in the seasonal lags of the PACF (i.e., at lags 12, 24, 36, …).

#   Similarly, an ARIMA(0,0,0)(1,0,0)[12] model will show:
#     exponential decay in the seasonal lags of the ACF;
#     a single significant spike at lag 12 in the PACF.

# In considering the appropriate seasonal orders for a seasonal ARIMA model, restrict attention to the seasonal lags.

# The modelling procedure is almost the same as for non-seasonal data, except that we need to select seasonal AR and MA terms as well as the non-seasonal components of the model.
# The process is best illustrated via examples.


#** Example: Monthly US leisure and hospitality employment ----
# We will describe seasonal ARIMA modelling using monthly US employment data for leisure and hospitality jobs from January 2000 to September 2019

leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality", year(Month) > 2000) %>%    # error in text. uses > instead of >= 2000. Sticking to it, since the analysis followed is based on >.
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)

autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality", y="Number of people (millions)")


# The data are clearly non-stationary, with strong seasonality and a nonlinear trend, so we will first take a seasonal difference.
# The seasonally differenced data are shown in Figure below.

leisure %>%
  gg_tsdisplay(difference(Employed, 12), plot_type='partial', lag = 36) +
  labs(title="Seasonally differenced", y="")


# These are also clearly non-stationary, so we take a further first difference in Figure below.
leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(), plot_type='partial', lag = 36) +
  labs(title = "Double differenced", y="")

# Our aim now is to find an appropriate ARIMA model based on the ACF and PACF shown in Figure above.
#   The significant spike at lag 2 in the ACF suggests a non-seasonal MA(2) component.
#   The significant spike at lag 12 in the ACF suggests a seasonal MA(1) component.
#   Consequently, we begin with an ARIMA(0,1,2)(0,1,1)[12] model, indicating a first difference, a seasonal difference, and non-seasonal MA(2) and seasonal MA(1) component.
# If we had started with the PACF, we may have selected an ARIMA(2,1,0)(0,1,1)[12] model —
#   using the PACF to select the non-seasonal part of the model and the ACF to select the seasonal part of the model.
# We will also include an automatically selected model.
#   By setting stepwise=FALSE and approximation=FALSE, we are making R work extra hard to find a good model.
#   This takes much longer, but with only one series to model, the extra time taken is not a problem.

fit <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )

fit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders")

glance(fit) %>% arrange(AICc) %>% select(.model:BIC)


# The ARIMA() function uses unitroot_nsdiffs() to determine D (the number of seasonal differences to use), and unitroot_ndiffs() to determine d (the number of ordinary differences to use), when these are not specified.
# The selection of the other model parameters (p,q,P and Q) are all determined by minimizing the AICc, as with non-seasonal ARIMA models.

# The three fitted models have similar AICc values, with the automatically selected model being a little better.
# Our second “guess” of ARIMA(2,1,0)(0,1,1)[12] turned out to be very close to the automatically selected model of ARIMA(2,1,0)(1,1,1)[12].

# The residuals for the best model are shown in Figure below.

fit %>% select(auto) %>% gg_tsresiduals(lag = 36)


# One small but significant spike (at lag 11) out of 36 is still consistent with white noise.
# To be sure, we use a Ljung-Box test, which has a large p-value, confirming that the residuals are similar to white noise.
#   Note that the alternative models also pass this test.

augment(fit) %>% features(.innov, ljung_box, lag = 24, dof = 4)


# Thus, we now have a seasonal ARIMA model that passes the required checks and is ready for forecasting.
# Forecasts from the model for the next three years are shown in Figure below.
# The forecasts have captured the seasonal pattern very well, and the increasing trend extends the recent pattern.
# The trend in the forecasts is induced by the double differencing.

forecast(fit, h = 36) %>%
  filter(.model == 'auto') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality", y = "Number of people (millions)")


#** Example: Corticosteroid drug sales in Australia ----
# For our second example, we will try to forecast monthly corticosteroid drug sales in Australia.
#   These are known as H02 drugs under the Anatomical Therapeutic Chemical classification scheme.

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y = "", title = "Corticosteroid drug scripts (H02)")

# Data from July 1991 to June 2008 are plotted in Figure above.
# There is a small increase in the variance with the level, so we take logarithms to stabilise the variance.

# The data are strongly seasonal and obviously non-stationary, so seasonal differencing will be used.
# The seasonally differenced data are shown in Figure below.
# It is not clear at this point whether we should do another difference or not.
# We decide not to, but the choice is not obvious.

# The last few observations appear to be different (more variable) from the earlier data.
# This may be due to the fact that data are sometimes revised when earlier sales are reported late.

h02 %>% gg_tsdisplay(difference(log(Cost), 12), plot_type='partial', lag_max = 24)


# In the plots of the seasonally differenced data, there are spikes in the PACF at lags 12 and 24, but nothing at seasonal lags in the ACF.
# This may be suggestive of a seasonal AR(2) term.
# In the non-seasonal lags, there are three significant spikes in the PACF, suggesting a possible AR(3) term.
# The pattern in the ACF is not indicative of any simple model.

# Consequently, this initial analysis suggests that a possible model for these data is an ARIMA(3,0,0)(2,1,0)[12].
# We fit this model, along with some variations on it, and compute the AICc values.

fit <- h02 %>%
  model(
    arima301012 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)),
    arima301111 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(1,1,1)),
    arima301011 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,1)),
    arima301210 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(2,1,0)),
    arima300210 = ARIMA(log(Cost) ~ 0 + pdq(3,0,0) + PDQ(2,1,0)),
    arima302210 = ARIMA(log(Cost) ~ 0 + pdq(3,0,2) + PDQ(2,1,0)),
    arima301110 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(1,1,0))
  )

glance(fit) %>% arrange(AICc) %>% select(.model, AICc)

# Of these models, the best is the ARIMA(3,0,1)(0,1,2)[12] model (i.e., it has the smallest AICc value).
# The innovation residuals from this model are shown in Figure below.

fit <- h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))

fit %>% gg_tsresiduals(lag_max = 36)

augment(fit) %>% features(.innov, ljung_box, lag = 36, dof = 6)

# There are a few significant spikes in the ACF, and the model fails the Ljung-Box test.
# The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals.

# Next we will try using the automatic ARIMA algorithm.
#   Running ARIMA() with all arguments left at their default values led to an ARIMA(2,1,0)(0,1,1)[12] model.
#   Running ARIMA() with stepwise=FALSE and approximation=FALSE gives an ARIMA(2,1,3)(0,1,1)[12] model.
#   However, both models still fail the Ljung-Box test for 36 lags.
# Sometimes it is just not possible to find a model that passes all of the tests.


#** Test set evaluation: ----
# We will compare some of the models fitted so far using a test set consisting of the last two years of data.
# Thus, we fit the models using data from July 1991 to June 2006, and forecast the script sales for July 2006 – June 2008.

h02_test <- h02 %>% slice(n() - 23:0)
h02_train <- h02 %>% anti_join(h02_test)

fit <- h02_train %>%
  model(
    arima301111 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(1,1,1)),
    arima301012 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)),
    arima211011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,1) + PDQ(0,1,1)),
    arima212011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,2) + PDQ(0,1,1)),
    arima214011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,4) + PDQ(0,1,1)),
    arima213011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,3) + PDQ(0,1,1)),
    arima301011 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,1)),
    arima302011 = ARIMA(log(Cost) ~ 0 + pdq(3,0,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,0) + PDQ(0,1,1)),
    arima301013 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,3)),
    arima303011 = ARIMA(log(Cost) ~ 0 + pdq(3,0,3) + PDQ(0,1,1)),
    arima302210 = ARIMA(log(Cost) ~ 0 + pdq(3,0,2) + PDQ(2,1,0)),
    arima301210 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(2,1,0)),
    arima210110 = ARIMA(log(Cost) ~ 0 + pdq(2,1,0) + PDQ(1,1,0)),
    arima301110 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(1,1,0)),
    arima300210 = ARIMA(log(Cost) ~ 0 + pdq(3,0,0) + PDQ(2,1,0))
  )

fc <- fit %>% forecast(new_data = h02_test)

fc %>% accuracy(h02) %>% select(.model, RMSE) %>% arrange(RMSE)

# The models chosen manually are close to the best model over this test set based on the RMSE values, while those models chosen automatically with ARIMA() are not far behind.

#   When models are compared using AICc values, it is important that all models have the same orders of differencing.
#   However, when comparing models using a test set, it does not matter how the forecasts were produced — the comparisons are always valid.

# Consequently, in the table above, we can include some models with only seasonal differencing and some models with both first and seasonal differencing, while in the earlier table containing AICc values, we only compared models with seasonal differencing but no first differencing.

# None of the models considered here pass all of the residual tests.
#   In practice, we would normally use the best model we could find, even if it did not pass all of the tests.

# Forecasts from the ARIMA(3,0,1)(0,1,2)[12] model (which has the second lowest RMSE value on the test set, and the best AICc value amongst models with only seasonal differencing) are shown in Figure below.


h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) %>%
  forecast() %>%
  autoplot(h02) +
  labs(y = " $AU (millions)", title = "Corticosteroid drug scripts (H02) sales")


# _----


#* 9.10 ARIMA vs ETS -------------------------------------------------------
# It is a commonly held myth that ARIMA models are more general than exponential smoothing.
# While linear exponential smoothing models are all special cases of ARIMA models, the non-linear exponential smoothing models have no equivalent ARIMA counterparts.
# On the other hand, there are also many ARIMA models that have no exponential smoothing counterparts.
# In particular, all ETS models are non-stationary, while some ARIMA models are stationary.
# Figure 9.27 (Image) shows the overlap between the two model classes.

# The ETS models with seasonality or non-damped trend or both have two unit roots (i.e., they need two levels of differencing to make them stationary).
# All other ETS models have one unit root (they need one level of differencing to make them stationary).

# Table 9.4 gives the equivalence relationships for the two classes of models.
# For the seasonal models, the ARIMA parameters have a large number of restrictions.

#  Table 9.4: Equivalence relationships between ETS and ARIMA models.
#   ETS model       ARIMA model               Parameters
#   ETS(A,N,N)      ARIMA(0,1,1)              θ1=α−1
#   ETS(A,A,N)      ARIMA(0,2,2)              θ1=α+β−2; θ2=1−α
#   ETS(A,Ad,N)     ARIMA(1,1,2)              ϕ1=ϕ; θ1=α+ϕβ−1−ϕ; θ2=(1−α)ϕ
#   ETS(A,N,A)      ARIMA(0,1,m)(0,1,0)m
#   ETS(A,A,A)      ARIMA(0,1,m+1)(0,1,0)m
#   ETS(A,Ad,A)     ARIMA(1,0,m+1)(0,1,0)m


# The AICc is useful for selecting between models in the same class.
#   example, we can use it to select
#     an ARIMA model between candidate ARIMA models or
#       As already noted, comparing information criteria is only valid for ARIMA models of the same orders of differencing.
#     an ETS model between candidate ETS models.
# However, it cannot be used to compare between ETS and ARIMA models because they are in different model classes, and the likelihood is computed in different ways.
# The examples below demonstrate selecting between these classes of models.


#** Comparing ARIMA() and ETS() on non-seasonal data ----
# We can use time series cross-validation to compare ARIMA and ETS models.
# Let’s consider the Australian population from the global_economy dataset, as introduced in Section 8.2.

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, RMSE:MAPE)

# In this case the ETS model has higher accuracy on the cross-validated performance measures.
# Below we generate and plot forecasts for the next 5 years generated from an ETS model.

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year >= 2000)) +
  labs(title = "Australian population", y = "People (millions)")


#** Comparing ARIMA() and ETS() on seasonal data ----
# In this case we want to compare seasonal ARIMA and ETS models applied to the quarterly cement production data (from aus_production).
# Because the series is relatively long, we can afford to use a training and a test set rather than time series cross-validation.
#   The advantage is that this is much faster.
# We create a training set from the beginning of 1988 to the end of 2007 and select an ARIMA and an ETS model using the ARIMA() and ETS() functions.

cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)

train <- cement %>% filter_index(. ~ "2007 Q4")

# The output below shows the model selected and estimated by ARIMA().
# The ARIMA model does well in capturing all the dynamics in the data as the residuals seem to be white noise.

fit_arima <- train %>% model(ARIMA(Cement))

report(fit_arima)

fit_arima %>% gg_tsresiduals(lag_max = 16)

augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)


# The output below also shows the ETS model selected and estimated by ETS().
# This model also does well in capturing all the dynamics in the data, as the residuals similarly appear to be white noise.

fit_ets <- train %>% model(ETS(Cement))

report(fit_ets)

fit_ets %>%
  gg_tsresiduals(lag_max = 16)

augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

# The output below evaluates the forecasting performance of the two competing models over the test set.
# In this case the ARIMA model seems to be the slightly more accurate model based on the test set RMSE, MAPE and MASE.

# Generate forecasts and compare accuracy over the test set
bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 10) %>% accuracy(cement),
  fit_ets %>% forecast(h = 10) %>% accuracy(cement)
) %>%
  select(-ME, -MPE, -ACF1)


# Below we generate and plot forecasts from the ARIMA model for the next 3 years.
cement %>%
  model(ARIMA(Cement)) %>%
  forecast(h="3 years") %>%
  autoplot(cement) +
  labs(title = "Cement production in Australia", y = "Tonnes ('000)")


# ____----