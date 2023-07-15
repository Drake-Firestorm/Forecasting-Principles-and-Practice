# Chapter 8 Exponential smoothing -----------------------------------------
# proposed in the late 1950s (Brown, 1959; Holt, 1957; Winters, 1960), and has motivated some of the most successful forecasting methods.
# Forecasts produced using exponential smoothing methods are weighted averages of past observations, with the weights decaying exponentially as the observations get older.
# In other words, the more recent the observation the higher the associated weight.
# This framework generates reliable forecasts quickly and for a wide range of time series, which is a great advantage and of major importance to applications in industry.

# The selection of the method is generally based on recognising key components of the time series (trend and seasonal) and the way in which these enter the smoothing method (e.g., in an additive, damped or multiplicative manner).


# https://boostedml.com/2020/05/an-introduction-to-time-series-smoothing-in-r.html
# What is smoothing
# A time series can generally be decomposed into several components: trend, cyclical, seasonal, and noise.
# These are in order of increasing frequency.
#   As a blog owner, one way I see these is as follows.
#     Noise is simply the random variation in daily views.
#     The seasonal component is that there are fewer viewers on the weekends.
#     The cyclical is that Google’s algorithms cause readership to go up and down, and
#     the trend is that over time, readership tends to go up.


# https://grisha.org/blog/2016/01/29/triple-exponential-smoothing-forecasting/
# Why is it called “smoothing”?
#   To the best of my understanding this simply refers to the effect these methods have on a graph if you were to plot the values: jagged lines become smoother.
#   Moving average also has the same effect, so it deserves the right to be called smoothing just as well.

# Level
# Expected value has another name, which, again varies depending on who wrote the text book: baseline, intercept (as in Y-intercept) or level.

# Trend or Slope


#* 8.1 Simple exponential smoothing ----------------------------------------
# simplest of the exponentially smoothing methods is naturally called simple exponential smoothing (SES).
# This method is suitable for forecasting data with no clear trend or seasonal pattern.

# For example, the data in Figure below do not display any clear trending behaviour or any seasonality.
# We have already considered the naïve and the average as possible methods for forecasting such data.
algeria_economy <- global_economy %>%
  filter(Country == "Algeria")

algeria_economy %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")


# Using the naïve method, all forecasts for the future are equal to the last observed value of the series,
#   ^y[T+h|T] = yT,   for h=1,2,….
# Hence, the naïve method assumes that the most recent observation is the only important one, and all previous observations provide no information for the future. 
# This can be thought of as a weighted average where all of the weight is given to the last observation.

# Using the average method, all future forecasts are equal to a simple average of the observed data,
#   y[T+h|T] = 1/T * [T∑t=1]y[t],     for h=1,2,….

# Hence, the average method assumes that all observations are of equal importance, and gives them equal weights when generating forecasts.

# We often want something between these two extremes.
#   example, it may be sensible to attach larger weights to more recent observations than to observations from the distant past.
# This is exactly the concept behind simple exponential smoothing.
# Forecasts are calculated using weighted averages, where the weights decrease exponentially as observations come from further in the past — the smallest weights are associated with the oldest observations: 
#   y[T+1|T] = αy[T] + α(1−α)y[T−1] + α(1−α)[^2]y[T−2] + ⋯,     (8.1)
#       where 0 ≤ α ≤ 1 is the smoothing parameter. 
# The one-step-ahead forecast for time T+1 is a weighted average of all of the observations in the series y1,…,yT.
# The rate at which the weights decrease is controlled by the parameter α.

# For any α between 0 and 1, the weights attached to the observations decrease exponentially as we go back in time, hence the name “exponential smoothing.”
# If α is small (i.e., close to 0), more weight is given to observations from the more distant past.
# If α is large (i.e., close to 1), more weight is given to the more recent observations.
# For the extreme case where α=1, ^yT+1|T=yT, and the forecasts are equal to the naïve forecasts.

# We present two equivalent forms of simple exponential smoothing, each of which leads to the forecast Equation (8.1).


#** Weighted average form ----
# The forecast at time T+1 is equal to a weighted average between the most recent observation yT and the previous forecast ^y[T|T−1]: 
#     ^y[T+1|T] = αyT + (1−α)^y[T|T−1],   where 0 ≤ α ≤ 1 is the smoothing parameter.
# Similarly, we can write the fitted values as
#     ^y[t+1|t] = αy[t] + (1−α)^y[t|t−1],     for t=1,…,T.
#     (Recall that fitted values are simply one-step forecasts of the training data.)

# The process has to start somewhere, so we let the first fitted value at time 1 be denoted by ℓ0 (which we will have to estimate).Then
#     ^y[2|1] = αy[1] + (1−α)ℓ0
#     ^y[3|2] = αy[2] + (1−α)^y[2|1]
#     ^y[4|3] = αy[3] + (1−α)^y[3|2]
#     ⋮
#     ^y[T|T−1] = αy[T−1] + (1−α)^y[T−1|T−2]
#     ^y[T+1|T] = αy[T] + (1−α)^y[T|T−1]

# Substituting each equation into the following equation, we obtain
#     ^y[3|2] = αy[2] + (1−α)[ αy1 + (1−α)ℓ0 ]
#             = αy[2] + α(1−α)y[1] + (1−α)[^2]ℓ0
#     ^y[4|3] = αy[3] + (1−α)[ αy[2] + α(1−α)y[1] + (1−α)[^2]ℓ0 ]
#             = αy[3] + α(1−α)y[2] + α(1−α)[^2]y[1] + (1−α)[^3]ℓ0
#       ⋮
#     ^y[T+1|T] = [T−1∑j=0]α(1−α)[^j]y[T−j] + (1−α)[^T]ℓ0
# The last term becomes tiny for large T.
# So, the weighted average form leads to the same forecast Equation (8.1).


#** Component form ----
# An alternative representation is the component form.
# For simple exponential smoothing, the only component included is the level, ℓt.
#     (Other methods which are considered later in this chapter may also include a trend bt and a seasonal component st.)
# Component form representations of exponential smoothing methods comprise a forecast equation and a smoothing equation for each of the components included in the method.
# The component form of simple exponential smoothing is given by: 
#     Forecast equation       ^y[t+h|t] = ℓ[t]
#     Smoothing equation      ℓ[t]      = αy[t] + (1−α)ℓ[t−1]
#       where ℓt is the level (or the smoothed value) of the series at time t.
#   Setting h=1 gives the fitted values, while
#   setting t=T gives the true forecasts beyond the training data.

# The forecast equation shows that the forecast value at time t+1 is the estimated level at time t.
# The smoothing equation for the level (usually referred to as the level equation) gives the estimated level of the series at each period t.

# If we replace ℓt with ^y[t+1|t] and ℓ[t−1] with ^y[t|t−1] in the smoothing equation, we will recover the weighted average form of simple exponential smoothing.

# The component form of simple exponential smoothing is not particularly useful on its own, but it will be the easiest form to use when we start adding other components.


#** Flat forecasts ----
# Simple exponential smoothing has a “flat” forecast function:
#     ^y[T+h|T] = ^y[T+1|T] = ℓ[T],     h=2,3,….
# That is, all forecasts take the same value, equal to the last level component.
# Remember that these forecasts will only be suitable if the time series has no trend or seasonal component.


#** Optimisation ----
# The application of every exponential smoothing method requires the smoothing parameters and the initial values to be chosen.
# In particular, for simple exponential smoothing, we need to select the values of α and ℓ0.
# All forecasts can be computed from the data once we know those values.
# For the methods that follow there is usually more than one smoothing parameter and more than one initial component to be chosen.

# In some cases, the smoothing parameters may be chosen in a subjective manner — the forecaster specifies the value of the smoothing parameters based on previous experience.
# However, a more reliable and objective way to obtain values for the unknown parameters is to estimate them from the observed data.

# In Section 7.2, we estimated the coefficients of a regression model by minimising the sum of the squared residuals (usually known as SSE or “sum of squared errors”).
# Similarly, the unknown parameters and the initial values for any exponential smoothing method can be estimated by minimising the SSE.
# The residuals are specified as e[t] = y[t] − ^y[t|t−1] for t=1,…,T.
# Hence, we find the values of the unknown parameters and the initial values that minimise 

# SSE = [T∑t=1](yt − ^y[t|t−1])[^2] = [T∑t=1]e[^2]t         (8.2)

# Unlike the regression case (where we have formulas which return the values of the regression coefficients that minimise the SSE), this involves a non-linear minimisation problem, and we need to use an optimisation tool to solve it.


#** Example: Algerian exports ----
# Estimate parameters
fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

fc <- fit %>%
  forecast(h = 5)

tidy(fit)
# This gives parameter estimates ^α=0.84 and ^ℓ0=39.5, obtained by minimising SSE over periods t=1,2,…,58, subject to the restriction that 0≤α≤1.

# The black line in Figure below shows the data, which has a changing level over time.

fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00", data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = FALSE)

# The forecasts for the period 2018–2022 are plotted in Figure 8.2.
# Also plotted are one-step-ahead fitted values alongside the data over the period 1960–2017.
# The large value of α in this example is reflected in the large adjustment that takes place in the estimated level ℓt at each time.
# A smaller value of α would lead to smaller changes over time, and so the series of fitted values would be smoother.

# The prediction intervals shown here are calculated using the methods described in Section 8.7.
# The prediction intervals show that there is considerable uncertainty in the future exports over the five-year forecast period.
# So interpreting the point forecasts without accounting for the large uncertainty can be very misleading.


# _----


#* 8.2 Methods with trend --------------------------------------------------
#** Holt’s linear trend method ----
# Holt (1957) extended simple exponential smoothing to allow the forecasting of data with a trend. 
# This method involves a forecast equation and two smoothing equations (one for the level and one for the trend):
#     Forecast equation     ^y[t+h|t] = ℓ[t] + hb[t]
#     Level equation        ℓ[t]      = αy[t] + (1−α)(ℓ[t−1] + b[t−1])
#     Trend equation        b[t]      = β∗(ℓ[t] − ℓ[t−1]) + (1−β∗)b[t−]1
#   where
#     ℓ[t] denotes an estimate of the level of the series at time t,
#     b[t] denotes an estimate of the trend (slope) of the series at time t,
#     α is the smoothing parameter for the level, 0 ≤ α ≤ 1, and
#     β∗ is the smoothing parameter for the trend, 0 ≤ β∗ ≤ 1. (We denote this as β∗ instead of β for reasons that will be explained in Section 8.5.)

# As with simple exponential smoothing, the level equation here shows that ℓt is a weighted average of observation yt and the one-step-ahead training forecast for time t, here given by ℓ[t−1] + b[t−1].
# The trend equation shows that bt is a weighted average of the estimated trend at time t based on ℓ[t] − ℓ[t−1] and b[t−1], the previous estimate of the trend.

# The forecast function is no longer flat but trending.
# The h-step-ahead forecast is equal to the last estimated level plus h times the last estimated trend value.
#   Hence the forecasts are a linear function of h.


#** Example: Australian population ----
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)

autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

# We will apply Holt’s method to this series.
# The smoothing parameters, α and β∗, and the initial values ℓ[0] and b[0] are estimated by minimising the SSE for the one-step training errors as in Section 8.1.


fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))

fc <- fit %>% forecast(h = 10)

tidy(fit) %>% print.data.frame()

# The estimated smoothing coefficient for the level is ^α = 0.9999.
#   The very high value shows that the level changes rapidly in order to capture the highly trended series.
# The estimated smoothing coefficient for the slope is ^β∗ = 0.3267.
#   This is relatively large suggesting that the trend also changes often (even if the changes are slight).


#** Damped trend methods ----
# The forecasts generated by Holt’s linear method display a constant trend (increasing or decreasing) indefinitely into the future.
# Empirical evidence indicates that these methods tend to over-forecast, especially for longer forecast horizons.
# Motivated by this observation, Gardner & McKenzie (1985) introduced a parameter that “dampens” the trend to a flat line some time in the future.
# Methods that include a damped trend have proven to be very successful, and are arguably the most popular individual methods when forecasts are required automatically for many series.


# In conjunction with the smoothing parameters α and β∗ (with values between 0 and 1 as in Holt’s method), this method also includes a damping parameter 0 < ϕ < 1:
#     ^y[t+h|t]     = ℓ[t] + (ϕ + ϕ[^2] + ⋯ + ϕ[^h])b[t]
#     ℓ[t]          = αy[t] + (1−α)(ℓ[t−1] + ϕb[t−1])
#     b[t]          = β∗(ℓ[t] − ℓ[t−1]) + (1−β∗)ϕb[t−1]

# If ϕ = 1, the method is identical to Holt’s linear method.
# For values between 0 and 1, ϕ dampens the trend so that it approaches a constant some time in the future.
# In fact, the forecasts converge to ℓ[T] + ϕb[T]/(1−ϕ) as h → ∞ for any value 0 < ϕ < 1.
# This means that short-run forecasts are trended while long-run forecasts are constant.

# In practice, ϕ is rarely less than 0.8 as the damping has a very strong effect for smaller values. 
# Values of ϕ close to 1 will mean that a damped model is not able to be distinguished from a non-damped model.
# For these reasons, we usually restrict ϕ to a minimum of 0.8 and a maximum of 0.98.


#** Example: Australian Population (continued) ----
# Figure below shows the forecasts for years 2018–2032 generated from Holt’s linear trend method and the damped trend method.

aus_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") + trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population", y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))


# We have set the damping parameter to a relatively low number (ϕ = 0.90) to exaggerate the effect of damping for comparison.
# Usually, we would estimate ϕ along with the other parameters.
# We have also used a rather large forecast horizon (h = 15) to highlight the difference between a damped trend and a linear trend.


#** Example: Internet usage ----
# In this example, we compare the forecasting performance of the three exponential smoothing methods that we have considered so far in forecasting the number of users connected to the internet via a server.
# The data is observed over 100 minutes and is shown in Figure below.

www_usage <- as_tsibble(WWWusage)

www_usage %>% autoplot(value) +
  labs(x="Minute", y="Number of users", title = "Internet usage per minute")


# We will use time series cross-validation to compare the one-step forecast accuracy of the three methods.

www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)

# Damped Holt’s method is best whether you compare MAE or RMSE values.
# So we will proceed with using the damped Holt’s method and apply it to the whole data set to get forecasts for future minutes.

fit <- www_usage %>%
  model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))

# Estimated parameters:
tidy(fit) %>% print.data.frame()

# The smoothing parameter for the slope is estimated to be almost one, indicating that the trend changes to mostly reflect the slope between the last two minutes of internet usage.
# The value of α is very close to one, showing that the level reacts strongly to each new observation.

fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users", title = "Internet usage per minute")

# The resulting forecasts look sensible with decreasing trend, which flattens out due to the low value of the damping parameter (0.815), and relatively wide prediction intervals reflecting the variation in the historical data.
# The prediction intervals are calculated using the methods described in Section 8.7.

# In this example, the process of selecting a method was relatively easy as both MSE and MAE comparisons suggested the same method (damped Holt’s).
# However, sometimes different accuracy measures will suggest different forecasting methods, and then a decision is required as to which forecasting method we prefer to use.
# As forecasting tasks can vary by many dimensions (length of forecast horizon, size of test set, forecast error measures, frequency of data, etc.), it is unlikely that one method will be better than all others for all forecasting scenarios.
# What we require from a forecasting method are consistently sensible forecasts, and these should be frequently evaluated against the task at hand.


# _----


#* 8.3 Methods with seasonality --------------------------------------------
# Holt (1957) and Winters (1960) extended Holt’s method to capture seasonality. 
# The Holt-Winters seasonal method comprises the forecast equation and three smoothing equations —
#   one for the level ℓt, 
#   one for the trend bt, and 
#   one for the seasonal component st, 
#   with corresponding smoothing parameters α, β∗ and γ. 
# We use m to denote the period of the seasonality, i.e., the number of seasons in a year. 
#   example, for quarterly data m = 4, and for monthly data m = 12.

# There are two variations to this method that differ in the nature of the seasonal component. 
# additive method
#   preferred when the seasonal variations are roughly constant through the series
#   the seasonal component is expressed in absolute terms in the scale of the observed series, and 
#   in the level equation the series is seasonally adjusted by subtracting the seasonal component.
#   Within each year, the seasonal component will add up to approximately zero.
# multiplicative method
#   the  is preferred when the seasonal variations are changing proportional to the level of the series
#   the seasonal component is expressed in relative terms (percentages), and 
#   the series is seasonally adjusted by dividing through by the seasonal component. 
#   Within each year, the seasonal component will sum up to approximately m.


#** Holt-Winters’ additive method ----
# The component form for the additive method is: 
#     ^y[t+h|t] = ℓ[t] + hb[t] + s[t+h−m(k+1)]
#     ℓ[t]      = α(y[t] − s[t−m]) + (1 − α)(ℓ[t−1] + b[t−1])
#     b[t]      = β∗ * (ℓ[t] − ℓ[t−1]) + (1 − β∗)b[t−1]
#     s[t]      = γ(y[t] − ℓ[t−1] − b[t−1]) + (1 − γ)s[t−m]
#   where k is the integer part of (h−1)/m, which ensures that the estimates of the seasonal indices used for forecasting come from the final year of the sample. 
# The level equation shows a weighted average between the seasonally adjusted observation (y[t] − s[t−m]) and the non-seasonal forecast (ℓ[t−1] + b[t−1]) for time t. 
# The trend equation is identical to Holt’s linear method. 
# The seasonal equation shows a weighted average between the current seasonal index, (y[t] − ℓ[t−1] − b[t−1]), and the seasonal index of the same season last year (i.e., m time periods ago).

# The equation for the seasonal component is often expressed as
#     s[t] = γ∗ * (y[t] − ℓ[t]) + (1 − γ∗)s[t−m]. 

# If we substitute ℓt from the smoothing equation for the level of the component form above, we get 
#     s[t] = γ∗ * (1 − α)(y[t] − ℓ[t−1] − b[t−1]) + [1 − γ∗ (1 − α)]s[t−m], 
#   which is identical to the smoothing equation for the seasonal component we specify here, with γ = γ∗ * (1 − α). 
# The usual parameter restriction is 0 ≤ γ∗ ≤ 1, which translates to 0 ≤ γ ≤ 1−α.


#** Holt-Winters’ multiplicative method ----
# The component form for the multiplicative method is: 
#     ^y[t+h|t] = (ℓ[t] + hb[t]) * s[t+h−m(k+1)]
#     ℓ[t]      = α(y[t] / s[t−m]) + (1 − α)(ℓ[t−1] + b[t−1])
#     b[t]      = β∗ * (ℓ[t] − ℓ[t−1]) + (1 − β∗)b[t−1]
#     s[t]      = γ(y[t] / (ℓ[t−1] + b[t−1])) + (1 − γ)s[t−m]


#** Example: Domestic overnight trips in Australia ----
# We apply Holt-Winters’ method with both additive and multiplicative seasonality to forecast quarterly visitor nights in Australia spent by domestic tourists. 
# Figure below shows the data from 1998–2017, and the forecasts for 2018–2020. 
# The data show an obvious seasonal pattern, with peaks observed in the March quarter of each year, corresponding to the Australian summer.

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)

fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )

fc <- fit %>% forecast(h = "3 years")

fc %>%
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism", y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

tidy(fit)
accuracy(fit)
components(fit)

# Because both methods have exactly the same number of parameters to estimate, we can compare the training RMSE from both models. 
# In this case, the method with multiplicative seasonality fits the data slightly better.

# The estimated states for both models are plotted in Figure below. 
# The small value of γ for the multiplicative model means that the seasonal component hardly changes over time. 
# The small value of β∗ means the slope component hardly changes over time (compare the vertical scales of the slope and level components).


p1 <- components(fit) %>%
  filter(.model == "additive") %>%
  autoplot() +
  labs(title = "ETS(A,A,A) decomposition", subtitle = "Additive seasonality")

p2 <- components(fit) %>%
  filter(.model == "multiplicative") %>%
  autoplot() +
  labs(title = "ETS(M,A,M) decomposition", subtitle = "Multiplicative seasonality")

p1 + p2

# overlapping plot
components(fit) %>%
  autoplot()


#** Holt-Winters’ damped method ----
# Damping is possible with both additive and multiplicative Holt-Winters’ methods. 
# A method that often provides accurate and robust forecasts for seasonal data is the Holt-Winters method with a damped trend and multiplicative seasonality: 

#     ^y[t+h|t] = (ℓ[t] + (ϕ + ϕ[^2] + ⋯ + ϕ[^h])b[t]) * s[t+h−m(k+1)]
#     ℓ[t]      = α(y[t] / s[t−m]) + (1 − α)(ℓ[t−1] + ϕb[t−1])
#     b[t]      = β∗ * (ℓ[t] − ℓ[t−1]) + (1 − β∗) * ϕb[t−1]
#     s[t]      = γ(y[t] / (ℓ[t−1] + ϕb[t−1])) + (1 − γ)s[t−m]


#** Example: Holt-Winters method with daily data ----
# The Holt-Winters method can also be used for daily type of data, where the seasonal period is m = 7, and the appropriate unit of time for h is in days. 
# Here we forecast pedestrian traffic at a busy Melbourne train station in July 2016.

sth_cross_ped <- pedestrian %>%
  filter(Date >= "2016-07-01", Sensor == "Southern Cross Station") %>%
  index_by(Date) %>%
  summarise(Count = sum(Count)/1000)

sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross", y="Pedestrians ('000)")

# Clearly the model has identified the weekly seasonal pattern and the increasing trend at the end of the data, and the forecasts are a close match to the test data.


# _----


#* 8.4 A taxonomy of exponential smoothing methods -------------------------
# Exponential smoothing methods are not restricted to those we have presented so far. 
# By considering variations in the combinations of the trend and seasonal components, nine exponential smoothing methods are possible, listed in Table 8.5. 
# Each method is labelled by a pair of letters (T,S) defining the type of ‘Trend’ and ‘Seasonal’ components. 
#   example, 
#     (A,M) is the method with an additive trend and multiplicative seasonality; 
#     (Ad,N) is the method with damped trend and no seasonality; and so on.

# Table 8.5: A two-way classification of exponential smoothing methods.
#   Trend Component               Seasonal Component
#                                 N 	      A             M
#                                 (None)    (Additive)    (Multiplicative)
#   N (None)                      (N,N)     (N,A)         (N,M)
#   A (Additive)                  (A,N)     (A,A)         (A,M)
#   Ad (Additive damped)          (Ad,N)    (Ad,A)        (Ad,M) 

# Some of these methods we have already seen using other names:
# Short hand        Method
# (N,N) 	        Simple exponential smoothing
# (A,N)             Holt’s linear method
# (Ad,N)            Additive damped trend method
# (A,A)             Additive Holt-Winters’ method
# (A,M)             Multiplicative Holt-Winters’ method
# (Ad,M)            Holt-Winters’ damped method 

# Table 8.6 (see image) gives the recursive formulas for applying the nine exponential smoothing methods in Table 8.5. 
# Each cell includes the forecast equation for generating h-step-ahead forecasts, and the smoothing equations for applying the method.
#   most formulas are obtained by setting ℓ[t] or s[t] as 0 in (A,A) amd (A,M) model formulas.


# _----


#* 8.5 Innovations state space models for exponential smoothing ------------
# In the rest of this chapter, we study the statistical models that underlie the exponential smoothing methods we have considered so far. 
# The exponential smoothing methods presented in Table 8.6 are algorithms which generate point forecasts. 
# The statistical models in this section generate the same point forecasts, but can also generate prediction (or forecast) intervals. 
# A statistical model is a stochastic (or random) data generating process that can produce an entire forecast distribution. 
# We will also describe how to use the model selection criteria introduced in Chapter 7 to choose the model in an objective manner.

# Each model consists of a measurement equation that describes the observed data, and some state equations that describe how the unobserved components or states (level, trend, seasonal) change over time. 
#     Hence, these are referred to as state space models.

# For each method there exist two models: 
#   one with additive errors and 
#   one with multiplicative errors. 
# The point forecasts produced by the models are identical if they use the same smoothing parameter values. 
# They will, however, generate different prediction intervals.

# To distinguish between a model with additive errors and one with multiplicative errors (and also to distinguish the models from the methods), we add a third letter to the classification of Table 8.5. 
# We label each state space model as ETS(⋅,⋅,⋅) for (Error, Trend, Seasonal).
#   This label can also be thought of as ExponenTial Smoothing. 
# Using the same notation as in Table 8.5, the possibilities for each component are: 
#   Error ={A,M},     Trend ={N,A,Ad}     and       Seasonal ={N,A,M}.


#** ETS(A,N,N): simple exponential smoothing with additive errors ----
# Recall the component form of simple exponential smoothing: 
#   Forecast equation     ^y[t+1|t] = ℓ[t]
#   Smoothing equation    ℓ[t]      = αy[t] + (1 − α)ℓ[t−1] 
# If we re-arrange the smoothing equation for the level, we get the “error correction” form, 
#   ℓ[t]  = ℓ[t−1] + α(y[t] − ℓ[t−1])
#         = ℓ[t−1] + αe[t],             where e[t] = y[t] − ℓ[t−1] = y[t] − ^y[t|t−1] is the residual at time t.

# The training data errors lead to the adjustment of the estimated level throughout the smoothing process for t = 1, …, T. 
#   example, 
#     if the error at time t is negative, then y[t] < ^y[t|t−1] and so the level at time t−1 has been over-estimated. 
#     The new level ℓ[t] is then the previous level ℓ[t−1] adjusted downwards. 
#   The closer α is to one, the “rougher” the estimate of the level (large adjustments take place). 
#   The smaller the α, the “smoother” the level (small adjustments take place).

# We can also write y[t] = ℓ[t−1] + e[t], so that each observation can be represented by the previous level plus an error. 
# To make this into an innovations state space model, all we need to do is specify the probability distribution for e[t]. 
# For a model with additive errors, we assume that residuals (the one-step training errors) e[t] are normally distributed white noise with mean 0 and variance σ[^2]. 
# A short-hand notation for this is e[t] = ε[t] ∼ NID(0,σ[^2]); NID stands for “normally and independently distributed.”

# Then the equations of the model can be written as 
#   y[t] = ℓ[t−1] + ε[t]      (8.3)
#   ℓ[t] = ℓ[t−1] + αε[t]     (8.4)

# We refer to 
#   (8.3) as the measurement (or observation) equation and 
#   (8.4) as the state (or transition) equation. 
# These two equations, together with the statistical distribution of the errors, form a fully specified statistical model. 
# Specifically, these constitute an innovations state space model underlying simple exponential smoothing.

# The term “innovations” comes from the fact that all equations use the same random error process, ε[t]. 
#   For the same reason, this formulation is also referred to as a “single source of error” model. 
# There are alternative multiple source of error formulations which we do not present here.

# The measurement equation shows the relationship between the observations and the unobserved states. # In this case, observation y[t] is a linear function of the level ℓ[t−1], the predictable part of y[t], and the error ε[t], the unpredictable part of y[t]. 
# For other innovations state space models, this relationship may be nonlinear.

# The state equation shows the evolution of the state through time. 
# The influence of the smoothing parameter α is the same as for the methods discussed earlier. 
#   example, 
#     α governs the amount of change in successive levels: 
#       high values of α allow rapid changes in the level; 
#       low values of α lead to smooth changes. 
#   If α = 0, the level of the series does not change over time; 
#   if α = 1, the model reduces to a random walk model, y[t] = y[t−1] + ε[t]. (See Section 9.1 for a discussion of this model.)


#** ETS(M,N,N): simple exponential smoothing with multiplicative errors ----
# In a similar fashion, we can specify models with multiplicative errors by writing the one-step-ahead training errors as relative errors 
#   ε[t] = (y[t] − ^y[t|t−1]) / ^y[t|t−1]   where ε[t]∼NID(0,σ2). 
# Substituting ^y[t|t−1] = ℓ[t−1] gives y[t] = ℓ[t−1] + ℓ[t−1]ε[t] and e[t] = y[t] − ^y[t|t−1] = ℓ[t−1]ε[t].

# Then we can write the multiplicative form of the state space model as 
#   y[t] = ℓ[t−1](1 + ε[t])
#   ℓ[t] = ℓ[t−1](1 + αε[t])


#** ETS(A,A,N): Holt’s linear method with additive errors ---
# For this model, we assume that the one-step-ahead training errors are given by 
#   ε[t] = y[t] − ℓ[t−1] − b[t−1] ∼ NID(0,σ[^2]). 
# Substituting this into the error correction equations for Holt’s linear method we obtain 
#   y[t] = ℓ[t−1] + b[t−1] + ε[t]
#   ℓ[t] = ℓ[t−1] + b[t−1] + αε[t]
#   b[t] = b[t−1] + βε[t],            where for simplicity we have set β = αβ∗.

#** ETS(M,A,N): Holt’s linear method with multiplicative errors ----
# Specifying one-step-ahead training errors as relative errors such that 
#   ε[t] = (y[t] − (ℓ[t−1] + b[t−1])) / (ℓ[t−1] + b[t−1]) 
# and following an approach similar to that used above, the innovations state space model underlying Holt’s linear method with multiplicative errors is specified as 
#   y[t] = (ℓ[t−1] + b[t−1])(1 + ε[t])
#   ℓ[t] = (ℓ[t−1] + b[t−1])(1 + αε[t])
#   b[t] = b[t−1] + β(ℓ[t−1] + b[t−1])ε[t]
# where again β = αβ∗   and   ε[t]∼NID(0,σ2).


#** Other ETS models ----
# In a similar fashion, we can write an innovations state space model for each of the exponential smoothing methods of Table 8.6. 
# Table 8.7 (see image) presents the equations for all of the models in the ETS framework.


# _----


#* 8.6 Estimation and model selection --------------------------------------
#** Estimating ETS models ----
# An alternative to estimating the parameters by minimising the sum of squared errors is to maximise the “likelihood.” 
#   likelihood 
#     is the probability of the data arising from the specified model. 
# Thus, a large likelihood is associated with a good model. 
# For an additive error model, maximising the likelihood (assuming normally distributed errors) gives the same results as minimising the sum of squared errors. 
# However, different results will be obtained for multiplicative error models. 
# In this section, we will estimate the smoothing parameters α, β, γ and ϕ, and the initial states ℓ0, b0, s0,s−1,…,s−m+1, by maximising the likelihood.

# The possible values that the smoothing parameters can take are restricted. 
# Traditionally, the parameters have been constrained to lie between 0 and 1 so that the equations can be interpreted as weighted averages. 
#   That is, 0 < α,β∗,γ∗,ϕ < 1.
# For the state space models, we have set 
#   β = αβ∗ and 
#   γ = (1 − α)γ∗
# Therefore, the traditional restrictions translate to 0 < α < 1, 0 < β < α and 0 < γ < 1−α. 
# In practice, the damping parameter ϕ is usually constrained further to prevent numerical difficulties in estimating the model. 
#   In the fable package, it is restricted so that 0.8 < ϕ < 0.98.

# Another way to view the parameters is through a consideration of the mathematical properties of the state space models. 
# The parameters are constrained in order to prevent observations in the distant past having a continuing effect on current forecasts. 
# This leads to some admissibility constraints on the parameters, which are usually (but not always) less restrictive than the traditional constraints region (Hyndman et al., 2008, p. Ch10). 
#   example, 
#     for the ETS(A,N,N) model, 
#       the traditional parameter region is 0 < α < 1 but 
#       the admissible region is 0 < α < 2. 
#     For the ETS(A,A,N) model, 
#       the traditional parameter region is 0 < α < 1 and 0 < β < α but 
#       the admissible region is 0 < α < 2 and 0 < β < 4−2α.


#** Model selection ----
# A great advantage of the ETS statistical framework is that information criteria can be used for model selection. 
# The AIC, AICc and BIC, introduced in Section 7.5, can be used here to determine which of the ETS models is most appropriate for a given time series.

# For ETS models, Akaike’s Information Criterion (AIC) is defined as 
#   AIC = −2log(L) + 2k, 
#   where 
#     L is the likelihood of the model and 
#     k is the total number of parameters and initial states that have been estimated (including the residual variance).

# The AIC corrected for small sample bias (AICc) is defined as 
#   AICc = AIC + (2k(k+1) / (T−k−1))

# and the Bayesian Information Criterion (BIC) is 
#   BIC = AIC + k[log(T) − 2]

# Three of the combinations of (Error, Trend, Seasonal) can lead to numerical difficulties. 
# Specifically, the models that can cause such instabilities are ETS(A,N,M), ETS(A,A,M), and ETS(A,Ad,M), due to division by values potentially close to zero in the state equations. 
# We normally do not consider these particular combinations when selecting a model.

# Models with multiplicative errors are useful when the data are strictly positive, but are not numerically stable when the data contain zeros or negative values. 
# Therefore, multiplicative error models will not be considered if the time series is not strictly positive. 
# In that case, only the six fully additive models will be applied.


#** Example: Domestic holiday tourist visitor nights in Australia ----
# We let the ETS() function select the model by minimising the AICc.

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)

fit <- aus_holidays %>%
  model(ETS(Trips))

report(fit)

# The model selected is ETS(M,N,A) 
#   y[t] = (ℓ[t−1] + s[t−m])(1 + ε[t])
#   ℓ[t] = ℓ[t−1](1 + αε[t])
#   s[t] = s[t−m](1 + γε[t])

# The parameter estimates are ^α = 0.3484, and ^γ = 0.0001. 
# The output also returns the estimates for the initial states ℓ[0], s[0], s[−1], s[−2] and s[−3].
# Compare these with the values obtained for the Holt-Winters method with additive seasonality presented in Table 8.3.

# Figure 1 shows the states over time, while Figure 3 (Chapter 8.7) shows point forecasts and prediction intervals generated from the model. 
# The small values of γ indicate that the seasonal components change very little over time.

components(fit) %>%
  autoplot() +
  labs(title = "ETS(M,N,A) components")

# Because this model has multiplicative errors, the innovation residuals are not equivalent to the regular residuals (i.e., the one-step training errors). 
# The innovation residuals are given by ^ε[t], while the regular residuals are defined as y[t] − ^y[t|t−1]. 
# We can obtain both using the augment() function. They are plotted in Figure 2.

augment(fit) %>%
  select(Quarter, .resid, .innov) %>%
  pivot_longer(-Quarter) %>%
  autoplot(value) +
  facet_wrap(vars(name), scales = "free_y", dir = "v", strip.position = "right") +
  theme(legend.position = "none")


# _----


#* 8.7 Forecasting with ETS models -----------------------------------------
# Point forecasts can be obtained from the models by iterating the equations for t = T+1, …, T+h and setting all ε[t]=0 for t > T.

# For example, for model ETS(M,A,N), y[T+1] = (ℓ[T] + b[T])(1 + ε[T+1]). 
# Therefore ^y[T+1|T] = ℓ[T] + b[T]. 
# Similarly, 
#   y[T+2]  = (ℓ[T+1] + b[T+1])(1 + ε[T+2])
#           = [(ℓ[T] + b[T])(1 + αε[T+1]) + b[T] + β(ℓ[T] + b[T])ε[T+1]](1 + ε[T+2])

# Therefore, ^y[T+2|T] = ℓ[T] + 2b[T], and so on. 
# These forecasts are identical to the forecasts from Holt’s linear method, and also to those from model ETS(A,A,N). 
# Thus, the point forecasts obtained from the method and from the two models that underlie the method are identical (assuming that the same parameter values are used). 
# ETS point forecasts constructed in this way are equal to the means of the forecast distributions, except for the models with multiplicative seasonality (Hyndman et al., 2008).

# To obtain forecasts from an ETS model, we use the forecast() function from the fable package. 
#   This function will always return the means of the forecast distribution, even when they differ from these traditional point forecasts.

# Figure 3
fit %>%
  forecast(h = 8) %>%
  autoplot(aus_holidays)+
  labs(title = "Australian domestic tourism", y = "Overnight trips (millions)")


#** Prediction intervals ----
# A big advantage of the models is that prediction intervals can also be generated — something that cannot be done using the methods. 
# The prediction intervals will differ between models with additive and multiplicative methods.

# For most ETS models, a prediction interval can be written as 
#   ^y[T+h|T] ± cσ[h]
#   where 
#     c depends on the coverage probability, and 
#     σ2[h] is the forecast variance. 
# Values for c were given in Table 5.1. 
# For ETS models, formulas for σ[^2][h] can be complicated; the details are given in Chapter 6 of Hyndman et al. (2008). 
# In Table 8.8 (image) we give the formulas for the additive ETS models, which are the simplest.

# For a few ETS models, there are no known formulas for prediction intervals. 
# In these cases, the forecast() function uses simulated future sample paths and computes prediction intervals from the percentiles of these simulated future paths.


# ____----
