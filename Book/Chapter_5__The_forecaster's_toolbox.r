# Chapter 5 The forecaster's toolbox --------------------------------------
#* 5.1 A tidy forecasting workflow ------
# The process of producing forecasts for time series data can be broken down into a few steps.
#                        ->  Specify ->
#                      /                \
# Tidy -> Visualise -                     -> Estimate -> Forecast
#                      \                /
#                        <- Evaluate <-


#** Data preparation (tidy) ----
# first step in forecasting is to prepare data in the correct format.
# This process may involve
#   loading in data,
#   identifying missing values,
#   filtering the time series and
#   other pre-processing tasks.

# Many models have different data requirements, some require the series to be in time order, others require no missing values. 
# Checking your data is an essential step to understanding its features and it is useful to do before models are estimated.

# The way in which your data is prepared can also be used to explore different features of the time series.
# pre-processing your dataset is an important step in evaluating model performance using cross-validation.


#** Plot the data (visualise) ----
# Looking at your data allows you to identify common patterns, and subsequently specify an appropriate model.

global_economy %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP) +
  ggtitle("GDP for Sweden") + ylab("$US billions")


#** Define a model (specify) ----
# Before fitting a model to the data, we first must describe the model.
# Specifying an appropriate model for the data is essential for producing appropriate forecasts.

# Models in R are specified using model functions, which each use a formula (y ~ x) interface.
# The response variable(s) are specified on the left of the formula, and the structure of the model is written on the right.

# e.g. TSLM(GDP ~ trend())
# In this case the model function is TSLM() (time series linear model), the response variable is GDP and it is being modelled using trend() (a "special" function specifying a linear trend).

# The special functions used to define the model's structure vary between models (as each model can support different structures).

# The left side of the formula also supports the transformations discussed in Section 3.1, which can be useful in simplifying the time series patterns or constraining the forecasts to be between specific values.


#** Train the model (estimate) ----
# Once an appropriate model is specified, we next train the model on some data.
# One or more model specifications can be estimated using the model() function.

fit <- global_economy %>%
  model(trend_model = TSLM(GDP ~ trend()))

# The resulting object is a model table or a "mable."
# Each row corresponds to one combination of the key variables.
# The trend_model column contains information about the fitted model for each country.

# Once a model has been fitted, it is important to check how well it has performed on the data.
# There are several diagnostic tools available to check model behaviour, and also accuracy measures that allow one model to be compared against another.
# Sections 5.8 and 5.9 go into further details.


#** Produce forecasts (forecast) ----
# With an appropriate model specified, estimated and checked, it is time to produce the forecasts using forecast().
# The easiest way to use this function is by specifying the number of future observations to forecast.
#   example
#     forecasts for the next 10 observations can be generated using h = 10.
#     can also use natural language; e.g., h = "2 years" can be used to predict two years into the future.

# In other situations, it may be more convenient to provide a dataset of future time periods to forecast.
# This is commonly required when your model uses additional information from the data, such as variables for exogenous regressors. 
# Additional data required by the model can be included in the dataset of observations to forecast.

fit %>% forecast(h = "3 years")

# This is a forecasting table, or "fable."
# Each row corresponds to one forecast period for each country.
# The GDP column contains the forecasting distribution, while the .mean column contains the point forecast.
# The point forecast is the mean (or average) of the forecasting distribution.

# The forecasts can be plotted along with the historical data using autoplot() as follows.
fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(global_economy) +
  ggtitle("GDP for Sweden") + ylab("$US billions")


# _----


#* 5.2 Some simple forecasting methods -------------------------------------
# Some forecasting methods are extremely simple and surprisingly effective.

bricks <- aus_production %>% filter_index("1970" ~ "2004")
# bricks <- aus_production %>% filter(between(year(Quarter), 1970, 2004))   # alternate

# filter_index() function is a convenient shorthand for extracting a section of a time series.


#** Average method ----
# Here, the forecasts of all future values are equal to the average (or "mean") of the historical data.
# If we let the historical data be denoted by y1,.,yT, then we can write the forecasts as
#   ^y[[T+h]|T] = ¯y = (y1 + ... + yT)/T.
# The notation ^y[[T+h]|T] is a short-hand for the estimate of y[T+h] based on the data y1,.,yT.

bricks %>%
  model(MEAN(Bricks)) %>%
  forecast(h = "10 years") %>%
  autoplot(bricks) +
  ggtitle("Clay brick production in Australia")


#** Naïve method ----
# For naïve forecasts, we simply set all forecasts to be the value of the last observation.
# That is,
#   ^y[[T+h]|T] = yT.
# This method works remarkably well for many economic and financial time series.

bricks %>%
  model(NAIVE(Bricks)) %>%
  forecast(h = "10 years") %>%
  autoplot(bricks) +
  ggtitle("Clay brick production in Australia")


#** Seasonal naïve method ----
# A similar method is useful for highly seasonal data.
# In this case, we set each forecast to be equal to the last observed value from the same season of the year (e.g., the same month of the previous year).
# Formally, the forecast for time T+h is written as
#   ^y[T+h|T] = y[T+h-m(k+1)]
#   where m = the seasonal period, and k is the integer part of (h-1)/m (i.e., the number of complete years in the forecast period prior to time T+h). 

# example
#   with monthly data, the forecast for all future February values is equal to the last observed February value.
#   With quarterly data, the forecast of all future Q2 values is equal to the last observed Q2 value (where Q2 means the second quarter).
#   Similar rules apply for other months and quarters, and for other seasonal periods.

bricks %>%
  model(SNAIVE(Bricks)) %>%
  forecast(h = "10 years") %>%
  autoplot(bricks) +
  ggtitle("Clay brick production in Australia")

# The lag() function is optional here as bricks is quarterly data and so a seasonal naïve model will need a one-year lag.
# However, for some time series there is more than one seasonal period, and then the required lag must be specified.


#** Drift method ----
# A variation on the naïve method is to allow the forecasts to increase or decrease over time, where the amount of change over time (called the drift) is set to be the average change seen in the historical data.
# Thus the forecast for time T+h is given by
#   ^y[[T+h]|T] = y[T] + (h/T-1) * [T∑t=2](y[t] - y[t-1]) = y[T] + h((y[T] - y[1]) / (T-1))
# This is equivalent to drawing a line between the first and last observations, and extrapolating it into the future.

bricks %>%
  model(RW(Bricks ~ drift())) %>%
  forecast(h = "10 years") %>%
  autoplot(bricks) +
  ggtitle("Clay brick production in Australia")

#** Example: Australian quarterly beer production ----
# Set training data from 1992 to 2006
train <- aus_production %>% filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )

# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)

# Plot forecasts against actual values
beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .), color = "black") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title = "Forecast"))


#** Example: Google's daily closing stock price ----
# non-seasonal methods are applied to Google's daily closing stock price in 2015, and used to forecast one month ahead.
# Because stock prices are not observed every day, we first set up a new time index based on the trading days rather than calendar days.

# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# Produce forecasts for the 19 trading days in January 2015
google_fc <- google_fit %>% forecast(h = 19)

# A better way using a tsibble to determine the forecast horizons
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>% forecast(google_jan_2016)

# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, color = "black") +
  ggtitle("Google stock (daily ending 31 Dec 2015)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour = guide_legend(title = "Forecast"))

# Sometimes one of these simple methods will be the best forecasting method available; but in many cases, these methods will serve as benchmarks rather than the method of choice.
# That is, any forecasting methods we develop will be compared to these simple methods to ensure that the new method is better than these simple alternatives.
# If not, the new method is not worth considering.


# _----


#* 5.3 Fitted values and residuals ------
#** Fitted values ----
# Each observation in a time series can be forecast using all previous observations.
# We call these fitted values and they are denoted by ^y[t|t-1], meaning the forecast of yt based on observations y1,.,yt-1 .
# We use these so often, we sometimes drop part of the subscript and just write ^yt instead of ^y[t|t-1].
# Fitted values always involve one-step forecasts.

# Actually, fitted values are often not true forecasts because any parameters involved in the forecasting method are estimated using all available observations in the time series, including future observations.
#   example, 
#     if we use the average method, the fitted values are given by ^yt=^c where ^c is the average computed over all available observations, including those at times after t.
#     Similarly, for the drift method, the drift parameter is estimated using all available observations.
#     In this case, the fitted values are given by
#       ^yt = y[t-1] + ^c where ^c = (yT-y1)/(T-1).

# In both cases, there is a parameter to be estimated from the data.
# The "hat" above the c reminds us that this is an estimate.
# When the estimate of c involves observations after time t, the fitted values are not true forecasts.
# On the other hand, naïve or seasonal naïve forecasts do not involve any parameters, and so fitted values are true forecasts in such cases.

#** Residuals ----
# The "residuals" in a time series model are what is left over after fitting a model.
# For many (but not all) time series models, the residuals are equal to the difference between the observations and the corresponding fitted values:
#   et=yt-^yt.

# augment() - function for obtaining fitted values and residuals from a model

augment(beer_fit)

# Residuals are useful in checking whether a model has adequately captured the information in the data.
# If patterns are observable in the residuals, the model can probably be improved.


# _----


#* 5.4 Residual diagnostics -------------
# A good forecasting method will yield residuals with the following properties:
#   1. The residuals are uncorrelated.
#     If there are correlations between residuals, then there is information left in the residuals which should be used in computing forecasts.
#   2. The residuals have zero mean.
#     If the residuals have a mean other than zero, then the forecasts are biased.

# Any forecasting method that does not satisfy these properties can be improved.
# However, that does not mean that forecasting methods that satisfy these properties cannot be improved.
# It is possible to have several different forecasting methods for the same data set, all of which satisfy these properties.
# Checking these properties is important in order to see whether a method is using all of the available information, but it is not a good way to select a forecasting method. 

# If either of these properties is not satisfied, then the forecasting method can be modified to give better forecasts.
# Adjusting for bias is easy: if the residuals have mean m, then simply add m to all forecasts and the bias problem is solved. 
# Fixing the correlation problem is harder, and we will not address it until Chapter 10.

# In addition to these essential properties, it is useful (but not necessary) for the residuals to also have the following two properties.
#   3. The residuals have constant variance.
#   4. The residuals are normally distributed.

# These two properties make the calculation of prediction intervals easier (see Section 5.5 for an example).
# However, a forecasting method that does not satisfy these properties cannot necessarily be improved.
# Sometimes applying a Box-Cox transformation may assist with these properties, but otherwise there is usually little that you can do to ensure that your residuals have constant variance and a normal distribution.
# Instead, an alternative approach to obtaining prediction intervals is necessary.

#** Example: Forecasting the Google daily closing stock price ---- 
# For stock market prices and indexes, the best forecasting method is often the naïve method.
# That is, each forecast is simply equal to the last observed value, or ^y[t] = y[t-1]
# Hence, the residuals are simply equal to the difference between consecutive observations:
#   e[t] = y[t] - ^y[t] = y[t] - y[t-1]

google_2015 %>% autoplot(Close) +
  labs(x = "Day", y = "Closing Price (US$)", title = "Google Stock in 2015")

# The large jump corresponds to 17 July 2015 when the price jumped 16% due to unexpectedly strong second quarter results.
aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()

aug %>%
  autoplot(.resid) +
  labs(x = "Day", y = "Residual", title = "Residuals from naïve method")
# The large positive residual is a result of the unexpected price jump in July.

aug %>%
  ggplot(aes(x = .resid)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")
# The right tail seems a little too long for a normal distribution.

aug %>%
  ACF(.resid) %>%
  autoplot() +
  labs(title = "ACF of residuals")
# The lack of correlation suggesting the forecasts are good. 

aug %>%
  filter(!is.na(.resid)) %>%
  filter(.resid != max(.resid)) %>%
  mean(.resid)

# These graphs show that the naïve method produces forecasts that appear to account for all available information.
# The mean of the residuals is close to zero and there is no significant correlation in the residuals series.
# The time plot of the residuals shows that the variation of the residuals stays much the same across the historical data, apart from the one outlier, and therefore the residual variance can be treated as constant.
# This can also be seen on the histogram of the residuals.
# The histogram suggests that the residuals may not be normal - the right tail seems a little too long, even when we ignore the outlier.
# Consequently, forecasts from this method will probably be quite good, but prediction intervals that are computed assuming a normal distribution may be inaccurate.

# A convenient shortcut for producing these residual diagnostic graphs is the
#   gg_tsresiduals() - will produce a time plot, ACF plot and histogram of the residuals.

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()


#** Portmanteau tests for autocorrelation ----
# In addition to looking at the ACF plot, we can also do a more formal test for autocorrelation by considering a whole set of rk values as a group, rather than treating each one separately.

# Recall that rk is the autocorrelation for lag k.
# When we look at the ACF plot to see whether each spike is within the required limits, we are implicitly carrying out multiple hypothesis tests, each one with a small probability of giving a false positive.
# When enough of these tests are done, it is likely that at least one will give a false positive, and so we may conclude that the residuals have some remaining autocorrelation, when in fact they do not.

# In order to overcome this problem, we test whether the first ℓ autocorrelations are significantly different from what would be expected from a white noise process.
# portmanteau test - A test for a group of autocorrelations, from a French word describing a suitcase or coat rack carrying several items of clothing.

# One such test is the Box-Pierce test, based on the following statistic
#   Q = T * [ℓ∑k=1]r[^2][k]

# whereℓis the maximum lag being considered and T is the number of observations.
#   If each rk is close to zero, then Q will be small.
#   If some rk values are large (positive or negative), then Q will be large.
# We suggest using ℓ=10 for non-seasonal data and ℓ=2m for seasonal data, where m is the period of seasonality.
# However, the test is not good when ℓ is large, so if these values are larger than T/5, then use ℓ=T/5

# A related (and more accurate) test is the Ljung-Box test, based on 
#   Q* = T(T+2)[ℓ∑k=1](T-k)[-1]r[^2][k]

# Again, large values of Q* suggest that the autocorrelations do not come from a white noise series.

# How large is too large?
#   If the autocorrelations did come from a white noise series, then both Q and Q* would have a χ2 distribution with (ℓ-K) degrees of freedom, where K is the number of parameters in the model.
#   If they are calculated from raw data (rather than the residuals from a model), then set K=0.

# For the Google stock price example, the naïve model has no parameters, so K=0 in that case also.
# lag=h and fitdf=K
aug %>% features(.resid, box_pierce, lag = 10, dof = 0)

aug %>% features(.resid, ljung_box, lag = 10, dof = 0)

# For both Q and Q*, the results are not significant (i.e., the p-values are relatively large).
# Thus, we can conclude that the residuals are not distinguishable from a white noise series.

# An alternative simple approach that may be appropriate for forecasting the Google daily closing stock price is the drift method. 
# tidy() - shows the one estimated parameter, the drift coefficient, measuring the average daily change observed in the historical data.

fit <- google_2015 %>% model(RW(Close ~ drift()))
fit %>% tidy()

# Applying the Ljung-Box test, we set K=1 to account for the estimated parameter.
augment(fit) %>% features(.resid, ljung_box, lag = 10, dof = 1)

# As with the naïve approach, the residuals from the drift method are indistinguishable from a white noise series.


# _----


#* 5.5 Distributional forecasts and prediction intervals -------------------
#** Forecast distributions ----
# As discussed in Section 1.7, we express the uncertainty in our forecasts using a probability distribution.
# It describes the probability of observing possible future values using the fitted model.
# The point forecast is the mean of this distribution.
# Most time series models produce normally distributed forecasts - that is, we assume that the distribution of possible future values follows a normal distribution.


#** Prediction intervals ----
# A prediction interval gives an interval within which we expect yt to lie with a specified probability.
#   example, assuming that distribution of future observations is normal, a 95% prediction interval for the h-step forecast is
#   ^y[T+h|T] ± 1.96^σ[h] where ^σh is an estimate of the standard deviation of the h-step forecast distribution.

# More generally, a prediction interval can be written as
#   ^y[T+h|T] ± c^σ[h]
# where the multiplier c depends on the coverage probability.
# In this book we usually calculate 80% intervals and 95% intervals, although any percentage may be used. 

# The value of prediction intervals is that they express the uncertainty in the forecasts.
# If we only produce point forecasts, there is no way of telling how accurate the forecasts are.
# However, if we also produce prediction intervals, then it is clear how much uncertainty is associated with each forecast.
# For this reason, point forecasts can be of almost no value without the accompanying prediction intervals.


#** One-step prediction intervals ----
# When forecasting one step ahead, the standard deviation of the forecast distribution is almost the same as the standard deviation of the residuals.
# (In fact, the two standard deviations are identical if there are no parameters to be estimated, as is the case with the naïve method.
# For forecasting methods involving parameters to be estimated, the standard deviation of the forecast distribution is slightly larger than the residual standard deviation, although this difference is often ignored.)


#** Multi-step prediction intervals ----
# A common feature of prediction intervals is that they increase in length as the forecast horizon increases.
# The further ahead we forecast, the more uncertainty is associated with the forecast, and thus the wider the prediction intervals.
# That is, σ[h] usually increases with h (although there are some non-linear forecasting methods that do not have this property).

# To produce a prediction interval, it is necessary to have an estimate of σ[h].
# As already noted, for one-step forecasts (h=1), the residual standard deviation provides a good estimate of the forecast standard deviation σ[1].
# For multi-step forecasts, a more complicated method of calculation is required.
# These calculations assume that the residuals are uncorrelated.

#** Benchmark methods ----
# For the four benchmark methods, it is possible to mathematically derive the forecast standard deviation under the assumption of uncorrelated residuals.
# If ^σ[h] denotes the standard deviation of the h-step forecast distribution, and ^σ is the residual standard deviation, then we can use the following expressions.
#   Mean forecasts:           ^σ[h] = ^σ * √(1+1/T)
#   Naïve forecasts:          ^σ[h] = ^σ * √h
#   Seasonal naïve forecasts: ^σ[h] = ^σ * √(k+1), where k is the integer part of (h-1)/m and m is the seasonal period.
#   Drift forecasts:          ^σ[h] = ^σ * √(h(1+h/T))

# Note that when h=1 and T is large, these all give the same approximate value ^σ.

# Prediction intervals can easily be computed for you when using the fable package. 

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo()

# hilo() - converts the forecast distributions into intervals.
# By default, 80% and 95% prediction intervals are returned, although other options are possible via the level argument.

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015)


#** Prediction intervals from bootstrapped residuals ----
# When a normal distribution for the residuals is an unreasonable assumption, one alternative is to use bootstrapping, which only assumes that the residuals are uncorrelated.

# A one-step forecast error is defined as
#   e[t] = y[t] - ^y[t|t-1].
# We can re-write this as
#   y[t] = ^y[t|t+1] + e[t].

# So we can simulate the next observation of a time series using
#   y[T+1] = ^y[T+1|T] + e[T+1]
# where ^y[T+1|T] is the one-step forecast and e[T+1] is a unknown future error.
# Assuming future errors will be similar to past errors, we can replace e[T+1] by sampling from the collection of errors we have seen in the past (i.e., the residuals).

# Adding the new simulated observation to our data set, we can repeat the process to obtain
#   y[T+2] = ^y[T+2|T+1] + e[T+2]
#     where e[T+2] is another draw from the collection of residuals.

# Continuing in this way, we can simulate an entire set of future values for our time series.

# Doing this repeatedly, we obtain many possible futures.

# generate() - To see some of them

fit <- google_2015 %>%
  model(NAIVE(Close))

sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)

# Here we have generated five possible sample paths for the next 30 trading days.
# The .rep variable provides a new key for the tsibble. The plot below shows the five sample paths along with the historical data.

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  labs(title = "Google closing stock price") +
  guides(col = FALSE)

# Then we can compute prediction intervals by calculating percentiles of the future sample paths for each forecast horizon.
# The result is called a bootstrapped prediction interval.
# The name "bootstrap" is a reference to pulling ourselves up by our bootstraps, because the process allows us to measure future uncertainty by only using the historical data.

# This is all built into the forecast() function so you do not need to call generate() directly.
fc <- fit %>% forecast(h = 30, bootstrap = TRUE)

# Notice that the forecast distribution is now represented as a simulation with 5000 sample paths.
# Because there is no normality assumption, the prediction intervals are not symmetric.
fc %>%
  autoplot(google_2015) +
  labs(title = "Google closing stock price")

# The number of samples can be controlled using the times argument for forecast().
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10, bootstrap = TRUE, times = 1000) %>%
  hilo()

# In this case, they are similar (but not identical) to the prediction intervals based on the normal distribution.


# _----


#* 5.6 Forecasting using transformations -----------------------------------
# When forecasting from a model with transformations, we first produce forecasts of the transformed data.
# Then, we need to reverse the transformation (or back-transform) to obtain forecasts on the original scale.
# The reverse Box-Cox transformation is given by
#        {exp(wt)                 if λ = 0;
#   yt = {sign(λwt+1)|λwt+1|^1/λ  otherwise.

# fable package will automatically back-transform the forecasts whenever a transformation has been used in the model definition. 
# The back-transformed forecast distribution is then a "transformed Normal" distribution.


#** Prediction intervals with transformations ----
# If a transformation has been used, then the prediction interval is first computed on the transformed scale, then the end points are back-transformed to give a prediction interval on the original scale.
# This approach preserves the probability coverage of the prediction interval, although it will no longer be symmetric around the point forecast.

# The back-transformation of prediction intervals is done automatically for fable models, provided that you have used a transformation in the model formula.

# Transformations sometimes make little difference to the point forecasts but have a large effect on prediction intervals.


#** Forecasting with constraints ----
# One common use of transformations is to ensure the forecasts remain on the appropriate scale.
#   example, log transformations constrain the forecasts to stay positive (because the back transformation is exp(wt)).

# Another useful transformation is the scaled logit, which can be used to ensure that the forecasts are kept within a specific interval.
# A scaled logit that ensures the forecasted values are between a and b (where a<b) is given by:
#   w[t] = f(y[t]) = log((y[t] - a)/(b - y[t]))

# Inverting this transformation gives the appropriate back-transformation of:
#   y[t] = (a + be[^wt]) / (1 + e[^wt]) = ((b-a)e[^wt] / 1+e[^wt]) +a

# To use this transformation when modelling, we can create a new transformation with the new_transformation() function, which is essentially a function factory.
# This allows us to define two functions that accept the same parameters, where the observations are provided as the first argument.
# The first the argument of new_transformation(), transformation shoule be a function taht is used to transform the data, the second inverse is used to back-transform forecasts:

scaled_logit <- new_transformation(
  transformation = function(x, lower = 0, upper = 1) {
    log(x - lower) / (upper - x)
  },
  inverse = function(x, lower = 0, upper = 1) {
    (upper - lower) * exp(x) / (1 + exp(x)) + lower
  }
)

# With this new tranformation function defined, it is now possible to restrict forecasts to be within a specific interval.
#   example, to restrict the forecasts to be between 0 and 100 you could use scaled_logit(y, 0, 100) as the model's left hand side formula.


#** Bias adjustments ----
# One issue with using mathematical transformations such as Box-Cox transformations is that the back-transformed point forecast will not be the mean of the forecast distribution.
# In fact, it will usually be the median of the forecast distribution (assuming that the distribution on the transformed space is symmetric).
# For many purposes, this is acceptable, but occasionally the mean forecast is required. For example, you may wish to add up sales forecasts from various regions to form a forecast for the whole country.
# But medians do not add up, whereas means do.

# For a Box-Cox transformation, the back-transformed mean is given (approximately) by
#             { exp(^w[T+h|T])[1+(σ[^2][h]/2)]                                      if λ=0
# ^y[T+h|T] = { (λ^w[T+h|T] + 1)[^1/λ][1 + (σ[^2][h](1−λ)/2(λ[^wT+h|T]+1)[^2])]     otherwise;

# where ^w[T+h|T] is the h-step forecast mean and σ[^2][h] is the h-step forecast variance on the transformed scale.
# The larger the forecast variance, the bigger the difference between the mean and the median.

# The difference between the simple back-transformed forecast given by (formula at the section) and the mean given by (here) is called the bias.
# When we use the mean, rather than the median, we say the point forecasts have been bias-adjusted.

# To see how much difference this bias-adjustment makes, consider the following example, where we forecast average annual price of eggs using the drift method with a log transformation (λ=0).
# The log transformation is useful in this case to ensure the forecasts and the prediction intervals stay positive.

prices %>%
  filter(!is.na(eggs)) %>%
  model(RW(log(eggs) ~ drift())) %>%
  forecast(h = 50) %>%
  autoplot(prices %>% filter(!is.na(eggs)), level = 80, point_forecast = lst(mean, median))

# Notice how the skewed forecast distribution pulls up the forecast distribution's mean, this is a result of the added term from the bias adjustment.

# Bias adjusted forecast means are automatically computed in the fable package when using mean() on a distribution.
# The forecast median (point forecast prior to bias adjustment) can be obtained using the median() function on the distribution.


# _----


#* 5.7 Forecasting with decomposition --------------------------------------
# Time series decomposition (discussed in Chapter 3) can be a useful step in producing forecasts.

# Assuming an additive decomposition, the decomposed time series can be written as
#   y[t] = ^S[t] + ^A[t], where ^A[t] = ^T[t] + ^R[t] is the seasonally adjusted component.
# Or, if a multiplicative decomposition has been used, we can write
#   y[t] = ^S[t]^A[t], where ^A[t] = ^T[t] * ^R[t]

# To forecast a decomposed time series, we forecast the seasonal component, ^St, and the seasonally adjusted component ^At, separately.
# It is usually assumed that the seasonal component is unchanging, or changing extremely slowly, so it is forecast by simply taking the last year of the estimated component.
# In other words, a seasonal naïve method is used for the seasonal component.

# To forecast the seasonally adjusted component, any non-seasonal forecasting method may be used.
#   example, a random walk with drift model, or Holt's method (discussed in Chapter 8), or a non-seasonal ARIMA model (discussed in Chapter 9), may be used.

#** Example: Employment in the US retail sector ----
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")

dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(y = "New orders index",
       title = "Naive forecasts of seasonally adjusted data")

# shows naïve forecasts of the seasonally adjusted electrical equipment orders data.
# These are then "reseasonalised" by adding in the seasonal naïve forecasts of the seasonal component.

# This is made easy with the decomposition_model() model function, which allows you to compute forecasts via any additive decomposition, using other model functions to forecast each of the decomposition's components.
# Seasonal components of the model will be forecasted automatically using SNAIVE() if a different model isn't specified.
# The function will also do the reseasonalising for you, ensuring that the resulting forecasts of the original data are shown in (in autoplot)

fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)


# The prediction intervals shown in this graph are constructed in the same way as the point forecasts.
# That is, the upper and lower limits of the prediction intervals on the seasonally adjusted data are "reseasonalised" by adding in the forecasts of the seasonal component.

# The ACF of the residuals shown in Figure, display significant autocorrelations.
# These are due to the naïve method not capturing the changing trend in the seasonally adjusted series.

fit_dcmp %>% gg_tsresiduals()


# _----


#* 5.8 Evaluating point forecast accuracy ----------------------------------
#** Training and test sets ----
# It is important to evaluate forecast accuracy using genuine forecasts.
# Consequently, the size of the residuals is not a reliable indication of how large true forecast errors are likely to be.
# The accuracy of forecasts can only be determined by considering how well a model performs on new data that were not used when fitting the model.

# When choosing models, it is common practice to separate the available data into two portions, training and test data, where the training data is used to estimate any parameters of a forecasting method and the test data is used to evaluate its accuracy. 
# Because the test data is not used in determining the forecasts, it should provide a reliable indication of how well the model is likely to forecast on new data.

# The size of the test set is typically about 20% of the total sample, although this value depends on how long the sample is and how far ahead you want to forecast.
# The test set should ideally be at least as large as the maximum forecast horizon required.
# The following points should be noted.
#   - A model which fits the training data well will not necessarily forecast well.
#   - A perfect fit can always be obtained by using a model with enough parameters.
#   - Over-fitting a model to data is just as bad as failing to identify a systematic pattern in the data.

# Some references describe the test set as the "hold-out set" because these data are "held out" of the data used for fitting.
# Other references call the training set the "in-sample data" and the test set the "out-of-sample data."
# We prefer to use "training data" and "test data" in this book.

#** Functions to subset a time series ----
# filter() - is useful when extracting a portion of a time series, such as we need when creating training and test sets.
# When splitting data into evaluation sets, filtering the index of the data is particularly useful.

aus_production %>% filter(year(Quarter) >= 1995)  # extracts all data from 1995 onward.

# It also allows extracting all values for a specific season.
aus_production %>% filter(quarter(Quarter) == 1)  # extracts the first quarters for all years.

# slice() - allows the use of indices to choose a subset from each group.
aus_production %>% slice(n()-19:0)  # extracts the last 20 observations (5 years).

# Slice also works with groups, making it possible to subset observations from each key.
aus_retail %>%
  group_by(State, Industry) %>%
  slice(1:12)                       # will subset the first year of data from each time series in the data.

# top_n() - is useful for extracting the most extreme observations.
gafa_stock %>%
  group_by(Symbol) %>%
  top_n(1, Close)                   # highest closing stock price for Google, Apple, Facebook and Amazon


#** Forecast errors ----
# A forecast "error" is the difference between an observed value and its forecast.
# Here "error" does not mean a mistake, it means the unpredictable part of an observation.
# It can be written as
#   e[T+h] = y[T+h] - ^y[T+h|T], where the training data is given by {y1,.,yT} and the test data is given by {y[T+1],y[T+2],.}.

# Note that forecast errors are different from residuals in two ways.
#   First, residuals are calculated on the training set while forecast errors are calculated on the test set.
#   Second, residuals are based on one-step forecasts while forecast errors can involve multi-step forecasts.

# We can measure forecast accuracy by summarising the forecast errors in different ways.


#** Scale-dependent errors ----
# forecast errors are on the same scale as the data.
# Accuracy measures that are based only on et are therefore scale-dependent and cannot be used to make comparisons between series that involve different units.

# The two most commonly used scale-dependent measures are based on the absolute errors or squared errors:
#   Mean absolute error:      MAE = mean(|e[t]|)
#   Root mean squared error:  RMSE = √mean(e[^2][t])

# When comparing forecast methods applied to a single time series, or to several time series with the same units, the MAE is popular as it is easy to both understand and compute.
# A forecast method that minimises the MAE will lead to forecasts of the median, while minimising the RMSE will lead to forecasts of the mean.
# Consequently, the RMSE is also widely used, despite being more difficult to interpret.


#** Percentage errors ----
# percentage error is given by pt = 100 * et/yt.
# Percentage errors have the advantage of being unit-free, and so are frequently used to compare forecast performances between data sets.
# The most commonly used measure is:
#   Mean absolute percentage error: MAPE = mean(|pt|)

# Measures based on percentage errors have the disadvantage of being infinite or undefined if yt=0 for any t in the period of interest, and having extreme values if any yt is close to zero.
# Another problem with percentage errors that is often overlooked is that they assume the unit of measurement has a meaningful zero.
#   That is, a percentage is valid on a ratio scale, but not on an interval scale.
#   Only ratio scale variables have meaningful zeros.

# They also have the disadvantage that they put a heavier penalty on negative errors than on positive errors.
# This observation led to the use of the so-called "symmetric" MAPE (sMAPE)
# It is defined by
#   sMAPE = mean(200|yt - ^yt|/(yt + ^yt))

# However, if yt is close to zero, ^yt is also likely to be close to zero.
# Thus, the measure still involves division by a number close to zero, making the calculation unstable.
# Also, the value of sMAPE can be negative, so it is not really a measure of "absolute percentage errors" at all.

# recommend that the sMAPE not be used. It is included here only because it is widely used, although we will not use it in this book.


#** Scaled errors ----
# alternative to using percentage errors when comparing forecast accuracy across series with different units.
# They proposed scaling the errors based on the training MAE from a simple forecast method.

# For a non-seasonal time series, a useful way to define a scaled error uses naïve forecasts:
#   qj = ej / (1 / ((T-1) * [T∑t=2]|y[t] - y[t-1]|))

# Because the numerator and denominator both involve values on the scale of the original data, qj is independent of the scale of the data.
# A scaled error is less than one if it arises from a better forecast than the average naïve forecast computed on the training data.
# Conversely, it is greater than one if the forecast is worse than the average naïve forecast computed on the training data.

# For seasonal time series, a scaled error can be defined using seasonal naïve forecasts:
#   qj = ej / (1 / ((T - m) [T∑t=m+1]|y[t] - y[t-m]|))

# The mean absolute scaled error is simply
#   MASE = mean(|qj|)
# Similarly, the root mean squared scaled error is given by
#   RMSSE = √mean(q[^2][j]),, where q[^2][j] = e[^2][j] (1 / ((T-m)[T∑t=m+1](y[t] - y[t-m])[^2])), and we set m=1 for non-seasonal data.


#** Examples ----
recent_production <- aus_production %>% filter(year(Quarter) >= 1992)

beer_train <- recent_production %>% filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(y = "Megalitres", title = "Forecasts for quarterly beer production") +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)

# It is obvious from the graph that the seasonal naïve method is best for these data, although it can still be improved, as we will discover later.
# Sometimes, different accuracy measures will lead to different results as to which forecast method is best.
# However, in this case, all of the results point to the seasonal naïve method as the best of these three methods for this data set.

# To take a non-seasonal example, consider the Google stock price.
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit %>%
  forecast(google_jan_2016)

google_fc %>%
  autoplot(bind_rows(google_2015, google_jan_2016), level = NULL) +
  labs(x = "Day", y = "Closing Price (US$)",
       title = "Google stock price (daily ending 6 Dec 13)") +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(google_fc, google_stock)

# Here, the best method is the naïve method (regardless of which accuracy measure is used).


# _----


#* 5.9 Evaluating distributional forecast accuracy -------------------------
# When evaluating distributional forecasts, we need to use some other measures.

#** Quantile scores ----
google_fc %>%
  filter(.model == "Naïve") %>%
  autoplot(bind_rows(google_2015, google_jan_2016), level = 80)

# The lower limit of this prediction interval gives the 10th percentile (or 0.1 quantile) of the forecast distribution, so we would expect the actual value to lie below the lower limit about 10% of the time, and to lie above the lower limit about 90% of the time.
# When we compare the actual value to this percentile, we need to allow for the fact that it is more likely to be above than below.

# More generally, suppose we are interested in the quantile forecast with probability p at future time t, and let this be denoted by fp,t.
# That is, we expect the observation at time t to be less than fp,t with probability p.
#   example, the 10th percentile would be f0.10,t.
# If yt denotes the observation at time t, then the Quantile Score is
#             { 2(1-p)(f[p,t] - y[t]),    if yt < f[p,t]
#   Q[p,t] =  { 2p(y[t] - f[p,t]),        if yt ≥ f[p,t]

# This is sometimes called the "pinball loss function" because a graph of it resembles the trajectory of a ball on a pinball table.
# The multiplier of 2 is often omitted, but including it makes the interpretation a little easier.
# A low value of Qp,t indicates a better estimate of the quantile.

# The quantile score can be interpreted like an absolute error.
# In fact, when p=0.5, the quantile score Q0.5,t is the same as the absolute error.
# For other values of p, the "error" (y[t] - f[p,t]) is weighted to take account of how likely it is be positive or negative.
# If p>0.5, Qp,t gives a heavier penalty when the observation is greater than the estimated quantile than when the observation is less than the estimated quantile.
# The reverse is true for p<0.5.

# In Figure (google_fc), the one-step-ahead 10% quantile forecast (for 4 January 2016) is f[0.1,t] = 745 and the observed value is yt=742.
# Then
#   Q[0.1,t] = 2(1 - 0.1)(744.54 - 741.84) = 4.86.
# This is easily computed using accuracy with the quantile_score function:
google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(qs = quantile_score), probs = 0.10)


#** Winkler Score ----
# It is often of interest to evaluate a prediction interval, rather than a few quantiles, and the Winkler score is designed for this purpose.
# If the 100(1 − α)% prediction interval at time t is given by [ℓ[α,t], u[α,t]], then the Winkler score is defined as the length of the interval plus a penalty if the observation is outside the interval:
#             { u[α,t] - ℓ[α,t]) + (2/α)(ℓ[α,t] - y[t])   if y[t] < ℓ[α,t]
#   W[α,t] =  { (u[α,t] - ℓ[α,t])                         if ℓ[α,t] ≤ yt ≤ u[α,t]
#             { (u[α,t] - ℓ[α,t]) + (2/α)(y[t] - u[α,t])  if y[t] > u[α,t]

# For observations that fall within the interval, the Winkler score is simply the length of the interval.
# So low scores are associated with narrow intervals.
# However, if the observation falls outside the interval, the penalty applies, with the penalty proportional to how far the observation is outside the interval.

# Prediction intervals are usually constructed from quantiles by setting ℓ[α,t] = f[α/2,t] and u[α,t] = f[1-α/2,t].
# If we add the corresponding quantile scores and divide by α, we get the Winkler score:
#   W[α,t] = (Q[α/2,t] + Q[1-α/2,t]) / α

# The one-step-ahead 80% interval shown in Figure (google_fc) for 4 January 2016 is [744.54, 773.22], and the actual value was 741.84, so the Winkler score is
#   W[α,t] = (773.22 - 744.54) + (2/0.2)(744.54 - 741.84) = 55.68.

# This is easily computed using accuracy with the winkler_score function:
google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(winkler = winkler_score), level = 80)


#** Continuous Ranked Probability Score ----
# Often we are interested in the whole forecasting distribution, rather than particular quantiles or prediction intervals.
# In that case, we can average the quantile scores over all values of p to obtain the Continuous Ranked Probability Score or CRPS

# In the Google stock price example, we can compute the average CRPS value for all days in the test set.
# A CRPS value is a little like a weighted absolute error computed from the entire forecast distribution, where the weighting takes account of the probabilities.
google_fc %>%
  accuracy(google_stock, list(crps = CRPS))

# Here, the Naïve method is giving better distributional forecasts than the Drift or Mean methods.


#** Scale-free comparisons using skill scores ----
# As with point forecasts, it is useful to be able to compare the distributional forecast accuracy of several methods across series on different scales.
# For point forecasts, we used scaled errors for that purpose.
# Another approach is to use skill scores.
#   These can be used for both point forecast accuracy and distributional forecast accuracy.

# With skill scores, we compute a forecast accuracy measure relative to some benchmark method.
#   example, if we use the Naïve method as a benchmark, and also compute forecasts using the Drift method, we can compute the CRPS skill score of the Drift method relative to the Naïve method as
#   (CRPS[N] - CRPS[D]) / CRPS[N]
# This gives the proportion that the Drift method improves over the Naïve method based on CRPS.

# It is easy to compute using the accuracy() function.
google_fc %>%
  accuracy(
    google_stock %>% filter(year(Date) >= 2015),
    list(skill = skill_score(CRPS))
  )

# Of course, the skill score for Naïve is 0 because it can't improve on itself.
# The other two methods have larger CRPS values than Naïve, so the skills scores are negative; the Drift method is 26.6% worse than the Naïve method.

# When the data are seasonal, skill_score() will use a Seasonal Naïve benchmark method rather than the Naïve benchmark.
# This will work even when the benchmark forecast is not included in the fable object as skill_score() computes the benchmark forecasts that are needed.
# It is important that the data provided to accuracy() include only the training and test data to ensure the same training data are used for the benchmark forecasts.
#   If we had not filtered the google_stock data to remove data prior to 2015, the benchmark CRPS would have used all available data, and then the skill value for Naïve would not have been 0 as the CRPS values would have been computed on two different training sets.

# The skill_score() function can be used with any accuracy measure.
#   example, skill_score(MSE) provides a way of comparing MSE values across diverse series.
# However, it is important that the test set is large enough to allow reliable calculation of the error measure, especially in the denominator.
# For that reason, MASE or RMSSE are often preferable scale-free measures for point forecast accuracy.


# _----


#* 5.10 Time series cross-validation ----
# A more sophisticated version of training/test sets is time series cross-validation.
# In this procedure, there are a series of test sets, each consisting of a single observation.
# The corresponding training set consists only of observations that occurred prior to the observation that forms the test set.
# Thus, no future observations can be used in constructing the forecast.
# Since it is not possible to obtain a reliable forecast based on a small training set, the earliest observations are not considered as test sets.

# The forecast accuracy is computed by averaging over the test sets.

# This procedure is sometimes known as "evaluation on a rolling forecasting origin" because the "origin" at which the forecast is based rolls forward in time.

# With time series forecasting, one-step forecasts may not be as relevant as multi-step forecasts.
# In this case, the cross-validation procedure based on a rolling forecasting origin can be modified to allow multi-step errors to be used.

# In the following example, we compare the accuracy obtained via time series cross-validation with the residual accuracy.
# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = 3, .step = 1)

fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1)

fc %>% accuracy(google_2015)

# Residual accuracy
google_2015 %>%
  model(RW(Close ~ drift())) %>%
  accuracy()

# As expected, the accuracy measures from the residuals are smaller, as the corresponding "forecasts" are based on a model fitted to the entire data set, rather than being true forecasts.

# A good way to choose the best forecasting model is to find the model with the smallest RMSE computed using time series cross-validation.


#** Example: Forecast horizon accuracy with cross-validation ----
# The code below evaluates the forecasting performance of 1- to 8-step-ahead drift forecasts.
# The plot shows that the forecast error increases as the forecast horizon increases, as we would expect.
google_2015_tr <- google_2015 %>%
  slice(1:(n()-8)) %>%
  stretch_tsibble(.init = 3, .step = 1)

fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()

fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()


# _----