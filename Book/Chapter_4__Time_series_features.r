# Chapter 4 Time series features ----
# time series features are numerical summaries computed from the series.
#   e.g. autocorrelations, Guerrero estimate
# We can compute many different features on many different time series, and use them to explore the properties of the series.


#* 4.1 Some simple statistics ----
# Any numerical summary computed from a time series is a feature of that time series
#   example - the mean, minimum or maximum

# features() - compute the features

tourism %>% features(Trips, mean)

# It is useful to give the resulting feature columns names to help us remember where they came from.
# This can be done by using a list of functions.
tourism %>% features(Trips, list(mean=mean)) %>% arrange(mean)

# Rather than compute one feature at a time, it is convenient to compute many features at once.
# A common short summary of a data set is to compute five summary statistics:
#  the minimum, 
#  first quartile, 
#  median, 
#  third quartile and 
#  maximum.
# These divide the data into four equal-size sections, each containing 25% of the data.
# The quantile() function can be used to compute them.

tourism %>% features(Trips, quantile, prob = seq(0, 1, by = 0.25))
# tourism %>% features(Trips, quantile)   # gives the same result

#_ ----


#* 4.2 ACF features ----
# All the autocorrelations of a series can be considered features of that series.
# We can also summarise the autocorrelations to produce new features
#   example, the sum of the first ten squared autocorrelation coefficients is a useful summary of how much autocorrelation there is in a series, regardless of lag.

# We can also compute autocorrelations of transformations of a time series.
# A useful transformation in this context is to look at changes in the series between periods.
# That is, we "difference" the data and create a new time series consisting of the differences between consecutive observations.
# Then we can compute the autocorrelations of this new differenced series.

# Occasionally it is useful to apply the same differencing operation again, so we compute the differences of the differences.
# The autocorrelations of this double differenced series may provide useful information.

# Another related approach is to compute seasonal differences of a series.
#   If we had monthly data, for example, we would compute the difference between consecutive Januaries, consecutive Februaries, and so on.
# This enables us to look at how the series is changing between years, rather than between months.
# Again, the autocorrelations of the seasonally differenced series may provide useful information.

# feat_acf() - computes a selection of the autocorrelations discussed here.
#   It will return six or seven features:
#     the first autocorrelation coefficient from the original data;
#     the sum of square of the first ten autocorrelation coefficients from the original data;
#     the first autocorrelation coefficient from the differenced data;
#     the sum of square of the first ten autocorrelation coefficients from the differenced data;
#     the first autocorrelation coefficient from the twice differenced data;
#     the sum of square of the first ten autocorrelation coefficients from the twice differenced data;
#     For seasonal data, the autocorrelation coefficient at the first seasonal lag is also returned.

tourism %>% features(Trips, feat_acf)

# _----


#* 4.3 STL Features ----
# For strongly trended data, the seasonally adjusted data should have much more variation than the remainder component.
# Therefore Var(R[t])/Var(T[t] + R[t]) should be relatively small.
# But for data with little or no trend, the two variances should be approximately the same.

# So we define the strength of trend as: 
#   F[T] = max(0, 1 - (Var(R[t]) / Var(T[t] + R[t])))
# This will give a measure of the strength of the trend between 0 and 1.
# Because the variance of the remainder might occasionally be even larger than the variance of the seasonally adjusted data, we set the minimal possible value of FT equal to zero.

# The strength of seasonality is defined similarly, but with respect to the detrended data rather than the seasonally adjusted data:
#   F[S] = max(0, 1 - (Var(R[t]) / Var(S[t] + R[t])))
# A series with seasonal strength FS close to 0 exhibits almost no seasonality, while a series with strong seasonality will have FS close to 1 because Var(Rt) will be much smaller than Var(St+Rt).

# These measures can be useful, for example, when you have a large collection of time series, and you need to find the series with the most trend or the most seasonality.

# Other useful features based on STL include the timing of peaks and troughs - which month or quarter contains the largest seasonal component and which contains the smallest seasonal component.
# This tells us something about the nature of the seasonality.

# feat_stl() - compute these STL-based features

# We can then use these features in plots to identify what type of series are heavily trended and what are most seasonal.

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Purpose)) +
  geom_point() + facet_wrap(vars(State))
# Clearly, holiday series are most seasonal which is unsurprising.
# The strongest trends tend to be in Western Australia.

# The most seasonal series can also be easily identified and plotted.
tourism %>%
  features(Trips, feat_stl) %>%
  filter(seasonal_strength_year == max(seasonal_strength_year)) %>%
  left_join(tourism, by = c("State","Region","Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) + geom_line() +
  facet_grid(vars(State,Region,Purpose))
# This shows holiday trips to the most popular ski region of Australia.

# feat_stl() function returns several more features other than those discussed above.
#   spikiness - measures the prevalence of spikes in the remainder component Rt of the STL decomposition. It is the variance of the leave-one-out variances of Rt.
# linearity - measures the linearity of the trend component of the STL decomposition. It is based on the coefficient of a linear regression applied to the trend component.
# curvature - measures the curvature of the trend component of the STL decomposition. It is based on the coefficient from an orthogonal quadratic regression applied to the trend component.
# stl_e_acf1 - is the first autocorrelation coefficient of the remainder series.
# stl_e_acf10 - is the sum of squares of the first ten autocorrelation coefficients of the remainder series.

# _----


#* 4.4 Other features ----
# remaining features in the feasts package
#   coef_hurst - will calculate the Hurst coefficient of a time series which is a measure of "long memory." A series with long memory will have significant autocorrelations for many lags.
#   feat_spectral - will compute the (Shannon) spectral entropy of a time series, which is a measure of how easy the series is to forecast. A series which has strong trend and seasonality (and so is easy to forecast) will have entropy close to 0. A series that is very noisy (and so is difficult to forecast) will have entropy close to 1.
#   box_pierce - gives the Box-Pierce statistic for testing if a time series is white noise, and the corresponding p-value. discussed in Section 5.4.
#   ljung_box gives the Ljung-Box statistic for testing if a time series is white noise, and the corresponding p-value. discussed in Section 5.4.
#   The kth partial autocorrelations - measure the relationship between observations k periods apart after removing the effects of observations between them. So the first partial autocorrelation (k=1) is identical to the first autocorrelation, because there is nothing between them to remove. The feat_pacf function contains several features involving partial autocorrelations including the sum of squares of the first five partial autocorrelations for the original series, the first-differenced series and the second-differenced series. For seasonal data, it also includes the partial autocorrelation at the first seasonal lag. discussed in Section 9.5.
#   unitroot_kpss - gives the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) statistic for testing if a series is stationary, and the corresponding p-value. discussed in Section 9.1.
#   unitroot_pp - gives the Phillips-Perron statistic for testing if a series is non-stationary, and the corresponding p-value.
#   unitroot_ndiffs - gives the number of differences required to lead to a stationary series based on the KPSS test. discussed in Section 9.1
#   unitroot_nsdiffs - gives the number of seasonal differences required to make a series stationary. discussed in Section 9.1
#   var_tiled_mean - gives the variances of the "tiled means" (i.e., the means of consecutive non-overlapping blocks of observations). The default tile length is either 10 (for non-seasonal data) or the length of the seasonal period. This is sometimes called the "stability" feature.
#   var_tiled_var - gives the variances of the "tiled variances" (i.e., the variances of consecutive non-overlapping blocks of observations). This is sometimes called the "lumpiness" feature.
#   shift_level_max - finds the largest mean shift between two consecutive sliding windows of the time series. This is useful for finding sudden jumps or drops in a time series.
#   shift_level_index - gives the index at which the largest mean shift occurs.
#   shift_var_max - finds the largest variance shift between two consecutive sliding windows of the time series. This is useful for finding sudden changes in the volatility of a time series.
#   shift_var_index - gives the index at which the largest mean shift occurs
#   shift_kl_max - finds the largest distributional shift (based on the Kulback-Leibler divergence) between two consecutive sliding windows of the time series. This is useful for finding sudden changes in the distribution of a time series.
#   shift_kl_index - gives the index at which the largest KL shift occurs.
#   n_crossing_points - computes the number of times a time series crosses the median.
#   longest_flat_spot - computes the number of sections of the data where the series is relatively unchanging.
#   stat_arch_lm - returns the statistic based on the Lagrange Multiplier (LM) test of Engle (1982) for autoregressive conditional heteroscedasticity (ARCH).
#   guerrero - computes the optimal λ value for a Box-Cox transformation using the Guerrero method (discussed in Section 3.1).

# _----


#* 4.5 Exploring Australian tourism data ----
# All of the features included in the feasts package can be computed in one line like this.

tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs="feasts"))
tourism_features
# This gives 48 features for every combination of the three key variables (Region, State and Purpose).

# We can treat this tibble like any data set and analyse it find interesting observations or groups of observations.

tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = glue::glue("Q{seasonal_peak_year+1}"),
    seasonal_trough_year = glue::glue("Q{seasonal_trough_year+1}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Purpose))

# It is difficult to explore more than a handful of variables in this way.
# A useful way to handle many more variables is to use a dimension reduction technique such as principal components.
# This gives linear combinations of variables that explain the most variation in the original data.

# We can compute the principal components of the tourism features as follows.

pcs <- tourism_features %>%
  select(-State, -Region, -Purpose) %>%
  prcomp(scale = TRUE) %>%
  augment(tourism_features)

pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() + theme(aspect.ratio = 1)

# Each point on Figure represents one series and its location on the plot is based on all 48 features.
# The first principal component (.fittedPC1) is the linear combination of the features which explains the most variation in the data.
# The second principal component (.fittedPC2) is the linear combination which explains the next most variation in the data, while being uncorrelated with the first principal component. 

# Figure reveals a few things about the tourism data.
# First, the holiday series behave quite differently from the rest of the series.
# Almost all of the holiday series appear in the top half of the plot, while almost all of the remaining series appear in the bottom half of the plot.
# Clearly, the second principal component is distinguishing between holidays and other types of travel.

# The plot also allows us to identify anomalous time series - series which have unusual feature combinations.
# These appear as points that are separate from the majority of series in Figure.
# There are three which stand out, and we can identify which series they correspond to as follows.

outliers <- pcs %>%
  filter(.fittedPC1 > 10.5) %>%
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)

outliers %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  mutate(Series = glue::glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = 'free') +
  ggtitle("Outlying time series in PC space")

# _----

# ____----