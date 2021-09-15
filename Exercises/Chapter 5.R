#* 1 ----
# Produce forecasts for the following series using whichever of NAIVE(y), SNAIVE(y) or RW(y ~ drift()) is more appropriate in each case:
#    Australian Population (global_economy)
#    Bricks (aus_production)
#    NSW Lambs (aus_livestock)
#    Household wealth (hh_budget).
#    Australian takeaway food turnover (aus_retail).


#*** Australian Population (global_economy) ----
global_economy_aus_tr <- global_economy %>%
  filter(Code == "AUS") %>%
  slice(1 : (n() - 5))

fit <- global_economy_aus_tr %>%
  model(
    Naive = NAIVE(Population),
    Drift = RW(Population ~ drift())
  )
# SNaive not possible as it is annual data

fc <- fit %>%
  forecast(h = 5)

fc %>%
  autoplot(global_economy, level = NULL) +
  ggtitle("Population Forecast")

# Drift seems to be the best

fc %>%
  accuracy(global_economy %>% filter(Code == "AUS"))

# All error terms show Drift to be the best


#*** Bricks (aus_production) ----
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production %>% filter(!is.na(Bricks)) %>% tail()

recent_production_tr <- recent_production %>% filter(year(Quarter) <= 2003)

bricks_fit <- recent_production_tr %>%
  model(
    Naive = NAIVE(Bricks),
    sNaive = SNAIVE(Bricks),
    Drift = RW(Bricks ~ drift())
  )

bricks_fc <- bricks_fit %>%
  forecast(h = 10)

bricks_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(x = "Year", y = "Millions of bricks", title = "Forecast for quarterly bricks production") +
  guides(color = guide_legend(title = "Forecast"))

accuracy(bricks_fc, recent_production)

# SNaive is the best model


#*** NSW Lambs (aus_livestock) ----
recent_count <- aus_livestock %>%
  filter(Animal == "Lambs", State == "New South Wales") %>%
  filter(year(Month) >= 2000)

recent_count_tr <- recent_count %>%
  filter(year(Month) <= 2017)

nsw_lamb_fit <- recent_count_tr %>%
  model(
    Naive = NAIVE(Count),
    sNaive = SNAIVE(Count),
    Drift = RW(Count ~ drift())
  )

nsw_lamb_fc <- nsw_lamb_fit %>%
  forecast(h = 12)

nsw_lamb_fc %>%
  autoplot(recent_count, level = NULL)

# No forecast seems to be good.

accuracy(nsw_lamb_fc, recent_count)
# Between the 3 the SNaive has the lowest error


#*** Household wealth (hh_budget) ----
wealth_tr <- hh_budget %>%
  filter(Country == "Australia", Year <= 2015)

wealth_fit <- wealth_tr %>%
  model(
    Naive = NAIVE(Wealth),
    Drift = RW(Wealth ~ drift())
  )
# SNaive not possible as it is annual data

wealth_fc <- wealth_fit %>%
  forecast(h = 12)

wealth_fc %>%
  autoplot(wealth_tr, level = NULL)

accuracy(wealth_fc, hh_budget)

# Drift seems to be better


#*** Australian takeaway food turnover (aus_retail) ----
aus_retail_food <- aus_retail %>%
  filter(Industry == "Takeaway food services") %>%
  index_by() %>%
  group_by(Industry) %>%
  summarise(Turnover = sum(Turnover)) %>%
  ungroup()

food_tr <- aus_retail_food %>%
  filter(year(Month) <= 2017)

food_fit <- food_tr %>%
  model(
    Naive = NAIVE(Turnover),
    SNaive = SNAIVE(Turnover),
    Drift = RW(Turnover ~ drift())
  )

food_fc <- food_fit %>%
  forecast(h = 12)

food_fc %>%
  autoplot(food_tr, level = NULL)

accuracy(food_fc, aus_retail_food)

#SNaive has the better forecast


#* 2 ----
# Use the Facebook stock price (data set gafa_stock) to do the following:

fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

fb_tr <- fb_stock %>%
  filter(year(Date) <= 2017)

fb_test <- fb_stock %>%
  filter(yearmonth(Date) == yearmonth("2018 Jan"))


#** 2.a ----
# Produce a time plot of the series.

fb_stock %>% autoplot(Close)


#** 2.b ----
# Produce forecasts using the drift method and plot them.

fb_fit <- fb_tr %>%
  model(Drift = RW(Close ~ drift()))

fb_fc <- fb_fit %>%
  forecast(h = 12)

fb_fc %>%
  autoplot(fb_tr)


#** 2.c ----
# Show that the forecasts are identical to extending the line drawn between the first and last observations.

p <- as.data.frame(last(fb_tr$day) + 12)
colnames(p) <- "day"

fb_lm <- fb_tr %>%
  filter(day %in% c(first(fb_tr$day), last(fb_tr$day))) %>%
  lm(Close ~ day, .) %>%
  predict(newdata = p)

autoplot(fb_fc, level = NULL) +
  geom_line(
    aes(
      x = c(1, last(fb_tr$day) + 12),
      y = c(fb_tr$Close[1], fb_lm[[1]]),
      # zoom into only the forecast range
      # x = c(last(fb_tr$day), last(fb_tr$day) + 12),
      # y = c(fb_tr$Close[last(fb_tr$day)], fb_lm[[1]]),
      color = "red"
    )
  )


#** 2.d ----
# Try using some of the other benchmark functions to forecast the same data set.
# Which do you think is best?
# Why?

fb_fit <- fb_tr %>%
  filter(year(Date) == 2017) %>%
  model(
    Naive = NAIVE(Close),
    SNaive = SNAIVE(Close ~ lag(30)),
    Drift = RW(Close ~ drift())
  )

fb_fc <- fb_fit %>%
  forecast(new_data = fb_test)

fb_fc %>%
  autoplot(fb_tr %>% filter(year(Date) == 2017), level = NULL)

fb_fc %>%
  autoplot(
    fb_stock %>% filter(year(Date) >= 2017, yearmonth(Date) < yearmonth("2018 Feb")),
    level = NULL
  )

accuracy(fb_fc, fb_stock)

# drift method seems to be better
# SNaive seems to be better than Naive but this is based on adjusting the lag parameter.Current setting might be limited to present test set.
# therefore, Naive might be better


#* 3 ----
# Apply a seasonal naïve method to the quarterly Australian beer production data from 1992.
# Check if the residuals look like white noise, and plot the forecasts.
# The following code will help.
#   # Extract data of interest
#       recent_production <- aus_production %>%
#           filter(year(Quarter) >= 1992)
#   # Define and estimate a model
#       fit <- recent_production %>% model(SNAIVE(Beer))
#   # Look at the residuals
#       fit %>% gg_tsresiduals()
#   # Look a some forecasts
#       fit %>% forecast() %>% autoplot(recent_production)
# What do you conclude?

aus_beer <- aus_production %>%
  filter(year(Quarter) >= 1992)

beer_tr <- aus_beer %>%
  filter(year(Quarter) < 2009)

beer_test <- aus_beer %>%
  filter(year(Quarter) >= 2009)

beer_fit <- beer_tr %>%
  model(SNaive = SNAIVE(Beer))

beer_fit %>% gg_tsresiduals()

# method 1
beer_fc <- beer_fit %>%
  forecast(beer_test)

beer_fc %>% autoplot(beer_tr)

# alternate
beer_fit %>%
  forecast() %>%
  autoplot(aus_beer)

# the residuals have near zero mean.
# the histogram also show a near normal distribution. there is a slightly longer left tail.
# acf plot has only 1 spike outside the limits and therefore there seems to be a lack corelation suggesting good forecast.
# these suggest that the residuals look like white noise.

accuracy(beer_fc, aus_beer)

# the forecast is good


#* 4 ----
# Repeat the previous exercise using the Australian Exports series from global_economy and the Bricks series from aus_production.
# Use whichever of NAIVE() or SNAIVE() is more appropriate in each case.

#** AUS Exports ----
aus_exports <- global_economy %>%
  filter(Code == "AUS")

exports_tr <- aus_exports %>%
  filter(between(Year, 1982, 2010))

exports_test <- aus_exports %>%
  filter(Year > 2010)

exports_fit <- exports_tr %>%
  model(
    Naive = NAIVE(Exports),
    Drift = RW(Exports ~ drift())
  )
# SNaive not possible as it is annual data

exports_fit %>%
  select(Country, Naive) %>%
  gg_tsresiduals()
# These graphs show that the naive method produces foreecasts that appear to account for all available information.
# The mean of the residuals is close to zero and there is no significant correlation in the residuals series.
# The time plot of the residuals shows that the variation of the residuals stays much the same across historical data, and therefore the residual variance can be treated as constant.
# This can also be seen on the histogram of the residuals.
# Consequently, forecasts from this method will probably be quite good.

exports_fit %>%
  select(Country, Drift) %>%
  gg_tsresiduals()
# These graphs show that the drift method produces foreecasts that appear to account for all available information.
# The mean of the residuals is close to zero and there is no significant correlation in the residuals series.
# The time plot of the residuals shows that the variation of the residuals stays much the same across historical data, and therefore the residual variance can be treated as constant.
# This can also be seen on the histogram of the residuals.
# Consequently, forecasts from this method will probably be quite good.

exports_fit %>%
  forecast(exports_test) %>%
  autoplot(aus_exports, level = NULL)

accuracy(exports_fit %>% forecast(exports_test), aus_exports)
# The drift method seems to be give a slightly better forecast.


#** Bricks ----
aus_bricks <- aus_production %>%
  filter(year(Quarter) >= 1983)

bricks_tr <- aus_bricks %>%
  filter(year(Quarter) < 2004)

bricks_test <- aus_bricks %>%
  filter_index("2004 Q1" ~ "2005 Q2")

bricks_fit <- bricks_tr %>%
  model(
    Naive = NAIVE(Bricks),
    SNaive = SNAIVE(Bricks),
    Drift = RW(Bricks ~ drift())
  )

bricks_fit %>% select(Naive) %>% gg_tsresiduals()
# residuals have a mean above zero.
# the histogram also show a near normal distribution.
# acf plot shows significant correlation, suggesting there is information left in the residuals which should be used in computing forcasts.

bricks_fit %>% select(SNaive) %>% gg_tsresiduals()
# residuals have a mean above zero.
# the histogram has a slightly longer left tail.
# acf plot shows significant correlation, suggesting there is information left in the residuals which should be used in computing forcasts.

bricks_fit %>% select(Drift) %>% gg_tsresiduals()
# residuals have a mean above zero.
# the histogram has a slightly longer left tail.
# acf plot shows significant correlation, suggesting there is information left in the residuals which should be used in computing forcasts.


# based on the residual plots of all 3 methods there is indication of information left in the residuals 

bricks_fc <- bricks_fit %>%
  forecast(bricks_test)

bricks_fc %>% autoplot(bricks_tr, level = NULL)

bricks_fc %>% autoplot(aus_bricks %>% filter(Quarter <= yearquarter("2005 Q2")), leveel = NULL)

accuracy(bricks_fc, aus_bricks)

# SNaive seems to be better forecast. However, there is a visible lag between the test and the forecast data. Hence, the model can be improved.


#* 5 ----
# Produce forecasts for the 7 Victorian series in aus_livestock using SNAIVE().
# Plot the resulting forecasts including the historical data.
# Is this a reasonable benchmark for these series?

lifestock <- aus_livestock %>%
  group_by(Animal) %>%
  index_by() %>%
  summarise(total_count = sum(Count)) %>%
  ungroup()

lifestock_tr <- lifestock %>%
  filter(year(Month) < 2018)

lifestock_test <- lifestock %>%
  filter(year(Month) == 2018)

lifestock_tr %>% autoplot(total_count)

lifestock_fit <- lifestock_tr %>%
  model(SNAIVE(total_count))

lifestock_fc <- lifestock_fit %>%
  forecast(lifestock_test)

lifestock_fc %>%
  # filter(str_detect(Animal, "Cows")) %>%
  autoplot(lifestock, level = NULL) +
  facet_wrap(vars(Animal))

# Cattle, cows, lambs, sheep have a slight deviation in their actual and forecast values

accuracy(lifestock_fc, lifestock)
# Cows, Lambs have large errors
# Cattle, Pigs, Sheeps have deviations between forecast and actual but are reasonable for benchmark
# based on RMSSE


#* 6 ----
# Are the following statements true or false?
# Explain your answer.

#** 6.a ----
# Good forecast methods should have normally distributed residuals.

#   False, normally distributed residuals make calculating prediction intervals easier. However, they do not neccessarily improve the forecast itself.


#** 6.b ---- 
# A model with small residuals will give good forecasts.

#   False, residuals are the difference between fitted and actual values. They are based on the train data. So overfitted models give small residuals but the forecast might be bad.


#** 6.c ----
# The best measure of forecast accuracy is MAPE.

#   False, MAPE is a good measure but not the best. It has issues like
#     - assumes data has an absolute zero
#     - heavier penalty for negative zeros than positive
#     - is infinite/very high if the actual value is equal or close to 0


#** 6.d ----
# If your model doesn’t forecast well, you should make it more complicated.

#   True, if forecasts are not good might need to perform transformations on the data or using more complicated models.


#** 6.e ----
# Always choose the model with the best forecast accuracy as measured on the test set.

#   True, however, there is a possibility that the best model can be further improved.


#* 7 ----
# For your retail time series (from Exercise 8 in Section 2.10):

# Q2.10 - 6. A new question (Q3 usgas) was added later leading to down shift of the Q#
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))


#** 7.a ----
# Create a training dataset consisting of observations before 2011 using
#   myseries_train <- myseries %>%
#       filter(year(Month) < 2011)

myseries_tr <- myseries %>%
  filter(Month <= yearmonth("2010 Dec"))


#** 7.b ----
# Check that your data have been split appropriately by producing the following plot.
# autoplot(myseries, Turnover) +
#   autolayer(myseries_train, Turnover, colour = "red")

autoplot(myseries, Turnover) +
  autolayer(myseries_tr, Turnover, color = "red")


#** 7.c ----
# Fit a seasonal naïve model using SNAIVE() applied to your training data (myseries_train).
#   fit <- myseries_train %>%
#       model(SNAIVE())
# Produce forecasts for the test data
#   fc <- fit %>%
#       forecast(new_data = anti_join(myseries, myseries_train))
#   fc %>% autoplot(myseries)

fit <- myseries_tr %>%
  model(SNAIVE(Turnover))

fc <- fit %>%
  forecast()

fc %>%
  autoplot(myseries_tr, level = NULL)

fc %>%
  autoplot(myseries, level = NULL)


#** 7.d ----
# Compare the accuracy of your forecasts against the actual values.
# fit %>% accuracy()
#   fc %>% accuracy(myseries)

fit %>% accuracy()
fc %>% accuracy(myseries)

# accuracy of the forceast is slightly better than the fitted accuracy


#** 7.e ----
# Check the residuals.
#   fit %>% gg_tsresiduals()

fit %>% gg_tsresiduals()

# there are visible deviations form mean (especially around the 2000-2010 period) 
# the histogram is normal
# the acf plot shows significant correlation leading to the possibility of improving the model


#** 7.f ----
# How sensitive are the accuracy measures to the amount of training data used?  

myseries %>%
  filter(year(Month) >= 2005, Month <= yearmonth("2010 Dec")) %>%
  model(SNAIVE(Turnover)) %>%
  # augment()
  forecast() %>%
  # accuracy(myseries)
  autoplot(myseries %>% filter(year(Month) >= 2005, Month <= yearmonth("2010 Dec"))) # ,level = NULL 

# the mean forecast and hence the accuracy measures do not change based on the training data. However, the prediction changes.
# So accuracy measures have low sensitivity to amount of training data.


#* 8 ----
# Consider the number of pigs slaughtered in New South Wales (data set aus_livestock).

pigs <- aus_livestock %>%
  filter(Animal == "Pigs", State == "New South Wales")


#** 8.a ----
# Produce some plots of the data in order to become familiar with it.

pigs %>% autoplot(Count)
# increasing trend till ~2002 then reducing
# major drop ~2009 then flat trend
# seasonal patterns present
# cyclicity every ~10 years(?)

pigs %>% gg_season(Count)
# Count reduced post ~2011
# higher monthly fluctuations in ~1991-2001 period
# visible spikes in Mar, May, Dec

pigs %>% gg_subseries(Count)
# mean of Mar, May, Jun, Dec are higher than other months
# mean lowest in Jan

pigs %>% gg_lag(Count)
# postive correlation between lagged values

pigs %>% ACF(Count) %>% autoplot()
# high correlation in nearby lags which decrease over time show trend
# spike in multiple of 3 show seasonality in the data

# Pigs slaughtered in New South Wales had increasing trend till ~2002 post which it started reducing. Major drop in 2009 post which trend flattened. Seasonality present around Quarter end. Significant drop in pig slaughter from Dec to Jan which has lowest slaughter on average.


#** 8.b ----
# Create a training set of 486 observations, withholding a test set of 72 observations (6 years).

pigs_test <- pigs %>% slice(n() - 71:0)

pigs_tr <- pigs %>% anti_join(pigs_test)


#** 8.c ----
# Try using various benchmark methods to forecast the training set and compare the results on the test set.
# Which method did best?

pigs_fit <- pigs_tr %>%
  model(
    Naive = NAIVE(Count),
    SNaive = SNAIVE(Count),
    Drift = RW(Count ~ drift())
  )

pigs_fc <- pigs_fit %>%
  forecast(pigs_test)

pigs_fc %>% autoplot(pigs, level = NULL)

accuracy(pigs_fc, pigs)
# drift method if the best


#** 8.d ----
# Check the residuals of your preferred method.
# Do they resemble white noise?

pigs_fit %>% select(Drift) %>% gg_tsresiduals()

# residuals trend is distributed around 0, however they do not have constant variance
# the residual histogram is right shifted (non-normal)
# there is significant correlation visible in the lag plot

# the residuals do not resemble white noise


#* 9 ----
#** 9.a ----
# Create a training set for household wealth (hh_budget) by withholding the last four years as a test set.

wealth_test <- hh_budget %>%
  group_by(Country) %>%
  slice(Year, n()-3:0)

wealth_tr <- anti_join(hh_budget, wealth_test)

wealth_tr %>% autoplot(Wealth)


#** 9.b ----
# Fit all the appropriate benchmark methods to the training set and forecast the periods covered by the test set.

wealth_fit <- wealth_tr %>%
  model(
    Naive = NAIVE(Wealth),
    Drift = RW(Wealth ~ drift())
  )
# SNaive not possible since it is annual data

wealth_fc <- wealth_fit %>%
  forecast(wealth_test)


#** 9.c ----
# Compute the accuracy of your forecasts.
# Which method does best?

wealth_fc %>%
  autoplot(hh_budget, level = NULL)

accuracy(wealth_fc, hh_budget) %>% arrange(Country)
# drift is the best


#** 9.d ----
# Do the residuals from the best method resemble white noise?

wealth_fit %>%
  select(Drift) %>%
  filter(Country == "USA") %>%
  gg_tsresiduals()

# residuals represent white noise


#* 10 ----
#** 10.a ----
# Create a training set for Australian takeaway food turnover (aus_retail) by withholding the last four years as a test set.

# identify max mouth in each series
aus_retail %>%
  group_by(State) %>%
  filter(Industry == "Takeaway food services", Month == max(Month))

takeaway <- aus_retail %>%
  filter(Industry == "Takeaway food services") %>%
  index_by() %>%
  summarise(total_turnover = sum(Turnover))

takeaway_tr <- takeaway %>% filter(year(Month) <= 2014)

takeaway_test <- takeaway %>% filter(year(Month) > 2014)


#** 10.b ----
# Fit all the appropriate benchmark methods to the training set and forecast the periods covered by the test set.

takeaway_fit <- takeaway_tr %>%
  model(
    Naive = NAIVE(total_turnover),
    SNaive = SNAIVE(total_turnover),
    Drift = RW(total_turnover ~ drift())
  )

takeaway_fc <- takeaway_fit %>%
  forecast(takeaway_test)


#** 10.c ----
# Compute the accuracy of your forecasts.
# Which method does best?

takeaway_fc %>%
  autoplot(takeaway, level = NULL)

accuracy(takeaway_fc, takeaway)

# Naive is the best


#** 10.d ----
# Do the residuals from the best method resemble white noise?

takeaway_fit %>% select(Naive) %>% gg_tsresiduals()

# residuals do not represent white noise


#* 11 ----
# We will use the Bricks data from aus_production (Australian quarterly clay brick production 1956–2005) for this exercise.

aus_bricks <- aus_production %>%
  # filter(between(year(Quarter), 1956, 2005))
  filter_index("1956 Q1" ~ "2005 Q2")

aus_bricks %>% autoplot(Bricks)


#** 11.a ----
# Use an STL decomposition to calculate the trend-cycle and seasonal indices.
# (Experiment with having fixed or changing seasonality.)

bricks_stl_fixed <- aus_bricks %>%
  model(STL(Bricks ~ trend() + season(window = "periodic"))) %>%
  components()

bricks_stl_changing <- aus_bricks %>%
  model(STL(Bricks ~ trend() + season(window = 10))) %>%
  components()

bricks_stl_fixed %>%
  autoplot() +
  ggtitle("Fixed Seasonality")
# has the same trend for seasonal data

bricks_stl_changing %>%
  autoplot() +
  ggtitle("Changing Seasonality")

# fixed has higher remainder values than changing


#** 11.b ----
# Compute and plot the seasonally adjusted data.

bricks_stl_fixed %>%
  autoplot(season_adjust) +
  ggtitle("Fixed Seasonality")

bricks_stl_changing %>%
  autoplot(season_adjust) +
  ggtitle("Changing Seasonality")

# changing seasonality has a smoother seasonality adjusted than fixed


#** 11.c ----
# Use a naïve method to produce forecasts of the seasonally adjusted data.

bricks_stl_fit <- bricks_stl_fixed %>%
  select(season_adjust) %>%
  model(NAIVE(season_adjust))

bricks_fixed_fc <- bricks_stl_fit %>% forecast()

bricks_fixed_fc %>% autoplot(bricks_stl_fixed %>% select(season_adjust))


bricks_changing_fit <- bricks_stl_changing %>%
  select(season_adjust) %>%
  model(NAIVE(season_adjust))

bricks_changing_fc <- bricks_changing_fit %>% forecast()

bricks_changing_fc %>% autoplot(bricks_stl_changing %>% select(season_adjust))

# changing seasonality has a lower prediction interval than fixed


#** 11.d ----
# Use decomposition_model() to reseasonalise the results, giving forecasts for the original data.

bricks_fixed_dcmp <- aus_bricks %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() + season(window = "periodic")),
      NAIVE(season_adjust)
    )
  )

bricks_fixed_dcmp %>%
  forecast() %>%
  autoplot(aus_bricks)


bricks_changing_dcmp <- aus_bricks %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() + season(window = 10)),
      NAIVE(season_adjust)
    )
  )

bricks_changing_dcmp %>%
  forecast() %>%
  autoplot(aus_bricks)

# the forecasted values are larger for fixed v/s changing


#** 11.e ----
# Do the residuals look uncorrelated?

bricks_fixed_dcmp %>% gg_tsresiduals()
# residuals are spread around 0 with a slight positive focus
# the histogram shows a similar left skew
# the acf plot shows correlation

bricks_changing_dcmp %>% gg_tsresiduals()


#** 11.f ----
# Repeat with a robust STL decomposition.
# Does it make much difference?

bricks_fixed_dcmp_robust <- aus_bricks %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() + season(window = "periodic"), robust = TRUE),
      NAIVE(season_adjust)
    )
  )

bricks_fixed_dcmp %>%
  forecast() %>%
  autoplot(aus_bricks)

bricks_fixed_dcmp %>% gg_tsresiduals()

bricks_changing_dcmp_robust <- aus_bricks %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() + season(window = 10), robust = TRUE),
      NAIVE(season_adjust)
    )
  )

bricks_changing_dcmp %>%
  forecast() %>%
  autoplot(aus_bricks)

bricks_changing_dcmp_robust %>% gg_tsresiduals()

# periodic model - robust has no impact
# seasonal model - robust reduces the residual correlation and creates a more normal histogram. however the range of negative residuals has increased for a few values.


#** 11.g ----
# Compare forecasts from decomposition_model() with those from SNAIVE(), using a test set comprising the last 2 years of data.
# Which is better?

aus_bricks_test <- aus_bricks %>% slice(n()-9:0)

aus_bricks_train <- anti_join(aus_bricks, aus_bricks_test)


bricks_fixed_robust <- aus_bricks_train %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() + season(window = "periodic"), robust = TRUE),
      NAIVE(season_adjust)
    ),
    SNaive = SNAIVE(Bricks, robust = TRUE)
  )

bricks_fixed_robust %>%
  forecast(aus_bricks_test) %>%
  autoplot(aus_bricks, level = NULL)

bricks_fixed_robust %>%
  forecast(aus_bricks_test) %>%
  accuracy(aus_bricks)

bricks_changing_robust <- aus_bricks_train %>%
  model(
    stlf = decomposition_model(
      STL(Bricks ~ trend() + season(window = 10), robust = TRUE),
      NAIVE(season_adjust)
    ),
    SNaive = SNAIVE(Bricks, robust = TRUE)
  )

bricks_changing_robust %>%
  forecast(aus_bricks_test) %>%
  autoplot(aus_bricks, level = NULL)

bricks_changing_robust %>%
  forecast(aus_bricks_test) %>%
  accuracy(aus_bricks)

# decomposition model is better using both fixed and changing seasonal model


#* 12 ----
# tourism contains quarterly visitor nights (in thousands) from 1998 to 2017 for 76 regions of Australia.

#** 12.a ----
# Extract data from the Gold Coast region using filter() and aggregate total overnight trips (sum over Purpose) using summarise().
# Call this new dataset gc_tourism.

gc_tourism <- tourism %>%
  filter(Region == "Gold Coast") %>%
  group_by(Region) %>%
  summarise(Trips = sum(Trips))


#** 12.b ----
# Using slice() or filter(), create three training sets for this data excluding the last 1, 2 and 3 years.
# For example, gc_train_1 <- gc_tourism %>% slice(1:(n()-4)).

gc_train_1 <- gc_tourism %>% slice(1:(n()-4))

gc_train_2 <- gc_tourism %>% slice(1:(n()-8))

gc_train_3 <- gc_tourism %>% slice(1:(n()-12))


#** 12.c ----
# Compute one year of forecasts for each training set using the seasonal naïve (SNAIVE()) method.
# Call these gc_fc_1, gc_fc_2 and gc_fc_3, respectively.

gc_fc_1 <- gc_train_1 %>% model(SNAIVE(Trips)) %>% forecast(h = 4)

gc_fc_2 <- gc_train_2 %>% model(SNAIVE(Trips)) %>% forecast(h = 8)

gc_fc_3 <- gc_train_3 %>% model(SNAIVE(Trips)) %>% forecast(h = 12)


#** 12.d ----
# Use accuracy() to compare the test set forecast accuracy using MAPE.
# Comment on these.

gc_fc_1 %>% autoplot(gc_tourism, level = NULL)

gc_fc_2 %>% autoplot(gc_tourism, level = NULL)

gc_fc_3 %>% autoplot(gc_tourism, level = NULL)


accuracy(gc_fc_1, gc_tourism)
accuracy(gc_fc_2, gc_tourism)
accuracy(gc_fc_3, gc_tourism)

# the best model is 2, followed by 3 then 1
