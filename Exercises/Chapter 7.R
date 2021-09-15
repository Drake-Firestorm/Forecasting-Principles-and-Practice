#* 1 ----
# Half-hourly electricity demand for Victoria, Australia is contained in vic_elec.
# Extract the January 2014 electricity demand, and aggregate this data to daily with daily total demands and maximum temperatures.
#   jan14_vic_elec <- vic_elec %>%
#     filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
#     index_by(Date = as_date(Time)) %>%
#     summarise(
#       Demand = sum(Demand),
#       Temperature = max(Temperature)
#     )


jan14_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  index_by(Date) %>%
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

#** 1.a ----
# Plot the data and find the regression model for Demand with temperature as an explanatory variable.
# Why is there a positive relationship?

jan14_vic_elec %>%
  autoplot(Demand) +
  labs(title = "Daily Electricity Demand Trend")

jan14_vic_elec %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Temperature v/s Daily Electricity Demand")

# There is a positive relationship possibly because as Temperature rises there is no more usage of air conditioners.


#** 1.b ----
# Produce a residual plot.
# Is the model adequate?
# Are there any outliers or influential observations?

demand_fit <- jan14_vic_elec %>%
  model(TSLM(Demand ~ Temperature))

demand_fit %>% gg_tsresiduals()
# There is a visible pattern in the residual plot.
# This is confirmed by the histogram, which is slightly right skewed.
# The ACF plot reveals no correlation.


#** 1.c ----
# Use the model to forecast the electricity demand that you would expect for the next day if the maximum temperature was 15∘C and compare it with the forecast if the with maximum temperature was 35∘C.
# Do you believe these forecasts? The following R code will get you started:
#   jan14_vic_elec %>%
#     model(TSLM(Demand ~ Temperature)) %>%
#     forecast(
#       new_data(jan14_vic_elec, 1) %>%
#         mutate(Temperature = 15)
#     ) %>%
#     autoplot(jan14_vic_elec)


future_scenarios <- scenarios(
  max_temp_15 = new_data(jan14_vic_elec, 1) %>%
    mutate(Temperature = 15),
  max_temp_35 = new_data(jan14_vic_elec, 1) %>%
    mutate(Temperature = 35)
)

demand_fc <- forecast(demand_fit, new_data = future_scenarios)

jan14_vic_elec %>%
  autoplot(Demand) +
  autolayer(demand_fc)

# The forecast looks correct as the forecasted temperature values are near the historical values.


#** 1.d ----
# Give prediction intervals for your forecasts.

demand_fc %>% hilo()


#** 1.e ----
# Plot Demand vs Temperature for all of the available data in vic_elec aggregated to daily total demand and maximum temperature.
# What does this say about your model?

vic_elec %>%
  index_by(Date) %>%
  mutate(Demand ~ sum(Demand), Temperature = max(Temperature)) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# There is a non-linear relationship between Temperature and Demand.
# Upto 20 deg there is a reduction in Demand, followed by Increease.
# Therefore our model is overfitted for Jan 2014 Demand, and is not useful on a generic basis.


#* 2 ----
# Data set olympic_running contains the winning times (in seconds) in each Olympic Games sprint, middle-distance and long-distance track events from 1896 to 2016.

#** 2.a ----
# Plot the winning time against the year for each event.
# Describe the main features of the plot.

olympic_running %>%
  autoplot(Time) +
  facet_grid(Length ~ Sex, scales = "free_y")

# Winning times for all events have been decreasing.
# There are 2 gaps in the events, due to the 2 world wars.

# Focusing only on events after WW2.
event_ww2 <- olympic_running %>%
  filter(Year >= 1948)

event_ww2 %>%
  autoplot(Time) +
  facet_grid(Length ~ Sex, scales = "free_y")

# mens' time is lower than womens'.
# there is an increase in time in the 1500 event.


#** 2.b ----
# Fit a regression line to the data for each event.
# Obviously the winning times have been decreasing, but at what average rate per year?

event_fit <- event_ww2 %>%
  model(
    TSLM_trend = TSLM(Time ~ trend()),
    TSLM_year = TSLM(Time ~ Year),
    RW = RW(Time ~ drift())
  )

# trend() and Year are both the same models. 
# trend() model is built by treating Year as the predictor

event_ww2 %>%
  # filter(Length == 400, Sex == "men") %>%
  autoplot(Time, color = "black") +
  # geom_line(
  #   aes(y = .fitted, color = .model), 
  #   data = fitted(event_fit) %>% filter(Length == 400, Sex == "men")
  # ) +
  geom_line(
    aes(y = .fitted, color = .model),
    data = fitted(event_fit)
  ) +
  facet_grid(Length ~ Sex, scales = "free_y") +
  labs(title = "Regression Line plot for TSLM and RW models")

# rw is a lagged model


event_fit %>%
  coef() %>%
  filter(.model == c("TSLM_trend", "RW"), term == c("trend()", "b")) %>%
  group_by(.model) %>%
  summarise(estimate = mean(estimate)) %>%
  ungroup() %>%
  summarise(estimate = mean(estimate))

# every year on average across all events the time is decreased by ~2.7 min
# however there is a wide range between the 100m and 1000m events


#** 2.c ----
# Plot the residuals against the year.
# What does this indicate about the suitability of the fitted lines?

event_fit %>%
  resid() %>%
  ggplot(aes(x = Year, y = .resid)) +
  geom_line(aes(color = .model)) +
  facet_grid(Length ~ Sex, scales = "free_y") +
  labs(title = "Residual plot for TSLM and RW models")

# The time plots show changing variation over time, with rising trends in recent years for a few events
# This heteroscedasticity will potential make the prediction interval coverage innacurate.
# This residual plots show that the regression models genrally fit the data well.


event_fit %>%
  resid() %>%
  ggplot(aes(y = .resid)) +
  geom_histogram(aes(color = .model)) +
  facet_grid(Length ~ Sex, scales = "free_y") +
  labs(title = "Residual plot for TSLM and RW models")

# The residuals are not normally distributed.


#** 2.d ----
# Predict the winning time for each race in the 2020 Olympics.
# Give a prediction interval for your forecasts.
# What assumptions have you made in these calculations?

event_fc <- event_fit %>% forecast(h = 1)

event_fc %>%
  hilo() %>%
  print(width = Inf)

event_fc %>%
  autoplot(event_ww2, level = NULL) +
  facet_grid(Length ~ Sex, scales = "free_y")

# Assumptions made:
#   - the existing winning time trends will continue
#   - the event lengths will remain the same
#   - the event rules/formats will remain the same
#   - the residuals are normally distributed (even though we saw from the histogram that this is incorrect)


#* 3 ----
# An elasticity coefficient is the ratio of the percentage change in the forecast variable (y) to the percentage change in the predictor variable (x). 
# Mathematically, the elasticity is defined as (dy/dx) × (x/y).
# Consider the log-log model,
#   logy = β0 + β1logx + ε
# Express y as a function of x and show that the coefficient β1 is the elasticity coefficient.

#   logy = B0 + B1logx + E
#   e[^logy] = e[^(B0 + B1logx + E)]
#   y = e[^(B0 + B1logx + E)]
    
#   dy/dx = d/dx * e[^(B0 + B1logx + E)]
    
#   u = B1logx
#   C = B0 + E
#   y = e[C + u] = e[C].e[u]
    
#   dy/dx = d/dx * e[C + u]
#   dy/dx = (d/du * e[C].e[u]) * (d/dx * B1logx)
#   dy/dx = e[C].e[u] * B1/x
#   dy/dx = y * B1/x
#   dy/dx * x/y = B1 
    
#   therefore, coefficient B1 is the elasticity coefficient


#* 4 ----
# The data set souvenirs concerns the monthly sales figures of a shop which opened in January 1987 and sells gifts, souvenirs, and novelties.
# The shop is situated on the wharf at a beach resort town in Queensland, Australia.
# The sales volume varies with the seasonal population of tourists.
# There is a large influx of visitors to the town at Christmas and for the local surfing festival, held every March since 1988.
# Over time, the shop has expanded its premises, range of products, and staff.


#** 4.a ----
# Produce a time plot of the data and describe the patterns in the graph.
# Identify any unusual or unexpected fluctuations in the time series.

souvenirs %>% autoplot(Sales)
# increasing trend
# 1987 lowest Sales, 1993 highest
# average annual sales ~ 17,500 with Sales in Dec shooting up to 100,000 in recent years
# seasonal spikes towards end of year
# spike sizes are increasing YoY except in 1993
# Sales drops back in usual in Jan
# a small spike occurs in Feb/Mar


souvenirs %>% gg_season(Sales)
# small spike in Mar before Sales fall back to normal, due to local surfing festival, held every March since 1988
# Sales start increasing in Nov and peak in Dec for Christmas

souvenirs %>% gg_subseries(Sales)
# average sales increase from Jan to Dec with a slight bump in Mar
# The increase YoY is more starting from Mar, Jan/Feb Sales have had a lower increase from previous years.
# The Sales increase YoY for Dec is exceptionally high


#** 4.b ----
# Explain why it is necessary to take logarithms of these data before fitting a model.

#   Sales in recent years have ranged from ~17,500 in Jan to ~75,000 in Dec
#   even historically the range from Jan to Dec has been high
#   to ensure that the Dec Sales does not dominate the regression model, it is essential to take logs
    
#   Logarithms are useful because they are interpretable: changes in a log value are relative (or percentage) changes on the original scale


#** 4.c ----
# Fit a regression model to the logarithms of these sales data with a linear trend, seasonal dummies and a “surfing festival” dummy variable.

souvenirs <- souvenirs %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0))

souvenirs_fit <- souvenirs %>%
  model(TSLM(log(Sales) ~ trend() + season() + `Surfing Festival`))


#** 4.d ----
# Plot the residuals against time and against the fitted values.
# Do these plots reveal any problems with the model?

augment(souvenirs_fit) %>%
  autoplot(.resid)

# the time plot reveals there is a visible pattern post 1990, meaning there is correlation between time and residuals

souvenirs_fit %>% gg_tsresiduals()
# the acf plot confirms this


augment(souvenirs_fit) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()

# the fitted plot reveals that the size of residuals changes as we move along the x-axis.
# It means that even after log transformation, there are still heteroscedacity in the errors or that the variance of the residuals aren't still constant.


#** 4.e ----
# Do boxplots of the residuals for each month.
# Does this reveal any problems with the model?

augment(souvenirs_fit) %>%
  ggplot(aes(x = as.factor(month(Month)), y = .resid)) +
  geom_boxplot(aes(color = as.factor(month(Month)))) +
  # plot mean
  stat_summary(fun = mean, color = "darkred", geom = "point", size = 3) +
  stat_summary(fun = mean, color = "darkred", geom = "text", size = 3, aes(label = round(..y.., digits = 1)), vjust = -0.7) +
  # hide legend
  theme(legend.position = "none") +
  labs(x = "Month")
  # hide x-axis
  # scale_x_discrete(labels = NULL) +
  # labs(x = NULL, color = "Month")

# there are a few outliers present in months 5, 7, 8
# the range for months 3,8-12 is very large (especially 12)
#   this means that the Sales trend has changed significantly during the range of the dataset
# hence the model fit might not be good

# The distribution of the residuals was unsymetrical for some months.
# And for some months, the mean of the residuals wasn't 0.
# Residuals with such properties can't have normal distribution, which will make it difficult to get prediction interval. 


#** 4.f ----
# What do the values of the coefficients tell you about each variable?

report(souvenirs_fit)

# The +ve trend coeff means that it increases YoY but the rate is slow
# The +ve coeff for the seasonal variable means that the Jan sales are lowest, and the sales of other months are higher than Jan's for most years. The impact of Nov & Dec is especially high compared to the remaining months.
# The 0.501 coeff for Surfing Festival means that it has a high positive impact on the sales.


#** 4.g ----
# What does the Ljung-Box test tell you about your model?

augment(souvenirs_fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)

# The p-value of the test is less than 0.001
# hence residuals can be distinguished from white noise.
# The residuals can be correlated with each other.


#** 4.h ----
# Regardless of your answers to the above questions, use your regression model to predict the monthly sales for 1994, 1995, and 1996.
# Produce prediction intervals for each of your forecasts.

souvenirs_future_data <- new_data(souvenirs, 36) %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & 
                                   month(Month) == 3, 1, 0))

souvenirs_future_data_1994 <- souvenirs_future_data %>%
  filter(year(Month) == 1994)

souvenirs_future_data_1995 <- souvenirs_future_data %>%
  filter(year(Month) == 1995)

souvenirs_future_data_1996 <- souvenirs_future_data %>%
  filter(year(Month) == 1996)

# 1994 forecast
souvenirs_fc_1994 <- forecast(souvenirs_fit, new_data = souvenirs_future_data_1994)
souvenirs_fc_1994 %>% hilo()

souvenirs_fc_1994 %>% autoplot(souvenirs)

# 1995 forecast
souvenirs_fc_1995 <- forecast(souvenirs_fit, new_data = souvenirs_future_data_1995)
souvenirs_fc_1995 %>% hilo()

souvenirs_fc_1995 %>% autoplot(souvenirs)

# 1996 forecast
souvenirs_fc_1996 <- forecast(souvenirs_fit, new_data = souvenirs_future_data_1996)
souvenirs_fc_1996 %>% hilo()

souvenirs_fc_1996 %>% autoplot(souvenirs)


# the "all forecast" is the same as combining the individual forecasts above.

# all forecast
souvenirs_fc <- forecast(souvenirs_fit, new_data = souvenirs_future_data)
souvenirs_fc %>% hilo()

souvenirs_fc %>% autoplot(souvenirs)


#** 4.i ----
# How could you improve these predictions by modifying the model?

souvenirs %>% features(Sales, feat_acf)

#**** Box-Cox transformation
# We can try a Box-Cox transformation which will make the size of the seasonal variation about the same across the whole series, as that makes the forecasting model simpler.

lambda <- souvenirs %>%
  features(Sales, features = guerrero) %>%
  pull(lambda_guerrero)

souvenirs %>% autoplot(box_cox(Sales, lambda))

souvenirs_fit <- souvenirs %>%
  model(TSLM(box_cox(Sales, lambda) ~ trend() + season() + `Surfing Festival`))

souvenirs_fc <- forecast(souvenirs_fit, souvenirs_future_data)
souvenirs_fc %>% autoplot(souvenirs)

# Doesn't work as this doesn't answer the issue with the autocorrelation.


#**** Christmas dummy variable
souvenirs <- souvenirs %>%
  mutate(Christmas = if_else(month(Month) == 12, 1, 0))

souvenirs_fit <- souvenirs %>%
  model(TSLM(log(Sales) ~ trend() + season() + `Surfing Festival` + Christmas))

souvenirs_fit %>% gg_tsresiduals()
augment(souvenirs_fit) %>% features(.resid, ljung_box, lag = 10, dof = 3)

souvenirs_future_data <- souvenirs_future_data %>%
  mutate(Christmas = if_else(month(Month) == 12, 1, 0))

souvenirs_fc <- forecast(souvenirs_fit, souvenirs_future_data)
souvenirs_fc %>% hilo()

# Not useful as it is already handled by the trend + season


#**** Decomposition model
souvenirs_fit_dcmp <- souvenirs %>%
  model(
    stlf = decomposition_model(
      STL(log(Sales) ~ trend() + season(), robust = TRUE),
      TSLM(season_adjust ~ trend() + `Surfing Festival`)
    )
  )

souvenirs_fc <- forecast(souvenirs_fit_dcmp, new_data = souvenirs_future_data)

souvenirs_fc %>% autoplot(souvenirs)

souvenirs_fit_dcmp %>% gg_tsresiduals()

# The pattern in the residual is reduced, but a trend starts occurring post 1992, meaning the model has left even information out.
# There is correlation and pattern also visible in the correlogram.


#**** Lag variable
souvenirs %>%
  mutate(
    lagged = Sales - lag(Sales, 12),
    `lagged %` = lagged / lag(Sales, 12)
  ) %>%
  filter(`lagged %` >= 0.5)

# It is clear that there is lot of information being left out in the seasonal data.
# If the model can be modified to better forecast the seasonal changes, then the model will be improved.


#**** Fourier
souvenirs_fit <- souvenirs %>%
  model(TSLM(Sales ~ trend() + fourier(K = 5) + `Surfing Festival`))

souvenirs_fc <- forecast(souvenirs_fit_dcmp, new_data = souvenirs_future_data)

souvenirs_fc %>% autoplot(souvenirs)

souvenirs_fit %>% gg_tsresiduals()

# The seasonal information is captured much better using a fourier transformation.
# The pattern in the lagged data is more random.


souvenirs_fit <- souvenirs %>%
  model(
    k_1 = TSLM(Sales ~ trend() + fourier(K = 1) + `Surfing Festival`),
    k_2 = TSLM(Sales ~ trend() + fourier(K = 2) + `Surfing Festival`),
    k_3 = TSLM(Sales ~ trend() + fourier(K = 3) + `Surfing Festival`),
    k_4 = TSLM(Sales ~ trend() + fourier(K = 4) + `Surfing Festival`),
    k_5 = TSLM(Sales ~ trend() + fourier(K = 5) + `Surfing Festival`),
    k_6 = TSLM(Sales ~ trend() + fourier(K = 6) + `Surfing Festival`)
  )

glance(souvenirs_fit) %>%
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) %>%
  arrange(AIC)

# Based on the AIC, AICc, and BIC there is little difference between K = 5 and K = 6.
# Hence we can use either model.


#* 5 ----
# The us_gasoline series consists of weekly data for supplies of US finished motor gasoline product, from 2 February 1991 to 20 January 2017.
# The units are in “million barrels per day.”
# Consider only the data to the end of 2004.

us_gasoline_2004 <- us_gasoline %>%
  filter(year(Week) <= 2004)


#** 5.a ----
# Fit a harmonic regression with trend to the data.
# Experiment with changing the number Fourier terms.
# Plot the observed gasoline and fitted values and comment on what you see.

us_gasoline_2004 %>% autoplot(Barrels)

us_gasoline_2004 %>% gg_season(Barrels)

us_gasoline_2004 %>% gg_subseries(Barrels)


us_gasoline_2004_fit <- us_gasoline_2004 %>%
  model(
    k_1 = TSLM(Barrels ~ trend() + fourier(K = 1)),
    k_5 = TSLM(Barrels ~ trend() + fourier(K = 5)),
    k_10 = TSLM(Barrels ~ trend() + fourier(K = 10)),
    k_15 = TSLM(Barrels ~ trend() + fourier(K = 15)),
    k_20 = TSLM(Barrels ~ trend() + fourier(K = 20)),
    k_26 = TSLM(Barrels ~ trend() + fourier(K = 26))
  )


glance(us_gasoline_2004_fit) %>%
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) %>%
  arrange(AIC)


# K = 10 is the best based on the AIC value


augment(us_gasoline_2004_fit) %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Barrels, color = "Data")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(y = NULL, title = "US Finished motor gasoline supplies (million barrels per day)") +
  scale_color_manual(values = c(Data = "black", Fitted = "red")) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap(vars(.model))


# as more number of Fourier pairs are used, the fitted line looks more like the original data.
# But the fitted lines don't seem to be improving when we compare k_10 with k_26


#** 5.b ----
# Select the appropriate number of Fourier terms to include by minimising the AICc or CV value.

us_gasoline_2004_fit <- us_gasoline_2004 %>%
  model(
    k_10 = TSLM(Barrels ~ trend() + fourier(K = 10)),
    k_11 = TSLM(Barrels ~ trend() + fourier(K = 11)),
    k_12 = TSLM(Barrels ~ trend() + fourier(K = 12)),
    k_13 = TSLM(Barrels ~ trend() + fourier(K = 13)),
    k_14 = TSLM(Barrels ~ trend() + fourier(K = 14)),
    k_15 = TSLM(Barrels ~ trend() + fourier(K = 15)),
    k_5 = TSLM(Barrels ~ trend() + fourier(K = 5)),
    k_6 = TSLM(Barrels ~ trend() + fourier(K = 6)),
    k_7 = TSLM(Barrels ~ trend() + fourier(K = 7)),
    k_8 = TSLM(Barrels ~ trend() + fourier(K = 8)),
    k_9 = TSLM(Barrels ~ trend() + fourier(K = 9))
  )

glance(us_gasoline_2004_fit) %>%
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) %>%
  arrange(AICc, CV)

# K = 12 and K = 11 are equally good based on the AIC and CV value.
# K = 11 is slightly better based on AICc value.


# The below code give K = 7 as the best model for some reason.

us_gasoline_2004_fit <- us_gasoline_2004 %>%
  model(
    k_10 = TSLM(Barrels ~ trend() + fourier(K = 10)),
    k_11 = TSLM(Barrels ~ trend() + fourier(K = 11)),
    k_12 = TSLM(Barrels ~ trend() + fourier(K = 12)),
    k_13 = TSLM(Barrels ~ trend() + fourier(K = 13)),
    k_14 = TSLM(Barrels ~ trend() + fourier(K = 14)),
    k_15 = TSLM(Barrels ~ trend() + fourier(K = 15)),
    k_1 = TSLM(Barrels ~ trend() + fourier(K = 1)),
    k_5 = TSLM(Barrels ~ trend() + fourier(K = 5)),
    k_7 = TSLM(Barrels ~ trend() + fourier(K = 7)),
    k_26 = TSLM(Barrels ~ trend() + fourier(K = 26))
  )

glance(us_gasoline_2004_fit) %>%
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) %>%
  arrange(AICc)


#** 5.c ----
# Plot the residuals of the final model using the gg_tsresiduals() function and comment on these.
# Use a Ljung-Box test to check for residual autocorrelation.

us_gasoline_2004_fit <- us_gasoline_2004 %>%
  model(K_11 = TSLM(Barrels ~ trend() + fourier(K = 11)))

us_gasoline_2004_fit %>% gg_tsresiduals()

# The residuals are centered around 0 with no noticeable trend
# However, the autocorrelation is not particularly large, and except the spike at lag 3, there are unlikely to have any noticeable impact on the forecasts or the prediction intervals.

augment(us_gasoline_2004_fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# The p-value is small at 0.00467, so the residuals can be distinguished from white noise.
# However, results are probably not severe enough to make much difference to the forecasts and prediction intervals. (Note that the correlations are relatively small, even though they are significant.)

# If we run the tests for K = 7, the number of spikes in the lag plot reduce and the histogram is a normal plot.
# The p-value is also larger at 0.0174, although still significant.

us_gasoline_2004_fit <- us_gasoline_2004 %>%
  model(K_7 = TSLM(Barrels ~ trend() + fourier(K = 7)))

us_gasoline_2004_fit %>% gg_tsresiduals()

augment(us_gasoline_2004_fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)


#** 5.d ----
# Generate forecasts for the next year of data and plot these along with the actual data for 2005.
# Comment on the forecasts.

us_gasoline_test <- us_gasoline %>%
  filter(year(Week) == 2005)

us_gasoline_2004_fit <- us_gasoline_2004 %>%
  model(
    K_7 = TSLM(Barrels ~ trend() + fourier(K = 7)),
    K_11 = TSLM(Barrels ~ trend() + fourier(K = 11))
  )

us_gasoline_fc <- forecast(us_gasoline_2004_fit, new_data = us_gasoline_test)

us_gasoline_fc %>%
  autoplot(us_gasoline %>% filter(year(Week) <= 2005), level = NULL)

# Both models have a similar forecasts.
# They capture most of the trend, however, it is not perfect. The model couldn't predict the sudden drop in the fall. The drop was a lot bigger than expected.

accuracy(us_gasoline_fc, us_gasoline)
# The model K = 7 has a marginally better accuracy.


#* 6 ----
# The annual population of Afghanistan is available in the global_economy data set.

popu_afg_ge <- global_economy %>%
  filter(Code == "AFG") %>%
  select(Country, Code, Year, Population)


#** 6.a ----
# Plot the data and comment on its features.
# Can you observe the effect of the Soviet-Afghan war?

popu_afg_ge %>% autoplot(Population)
# During the period of 1980-1987/88 there is decline in the population.
# This is due to the effect of the Soviet-Afghan war which occured during 1980-1989.


#** 6.b ----
# Fit a linear trend model and compare this to a piecewise linear trend model with knots at 1980 and 1989.

fit_popu <- popu_afg_ge %>%
  model(
    linear = TSLM(Population ~ trend()),
    piecewise = TSLM(Population ~ trend(knots = c(1980, 1989)))
  )

popu_afg_ge %>%
  autoplot(Population) +
  geom_line(aes(y = .fitted, color = .model), data = fitted(fit_popu)) +
  labs(y = "Winning times", title = "Afghan Population")


#** 6.c ----
# Generate forecasts from these two models for the five years after the end of the data, and comment on the results.

popu_fc <- fit_popu %>% forecast(h = 5)

popu_afg_ge %>%
  autoplot(Population) +
  geom_line(aes(y = .fitted, color = .model), data = fitted(fit_popu)) +
  labs(y = "Winning times", title = "Afghan Population") +
  autolayer(popu_fc, alpha = 0.5, level = 95)

# The piecewise forecast closely follows the actual data. Hence the prediction interval also has a small range.
# The linear forecast doesn't follow the actual data. Hence the prediction interval is also very large.


#* 7 ----
# (For advanced readers following on from Section 7.9).
# Using matrix notation it was shown that if y = Xβ + ε, where ε has mean 0 and variance matrix σ^2I, the estimated coefficients are given by ^β = (X′X)^−1X′y and a forecast is given by ^y = x∗^β = x∗(X′X)^−1X′y where x∗ is a row vector containing the values of the regressors for the forecast (in the same format as X), and the forecast variance is given by Var(^y) = σ^2[1 + x∗(X′X)^−1(x∗)′].

# Consider the simple time trend model where yt = β0 + β1t.
# Using the following results,
# [T∑t=1]t = 1/2 * T(T+1),      [T∑t=1]t^2 = 1/6 * T(T+1)(2T+1)
# derive the following expressions:
# a.    X′X   = 1/6 [ 6T        3T(T+1)
#                 3T(T+1)   T(T+1)(2T+1) ]
# b.    (X'X)[^-1] = 2/(T(T[^2]-1)) [ (T+1)(2T+1)  -3(T+1)
#                                     -3(T+1)      6       ]
# c.    ^β0 = 2/(T(T-1)) * [ (2T+1)([T∑t=1]y[t] - 3([T∑t=1]ty[t])) ]
#       ^β1 = 6/(T(T[^2]-1)) * [ 2([T∑t=1]ty[t]) - (T+1)([T∑t=1]y[t]) ]    
# d.    Var(^y) = σ2[ 1 + 2/(T(T[^2]-1)) [ 1 - 4T - 6h + 6(T+h)[^2]/(T+1) ]



# y               = Xβ + ε
# ^β              = (X′X)[^−1]X′y
# ^y      = x∗^β  = x∗(X′X)[^−1]X′y
# Var(^y)         = σ2[1 + x∗(X′X)[^−1](x∗)′]

# y[t]          = β[0] + β[1]t
# [T∑t=1]t      = (1/2)T(T+1)
# [T∑t=1]t[^2]  = (1/6)T(T+1)(2T+1)


#*** 7.a ----
# y[t] = β[0] + β[1]t
# X = [ 1 t1
#       1 t2
#       ....
#       1 T ]
# X'X = [ 1   1   ... 1     *   [ 1   t1
#         t1  t2  ... T ]         1   t2
#                                 ......
#                                 1   T ]
#     = [ 1+1+...+1(t time)   t1+t2+...+T
#         t1+t2+...+T         t1[^2]+t2[^2]+...+T[^2] ]
#     = [ T           T(T+1)/2
#         T(T+1)/2    T(T+1)(2T+1)/6 ]
#     = 1/6 [ 6T        3T(T+1)
#             3T(T+1)   T(T+1)(2T+1) ]


#*** 7.b ---- 
# (X'X)[^-1] = 1/( T*T(T+1)(2T+1)/6) - (T(T+1)/2[^2]) [ T(T+1)(2T+1)/6    -T(T+1)/2
#                                                       -T(T+1)/2         T         ]
#            = 12/(T[^2](T[^2]-1)) [ T(T+1)(2T+1)/6    -T(T+1)/2
#                                    -T(T+1)/2         T         ]
#            = 2/(T(T[^2]-1))[ (T+1)(2T+1)  -3(T+1)
#                              -3(T+1)      6       ]


#*** 7.c ----  
# ^β = (X′X)[^−1]X′y

# ^β  = 2/(T(T[^2]-1)) * [  (T+1)(2T+1)  -3(T+1)    * [ 1   1   ... 1   * [ y1  y2  ... yT ]
#                           -3(T+1)      6       ]      t1  t2  ... T ]
#     = 2/(T(T[^2]-1)) * [  (T+1)(2T+1)  -3(T+1)    * [ y1 + y2 + ... + yT
#                           -3(T+1)      6       ]      t1y1 + t2y2 + ... + TyT ]
#     = 2/(T(T[^2]-1)) * [  (T+1)(2T+1)(y1+y2+...+yT) - 3(T+1)(t1y1 + t2y2 + ... + TyT)
#                           -3(T+1)(y1+y2+...+yT) + 6(t1y1 + t2y2 + ... + TyT)          ]

# ^β0 = 2/(T(T[^2]-1)) * [  (T+1)(2T+1)(y1+y2+...+yT) - 3(T+1)(t1y1 + t2y2 + ... + TyT) ]
#     = 2/(T(T-1)) * [ (2T+1)([T∑t=1]y[t] - 3([T∑t=1]ty[t])) ]

# ^β1 = 2/(T(T[^2]-1)) * [ -3(T+1)(y1+y2+...+yT) + 6(t1y1 + t2y2 + ... + TyT) ]
#     = 6/(T(T[^2]-1)) * [ 2([T∑t=1]ty[t]) - (T+1)([T∑t=1]y[t]) ]


#*** 7.d ----  
# x*  = [ 1   T+1
#         1   T+2
#         .......
#         1   T+h ]

# Var(^y) = σ2[ 1 + x∗(X′X)[^−1](x∗)′ ]
#         = σ2[ 1 + [1  T+h]  * 2/(T(T[^2]-1))  [ (T+1)(2T+1)   -3(T+1)   * [  1
#                                                 -3(T+1)       6       ]     T+h ] ]
#         = σ2[ 1 + [1  T+h]  * 2/(T(T[^2]-1))  [ (T+1)(2T+1) - 3(T+1)(T+h)
#                                                 -3(T+1) + 6(T+h)          ] ]
#         = σ2[ 1 + 2/(T(T[^2]-1)) [ (T+1)(2T+1) - 3(T+1)(T+h) + (T+h)(-3(T+1) + 6(T+h)) ]
#         = σ2[ 1 + 2/(T(T[^2]-1)) [ (T+1)(2T+1) - 3(T+1)(T+h) - 3(T+1)(T+h) + 6(T+h)[^2] ]
#         = σ2[ 1 + 2/(T(T[^2]-1)) [ 2T + 1 - 6(T+h) + 6(T+h)[^2]/(T+1) ]
#         = σ2[ 1 + 2/(T(T[^2]-1)) [ 1 - 4T - 6h + 6(T+h)[^2]/(T+1) ]
