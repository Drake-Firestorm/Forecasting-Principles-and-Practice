# Chapter 3 Time series decomposition ----
# When we decompose a time series into components, we usually combine the trend and cycle into a single trend-cycle component (often just called the trend for simplicity).
# Thus we think of a time series as comprising three components:
#   a trend-cycle component,
#   a seasonal component, and 
#   a remainder component (containing anything else in the time series).

# Often this is done to help improve understanding of the time series, but it can also be used to improve forecast accuracy.

# When decomposing a time series, it is sometimes helpful to first transform or adjust the series in order to make the decomposition (and later analysis) as simple as possible.


#* 3.1 Transformations and adjustments ----
# Adjusting the historical data can often lead to a simpler time series. 
# Here, we deal with four kinds of adjustments: 
#   calendar adjustments, 
#   population adjustments, 
#   inflation adjustments and 
#   mathematical transformations.
# The purpose of these adjustments and transformations is to simplify the patterns in the historical data by removing known sources of variation or by making the pattern more consistent across the whole data set. 
# Simpler patterns usually lead to more accurate forecasts.


#** Calendar adjustments ----
# example, if you are studying the total monthly sales in a retail store, there will be variation between the months simply because of the different numbers of trading days in each month, in addition to the seasonal variation across the year. It is easy to remove this variation by computing average sales per trading day in each month, rather than total sales in the month. Then we effectively remove the calendar variation.


#** Population adjustments ----
# Any data that are affected by population changes can be adjusted to give per-capita data. That is, consider the data per person (or per thousand people, or per million people) rather than the total.
#   example, if you are studying the number of hospital beds in a particular region over time, the results are much easier to interpret if you remove the effects of population changes by considering the number of beds per thousand people.
# For most data that are affected by population changes, it is best to use per-capita data rather than the totals.

# a common transformation of GDP is GDP per-capita.
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP / Population)


#** Inflation adjustments ----
# Data which are affected by the value of money are best adjusted before modelling. 
# For this reason, financial time series are usually adjusted so that all values are stated in dollar values from a particular year.

# To make these adjustments, a price index is used. If zt denotes the price index and yt denotes the original house price in year t, then x[t] = y[t] / z[t] * z[2000] gives the adjusted house price at year 2000 dollar values. Price indexes are often constructed by government agencies.

# This allows us to compare the growth or decline of industries relative to a common price value.

# example, looking at aggregate "newspaper and book" retail turnover from aus_retail, and adjusting the data for inflation using CPI from global_economy allows us to understand the changes over time.

print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

aus_economy <- global_economy %>%
  filter(Code == "AUS")

print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
  gather("Type", "Turnover", Turnover, Adjusted_turnover, factor_key = TRUE) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  xlab("Years") + ylab(NULL) +
  ggtitle("Turnover for the Australian print media industry")
# By adjusting for inflation using the CPI, we can see that Australia's newspaper and book retailing industry has been in decline much longer than the original data suggests.


#** Mathematical adjustments ----
# If the data shows variation that increases or decreases with the level of the series, then a transformation can be useful. 

# example, a logarithmic transformation is often useful.
# Logarithms are useful because they are interpretable: changes in a log value are relative (or percentage) changes on the original scale.

# Sometimes other transformations are also used (although they are not so interpretable). 
#   example, square roots and cube roots can be used. These are called "power transformations"

# family of Box-Cox transformations
# A useful family of transformations, that includes both logarithms and power transformations, which depend on the parameter λ and are defined as follows: 
#    {  log(y[t])                   if  λ=0;
# wt={  sign(y[t](|y[t]^λ - 1)/λ    otherwise.

# If any values of the series are negative then a Box-Cox transformation is not possible.

# The logarithm in a Box-Cox transformation is always a natural logarithm (i.e., to base e). So if λ=0, natural logarithms are used, but if λ!=0, a power transformation is used, followed by some simple scaling.

# If λ=1, then wt=yt-1, so the transformed data is shifted downwards but there is no change in the shape of the time series. But for all other values of λ, the time series will change shape.

# A good value of λ is one which makes the size of the seasonal variation about the same across the whole series, as that makes the forecasting model simpler.
# A low value of λ can give extremely large prediction intervals

# The guerrero feature can be used to choose a value of lambda for you.
lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)

aus_production %>% autoplot(box_cox(Gas, lambda))
# aus_production %>% autoplot(Gas)

# A Box-Cox transformation followed by an additive ETS model is often better than an ETS model without transformation.
# It makes no sense to use a Box-Cox transformation and a non-additive ETS model.


# _----


#* 3.2 Time series components ----
# If we assume an additive decomposition, then we can write
#  yt = St + Tt + Rt, where 
#   yt is the data, 
#   St is the seasonal component, 
#   Tt is the trend-cycle component, and 
#   Rt is the remainder component, all at period t.

# a multiplicative decomposition would be written as 
#  yt = St × Tt × Rt

# additive decomposition is the most appropriate if the magnitude of the seasonal fluctuations, or the variation around the trend-cycle, does not vary with the level of the time series.

# When the variation in the seasonal pattern, or the variation around the trend-cycle, appears to be proportional to the level of the time series, then a multiplicative decomposition is more appropriate. 

# Multiplicative decompositions are common with economic time series.

# An alternative to using a multiplicative decomposition is to first transform the data until the variation in the series appears to be stable over time, then use an additive decomposition.
# When a log transformation has been used, this is equivalent to using a multiplicative decomposition because 
#  yt = St × Tt × Rt is equivalent to logyt = logSt + logTt + logRt


#** Employment in the US retail sector ----
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed) +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(STL(Employed))

components(dcmp)

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

components(dcmp) %>% autoplot() + xlab("Year")
# Notice that the seasonal component changes over time, so that any two consecutive years have similar patterns, but years far apart may have different seasonal patterns.

# The grey bars to the left of each panel show the relative scales of the components.
# Each grey bar represents the same length but because the plots are on different scales, the bars vary in size. 
# The large grey bar in the bottom panel shows that the variation in the remainder component is smallest compared to the variation in the data, which has a bar about one quarter the size. 
# If we shrunk the bottom three panels until their bars became the same size as that in the data panel, then all the panels would be on the same scale.
#     smaller bar == larger variation


#** Seasonally adjusted data ----
# If the seasonal component is removed from the original data, the resulting values are the "seasonally adjusted" data. 
# For an additive decomposition, the seasonally adjusted data are given by yt-St, and 
# for multiplicative data, the seasonally adjusted values are obtained using yt/St.


us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), season_adjust, color='blue') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

# If the variation due to seasonality is not of primary interest, the seasonally adjusted series can be useful. 
#  example, monthly unemployment data are usually seasonally adjusted in order to highlight variation due to the underlying state of the economy rather than the seasonal variation.

# Seasonally adjusted series contain the remainder component as well as the trend-cycle. 
# Therefore, they are not "smooth", and "downturns" or "upturns" can be misleading. 
# If the purpose is to look for turning points in a series, and interpret any changes in direction, then it is better to use the trend-cycle component rather than the seasonally adjusted data.


# _----


#* 3.3 Moving averages -----------
# classical method of time series decomposition - 1920-1950
# forms the basis of many time series decomposition methods
# first step in a classical decomposition is to use a moving average method to estimate the trend-cycle


#** Moving average smoothing ----
# moving average of order m can be written as 
# Tt = 1/m(k∑[j=-k]y[t+j]); m = 2k + 1

# Observations that are nearby in time are also likely to be close in value. 
# Therefore, the average eliminates some of the randomness in the data, leaving a smooth trend-cycle component. 
# We call this an m-MA, meaning a moving average of order m.

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  xlab("Year") + ylab("% of GDP") +
  ggtitle("Total Australian exports")

aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slide_dbl(Exports, mean, .size = 5, .align = "center")
  )
# slide_dbl() which applies a function to "sliding" time windows.

aus_exports %>%
  autoplot(Exports) +
  autolayer(aus_exports, `5-MA`, color='red') +
  xlab("Year") + ylab("Exports (% of GDP)") +
  ggtitle("Total Australian exports") +
  guides(colour=guide_legend(title="series"))
# trend-cycle (in red) is smoother than the original data and captures the main movement of the time series without all of the minor fluctuations.

# a larger order means a smoother curve.

# Simple moving averages such as these are usually of an odd order (e.g., 3, 5, 7, etc.). 
# This is so they are symmetric: in a moving average of order m = 2k + 1, the middle observation, and k observations on either side, are averaged. 
# But if m was even, it would no longer be symmetric.


#** Moving averages of moving averages ----
# It is possible to apply a moving average to a moving average. 
# One reason for doing this is to make an even-order moving average symmetric.
# example, take MA = 4, then apply another moving average of order 2

beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)

beer_ma <- beer %>%
  mutate(
    `4-MA` = slide_dbl(Beer, mean, .size = 4, .align = "center-left"),
    `2x4-MA` = slide_dbl(`4-MA`, mean, .size = 2, .align = "center-right")
  )


# When a 2-MA follows a moving average of an even order (such as 4), it is called a "centred moving average of order 4". This is because the results are now symmetric.

# Other combinations of moving averages are also possible. 
#  example, a 3×3-MA is often used, and consists of a moving average of order 3 followed by another moving average of order 3. 
#  In general, an even order MA should be followed by an even order MA to make it symmetric. 
#  Similarly, an odd order MA should be followed by an odd order MA.


#** Estimating the trend-cycle with seasonal data ----
# The most common use of centred moving averages is for estimating the trend-cycle from seasonal data.
# In general, a 2×m-MA is equivalent to a weighted moving average of order m+1 where all observations take the weight 1/m, except for the first and last terms which take weights 1/(2m).
 
# So, if the seasonal period is even and of order m, we use a 2×m-MA to estimate the trend-cycle. 
# If the seasonal period is odd and of order m, we use a m-MA to estimate the trend-cycle.
#  example, a 2×12-MA can be used to estimate the trend-cycle of monthly data and 
#  a 7-MA can be used to estimate the trend-cycle of daily data with a weekly seasonality.

# Other choices for the order of the MA will usually result in trend-cycle estimates being contaminated by the seasonality in the data.


#** Example: Employment in the US retail sector ----
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slide_dbl(Employed, mean, .size = 12, .align = "cr"),
    `2x12-MA` = slide_dbl(`12-MA`, mean, .size = 2, .align = "cl")
  )

us_retail_employment_ma %>%
  autoplot(Employed, color='gray') +
  autolayer(us_retail_employment_ma, vars(`2x12-MA`), color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")


#** Weighted moving averages ----
# Combinations of moving averages result in weighted moving averages.
# a weighted m-MA can be written as 
#   Tt = k∑[j=-k]a(j)y(t+j); k = (m-1)/2; weights are given by [a[-k],.,a[k]]
# It is important that the weights all sum to one and that they are symmetric so that aj=a-j. 
# The simple m-MA is a special case where all of the weights are equal to 1/m.

# A major advantage of weighted moving averages is that they yield a smoother estimate of the trend-cycle. 
# Instead of observations entering and leaving the calculation at full weight, their weights slowly increase and then slowly decrease, resulting in a smoother curve.


# _----


#* 3.4 Classical decomposition ------------
# classical decomposition method - 1920
#  relatively simple procedure, and forms the starting point for most other methods of time series decomposition.
# two forms of classical decomposition: 
#  an additive decomposition and 
#  a multiplicative decomposition

# In classical decomposition, we assume that the seasonal component is constant from year to year. 
# For multiplicative seasonality, the m values that form the seasonal component are sometimes called the "seasonal indices".


#** Additive decomposition ----
#*** Step 1 ----
# If m is an even number, compute the trend-cycle component Tt using a 2×m-MA. 
# If m is an odd number, compute the trend-cycle component Tt using an m-MA. 

#*** Step 2 ----
# Calculate the detrended series: yt − ^Tt

#*** Step 3 ----
# To estimate the seasonal component for each season, simply average the detrended values for that season. 
#  example, with monthly data, the seasonal component for March is the average of all the detrended March values in the data.

# These seasonal component values are then adjusted to ensure that they add to zero. 
# The seasonal component is obtained by stringing together these monthly values, and then replicating the sequence for each year of data. 
# This gives St

#*** Step 4 ----
# remainder component is calculated by subtracting the estimated seasonal and trend-cycle components: 
#     ^R[t] = y[t]/(^Tt * ^St)

us_retail_employment %>%
  model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

# seasonal components sum to 0
# us_retail_employment %>%
#   model(classical_decomposition(Employed, type = "additive")) %>%
#   components() %>%
#   filter(year(Month) == 1990) %>%
#   as_tibble() %>%
#   summarise(seasonal = sum(seasonal))


#** Multiplicative decomposition ----
# subtractions are replaced by divisions.

#*** Step 1 ----
# If m is an even number, compute the trend-cycle component Tt using a 2×m-MA. 
# If m is an odd number, compute the trend-cycle component Tt using an m-MA. 

#*** Step 2 ----
# Calculate the detrended series: yt/Tt

#*** Step 3 ----
# To estimate the seasonal component for each season, simply average the detrended values for that season. 
#  example, with monthly data, the seasonal component for March is the average of all the detrended March values in the data.

# These seasonal component values are then adjusted to ensure that they add to zero. 
# The seasonal component is obtained by stringing together these monthly values, and then replicating the sequence for each year of data. 
# This gives St

#*** Step 4 ----
# remainder component is calculated by subtracting the estimated seasonal and trend-cycle components: 
#  Rt = yt / (Tt * St)

us_retail_employment %>%
  model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

# seasonal components sum to 0
# us_retail_employment %>%
#   model(classical_decomposition(Employed, type = "additive")) %>%
#   components() %>%
#   filter(year(Month) == 1990) %>%
#   as_tibble() %>%
#   summarise(seasonal = sum(seasonal))


#** Comments on classical decomposition ----
# While classical decomposition is still widely used, it is not recommended, as there are now several much better methods. 
# Some of the problems with classical decomposition are summarised below. 
# - The estimate of the trend-cycle is unavailable for the first few and last few observations. Consequently, there is also no estimate of the remainder component for the same time periods.
# - The trend-cycle estimate tends to over-smooth rapid rises and falls in the data.
# - Classical decomposition methods assume that the seasonal component repeats from year to year. For many series, this is a reasonable assumption, but for some longer series it is not. The classical decomposition methods are unable to capture these seasonal changes over time.
# - Occasionally, the values of the time series in a small number of periods may be particularly unusual. The classical method is not robust to these kinds of unusual values.


# _----


#* 3.5 X11 decomposition ---------
# originated in the US Census Bureau and Statistics Canada.

# is based on classical decomposition, but includes many extra steps and features in order to overcome the drawbacks of classical decomposition that were discussed in the previous section. 
#  In particular, trend-cycle estimates are available for all observations including the end points, and the seasonal component is allowed to vary slowly over time.
#  also has some sophisticated methods for handling trading day variation, holiday effects and the effects of known predictors.
#  handles both additive and multiplicative decomposition. 
#  The process is entirely automatic and tends to be highly robust to outliers and level shifts in the time series.

x11_dcmp <- us_retail_employment %>%
  model(x11 = feasts:::X11(Employed, type = "additive")) %>%
  components()

autoplot(x11_dcmp) + xlab("Year") +
  ggtitle("Additive X11 decomposition of US retail employment in the US")

x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
# It can be useful to use seasonal plots and seasonal sub-series plots of the seasonal component. 
# These help us to visualise the variation in the seasonal component over time.

x11_dcmp %>%
  gg_subseries(seasonal)


# _----


#* 3.6 SEATS decomposition -------
# "SEATS" stands for "Seasonal Extraction in ARIMA Time Series"
# developed at the Bank of Spain, and is now widely used by government agencies around the world.
# works only with quarterly and monthly data.

seats_dcmp <- us_retail_employment %>%
  model(seats = feasts:::SEATS(Employed)) %>%
  components()

autoplot(seats_dcmp) + xlab("Year") +
  ggtitle("SEATS decomposition of total US retail employment")

# seasonal package has many options for handling variations of X11 and SEATS.


# _----


#* 3.7 STL decomposition ---------
# STL is a versatile and robust method for decomposing time series. 
# STL is an acronym for "Seasonal and Trend decomposition using Loess", 
#   while Loess is a method for estimating nonlinear relationships. 
# developed by Cleveland, Cleveland, McRae, & Terpenning

# STL has several advantages over the classical, SEATS and X11 decomposition methods:
# - Unlike SEATS and X11, STL will handle any type of seasonality, not only monthly and quarterly data.
# - The seasonal component is allowed to change over time, and the rate of change can be controlled by the user.
# - The smoothness of the trend-cycle can also be controlled by the user.
# - It can be robust to outliers (i.e., the user can specify a robust decomposition), so that occasional unusual observations will not affect the estimates of the trend-cycle and seasonal components. They will, however, affect the remainder component.


# disadvantages
# - In particular, it does not handle trading day or calendar variation automatically, and 
# - it only provides facilities for additive decompositions. 


# It is possible to obtain a multiplicative decomposition by first taking logs of the data, then back-transforming the components. 
# Decompositions between additive and multiplicative can be obtained using a Box-Cox transformation of the data with 0 < λ < 1. 
#   A value of λ = 0 corresponds to the multiplicative decomposition while 
#   λ = 1 is equivalent to an additive decomposition.

us_retail_employment %>%
  model(STL(Employed ~ trend(window=7) + season(window='periodic'),
            robust = TRUE)) %>%
  components() %>%
  autoplot()


# two main parameters to be chosen when using STL are the trend-cycle window trend(window = ?) and the seasonal window season(window = ?). 
# These control how rapidly the trend-cycle and seasonal components can change. 
# Smaller values allow for more rapid changes. 
# Both trend and seasonal windows should be odd numbers; 
#   trend window is the number of consecutive observations to be used when estimating the trend-cycle; 
#   season window is the number of consecutive years to be used in estimating each value in the seasonal component. 
# Setting the seasonal window to be infinite is equivalent to forcing the seasonal component to be periodic season(window='periodic')(i.e., identical across years).


# By default, the STL() function provides a convenient automated STL decomposition using a seasonal window of season(window=13), and the trend window chosen automatically from the seasonal period. 
# default setting for monthly data is trend(window=21). 
#   This usually gives a good balance between overfitting the seasonality and allowing it to slowly change over time. 
# But, as with any automated procedure, the default settings will need adjusting for some time series.


# _----