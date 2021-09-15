#* 1 ----
# Consider the GDP information in global_economy.
# Plot the GDP per capita for each country over time.
# Which country has the highest GDP per capita?
# How has this changed over time?

global_economy %>%
  mutate(GDP_per_capita = GDP / Popoluation) %>%
  # ggplot(aes(x = Year, y = GDP_per_capita, color = Country)) +
  # geom_line() +
  autoplot(GDP_per_capita) +
  theme(legend.position = "none") # turns of the legends


global_economy %>%
  mutate(GDP_per_capita = GDP / Popoluation) %>%
  select(Country, Year, Population, GDP, GDP_per_capita) %>%
  arrange(desc(GDP_per_capita))
# highest per capita = Monaco


global_economy %>%
  filter(Country == "Monaco") %>%
  mutate(GDP_per_capita = GDP / Popoluation) %>%
  autoplot(GDP_per_capita) +
  theme(legend.position = "none") # turns of the legends
# increasing trend, with major spike post 1985 and 2000


#* 2 ----
# For each of the following series, make a graph of the data. If transforming seems appropriate, do so and describe the effect.
#    United States GDP from global_economy.
#    Slaughter of Victorian “Bulls, bullocks and steers” in aus_livestock.
#    Victorian Electricity Demand from vic_elec.
#    Gas production from aus_production.

#** United States GDP from global_economy ----
global_economoy %>%
  filter(Code == "USA") %>%
  autoplot(GDP)
# Smooth series with minor dip around 2008; no transformation neccessary


#** Slaughter of Victorian "Bulls, bullocks and steers" in aus_livestock ----
aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") %>%
  autoplot()
# Downward trend; very large range of values; transformation will help

aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") %>%
  mutate(log10_Count = log10(Count)) %>%
  autoplot(log10_Count)
# enhances the height of the smaller seasonal variations to make it inline with the larger values


#** Victorian Electricity Demand from vic_elec ----
vic_elec %>%
  autoplot(Demand)
# No trend with several large peaks at the start of each year; transformation might help


lambda <- vic_elec %>%
  features(Demand, features = guerrero) %>%
  pull(lambda_guerrero)

vic_elec %>%
  autoplot(box_cox(Demand, lambda))
# the distances of the peaks and the troughs are reduced


#** Gas production from aus_production ----
aus_production %>%
  autoplot(Gas)
# No noticible trend pre 1970; Upward trend post 1970; seasonality of period 4; large range of values
# transformation might help

# log10
aus_production %>%
  mutate(log10_gas = log10(Gas)) %>%
  autoplot(log10_gas)

# guerrero
lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)

aus_production %>%
  autoplot(box_cox(Gas, lambda))
# guerrero gives a more stable seasonal pattern


#* 3 ----
# Why is a Box-Cox transformation unhelpful for the canadian_gas data?

canadian_gas %>%
  autoplot()

lambda <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

canadian_gas %>%
  autoplot(box_cox(Volume, lambda))
# box_cox is unhelpful since the data post 1992 has a completly different pattern compared to the pre 1992 data.
# The transformation doesn't give a simpler pattern


#* 4 ----
# What Box-Cox transformation would you select for your retail data (from Exercise 8 in Section 2.10)?

myseries %>%
  autoplot(Turnover)

lambda <- myseries %>%
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)

myseries %>%
  autoplot(box_cox(Turnover, lambda))
# lambda = 0.22


#* 5 ----
# For the following series, find an appropriate Box-Cox transformation in order to stabilise the variance.
# Tobacco from aus_production, Economy class passengers between Melbourne and Sydney from ansett, and Pedestrian counts at Southern Cross Station from pedestrian.

aus_production %>%
  autoplot(Tobacco)

lambda <- aus_production %>%
  features(Tobacco, guerrero) %>%
  pull(lambda_guerrero)
# 0.9289589

aus_production %>%
  autoplot(box_cox(Tobacco, lambda))
# +autolayer(aus_production, Tobacco)


ansett_mel_syd_eco <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy")

ansett_mel_syd_eco %>%
  autoplot(Passengers)

lambda <- ansett_mel_syd_eco %>%
  features(Passengers, guerrero) %>%
  pull(lambda_guerrero)

ansett_mel_syd_eco %>%
  autoplot(box_cox(Passengers, lambda))
# not sure

pedestrian %>%
  filter(Sensor == "Southern Cross Station") %>%
  features(Count, guerrero) %>%
  pull(lambda_guerrero)

pedestrian %>%
  filter(Sensor == "Southern Cross Station") %>%
  autoplot(box_cox(Count, lambda))


#* 6 ----
# Show that a 3×5 MA is equivalent to a 7-term weighted moving average with weights of 0.067, 0.133, 0.200, 0.200, 0.200, 0.133, and 0.067.

# 1/3(
#   1/5(y[t-3] + y[t-2] + y[t-1] + y[t] + y[t+1]) +
#   1/5(y[t-2] + y[t-1] + y[t] + y[t+1] + y[t+2]) +
#   1/5(y[t-1] + y[t] + y[t+1] + y[t+2] + y[t+3]) +
# )
# =
# 1/3((1/5)*y[t-3] + (2/5)*y[t-2] + (3/5)*y[t-1] + (3/5)*y[t] + (3/5)*y[t+1] + (3/5)*y[t+2] + (1/5)*y[t+3])
# =
# 1/3(0.2*y[t-3] + 0.4*y[t-2] + 0.6*y[t-1] + 0.6*y[t] + 0.6*y[t+1] + 0.6*y[t+2] + 0.2*y[t+3])
# 0.067y[t-3], 0.133*y[t-2], 0.2*y[t-1], 0.2*y[t], 0.2*y[t+1], 0.2*y[t+2], 0.067*y[t+3]
# weights = 0.067, 0.133, 0.2, 0.2, 0.2, 0.133, 0.067
# count = 7 == 7-term weighted moving average


#* 7 ----
# Consider the last five years of the Gas data from aus_production.
#   gas <- tail(aus_production, 5*4) %>% select(Gas)

plastics_tsibble <- plastics %>%
  as_tsibble() %>%
  rename(Month = index, Sales = value)

#** 7.a ----
# Plot the time series.
# Can you identify seasonal fluctuations and/or a trend-cycle?

plastics_tsibble %>% autoplot()
# increasing trend, seasonality of 12 months with peaks in Jun/Jul

plastics_tsibble %>% gg_season()
# confirms 12 month seasonality

#** 7.b ----
# Use classical_decomposition with type=multiplicative to calculate the trend-cycle and seasonal indices.

plastics_tsibble %>%
  model(classical_decomposition(Sales, type = "multiplicative")) %>%
  components() %>%
  autoplot() + xlab("Month") +
  ggtitle("Classical multiplicative decomposition of total Monthly Sales")

#** 7.c ----
# Do the results support the graphical interpretation from part a?

# Yes, results support the graphical interpretation from part a

#** 7.d ----
# Compute and plot the seasonally adjusted data.

plastics_tsibble %>%
  model(classical_decomposition(Sales, type = "multiplicative")) %>%
  components() %>%
  select(Month, season_adjust) %>%
  autoplot(season_adjust) + xlab("Month") +
  ggtitle("Seasonaly Adjusted total Monthly Sales")

#** 7.e ----
# Change one observation to be an outlier (e.g., add 300 to one observation), and recompute the seasonally adjusted data.
# What is the effect of the outlier?

plastics_outlier <- plastics_tsibble
plastics_outlier$Sales[plastics_outlier$Month == ymd("00050601")] <-
  plastics_outlier$Sales[plastics_outlier$Month == ymd("00050601")] + 500

plastics_dcmp <- plastics_outlier %>%
  model(classical_decomposition(Sales, type = "multiplicative")) %>%
  components()

plastics_dcmp %>%
  select(Month, season_adjust) %>%
  autoplot(season_adjust) + xlab("Month") +
  ggtitle("Seasonaly Adjusted total Monthly Sales") +
  autolayer(plastics_dcmp, trend, color = "red")
# the trend is adjusted slightly, but the Seasonality adjusted chart is heavily affected. Seasonally adjusted chart has errors like the original data

#** 7.f ----
# Does it make any difference if the outlier is near the end rather than in the middle of the time series?

plastics_outlier <- plastics_tsibble
plastics_outlier$Sales[plastics_outlier$Month == ymd("00020601")] <-
  plastics_outlier$Sales[plastics_outlier$Month == ymd("00020601")] + 500

plastics_dcmp <- plastics_outlier %>%
  model(classical_decomposition(Sales, type = "multiplicative")) %>%
  components()

plastics_dcmp %>%
  select(Month, season_adjust) %>%
  autoplot(season_adjust) + xlab("Month") +
  ggtitle("Seasonaly Adjusted total Monthly Sales") +
  autolayer(plastics_dcmp, trend, color = "red")
# outlayers near the end impact the trend more


#* 8 ----
# Recall your retail time series data (from Exercise 8 in Section 2.10).
# Decompose the series using X-11.
# Does it reveal any outliers, or unusual features that you had not noticed previously?

set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries %>%
  autoplot(Turnover)

myseries_x11 <- myseries %>%
  model(x11 = feasts:::X11(Turnover, type = "multiplicative")) %>%
  components()

autoplot(myseries_x11)
# there were some outliers that I didn't expect. Especially the biggest outlier happened in 2001. And I didn't expect the seasonality effect decreases as trend increases.


#* 9 ----
# Figures 3.19 and 3.20 show the result of decomposing the number of persons in the civilian labour force in Australia each month from February 1978 to August 1995.
# Figure 3.19: Decomposition of the number of persons in the civilian labour force in Australia each month from February 1978 to August 1995. 
# Figure 3.20: Seasonal component from the decomposition shown in the previous figure. 

#** 9.a ----
# Write about 3–5 sentences describing the results of the decomposition.
# Pay particular attention to the scales of the graphs in making your interpretation.

# increasing trend
# variation in the seasonal component least
# Outlier around 1991/1992
# Seasonal components peak in Dec with a second peak in Mar; troughs in Jan and Aug

#** 9.b ----
# Is the recession of 1991/1992 visible in the estimated components?

# Yes


#* 10 ----
# This exercise uses the canadian_gas data (monthly Canadian gas production in billions of cubic metres, January 1960 – February 2005).

#** 10.a ----
# Plot the data using autoplot(), gg_subseries() and gg_season() to look at the effect of the changing seasonality over time.
# The evolving seasonal pattern is possibly due to changes in the regulation of gas prices

canadian_gas %>% autoplot()
# increasing trend with changing seasonality

canadian_gas %>% gg_subseries()
# mean of all months is nearly same, and all show a similar trend

canadian_gas %>% gg_season()
# volumes around 1975-1979 remained static with a dip from Mar to Sep

# changing seasonality is possible due to changes in the regulation of gas prices
# The gas production amount increased in winter and decreased in summer
# Maybe the cold weather in winter increased the demand for the gas that the production amount increased.
# But the production amount also increased in summer as time went on.
# Maybe it happened because the demand for electricity to run air conditionars increased over time.

#** 10.b ----
# Do an STL decomposition of the data.
# You will need to choose a seasonal window to allow for the changing shape of the seasonal component.

canadian_gas_stl <- canadian_gas %>%
  model(STL(Volume ~ season(window = 13)), robust = TRUE) %>%
  components()

canadian_gas_stl %>%
  autoplot()

# can see that the size of seasonality increased in 1980s and decreased in 1990s.

#** 10.c ----
# How does the seasonal shape change over time?
# [Hint: Try plotting the seasonal component using gg_season().]

canadian_gas_stl %>%
  gg_season(season_year)
# there is a changing seasonality in 1980-1990. more linear before that

#** 10.d ----
# Can you produce a plausible seasonally adjusted series?

canadian_gas_stl %>%
  select(Month, season_adjust) %>%
  autoplot(season_adjust) +
  xlab("Month") + ylab("Seasonaly Adjusted Production Volume")

#** 10.e ----
# Compare the results with those obtained using SEATS and X-11. How are they different?

canadian_gas_x11 <- canadian_gas %>%
  model(x11 = feasts:::X11(Volume)) %>%
  components()

canadian_gas_x11 %>%
  autoplot()
# components are similar to STL
# seasonality has more variance compared to STL

canadian_gas_seats <- canadian_gas %>%
  model(x11 = feasts:::SEATS(Volume)) %>%
  components()

canadian_gas_x11 %>%
  autoplot()
# shows greater seasonality in 19960's
# variance is low

# SEATS function did multiplicative decomposition.
# Therefore seasonal component and remainder component have mean at 1, not 0.
# And the proportion of seasonality to trend decreased, then increased, and then decreased again.
 
# From the plots, I could see that the seasonally adjusted data of STL decomposition have higher variance than the other methods.
# It can mean that STL method is more robust, that is unusual observations affect the remainder component more when STL method is used.
