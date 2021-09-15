# Links -------------------------------------------------------------------
# https://otexts.com/fpp3/
# solutions
# - https://robjhyndman.com/files/labs.R
# - https://github.com/JehyeonHeo/Forecasting_with_R_practices
# - https://github.com/ola-sumbo/Forecasting_with_R_practices
# - https://robjhyndman.com/seminars/isi2019workshop/
# - https://enixam.github.io/fpp/

# https://github.com/robjhyndman/fpp3-package/tree/master/data
# contains additional data (.rda files)
# aus_airpassengers.rda       aus_arrivals.rda        boston_marathon.rda
# canadian_gas.rda            guinea_rice.rda         us_change.rda
# us_employment.rda           us_gasoline.rda         souvenirs.rda
# bank_calls.rda              prices.rda

# https://github.com/robjhyndman/fma/tree/master/data
# eggs.rda                    plastics.rda            hsales.rda
# strikes.rda                 

# https://github.com/robjhyndman/Mcomp/tree/master/data
# M3.rda


# use >> load(file = "..../R/win-library/datasets/aus_airpassengers.rda)

# ____----


# Library -----------------------------------------------------------------
# install.packages(c("tsibble", "tsibbledata", "feasts", "magrittr", "tidyverse", "lubridate", "GGally", "seasonal", "broom", "fable", "patchwork"))
library(tsibble)
library(tsibbledata)
library(feasts)
library(magrittr)
library(tidyverse)
library(lubridate)
library(GGally)
library(seasonal)
library(broom)
library(fable)
library(patchwork)
library(Quandl)
library(fable.prophet)


# ____----


# Chapter 2 Time series graphics ------------------------------------------
#* 2.1 tsibble objects -----------------------------------------------------
#** The index variable ----
y <- tsibble(Year = 2015:2019, Observation = c(123,39,78,52,110), index = Year)

z <- tibble(
  Month = c("2019 Jan", "2019 Feb", "2019 Mar", "2019 Apr", "2019 May"),
  Observation = c(50,23,34,30,25)
)

z %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)


#*** time class functions ----
# Frequency   Function
# _________   ________
# Annual      start:end
# Quarterly   yearquarter()
# Monthly     yearmonth()
# Weekly      yearweek()
# Daily       as_date(), ymd()
# Sub-daily   as_datetime()



#** The key variables ----
olympic_running


#** Working with tsibble objects ----
PBS

PBS %>%
  filter(ATC2=="A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6) -> a10


#** Read a csv file and convert to a tsibble ----
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous), index = Quarter)


#** The seasonal period ----
# Data 	    Minute  Hour    Day     Week      Year
# Quarters 					                          4
# Months 					                            12
# Weeks 					                            52
# Days 				                      7         365.25
# Hours 			              24      168       8766
# Minutes 		      60      1440    10080     525960
# Seconds 	60      3600    86400   604800    31557600


# _----


#* 2.2 Time plots ----------------------------------------------------------
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class=="Economy")

melsyd_economy %>%
  autoplot(Passengers) +
    labs(title = "Ansett economy class passengers", subtitle = "Melbourne-Sydney") +
    xlab("Year")

a10 %>% autoplot(Cost) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") + xlab("Year")


# _----


#* 2.3 Time series patterns ------------------------------------------------
#** Trend ----
# exists when there is a long-term increase or decrease in the data.
# It does not have to be linear.
# Sometimes we will refer to a trend as "changing direction", when it might go from an increasing trend to a decreasing trend. 


#** Seasonal ----
# occurs when a time series is affected by seasonal factors such as the time of the year or the day of the week.
# Seasonality is always of a fixed and known period.


#** Cyclic ----
# occurs when the data exhibit rises and falls that are not of a fixed frequency.
# These fluctuations are usually due to economic conditions, and are often related to the "business cycle".
# The duration of these fluctuations is usually at least 2 years. 


# Seasonal v/s Cyclic
# Many people confuse cyclic behaviour with seasonal behaviour, but they are really quite different.
# If the fluctuations are not of a fixed frequency then they are cyclic;
# if the frequency is unchanging and associated with some aspect of the calendar, then the pattern is seasonal.
# In general, the average length of cycles is longer than the length of a seasonal pattern, and the magnitudes of cycles tend to be more variable than the magnitudes of seasonal patterns.


# _----


#* 2.4 Seasonal plots ------------------------------------------------------
# A seasonal plot allows the underlying seasonal pattern to be seen more clearly, and is especially useful in identifying years in which the pattern changes.

a10 %>% gg_season(Cost, labels = "both") +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


#** Multiple seasonal periods ----
vic_elec %>% gg_season(Demand, period="day") + theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period="week") + theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period="year")


# _----


#* 2.5 Seasonal subseries plots --------------------------------------------
#  data for each season are collected together in separate mini time plots.
# The blue horizontal lines indicate the means for each month.
# This form of plot enables the underlying seasonal pattern to be seen clearly, and also shows the changes in seasonality over time.
# It is especially useful in identifying changes within particular seasons.

a10 %>%
  gg_subseries(Cost) +
    ylab("$ million") +
    xlab("Year") +
    ggtitle("Seasonal subseries plot: antidiabetic drug sales")


#** Example: Australian holiday tourism ----
holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays %>% autoplot(Trips) +
  ylab("thousands of trips") + xlab("Year") +
  ggtitle("Australian domestic holiday nights")

holidays %>% gg_season(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

holidays %>%
  gg_subseries(Trips) + ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

# _----


#* 2.6 Scatterplots --------------------------------------------------------
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
    xlab("Year: 2014") + ylab(NULL) +
    ggtitle("Half-hourly electricity demand: Victoria, Australia")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
    xlab("Year: 2014") + ylab(NULL) +
    ggtitle("Half-hourly temperatures: Melbourne, Australia")

# scatterplot helps us to visualise the relationship between the variables.
vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
    geom_point() +
    ylab("Demand (GW)") + xlab("Temperature (Celsius)")


#** Correlation ----
# correlation coefficient only measures the strength of the linear relationship, and can sometimes be misleading.
# example, the correlation for the electricity demand and temperature data shown in previous plot is 0.28, but the non-linear relationship is stronger than that.
# This shows how important it is to look at the plots of the data and not simply rely on correlation values.


#** Scatterplot matrices ----
visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
    geom_line() +
    facet_grid(vars(State), scales = "free_y") +
    ylab("Number of visitor nights each quarter (millions)")

# To see the relationships between these eight time series, we can plot each time series against the others. These plots can be arranged in a scatterplot matrix
visitors %>%
  spread(State, Trips) %>%
  GGally::ggpairs(columns = 2:9)

# For each panel, the variable on the vertical axis is given by the variable name in that row, and the variable on the horizontal axis is given by the variable name in that column.
# The value of the scatterplot matrix is that it enables a quick view of the relationships between all pairs of variables.


# _----


#* 2.7 Lag plots -----------------------------------------------------------
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production %>% gg_lag(Beer, geom="point")

# https://www.itl.nist.gov/div898/handbook/eda/section3/lagplot.htm
# Purpose: Check for randomness 
#   A lag plot checks whether a data set or time series is random or not.
#   Random data should not exhibit any identifiable structure in the lag plot.
#   Non-random structure in the lag plot indicates that the underlying data are not random.

# Definition
#   A lag is a fixed time displacement.
#   example, given a data set Y1, Y2 ..., Yn, Y2 and Y7 have lag 5 since 7 - 2 = 5.
#    Lag plots can be generated for any arbitrary lag, although the most commonly used lag is 1.
#    A plot of lag 1 is a plot of the values of Yi versus Yi-1
#     Vertical axis: Yi for all i
#     Horizontal axis: Yi-1 for all i 

# Questions
#   Lag plots can provide answers to the following questions:
#     Are the data random?
#     Is there serial correlation in the data?
#     What is a suitable model for the data?
#     Are there outliers in the data? 

# Importance
#   Inasmuch as randomness is an underlying assumption for most statistical estimation and testing techniques, the lag plot should be a routine tool for researchers. 


# _----


#* 2.8 Autocorrelation -----------------------------------------------------
# autocorrelation measures the linear relationship between lagged values of a time series.
# autocorrelation coefficients make up the autocorrelation function or ACF.

recent_production %>% ACF(Beer, lag_max = 9)

# correlogram - ACF Plot

recent_production %>% ACF(Beer) %>% autoplot()
# recent_production %>% gg_season(Beer)


#** Trend and seasonality in ACF plots ----
# When data have a trend, the autocorrelations for small lags tend to be large and positive because observations nearby in time are also nearby in size. So the ACF of trended time series tend to have positive values that slowly decrease as the lags increase.
# When data are seasonal, the autocorrelations will be larger for the seasonal lags (at multiples of the seasonal frequency) than for other lags.
# When data are both trended and seasonal, you see a combination of these effects.

a10 %>% ACF(Cost, lag_max = 48) %>% autoplot()
# a10 %>% gg_season(Cost)


# _----


#* 2.9 White noise ---------------------------------------------------------
# Time series that show no autocorrelation are called white noise.
# For white noise series, we expect each autocorrelation to be close to zero.
# Of course, they will not be exactly equal to zero as there is some random variation.
# For a white noise series, we expect 95% of the spikes in the ACF to lie within ±2/√T where T is the length of the time series.
# It is common to plot these bounds on a graph of the ACF (the blue dashed lines above).
# If one or more large spikes are outside these bounds, or if substantially more than 5% of spikes are outside these bounds, then the series is probably not white noise.

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + ggtitle("White noise")

y %>% ACF(wn) %>% autoplot()


# _----


#* 2.10 Exercises ----------------------------------------------------------
#** 1.a ----
gafa_stock %>%
  autoplot(Close) +
  xlab("Day") + ylab("Closing Price")

PBS %>%
  select(Month, Concession, Scripts) %>%
  filter(Concession == "Concessional", ATC2 %in% c("A01", "A03")) %>%
  autoplot(Scripts)

vic_elec %>% autoplot()

pelt %>% autoplot()


#** 1.b ----
gafa_stock; PBS; vic_elec; pelt;
frequency(gafa_stock); frequency(PBS); frequency(vic_elec); frequency(pelt)
# daily;                monthly;        30 min;             annual


#** 1.c ----
gafa_stock %>%
  group_by(Symbol) %>%
  filter(Close == max(Close)) %>%
  select(Symbol, Date)


#** 2.a ----
tute1 <- readr::read_csv("tute1.csv")


#** 2.b ----
mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)


#** 2.c ----
mytimeseries %>%
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>%
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
    geom_line() +
    facet_grid(vars(Key), scales = "free_y")  # removing this plots all on the same plot


#** 3.a ----
# read_excel = Note that the Excel spreadsheet must be local (a URL does not work)
# below method is an alternate for making this work
httr::GET("http://otexts.com/fpp3/extrafiles/tourism.xlsx", httr::write_disk(td <- tempfile(fileext = ".xlsx")))
tourism_url <- readxl::read_xlsx(path = td, sheet = "Sheet1")
# tourism_url <- readxl::read_excel(path = td, sheet = "Sheet1")


#** 3.b ----
tourism_url %>%
  as_tibble() %>%
  group_by(Region, Purpose) %>%
  summarise(max_trips = mean(Trips)) %>%
  arrange(desc(max_trips)) %>%
  head(1)

# alternate
# tourism_url %>%
#   as_tibble() %>%
#   group_by(Region, Purpose) %>%
#   summarise(Trips = mean(Trips)) %>%
#   ungroup() %>%
#   filter(Trips == max(Trips))


#** 4 ----
aus_production %>% autoplot(Bricks)

pelt %>% autoplot(Lynx)

gafa_stock %>% autoplot(Close)

vic_elec %>%
  autoplot(Demand) +
  xlab("Time (30 min)") + ylab("Demand (MW)") +
  ggtitle("Australian Electricity Demand")


#** 5 ----
aus_arrivals %>% autoplot(Arrivals)
# # The biggest number of arrivals came from New Zealand in 1980s. And the title owner country changed to Japan in 1990s and came back to UK in 2000s.
# The arrival data of UK shows the biggest quarterly fluctuation.

aus_arrivals %>% gg_season(Arrivals)
# NZ has lowest arrivals in Q1 which increases and then flatens in Q3
# Uk has a dip in Q2 and Q3 and peaks in Q4

aus_arrivals %>% gg_subseries(Arrivals)
# The arrivals from Japan decrease a lot in 2nd quarter compared to the other quarteres.
# The arrivals from New Zealand are highest in 3rd quarter and lowest in 1st quarter.
# The arrivals from UK and US are low in 2nd and 3rd quarters and high in 1st and 4th quarters.


#** 6 ----
set.seed(123)
myseries <- aus_retail %>%
 filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries %>% autoplot(Turnover)
# increasing trend with seasonality

myseries %>% gg_season(Turnover)
# decreases in Feb, peaks in Dec

myseries %>% gg_subseries(Turnover)
# peaks in Dec, starts increasing from Jan

myseries %>% gg_lag(Turnover, geom = "point")
# data is not random, with increasing trend showcasing high correlation with historical values

myseries %>% ACF(Turnover, lag_max = 50) %>% autoplot()
# trend and seasonlity are confirmed
# correlation with lag = 1, 12


#** 7 ----
#*** us_employment ----
us_employment_total_private <- us_employment %>% filter(Title == "Total Private")

us_employment_total_private %>% autoplot(Employed)
# strong trend; seasonlity(?)

us_employment_total_private %>% gg_season(Employed)
# no seasonlity

us_employment_total_private %>% gg_subseries(Employed)
# no seasonlity, mean and trend same for every month

us_employment_total_private %>% gg_lag(Employed, geom = "point", lag = 1:13)
# strong correlation with lagged values

us_employment_total_private %>% ACF(Employed, lag_max = 48) %>% autoplot()
# strong trend


#*** aus_production_bricks ----
aus_production %>% autoplot(Bricks)
# increasing trend till 1980's then decreasing trend; seasonlity(?)

aus_production %>% gg_season(Bricks)
# Increasing till Q3

aus_production %>% gg_subseries(Bricks)
# Increasing till Q3, then dips; Peaked in 1980 and 1990, then started decreasing

aus_production %>% gg_lag(Bricks, geom = "point")
# strong correlation till lag 4 which becomes progressively weaker

aus_production %>% ACF(Bricks) %>% autoplot()
# trend, with seasonality every 4 Quarters


#*** pelt_hare ----
pelt %>% autoplot(Hare)
# no trend; seasonlity(?)

pelt %>% gg_season(Hare)
# can't plot for annual data

pelt %>% gg_subseries(Hare)
# not useful for annual data

pelt %>% gg_lag(Hare, geom = "point")
# positive to negative cycle(?)

pelt %>% ACF(Hare) %>% autoplot()
# Cycle of 5 years


#*** PBS_H02 ----
PBS_H02 <- PBS %>%
  as_tibble() %>%
  filter(ATC2 == "H02") %>%
  group_by(Month) %>%
  summarise(total_cost = sum(Cost)) %>%
  ungroup %>%
  as_tsibble(index = Month)

PBS_H02 %>% autoplot(total_cost)
# increasing trend

PBS_H02 %>% gg_season(total_cost)
# Peak in Jan/Dec; trough in Feb

PBS_H02 %>% gg_subseries(total_cost)
# Peak in Jan/Dec; trough in Feb; increasing from Feb

PBS_H02 %>% gg_lag(total_cost, geom = "point")
# strong correlation; changing halfs(?)

PBS_H02 %>% ACF(total_cost) %>% autoplot()
# season trend changes every 6 months(?)


#*** us_gasoline ----
us_gasoline %>% autoplot()
# increasing trend; seasonlity(?)

us_gasoline %>% gg_season()
# troughs in Jan; peaks in Jun

us_gasoline %>% gg_subseries()
# increasing towards mid year, then minor peaks and dips

us_gasoline %>% gg_lag(geom = "point")
# positive correlation

us_gasoline %>% ACF(lag_max = 75) %>% autoplot()
# deccreasing trend till half year, then increases


#** 8 ----
# visual check
# 1 - B; 2 - A; 3 - D; 4 - C


#** 9 ----
aus_livestock_pigs_victoria_90_95 <- 
  aus_livestock %>%
  filter(Animal == "Pigs", States == "Vicotria", year(Month) >= 1990, year(Month) <= 1995)

aus_livestock_pigs_victoria_90_95 %>% autoplot()

aus_livestock_pigs_victoria_90_95 %>% ACF() %>% autoplot()
# increasing trend; with lag correltaion spike at 3, 6, 12 mark


#** 10.a ----
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))
# dgoog %>% select(Date, trading_day, Close, diff)

# dgoog_org_index <- gafa_stock %>%
#   filter(Symbol == "GOOG", year(Date) >= 2018) %>%
#   mutate(diff = difference(Close))
# dgoog %>% select(Date, Close, diff)


#** 10.b ----
# this is to account for the weekend/market holidays which breaks the trend and might give incorrect result when annalysing visually
# check 10.c


#** 10.c ----
dgoog %>% autoplot(diff)
# no trend and seasonality
# dgoog_org_index %>% autoplot(diff)

dgoog %>% ACF(diff) %>% autoplot()
# no correlation
# dgoog_org_index %>% ACF(diff) %>% autoplot()
#   throws error due to missing dates


#** 10.d ----
# the change in prices looks like white noise


# ____----


# Chapter 3 Time series decomposition -------------------------------------
# When we decompose a time series into components, we usually combine the trend and cycle into a single trend-cycle component (often just called the trend for simplicity).
# Thus we think of a time series as comprising three components:
#   a trend-cycle component,
#   a seasonal component, and 
#   a remainder component (containing anything else in the time series).

# Often this is done to help improve understanding of the time series, but it can also be used to improve forecast accuracy.

# When decomposing a time series, it is sometimes helpful to first transform or adjust the series in order to make the decomposition (and later analysis) as simple as possible.


#* 3.1 Transformations and adjustments -------------------------------------
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


#* 3.2 Time series components ----------------------------------------------
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


#* 3.3 Moving averages -----------------------------------------------------
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


#* 3.4 Classical decomposition ---------------------------------------------
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


#* 3.5 X11 decomposition ---------------------------------------------------
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


#* 3.6 SEATS decomposition -------------------------------------------------
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


#* 3.7 STL decomposition ---------------------------------------------------
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


#* 3.8 Exercises -----------------------------------------------------------
#** 1 ----
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


#** 2 ----
#*** United States GDP from global_economy ----
global_economoy %>%
  filter(Code == "USA") %>%
  autoplot(GDP)
# Smooth series with minor dip around 2008; no transformation neccessary


#*** Slaughter of Victorian "Bulls, bullocks and steers" in aus_livestock ----
aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") %>%
  autoplot()
# Downward trend; very large range of values; transformation will help

aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") %>%
  mutate(log10_Count = log10(Count)) %>%
  autoplot(log10_Count)
# enhances the height of the smaller seasonal variations to make it inline with the larger values


#*** Victorian Electricity Demand from vic_elec ----
vic_elec %>%
  autoplot(Demand)
# No trend with several large peaks at the start of each year; transformation might help


lambda <- vic_elec %>%
  features(Demand, features = guerrero) %>%
  pull(lambda_guerrero)

vic_elec %>%
  autoplot(box_cox(Demand, lambda))
# the distances of the peaks and the troughs are reduced


#*** Gas production from aus_production ----
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


#** 3 ----
canadian_gas %>%
  autoplot()

lambda <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

canadian_gas %>%
  autoplot(box_cox(Volume, lambda))
# box_cox is unhelpful since the data post 1992 has a completly different pattern compared to the pre 1992 data.
# The transformation doesn't give a simpler pattern


#** 4 ----
myseries %>%
  autoplot(Turnover)

lambda <- myseries %>%
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)

myseries %>%
  autoplot(box_cox(Turnover, lambda))
# lambda = 0.22


#** 5 ----
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


#** 6 ----
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


#** 7 ----
plastics_tsibble <- plastics %>%
  as_tsibble() %>%
  rename(Month = index, Sales = value)

#** 7.a ----
plastics_tsibble %>% autoplot()
# increasing trend, seasonality of 12 months with peaks in Jun/Jul

plastics_tsibble %>% gg_season()
# confirms 12 month seasonality

#** 7.b ----
plastics_tsibble %>%
  model(classical_decomposition(Sales, type = "multiplicative")) %>%
  components() %>%
  autoplot() + xlab("Month") +
  ggtitle("Classical multiplicative decomposition of total Monthly Sales")

#** 7.c ----
# Yes, results support the graphical interpretation from part a

#** 7.d ----
plastics_tsibble %>%
  model(classical_decomposition(Sales, type = "multiplicative")) %>%
  components() %>%
  select(Month, season_adjust) %>%
  autoplot(season_adjust) + xlab("Month") +
  ggtitle("Seasonaly Adjusted total Monthly Sales")

#** 7.e ----
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


#** 8 ----
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


#** 9.a ----
# increasing trend
# variation in the seasonal component least
# Outlier around 1991/1992
# Seasonal components peak in Dec with a second peak in Mar; troughs in Jan and Aug

#** 9.b ----
# Yes


#** 10.a ----
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
canadian_gas_stl <- canadian_gas %>%
  model(STL(Volume ~ season(window = 13)), robust = TRUE) %>%
  components()

canadian_gas_stl %>%
  autoplot()

# can see that the size of seasonality increased in 1980s and decreased in 1990s.

#** 10.c ----
canadian_gas_stl %>%
  gg_season(season_year)
# there is a changing seasonality in 1980-1990. more linear before that

#** 10.d ----
canadian_gas_stl %>%
  select(Month, season_adjust) %>%
  autoplot(season_adjust) +
  xlab("Month") + ylab("Seasonaly Adjusted Production Volume")

#** 10.e ----
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



# ____----


# Chapter 4 Time series features ----------------------------------------
# time series features are numerical summaries computed from the series.
#   e.g. autocorrelations, Guerrero estimate
# We can compute many different features on many different time series, and use them to explore the properties of the series.


#* 4.1 Some simple statistics ----------------------------------------------
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


#* 4.2 ACF features --------------------------------------------------------
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


#* 4.3 STL Features --------------------------------------------------------
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


#* 4.4 Other features ------------------------------------------------------
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


#* 4.5 Exploring Australian tourism data -----------------------------------
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


#* 4.6 Exercises -----------------------------------------------------------
#** 1 ----
# calculate mean and std
PBS2 <- PBS %>%
  group_by(Concession, Type, ATC1, ATC2) %>%
  mutate(Mean = mean(Cost), STD = sd(Cost)) %>%
  ungroup()

PBS2 %>%
  filter(Mean == max(Mean)) %>%
  mutate(Series = glue:glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  ggtitle("Series with highest average Cost")

PBS2 %>%
  filter(STD == min(STD)) %>%
  mutate(Series = glue:glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  ggtitle("Series with lowest standard deviation in Cost")


#** 2 ----
tourism_features <- tourism %>%
  features(Trips, feature_set("feasts"))

tourism %>%
  features(Trips, feat_stl) %>%
  select(-Region, -State, -Region) %>%
  mutate(
    seasonal_peak_year = factor(seasonal_peal_year + 1, labels = c("Q1", "Q2", "Q3", "Q4")),
    seasonal_trough_year = factor(seasonal_trough_year + 1, labels = c("Q1", "Q2", "Q3", "Q4"))
  ) %>%
  ggpairs(mapping = aes(color = Purpose), upper = "black", legend = 1) +
  theme(legend.position = 'top')

tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) %>%
  features(Trips, feat_stl) %>%
  select(State, seasonal_peak_year)


#** 3 ----
PBS %>%
  as_tibble() %>%
  group_by(Concession, Type, ATC1, ATC2) %>%
  summarise(Total_Scripts = sum(Scripts), Total_Costs = sum(Cost)) %>%
  filter(Total_Scripts == 0, Total_Costs == 0)

PBS_nozeros <- PBS %>%
  filter(!(Concession == "General" & Type == "Co-payments" & (ATC2 %in% c("R", "S"))))

PBS_features <- PBS_nozeros %>%
  features(Cost, feature_set(pkgs = "feasts"))

PBS_pcs <- PBS_features %>%
  select(-Concession, -Type, -ATC1, -ATC2)

PBS_pcs <- as_tibble(t(na.omit(t(PBS_pcs)))) %>%
  prcomp(scale = TRUE) %>%
  augment(PBS_features)

PBS_pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point()

PBS_outliers <- PBS_pcs %>%
  filter(.fittedPC1 > 5.5) %>%
  select(Concession, Type, ATC1, ATC2, .fittedPC1, .fittedPC2)

PBS_outliers %>%
  left_join(PBS, by = c("Concession", "Type", "ATC1", "ATC2")) %>%
  mutate(Series = glue::glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  ggtitle("Outlying time series in PC space")


# ____----


# Chapter 5 The forecaster's toolbox --------------------------------------
#* 5.1 A tidy forecasting workflow -----------------------------------------
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


#* 5.3 Fitted values and residuals -----------------------------------------
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


#* 5.4 Residual diagnostics ------------------------------------------------
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


#* 5.10 Time series cross-validation ---------------------------------------
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


#* 5.11 Exercises ----------------------------------------------------------
#** 1 ----
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


#** 2 ----
fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

fb_tr <- fb_stock %>%
  filter(year(Date) <= 2017)

fb_test <- fb_stock %>%
  filter(yearmonth(Date) == yearmonth("2018 Jan"))


#*** 2.a ----
fb_stock %>% autoplot(Close)

#*** 2.b ----
fb_fit <- fb_tr %>%
  model(Drift = RW(Close ~ drift()))

fb_fc <- fb_fit %>%
  forecast(h = 12)

fb_fc %>%
  autoplot(fb_tr)


#*** 2.c ----
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


#*** 2.d ----
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


#** 3 ----
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


#** 4 ----
#*** AUS Exports ----
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


#*** Bricks ----
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


#** 5 ----
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


#** 6 ----
#*** 6.a ----
# False, normally distributed residuals make calculating prediction intervals easier. However, they do not neccessarily improve the forecast itself.

#*** 6.b ---- 
# False, residuals are the difference between fitted and actual values. They are based on the train data. So overfitted models give small residuals but the forecast might be bad.

#*** 6.c ----
# False, MAPE is a good measure but not the best. It has issues like
#   - assumes data has an absolute zero
#   - heavier penalty for negative zeros than positive
#   - is infinite/very high if the actual value is equal or close to 0

#*** 6.d ----
# True, if forecasts are not good might need to perform transformations on the data or using more complicated models.

#*** 6.e ----
# True, however, there is a possibility that the best model can be further improved.


#** 7 ----
# Q2.10 - 6. A new question (Q3 usgas) was added later leading to down shift of the Q#
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

#*** 7.a ----
myseries_tr <- myseries %>%
  filter(Month <= yearmonth("2010 Dec"))

#*** 7.b ----
autoplot(myseries, Turnover) +
  autolayer(myseries_tr, Turnover, color = "red")

#*** 7.c ----
fit <- myseries_tr %>%
  model(SNAIVE(Turnover))

fc <- fit %>%
  forecast()

fc %>%
  autoplot(myseries_tr, level = NULL)

fc %>%
  autoplot(myseries, level = NULL)

#*** 7.d ----
fit %>% accuracy()
fc %>% accuracy(myseries)

# accuracy of the forceast is slightly better than the fitted accuracy

#*** 7.e ----
fit %>% gg_tsresiduals()

# there are visible deviations form mean (especially around the 2000-2010 period) 
# the histogram is normal
# the acf plot shows significant correlation leading to the possibility of improving the model

#*** 7.f ----
myseries %>%
  filter(year(Month) >= 2005, Month <= yearmonth("2010 Dec")) %>%
  model(SNAIVE(Turnover)) %>%
  # augment()
  forecast() %>%
  # accuracy(myseries)
  autoplot(myseries %>% filter(year(Month) >= 2005, Month <= yearmonth("2010 Dec"))) # ,level = NULL 

# the mean forecast and hence the accuracy measures do not change based on the training data. However, the prediction changes.
# So accuracy measures have low sensitivity to amount of training data.


#** 8 ----
pigs <- aus_livestock %>%
  filter(Animal == "Pigs", State == "New South Wales")

#*** 8.a ----
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


#*** 8.b ----
pigs_test <- pigs %>% slice(n() - 71:0)

pigs_tr <- pigs %>% anti_join(pigs_test)


#*** 8.c ----
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


#*** 8.d ----
pigs_fit %>% select(Drift) %>% gg_tsresiduals()

# residuals trend is distributed around 0, however they do not have constant variance
# the residual histogram is right shifted (non-normal)
# there is significant correlation visible in the lag plot

# the residuals do not resemble white noise


#** 9 ----
#*** 9.a ----
wealth_test <- hh_budget %>%
  group_by(Country) %>%
  slice(Year, n()-3:0)

wealth_tr <- anti_join(hh_budget, wealth_test)

wealth_tr %>% autoplot(Wealth)


#*** 9.b ----
wealth_fit <- wealth_tr %>%
  model(
    Naive = NAIVE(Wealth),
    Drift = RW(Wealth ~ drift())
  )
# SNaive not possible since it is annual data

wealth_fc <- wealth_fit %>%
  forecast(wealth_test)


#*** 9.c ----
wealth_fc %>%
  autoplot(hh_budget, level = NULL)

accuracy(wealth_fc, hh_budget) %>% arrange(Country)
# drift is the best


#*** 9.d ----
wealth_fit %>%
  select(Drift) %>%
  filter(Country == "USA") %>%
  gg_tsresiduals()

# residuals represent white noise


#** 10 ----
#*** 10.a ----
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


#*** 10.b ----
takeaway_fit <- takeaway_tr %>%
  model(
    Naive = NAIVE(total_turnover),
    SNaive = SNAIVE(total_turnover),
    Drift = RW(total_turnover ~ drift())
  )

takeaway_fc <- takeaway_fit %>%
  forecast(takeaway_test)


#*** 10.c ----
takeaway_fc %>%
  autoplot(takeaway, level = NULL)

accuracy(takeaway_fc, takeaway)

# Naive is the best


#*** 10.d ----
takeaway_fit %>% select(Naive) %>% gg_tsresiduals()

# residuals do not represent white noise


#** 11 ----
aus_bricks <- aus_production %>%
  # filter(between(year(Quarter), 1956, 2005))
  filter_index("1956 Q1" ~ "2005 Q2")

aus_bricks %>% autoplot(Bricks)


#*** 11.a ----
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


#*** 11.b ----
bricks_stl_fixed %>%
  autoplot(season_adjust) +
  ggtitle("Fixed Seasonality")

bricks_stl_changing %>%
  autoplot(season_adjust) +
  ggtitle("Changing Seasonality")

# changing seasonality has a smoother seasonality adjusted than fixed


#*** 11.c ----
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


#*** 11.d ----
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


#*** 11.e ----
bricks_fixed_dcmp %>% gg_tsresiduals()
# residuals are spread around 0 with a slight positive focus
# the histogram shows a similar left skew
# the acf plot shows correlation

bricks_changing_dcmp %>% gg_tsresiduals()


#*** 11.f ----
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


#*** 11.g ----
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


#** 12 ----
#*** 12.a ----
gc_tourism <- tourism %>%
  filter(Region == "Gold Coast") %>%
  group_by(Region) %>%
  summarise(Trips = sum(Trips))


#*** 12.b ----
gc_train_1 <- gc_tourism %>% slice(1:(n()-4))

gc_train_2 <- gc_tourism %>% slice(1:(n()-8))

gc_train_3 <- gc_tourism %>% slice(1:(n()-12))


#*** 12.c ----
gc_fc_1 <- gc_train_1 %>% model(SNAIVE(Trips)) %>% forecast(h = 4)

gc_fc_2 <- gc_train_2 %>% model(SNAIVE(Trips)) %>% forecast(h = 8)

gc_fc_3 <- gc_train_3 %>% model(SNAIVE(Trips)) %>% forecast(h = 12)


#*** 12.d ----
gc_fc_1 %>% autoplot(gc_tourism, level = NULL)

gc_fc_2 %>% autoplot(gc_tourism, level = NULL)

gc_fc_3 %>% autoplot(gc_tourism, level = NULL)


accuracy(gc_fc_1, gc_tourism)
accuracy(gc_fc_2, gc_tourism)
accuracy(gc_fc_3, gc_tourism)

# the best model is 2, followed by 3 then 1


# ____----


# Chapter 6 Judgmental forecasts ------------------------------------------
# Forecasting using judgment is common in practice.
# In many cases, judgmental forecasting is the only option, such as when there is a complete lack of historical data, or when a new product is being launched, or when a new competitor enters the market, or during completely new and unique market conditions.

# There are also situations where the data are incomplete, or only become available after some delay.
#   example, central banks include judgment when forecasting the current level of economic activity, a procedure known as nowcasting, as GDP is only available on a quarterly basis.

# Research in this area has shown that the accuracy of judgmental forecasting improves when the forecaster has
#   (i) important domain knowledge, and
#   (ii) more timely, up-to-date information.
# A judgmental approach can be quick to adjust to such changes, information or events.

# Over the years, the acceptance of judgmental forecasting as a science has increased, as has the recognition of its need.
# More importantly, the quality of judgmental forecasts has also improved, as a direct result of recognising that improvements in judgmental forecasting can be achieved by implementing well-structured and systematic approaches.
# It is important to recognise that judgmental forecasting is subjective and comes with limitations.
# However, implementing systematic and well-structured approaches can confine these limitations and markedly improve forecast accuracy.

# There are three general settings in which judgmental forecasting is used:
#   (i) there are no available data, so that statistical methods are not applicable and judgmental forecasting is the only feasible approach;
#   (ii) data are available, statistical forecasts are generated, and these are then adjusted using judgment; and
#   (iii) data are available and statistical and judgmental forecasts are generated independently and then combined.
# We should clarify that when data are available, applying statistical methods (such as those discussed in other chapters of this book), is preferable and should always be used as a starting point.
# Statistical forecasts are generally superior to generating forecasts using only judgment.


# _----


#* 6.1 Beware of limitations -----------------------------------------------
# Judgmental forecasts are subjective, and therefore do not come free of bias or limitations.

# Judgmental forecasts can be inconsistent.
# Unlike statistical forecasts, which can be generated by the same mathematical formulas every time, judgmental forecasts depend heavily on human cognition, and are vulnerable to its limitations.
#   example,
#     a limited memory may render recent events more important than they actually are and may ignore momentous events from the more distant past; or
#     a limited attention span may result in important information being missed; or
#     a misunderstanding of causal relationships may lead to erroneous inferences. 
# Furthermore, human judgment can vary due to the effect of psychological factors. 
#   One can imagine a manager who is in a positive frame of mind one day, generating forecasts that may tend to be somewhat optimistic, and in a negative frame of mind another day, generating somewhat less optimistic forecasts.

# Judgment can be clouded by personal or political agendas, where targets and forecasts (as defined in Chapter 1) are not segregated.
#   example, if a sales manager knows that the forecasts she generates will be used to set sales expectations (targets), she may tend to set these low in order to show a good performance (i.e., exceed the expected targets).
# Even in cases where targets and forecasts are well segregated, judgment may be plagued by optimism or wishful thinking.
#   example, it would be highly unlikely that a team working towards launching a new product would forecast its failure. 
# this optimism can be accentuated in a group meeting setting. “Beware of the enthusiasm of your marketing and sales colleagues”.

# Another undesirable property which is commonly seen in judgmental forecasting is the effect of anchoring.
# In this case, the subsequent forecasts tend to converge or be close to an initial familiar reference point.
#   example, it is common to take the last observed value as a reference point.
# The forecaster is influenced unduly by prior information, and therefore gives this more weight in the forecasting process.
# Anchoring may lead to conservatism and undervaluing new and more current information, and thereby create a systematic bias.


# _----


#* 6.2 Key principles ------------------------------------------------------
# Using a systematic and well structured approach in judgmental forecasting helps to reduce the adverse effects of the limitations of judgmental forecasting, some of which we listed in the previous section.
# Whether this approach involves one individual or many, the following principles should be followed.


#** Set the forecasting task clearly and concisely ----
# Care is needed when setting the forecasting challenges and expressing the forecasting tasks.
# It is important that everyone be clear about what the task is.
# All definitions should be clear and comprehensive, avoiding ambiguous and vague expressions.
# Also, it is important to avoid incorporating emotive terms and irrelevant information that may distract the forecaster.
# In the Delphi method that follows (see Section 6.3), it may sometimes be useful to conduct a preliminary round of information gathering before setting the forecasting task.


#** Implement a systematic approach ----
# Forecast accuracy and consistency can be improved by using a systematic approach to judgmental forecasting involving checklists of categories of information which are relevant to the forecasting task.
#   example, it is helpful to identify what information is important and how this information is to be weighted.
#   When forecasting the demand for a new product, what factors should we account for and how should we account for them?
#   Should it be the price, the quality and/or quantity of the competition, the economic environment at the time, the target population of the product?
# It is worthwhile to devote significant effort and resources to put together decision rules that will lead to the best possible systematic approach.


#** Document and justify ----
# Formalising and documenting the decision rules and assumptions implemented in the systematic approach can promote consistency, as the same rules can be implemented repeatedly.
# Also, requesting a forecaster to document and justify their forecasts leads to accountability, which can lead to reduced bias.
# Furthermore, formal documentation aids significantly in the systematic evaluation process that is suggested next.


#** Systematically evaluate forecasts ----
# Systematically monitoring the forecasting process can identify unforeseen irregularities.
# In particular, keep records of forecasts and use them to obtain feedback when the corresponding observations become available.
# Although you may do your best as a forecaster, the environment you operate in is dynamic.
# Changes occur, and you need to monitor these in order to evaluate the decision rules and assumptions.
# Feedback and evaluation help forecasters learn and improve their forecast accuracy.


#** Segregate forecasters and users ----
# Forecast accuracy may be impeded if the forecasting task is carried out by users of the forecasts, such as those responsible for implementing plans of action about which the forecast is concerned.
#   We should clarify again here (as in Section 1.2), that forecasting is about predicting the future as accurately as possible, given all of the information available, including historical data and knowledge of any future events that may impact the forecasts.
# Forecasters and users should be clearly segregated.

# It is important that forecasters communicate forecasts to potential users thoroughly.
# As we will see in Section 6.7, users may feel distant and disconnected from forecasts, and may not have full confidence in them.
# Explaining and clarifying the process and justifying the basic assumptions that led to the forecasts will provide some assurance to users.

# The way in which forecasts may then be used and implemented will clearly depend on managerial decision making.
#   example, management may decide to adjust a forecast upwards (be over-optimistic), as the forecast may be used to guide purchasing and stock keeping levels.
#   Such a decision may be taken after a cost-benefit analysis reveals that the cost of holding excess stock is much lower than that of lost sales.
# This type of adjustment should be part of setting goals or planning supply, rather than part of the forecasting process.
# In contrast, if forecasts are used as targets, they may be set low so that they can be exceeded more easily.
# Again, setting targets is different from producing forecasts, and the two should not be confused.


#** Example: Pharmaceutical Benefits Scheme (PBS) ----


# _----


#* 6.3 The Delphi method ---------------------------------------------------
# invented by Olaf Helmer and Norman Dalkey of the Rand Corporation in the 1950s
# The method relies on the key assumption that forecasts from a group are generally more accurate than those from individuals.
# The aim of the Delphi method is to construct consensus forecasts from a group of experts in a structured iterative manner.
# A facilitator is appointed in order to implement and manage the process.
# The Delphi method generally involves the following stages:
#   1. A panel of experts is assembled.
#   2. Forecasting tasks/challenges are set and distributed to the experts.
#   3. Experts return initial forecasts and justifications. These are compiled and summarised in order to provide feedback.
#   4. Feedback is provided to the experts, who now review their forecasts in light of the feedback. This step may be iterated until a satisfactory level of consensus is reached.
#   5. Final forecasts are constructed by aggregating the experts’ forecasts.
# Each stage of the Delphi method comes with its own challenges. In what follows, we provide some suggestions and discussions about each one of these.


#** Experts and anonymity ----
# first challenge of the facilitator is to identify a group of experts who can contribute to the forecasting task.
# The usual suggestion is somewhere between 5 and 20 experts with diverse expertise.
# Experts submit forecasts and also provide detailed qualitative justifications for these.

# A key feature of the Delphi method is that the participating experts remain anonymous at all times.
# This means that the experts cannot be influenced by political and social pressures in their forecasts.
# Furthermore, all experts are given an equal say and all are held accountable for their forecasts.
#   This avoids the situation where a group meeting is held and some members do not contribute, while others dominate.
#   It also prevents members exerting undue influence based on seniority or personality.
# There have been suggestions that even something as simple as the seating arrangements in a group setting can influence the group dynamics.
# Furthermore, there is ample evidence that a group meeting setting promotes enthusiasm and influences individual judgment, leading to optimism and overconfidence.

# A by-product of anonymity is that the experts do not need to meet as a group in a physical location.
# An important advantage of this is that it increases the likelihood of gathering experts with diverse skills and expertise from varying locations.
# Furthermore, it makes the process cost-effective by eliminating the expense and inconvenience of travel, and it makes it flexible, as the experts only have to meet a common deadline for submitting forecasts, rather than having to set a common meeting time.


#** Setting the forecasting task in a Delphi ----
# In a Delphi setting, it may be useful to conduct a preliminary round of information gathering from the experts before setting the forecasting tasks.
# Alternatively, as experts submit their initial forecasts and justifications, valuable information which is not shared between all experts can be identified by the facilitator when compiling the feedback.


#** Feedback ----
# Feedback to the experts should include summary statistics of the forecasts and outlines of qualitative justifications.
# Numerical data summaries and graphical representations can be used to summarise the experts’ forecasts.

# As the feedback is controlled by the facilitator, there may be scope to direct attention and information from the experts to areas where it is most required.
#   example, the facilitator may direct the experts’ attention to responses that fall outside the interquartile range, and the qualitative justification for such forecasts.


#** Iteration ----
# The process of the experts submitting forecasts, receiving feedback, and reviewing their forecasts in light of the feedback, is repeated until a satisfactory level of consensus between the experts is reached.
# Satisfactory consensus does not mean complete convergence in the forecast value; it simply means that the variability of the responses has decreased to a satisfactory level.
# Usually two or three rounds are sufficient.
# Experts are more likely to drop out as the number of iterations increases, so too many rounds should be avoided.


#** Final forecasts ----
# The final forecasts are usually constructed by giving equal weight to all of the experts’ forecasts.
# However, the facilitator should keep in mind the possibility of extreme values which can distort the final forecast.


#** Limitations and variations ----
# Applying the Delphi method can be time consuming.
# In a group meeting, final forecasts can possibly be reached in hours or even minutes — something which is almost impossible to do in a Delphi setting.
#   If it is taking a long time to reach a consensus in a Delphi setting, the panel may lose interest and cohesiveness.

# In a group setting, personal interactions can lead to quicker and better clarifications of qualitative justifications.
# A variation of the Delphi method which is often applied is the “estimate-talk-estimate” method, where the experts can interact between iterations, although the forecast submissions can still remain anonymous.
# A disadvantage of this variation is the possibility of the loudest person exerting undue influence.


#** The facilitator ----
# The role of the facilitator is of the utmost importance.
# The facilitator is largely responsible for the design and administration of the Delphi process.
# The facilitator is also responsible for providing feedback to the experts and generating the final forecasts.
# In this role, the facilitator needs to be experienced enough to recognise areas that may need more attention, and to direct the experts’ attention to these.
# Also, as there is no face-to-face interaction between the experts, the facilitator is responsible for disseminating important information.
# The efficiency and effectiveness of the facilitator can dramatically increase the probability of a successful Delphi method in a judgmental forecasting setting.


# _----


#* 6.4 Forecasting by analogy ----------------------------------------------
#   example is the pricing of a house through an appraisal process.
#     An appraiser estimates the market value of a house by comparing it to similar properties that have sold in the area.
#     The degree of similarity depends on the attributes considered.
#     With house appraisals, attributes such as land size, dwelling size, numbers of bedrooms and bathrooms, and garage space are usually considered.

# Even thinking and discussing analogous products or situations can generate useful (and sometimes crucial) information.


#** Example: Designing a high school curriculum ----
# A small group of academics and teachers were assigned the task of developing a curriculum for teaching judgment and decision making under uncertainty for high schools in Israel. Each group member was asked to forecast how long it would take for the curriculum to be completed. Responses ranged between 18 and 30 months. One of the group members who was an expert in curriculum design was asked to consider analogous curricula developments around the world. He concluded that 40% of analogous groups he considered never completed the task. The rest took between 7 to 10 years. The Israel project was completed in 8 years.

# Obviously, forecasting by analogy comes with challenges.
# We should aspire to base forecasts on multiple analogies rather than a single analogy, which may create biases.
#   However, these may be challenging to identify.
# Similarly, we should aspire to consider multiple attributes.
#   Identifying or even comparing these may not always be straightforward.
# As always, we suggest performing these comparisons and the forecasting process using a systematic approach.
# Developing a detailed scoring mechanism to rank attributes and record the process of ranking will always be useful.


#** A structured analogy ----
# Alternatively, a structured approach comprising a panel of experts can be implemented,
#   as was proposed by Green & Armstrong (2007).
# The concept is similar to that of a Delphi; however, the forecasting task is completed by considering analogies.
# First, a facilitator is appointed.
# Then the structured approach involves the following steps.
#   1. A panel of experts who are likely to have experience with analogous situations is assembled.
#   2. Tasks/challenges are set and distributed to the experts.
#   3. Experts identify and describe as many analogies as they can, and generate forecasts based on each analogy.
#   4. Experts list similarities and differences of each analogy to the target situation, then rate the similarity of each analogy to the target situation on a scale.
#   5. Forecasts are derived by the facilitator using a set rule. This can be a weighted average, where the weights can be guided by the ranking scores of each analogy by the experts.

# As with the Delphi approach, anonymity of the experts may be an advantage in not suppressing creativity, but could hinder collaboration. Green and Armstrong found no gain in collaboration between the experts in their results.
# A key finding was that experts with multiple analogies (more than two), and who had direct experience with the analogies, generated the most accurate forecasts.


# _----


#* 6.5 Scenario forecasting ------------------------------------------------
# The aim of this approach is to generate forecasts based on plausible scenarios.
# In contrast to the two previous approaches (Delphi and forecasting by analogy) where the resulting forecast is intended to be a likely outcome, each scenario-based forecast may have a low probability of occurrence.
# The scenarios are generated by considering all possible factors or drivers, their relative impacts, the interactions between them, and the targets to be forecast.

# Building forecasts based on scenarios allows a wide range of possible forecasts to be generated and some extremes to be identified.
#   example it is usual for “best,” “middle” and “worst” case scenarios to be presented, although many other scenarios will be generated.
# Thinking about and documenting these contrasting extremes can lead to early contingency planning.

# With scenario forecasting, decision makers often participate in the generation of scenarios.
# While this may lead to some biases, it can ease the communication of the scenario-based forecasts, and lead to a better understanding of the results.


# _----


#* 6.6 New product forecasting ---------------------------------------------
# The definition of a new product can vary.
#   It may be an entirely new product which has been launched,
#   a variation of an existing product (“new and improved”),
#   a change in the pricing scheme of an existing product, or
#   even an existing product entering a new market.

# Judgmental forecasting is usually the only available method for new product forecasting, as historical data are unavailable.
# The approaches we have already outlined (Delphi, forecasting by analogy and scenario forecasting) are all applicable when forecasting the demand for a new product.

# Other methods which are more specific to the situation are also available.
# These methods are less structured than those already discussed, and are likely to lead to more biased forecasts as a result.


#** Sales force composite ----
# In this approach, forecasts for each outlet/branch/store of a company are generated by salespeople, and are then aggregated.
# This usually involves sales managers forecasting the demand for the outlet they manage.
# Salespeople are usually closest to the interaction between customers and products, and often develop an intuition about customer purchasing intentions.
# They bring this valuable experience and expertise to the forecast.

# However, having salespeople generate forecasts violates the key principle of segregating forecasters and users, which can create biases in many directions.
# It is common for the performance of a salesperson to be evaluated against the sales forecasts or expectations set beforehand.
# In this case, the salesperson acting as a forecaster may introduce some self-serving bias by generating low forecasts.
# On the other hand, one can imagine an enthusiastic salesperson, full of optimism, generating high forecasts.

# Moreover a successful salesperson is not necessarily a successful nor well-informed forecaster.
# A large proportion of salespeople will have no or limited formal training in forecasting.
# Finally, salespeople will feel customer displeasure at first hand if, for example, the product runs out or is not introduced in their store.
# Such interactions will cloud their judgment.


#** Executive opinion ----
# In contrast to the sales force composite, this approach involves staff at the top of the managerial structure generating aggregate forecasts.
# Such forecasts are usually generated in a group meeting, where executives contribute information from their own area of the company.
# Having executives from different functional areas of the company promotes great skill and knowledge diversity in the group.

# This process carries all of the advantages and disadvantages of a group meeting setting which we discussed earlier.
# In this setting, it is important to justify and document the forecasting process.
#   That is, executives need to be held accountable in order to reduce the biases generated by the group meeting setting.
# There may also be scope to apply variations to a Delphi approach in this setting; for example, the estimate-talk-estimate process described earlier.


#** Customer intentions ----
# Customer intentions can be used to forecast the demand for a new product or for a variation on an existing product.
# Questionnaires are filled in by customers on their intentions to buy the product.
# A structured questionnaire is used, asking customers to rate the likelihood of them purchasing the product on a scale; for example, highly likely, likely, possible, unlikely, highly unlikely.

# Survey design challenges, such as collecting a representative sample, applying a time- and cost-effective method, and dealing with non-responses, need to be addressed.

# Furthermore, in this survey setting we must keep in mind the relationship between purchase intention and purchase behaviour.
# Customers do not always do what they say they will.
# Many studies have found a positive correlation between purchase intentions and purchase behaviour; however, the strength of these correlations varies substantially.
# The factors driving this variation include the timings of data collection and product launch, the definition of “new” for the product, and the type of industry.
# Behavioural theory tells us that intentions predict behaviour if the intentions are measured just before the behaviour.
# The time between intention and behaviour will vary depending on whether it is a completely new product or a variation on an existing product.
# Also, the correlation between intention and behaviour is found to be stronger for variations on existing and familiar products than for completely new products.

# Whichever method of new product forecasting is used, it is important to thoroughly document the forecasts made, and the reasoning behind them, in order to be able to evaluate them when data become available.


# _----


#* 6.7 Judgmental adjustments ----------------------------------------------
# consider the situation where historical data are available and are used to generate statistical forecasts.
# It is common for practitioners to then apply judgmental adjustments to these forecasts.
# These adjustments can potentially provide all of the advantages of judgmental forecasting which have been discussed earlier in this chapter.
#   example, they provide an avenue for incorporating factors that may not be accounted for in the statistical model, such as promotions, large sporting events, holidays, or recent events that are not yet reflected in the data.
# However, these advantages come to fruition only when the right conditions are present.
# Judgmental adjustments, like judgmental forecasts, come with biases and limitations, and we must implement methodical strategies in order to minimise them.


#** Use adjustments sparingly ----
# Practitioners adjust much more often than they should, and many times for the wrong reasons.
# By adjusting statistical forecasts, users of forecasts create a feeling of ownership and credibility.
# Users often do not understand or appreciate the mechanisms that generate the statistical forecasts (as they will usually have no training in this area).
# By implementing judgmental adjustments, users feel that they have contributed to and completed the forecasts, and they can now relate their own intuition and interpretations to these.
# The forecasts have become their own.

# Judgmental adjustments should not aim to correct for a systematic pattern in the data that is thought to have been missed by the statistical model.
# This has been proven to be ineffective, as forecasters tend to read non-existent patterns in noisy series.
# Statistical models are much better at taking account of data patterns, and judgmental adjustments only hinder accuracy.

# Judgmental adjustments are most effective when there is significant additional information at hand or strong evidence of the need for an adjustment.
# We should only adjust when we have important extra information which is not incorporated in the statistical model.
# Hence, adjustments seem to be most accurate when they are large in size.
# Small adjustments (especially in the positive direction promoting the illusion of optimism) have been found to hinder accuracy, and should be avoided.


#** Apply a structured approach ----
# Using a structured and systematic approach will improve the accuracy of judgmental adjustments. Following the key principles outlined in Section 6.2 is vital. In particular, having to document and justify adjustments will make it more challenging to override the statistical forecasts, and will guard against unnecessary adjustments.

# It is common for adjustments to be implemented by a panel (see the example that follows).
# Using a Delphi setting carries great advantages.
# However, if adjustments are implemented in a group meeting, it is wise to consider the forecasts of key markets or products first, as panel members will get tired during this process.
# Fewer adjustments tend to be made as the meeting goes on through the day.


#** Example: Tourism Forecasting Committee (TFC) ----


# ____----


# Chapter 7 Time series regression models ---------------------------------
# The basic concept is that we forecast the time series of interest y assuming that it has a linear relationship with other time series x.

#   example, we might wish to forecast monthly sales y using total advertising spend x as a predictor. Or we might forecast daily electricity demand y using temperature x1 and the day of week x2 as predictors.

# The forecast variable y is sometimes also called the regressand, dependent or explained variable.
# The predictor variables x are sometimes also called the regressors, independent or explanatory variables.


# _----


#* 7.1 The linear model ----------------------------------------------------
# Simple linear regression ------------------------------------------------
# In the simplest case, the regression model allows for a linear relationship between the forecast variable y and a single predictor variable x:
#   y[t] = β[0] + β[1]x[t] + ε[t]
# The coefficients β0 and β1 denote the intercept and the slope of the line respectively.
# The intercept β0 represents the predicted value of y when x=0.
# The slope β1 represents the average predicted change in y resulting from a one unit increase in x.

# the observations do not lie on the straight line but are scattered around it.
# We can think of each observation yt as consisting of the systematic or explained part of the model, β0+β1xt, and the random “error,” εt.
# The “error” term does not imply a mistake, but a deviation from the underlying straight line model.
#   It captures anything that may affect yt other than xt.


#** Example: US consumption expenditure ----
# shows time series of quarterly percentage changes (growth rates) of real personal consumption expenditure, y, and real personal disposable income, x, for the US from 1970 Q1 to 2019 Q2.

us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) +
  labs(y = "% change")

# scatter plot of consumption changes against income changes along with the estimated regression line

us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# The equation is estimated using the TSLM() function:
us_change %>%
  model(TSLM(Consumption ~ Income)) %>%
  report()

# The fitted line has a positive slope, reflecting the positive relationship between income and consumption.
# The slope coefficient shows that a one unit increase in x (a 1 percentage point increase in personal disposable income) results on average in 0.27 units increase in y (an average increase of 0.27 percentage points in personal consumption expenditure).
# Alternatively the estimated equation shows that a value of 1 for x (the percentage increase in personal disposable income) will result in a forecast value of 0.54+0.27×1=0.82 for y (the percentage increase in personal consumption expenditure).

# The interpretation of the intercept requires that a value of x=0 makes sense.
# In this case when x=0 (i.e., when there is no change in personal disposable income since the last quarter) the predicted value of y is 0.54 (i.e., an average increase in personal consumption expenditure of 0.54%).
# Even when x=0 does not make sense, the intercept is an important part of the model.
# Without it, the slope coefficient can be distorted unnecessarily.
# The intercept should always be included unless the requirement is to force the regression line “through the origin.”


#** Multiple linear regression ----
# When there are two or more predictor variables, the model is called a multiple regression model.
# The general form of a multiple regression model is
#   y[t] = β[0] + β[1]x[1,t] + β[2]x[2,t] + ⋯ + β[k]x[k,t] + ε[t]   (7.1) 
#     where y is the variable to be forecast and x1,…,xk are the k predictor variables.
# Each of the predictor variables must be numerical.
# The coefficients β1,…,βk measure the effect of each predictor after taking into account the effects of all the other predictors in the model. 
# Thus, the coefficients measure the marginal effects of the predictor variables.


#** Example: US consumption expenditure ----
# Building a multiple linear regression model can potentially generate more accurate forecasts as we expect consumption expenditure to not only depend on personal income but on other predictors as well.

us_change %>%
  pivot_longer(c(Production, Savings, Unemployment), names_to = "Series") %>%
  autoplot(value) +
  facet_grid(Series ~ ., scales = "free_y") +
  theme(legend.position = "none")

# scatterplot matrix of five variables.
# The first column shows the relationships between the forecast variable (consumption) and each of the predictors.
# The scatterplots show positive relationships with income and industrial production, and negative relationships with savings and unemployment.
# The strength of these relationships are shown by the correlation coefficients across the first row.
# The remaining scatterplots and correlation coefficients show the relationships between the predictors.

us_change %>%
  GGally::ggpairs(columns = 2:6)


#** Assumptions ----
# When we use a linear regression model, we are implicitly making some assumptions about the variables in Equation (7.1).

# First, we assume that the model is a reasonable approximation to reality; that is, the relationship between the forecast variable and the predictor variables satisfies this linear equation.

# Second, we make the following assumptions about the errors (ε1,…,εT):
#   - they have mean zero; otherwise the forecasts will be systematically biased.
#   - they are not autocorrelated; otherwise the forecasts will be inefficient, as there is more information in the data that can be exploited.
#   - they are unrelated to the predictor variables; otherwise there would be more information that should be included in the systematic part of the model.

# It is also useful to have the errors being normally distributed with a constant variance σ2 in order to easily produce prediction intervals.

# Another important assumption in the linear regression model is that each predictor x is not a random variable.
# If we were performing a controlled experiment in a laboratory, we could control the values of each x (so they would not be random) and observe the resulting values of y.
# With observational data (including most data in business and economics), it is not possible to control the value of x, we simply observe it. Hence we make this an assumption.


# _----


#* 7.2 Least squares estimation --------------------------------------------
# In practice, of course, we have a collection of observations but we do not know the values of the coefficients β0,β1,…,βk.
# These need to be estimated from the data.

# The least squares principle provides a way of choosing the coefficients effectively by minimising the sum of the squared errors. That is, we choose the values of β0,β1,…,βk that minimise
#   [T∑t=1]ε[^2][t] = [T∑t=1](y[t] − β[0] − β[1]x[1,t] − β[2]x[2,t] − ⋯ − β[k]x[k,t])[^2]

# This is called least squares estimation because it gives the least value for the sum of squared errors.
# Finding the best estimates of the coefficients is often called “fitting” the model to the data, or sometimes “learning” or “training” the model.

# When we refer to the estimated coefficients, we will use the notation ^β0,…,^βk.
# The equations for these will be given in Section 7.9.

# The TSLM() function fits a linear regression model to time series data.
# It is similar to the lm() function which is widely used for linear models, but TSLM() provides additional facilities for handling time series.


# Example: US consumption expenditure -------------------------------------
# The following output provides information about the fitted model.
# The first column of Coefficients gives an estimate of each β coefficient and the second column gives its standard error (i.e., the standard deviation which would be obtained from repeatedly estimating the β coefficients on similar data sets).
# The standard error gives a measure of the uncertainty in the estimated β coefficient.

fit.consMR <- us_change %>%
  model(tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
report(fit.consMR)

# For forecasting purposes, the final two columns are of limited interest. 
#   “t value” is the ratio of an estimated β coefficient to its standard error and
#   the last column gives the
#     p-value: the probability of the estimated β coefficient being as large as it is if there was no real relationship between consumption and the corresponding predictor.
# This is useful when studying the effect of each predictor, but is not particularly useful for forecasting.


#** Fitted values ----
# Predictions of y can be obtained by using the estimated coefficients in the regression equation and setting the error term to zero.
# In general we write,
#   ^y[t] = ^β[0] + ^β[1]x[1,t] + ^β[2]x[2,t] + ⋯ + ^β[k]x[k,t]   (7.2)
# Plugging in the values of x1,t,…,xk,t for t=1,…,T returns predictions of yt within the training set, referred to as fitted values.
# Note that these are predictions of the data used to estimate the model, not genuine forecasts of future values of y.

# The following plots show the actual values compared to the fitted values for the percentage change in the US consumption expenditure series.
# The time plot in Figure 1 shows that the fitted values follow the actual data fairly closely.
# This is verified by the strong positive relationship shown by the scatterplot in Figure 2

augment(fit.consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption expenditure") +
  scale_color_manual(values = c(Data = "black", Fitted = "red")) +
  guides(colour = guide_legend(title = NULL))

augment(fit.consMR) %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)


#** Goodness-of-fit ----
# A common way to summarise how well a linear regression model fits the data is via the coefficient of determination, or R2.
# This can be calculated as the square of the correlation between the observed y values and the predicted ^y values.
# Alternatively, it can also be calculated as,
#   R2 = ∑(^yt−¯y)[^2] / ∑(yt−¯y)[^2]
#     where the summations are over all observations.
# Thus, it reflects the proportion of variation in the forecast variable that is accounted for (or explained) by the regression model.

# In simple linear regression, the value of R2 is also equal to the square of the correlation between y and x (provided an intercept has been included).

# If the predictions are close to the actual values, we would expect R2 to be close to 1.
# On the other hand, if the predictions are unrelated to the actual values, then R2=0 (again, assuming there is an intercept).
# In all cases, R2 lies between 0 and 1.

# The R2 value is used frequently, though often incorrectly, in forecasting.
# The value of R2 will never decrease when adding an extra predictor to the model and this can lead to over-fitting.
# There are no set rules for what is a good R2 value, and typical values of R2 depend on the type of data used.
# Validating a model’s forecasting performance on the test data is much better than measuring the R2 value on the training data.


#** Example: US consumption expenditure ----
# Figure 2 plots the actual consumption expenditure values versus the fitted values.
# The correlation between these variables is r=0.877 hence R2=0.768 (shown in the output above).
# In this case model does an excellent job as it explains 76.8% of the variation in the consumption data.
# Compare that to the R2 value of 0.15 obtained from the simple regression with the same data set in Section 7.1.
# Adding the three extra predictors has allowed a lot more of the variation in the consumption data to be explained.


#** Standard error of the regression ----
# Another measure of how well the model has fitted the data is the standard deviation of the residuals, which is often known as the “residual standard error.”
#   This is shown in the above output with the value 0.31.

# It is calculated using

#   ^σe = ⎷ (1 / (T−k−1)) * [T∑t=1]e[^2][t]   (7.3)
#     where k is the number of predictors in the model.
# Notice that we divide by T−k−1 because we have estimated k+1 parameters (the intercept and a coefficient for each predictor variable) in computing the residuals.

# The standard error is related to the size of the average error that the model produces.
# We can compare this error to the sample mean of y or with the standard deviation of y to gain some perspective on the accuracy of the model.

# The standard error will be used when generating prediction intervals, discussed in Section 7.6.


# _----


#* 7.3 Evaluating the regression model -------------------------------------
# The differences between the observed y values and the corresponding fitted ^y values are the training-set errors or “residuals” defined as,
#   e[t] = y[t] − ^y[t] = y[t] − ^β[0] − ^β[1]x[1,t] − ^β[2]x[2,t] − ⋯ − ^β[k]x[k,t]   for t=1,…,T.
# Each residual is the unpredictable component of the associated observation.

# The residuals have some useful properties including the following two: 
#   [T∑t=1]e[t] = 0   and   [T∑t=1]x[k,t]e[t] = 0   for all k.

# As a result of these properties, it is clear that the
#   average of the residuals is zero, and that the 
#   correlation between the residuals and the observations for the predictor variable is also zero. (This is not necessarily true when the intercept is omitted from the model.)

# After selecting the regression variables and fitting a regression model, it is necessary to plot the residuals to check that the assumptions of the model have been satisfied.
# There are a series of plots that should be produced in order to check different aspects of the fitted model and the underlying assumptions.


#** ACF plot of residuals ----
# With time series data, it is highly likely that the value of a variable observed in the current time period will be similar to its value in the previous period, or even the period before that, and so on.
# Therefore when fitting a regression model to time series data, it is common to find autocorrelation in the residuals.
# In this case, the estimated model violates the assumption of no autocorrelation in the errors, and our forecasts may be inefficient — there is some information left over which should be accounted for in the model in order to obtain better forecasts.
# The forecasts from a model with autocorrelated errors are still unbiased, and so are not “wrong,” but they will usually have larger prediction intervals than they need to.
# Therefore we should always look at an ACF plot of the residuals.


#** Histogram of residuals ----
# It is always a good idea to check whether the residuals are normally distributed.
# As we explained earlier, this is not essential for forecasting, but it does make the calculation of prediction intervals much easier.


#** Example ----
# gg_tsresiduals() - Using this we we can obtain all the useful residual diagnostics mentioned above.

fit.consMR %>% gg_tsresiduals()

augment(fit.consMR) %>%
  features(.innov, ljung_box, lag = 10, dof = 5)

# The time plot shows some changing variation over time, but is otherwise relatively unremarkable.
# This heteroscedasticity will potentially make the prediction interval coverage inaccurate.

# The histogram shows that the residuals seem to be slightly skewed, which may also affect the coverage probability of the prediction intervals.

# The autocorrelation plot shows a significant spike at lag 7, and a significant Ljung-Box test at the 5% level.
# However, the autocorrelation is not particularly large, and at lag 7 it is unlikely to have any noticeable impact on the forecasts or the prediction intervals.


#** Residual plots against predictors ----
# We would expect the residuals to be randomly scattered without showing any systematic patterns.
# A simple and quick way to check this is to examine scatterplots of the residuals against each of the predictor variables.
# If these scatterplots show a pattern, then the relationship may be nonlinear and the model will need to be modified accordingly.

# It is also necessary to plot the residuals against any predictors that are not in the model.
# If any of these show a pattern, then the corresponding predictor may need to be added to the model (possibly in a nonlinear form).


#** Example ----
us_change %>%
  left_join(residuals(fit.consMR), by = "Quarter") %>%
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") %>%
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# alternate
df <- us_change %>%
  left_join(residuals(fit.consMR), by = "Quarter")

p1 <- ggplot(df, aes(x = Income, y = .resid)) +
  geom_point() + labs("Residuals")

p2 <- ggplot(df, aes(x = Production, y = .resid)) +
  geom_point() + labs("Residuals")

p3 <- ggplot(df, aes(x = Savings, y = .resid)) +
  geom_point() + labs("Residuals")

p4 <- ggplot(df, aes(x = Unemployment, y = .resid)) +
  geom_point() + labs("Residuals")

(p1 | p2) / (p3 | p4)


# The residuals from the multiple regression model for forecasting US consumption plotted against each predictor seem to be randomly scattered. 
# Therefore we are satisfied with these in this case.


#** Residual plots against fitted values ----
# A plot of the residuals against the fitted values should also show no pattern.
# If a pattern is observed, there may be “heteroscedasticity” in the errors which means that the variance of the residuals may not be constant.
# If this problem occurs, a transformation of the forecast variable such as a logarithm or square root may be required (see Section 3.1.)


#** Example ----
augment(fit.consMR) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

# The random scatter suggests the errors are homoscedastic.


#** Outliers and influential observations ----
# outliers - Observations that take extreme values compared to the majority of the data.
# influential observations - Observations that have a large influence on the estimated coefficients of a regression model.
# Usually, influential observations are also outliers that are extreme in the x direction.

# There are formal methods for detecting outliers and influential observations that are beyond the scope of this textbook.
# As we suggested at the beginning of Chapter 2, becoming familiar with your data prior to performing any analysis is of vital importance.
# A scatter plot of y against each x is always a useful starting point in regression analysis, and often helps to identify unusual observations.

# One source of outliers is incorrect data entry.
# Simple descriptive statistics of your data can identify minima and maxima that are not sensible.
# If such an observation is identified, and it has been recorded incorrectly, it should be corrected or removed from the sample immediately.

# Outliers also occur when some observations are simply different.
# In this case it may not be wise for these observations to be removed.
# If an observation has been identified as a likely outlier, it is important to study it and analyse the possible reasons behind it.
# The decision to remove or retain an observation can be a challenging one (especially when outliers are influential observations).
# It is wise to report results both with and without the removal of such observations.


#** Example ----
us_change2 <- us_change1 <- us_change
us_change1$Consumption[round(us_change1$Income, 5) == 0.90208] <- -4
us_change2$Consumption[us_change2$Income == max(us_change2$Income)] <- -4

p1 <- us_change1 %>%
  ggplot(aes(x = Income, y = Consumption)) +
  geom_point() +
  geom_point(
    data = us_change1[us_change1$Consumption == -4, ],
    fill = NA, color = "blue", size = 6, pch = 21, stroke = 2
  ) +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  geom_smooth(data = us_change, method = lm, se = FALSE, color = "black") +
  labs(x = "% change in income", y = "% change in consumption")

p2 <- us_change2 %>%
  ggplot(aes(x = Income, y = Consumption)) +
  geom_point() +
  geom_point(
    data = us_change2[us_change2$Consumption == -4, ],
    fill = NA, color = "blue", size = 6, pch = 21, stroke = 2
  ) +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  geom_smooth(data = us_change, method = lm, se = FALSE, color = "black") +
  labs(x = "% change in income", y = "% change in consumption")

p1 + p2


# Figure highlights the effect of a single outlier when regressing US consumption on income.
# In the left panel the outlier is only extreme in the direction of y, as the percentage change in consumption has been incorrectly recorded as -4%. 
#   The red line is the regression line fitted to the data which includes the outlier, compared to the black line which is the line fitted to the data without the outlier.
# In the right panel the outlier now is also extreme in the direction of x with the 4% decrease in consumption corresponding to a 6% increase in income.
#   In this case the outlier is extremely influential as the red line now deviates substantially from the black line.


#** Spurious regression ----
# More often than not, time series data are “non-stationary”; that is, the values of the time series do not fluctuate around a constant mean or with a constant variance.
# We will deal with time series stationarity in more detail in Chapter 9, but here we need to address the effect that non-stationary data can have on regression models.

#   example, consider the two variables plotted below.
#     These appear to be related simply because they both trend upwards in the same manner.
#     However, air passenger traffic in Australia has nothing to do with rice production in Guinea.

p1 <- aus_airpassengers %>%
  autoplot(Passengers) +
  labs(x = "Air Passengers")

p2 <- guinea_rice %>%
  autoplot(Production) +
  labs(x = "Rice Production")

p3 <- aus_airpassengers %>%
  left_join(guinea_rice) %>%
  ggplot(aes(x = Production, y = Passengers)) +
  geom_point() +
  labs(
    x = "Rice Production in Guinea(million tons)",
    y = "Air Passengers in Australia (millions)"
  )

(p1 / p2) | p3


# Regressing non-stationary time series can lead to spurious regressions. 
# The output of regressing Australian air passengers on rice production in Guinea is shown in Figure 7.13.
# High R2 and high residual autocorrelation can be signs of spurious regression.
#   Notice these features in the output below.
# We discuss the issues surrounding non-stationary data and spurious regressions in more detail in Chapter 10.

# Cases of spurious regression might appear to give reasonable short-term forecasts, but they will generally not continue to work into the future.

fit <- aus_airpassengers %>%
  filter(Year <= 2011) %>%
  left_join(guinea_rice, by = "Year") %>%
  model(TSLM(Passengers ~ Production))

report(fit)

fit %>% gg_tsresiduals()


# _----


#* 7.4 Some useful predictors ----------------------------------------------
# There are several useful predictors that occur frequently when using regression for time series data.


#** Trend ----
# It is common for time series data to be trending.
# A linear trend can be modelled by simply using x1,t=t as a predictor,
#     y[t] = β[0] + β[1]t + ε[t], where t = 1,…,T.
# A trend variable can be specified in the TSLM() function using the trend() special.
# In Section 7.7 we discuss how we can also model nonlinear trends.


#** Dummy variables ----
# So far, we have assumed that each predictor takes numerical values.
# But what about when a predictor is a categorical variable taking only two values (e.g., “yes” and “no”)?
#   Such a variable might arise, for example, when forecasting daily sales and you want to take account of whether the day is a public holiday or not. # So the predictor takes value “yes” on a public holiday, and “no” otherwise.

# This situation can still be handled within the framework of multiple regression models by creating a “dummy variable” which takes value 1 corresponding to “yes” and 0 corresponding to “no.”
# A dummy variable is also known as an “indicator variable.”

# A dummy variable can also be used to account for an outlier in the data. 
# Rather than omit the outlier, a dummy variable removes its effect.
# In this case, the dummy variable takes value 1 for that observation and 0 everywhere else.
#   example is the case where a special event has occurred.
#     example when forecasting tourist arrivals to Brazil, we will need to account for the effect of the Rio de Janeiro summer Olympics in 2016.

# If there are more than two categories, then the variable can be coded using several dummy variables (one fewer than the total number of categories).
# TSLM() will automatically handle this case if you specify a factor variable as a predictor.
# There is usually no need to manually create the corresponding dummy variables.


#** Seasonal dummy variables ----
# Suppose that we are forecasting daily data and we want to account for the day of the week as a predictor.
# Then the following dummy variables can be created.

#  	            d1,t    d2,t    d3,t    d4,t    d5,t    d6,t
#   Monday 	      1 	    0 	    0 	    0 	    0 	    0
#   Tuesday 	    0 	    1 	    0 	    0 	    0 	    0
#   Wednesday 	  0 	    0 	    1 	    0 	    0 	    0
#   Thursday 	    0 	    0 	    0 	    1 	    0 	    0
#   Friday 	      0 	    0 	    0 	    0 	    1 	    0
#   Saturday 	    0 	    0 	    0 	    0 	    0 	    1
#   Sunday 	      0 	    0 	    0 	    0 	    0 	    0
#   Monday 	      1 	    0 	    0 	    0 	    0 	    0
#     ⋮ 	         ⋮ 	     ⋮ 	    	⋮ 	  	⋮ 	    ⋮       	⋮


# Notice that only six dummy variables are needed to code seven categories.
# That is because the seventh category (in this case Sunday) is captured by the intercept, and is specified when the dummy variables are all set to zero.

# Many beginners will try to add a seventh dummy variable for the seventh category.
# This is known as the “dummy variable trap”, because it will cause the regression to fail. There will be one too many parameters to estimate when an intercept is also included.
# The general rule is to use one fewer dummy variables than categories.
# So for
#   quarterly data, use three dummy variables;
#   monthly data, use 11 dummy variables; and
#   daily data, use six dummy variables, and so on.

# The interpretation of each of the coefficients associated with the dummy variables is that it is a measure of the effect of that category relative to the omitted category.
#   In the above example, the coefficient of d1,t associated with Monday will measure the effect of Monday on the forecast variable compared to the effect of Sunday.
#   An example of interpreting estimated dummy variable coefficients capturing the quarterly seasonality of Australian beer production follows.

# The TSLM() function will automatically handle this situation if you specify the special season().


#** Example: Australian quarterly beer production ----
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

# We want to forecast the value of future beer production.
# We can model this data using a regression model with a linear trend and quarterly dummy variables,
#   y[t] = β[0] + β[1]t + β[2]d[2,t] + β[3]d[3,t] + β[4]d[4,t] + ε[t],
#     where d[i,t]=1 if t is in quarter i and 0 otherwise.
# The first quarter variable has been omitted, so the coefficients associated with the other quarters are measures of the difference between those quarters and the first quarter.

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))

report(fit_beer)


# Note that trend() and season() are not standard functions; they are “special” functions that work within the TSLM() model formulae.

# There is an average downward trend of -0.34 megalitres per quarter.
# On average,
#   the second quarter has production of 34.7 megalitres lower than the first quarter,
#   the third quarter has production of 17.8 megalitres lower than the first quarter, and
#   the fourth quarter has production of 72.8 megalitres higher than the first quarter.

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_color_manual(
    values = c(Data = "black", Fitted = "red")
  ) +
  labs(
    y = "Megalitres",
    title = "Australian quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Series"))


augment(fit_beer) %>%
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(
    y = "Fitted", x = "Actual values",
    title = "Australian quarterly beer production"
  ) +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))


#** Intervention variables ----
# It is often necessary to model interventions that may have affected the variable to be forecast.
#   example, competitor activity, advertising expenditure, industrial action, and so on, can all have an effect.

# When the effect lasts only for one period, we use a “spike” variable. 
#   This is a dummy variable that takes value one in the period of the intervention and zero elsewhere.
# A spike variable is equivalent to a dummy variable for handling an outlier.

# Other interventions have an immediate and permanent effect.
# If an intervention causes a level shift (i.e., the value of the series changes suddenly and permanently from the time of intervention), then we use a “step” variable.
# A step variable takes value zero before the intervention and one from the time of intervention onward.

# Another form of permanent effect is a change of slope.
# Here the intervention is handled using a piecewise linear trend; a trend that bends at the time of intervention and hence is nonlinear.
#   We will discuss this in Section 7.7.


#** Trading days ----
# The number of trading days in a month can vary considerably and can have a substantial effect on sales data.
# To allow for this, the number of trading days in each month can be included as a predictor.

# An alternative that allows for the effects of different days of the week has the following predictors:
#   x1 = number of Mondays in month;
#   x2 = number of Tuesdays in month;
#   ⋮
#   x7 = number of Sundays in month.


#** Distributed lags ----
# It is often useful to include advertising expenditure as a predictor. 
# However, since the effect of advertising can last beyond the actual campaign, we need to include lagged values of advertising expenditure.
# Thus, the following predictors may be used.
#   x1 = advertising for previous month;
#   x2 = advertising for two months previously;
#   ⋮
#   xm = advertising for m months previously.

# It is common to require the coefficients to decrease as the lag increases, although this is beyond the scope of this book.


#** Easter ----
# Easter differs from most holidays because it is not held on the same date each year, and its effect can last for several days.
# In this case, a dummy variable can be used with value one where the holiday falls in the particular time period and zero otherwise.

# With monthly data, if Easter falls in March then the dummy variable takes value 1 in March, and if it falls in April the dummy variable takes value 1 in April.
# When Easter starts in March and finishes in April, the dummy variable is split proportionally between months.


#** Fourier series ----
# An alternative to using seasonal dummy variables, especially for long seasonal periods, is to use Fourier terms.
#   Jean-Baptiste Fourier was a French mathematician, born in the 1700s, who showed that
# a series of sine and cosine terms of the right frequencies can approximate any periodic function.
# We can use them for seasonal patterns.

# If m is the seasonal period, then the first few Fourier terms are given by
#   x[1,t] = sin(2πt/m),    x[2,t] = cos(2πt/m),
#   x[3,t] = sin(4πtm),     x[4,t] = cos(4πt/m),
#   x[5,t] = sin(6πt/m),    x[6,t] = cos(6πt/m),
#     and so on.
# If we have monthly seasonality, and we use the first 11 of these predictor variables, then we will get exactly the same forecasts as using 11 dummy variables.

# With Fourier terms, we often need fewer predictors than with dummy variables, especially when m is large.
#   This makes them useful for weekly data, for example, where m ≈ 52.
# For short seasonal periods (e.g., quarterly data), there is little advantage in using Fourier terms over seasonal dummy variables.

# These Fourier terms are produced using the fourier() function.
#   example, the Australian beer data can be modelled like this.


fourier_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + fourier(K = 2)))

report(fourier_beer)


# The K argument to fourier() specifies how many pairs of sin and cos terms to include.
# The maximum allowed is K = m/2 where m is the seasonal period.
# Because we have used the maximum here, the results are identical to those obtained when using seasonal dummy variables.

# augment(fourier_beer) %>%
#   ggplot(aes(x = Quarter)) +
#   geom_line(aes(y = Beer, color = "Data")) +
#   geom_line(aes(y = .fitted, color = "Fitted")) +
#   scale_color_manual(values = c(Data = "black", Fitted = "red")) +
#   labs(
#     y = "Megalitres",
#     title = "Quarterly Beer Production"
#   ) +
#   guides(color = guide_legend(title = "Series"))

# If only the first two Fourier terms are used (x1,t and x2,t), the seasonal pattern will follow a simple sine wave.
# A regression model containing Fourier terms is often called a harmonic regression because the successive Fourier terms represent harmonics of the first two Fourier terms.


# _----


#* 7.5 Selecting predictors ------------------------------------------------
# When there are many possible predictors, we need some strategy for selecting the best predictors to use in a regression model.

# Invalid Common Approaches
#   A common approach that is not recommended is to plot the forecast variable against a particular predictor and if there is no noticeable relationship, drop that predictor from the model.
#   This is invalid because it is not always possible to see the relationship from a scatterplot, especially when the effects of other predictors have not been accounted for.

#   Another common approach which is also invalid is to do a multiple linear regression on all the predictors and disregard all variables whose p-values are greater than 0.05.
#   To start with, statistical significance does not always indicate predictive value.
#   Even if forecasting is not the goal, this is not a good strategy because the p-values can be misleading when two or more predictors are correlated with each other (see Section 7.8).

# Instead, we will use a measure of predictive accuracy.
# Five such measures are introduced in this section.
# They can be shown using the glance() function

glance(fit.consMR) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

# We compare these values against the corresponding values from other models.
# For the CV, AIC, AICc and BIC measures, we want to find the model with the lowest value;
# for Adjusted R2, we seek the model with the highest value.


#** Adjusted R2 ----
# Computer output for a regression will always give the R2 value, discussed in Section 7.2.
# However, it is not a good measure of the predictive ability of a model. 
# It measures how well the model fits the historical data, but not how well the model will forecast future data.

# In addition, R2 does not allow for “degrees of freedom.”
# Adding any variable tends to increase the value of R2, even if that variable is irrelevant.
# For these reasons, forecasters should not use R2 to determine whether a model will give good predictions, as it will lead to overfitting.

# An equivalent idea is to select the model which gives the minimum sum of squared errors (SSE), given by
#   SSE = [T∑t=1]e[^2][t].

# Minimising the SSE is equivalent to maximising R2 and will always choose the model with the most variables, and so is not a valid way of selecting predictors.

# An alternative which is designed to overcome these problems is the adjusted R2 (also called “R-bar-squared”):
#   ¯R2 = 1 − (1−R2)(T−1 / T−k−1), where
#     T is the number of observations and
#     k is the number of predictors.
# This is an improvement on R2, as it will no longer increase with each added predictor.
# Using this measure, the best model will be the one with the largest value of ¯R2.
# Maximising ¯R2 is equivalent to minimising the standard error ^σe given in Equation (7.3).

# Maximising ¯R2 works quite well as a method of selecting predictors, although it does tend to err on the side of selecting too many predictors.


#** Cross-validation ----
# Time series cross-validation was introduced in Section 5.8 as a general tool for determining the predictive ability of a model.
# For regression models, it is also possible to use classical leave-one-out cross-validation to selection predictors.
# This is faster and makes more efficient use of the data.
# The procedure uses the following steps:
#   1. Remove observation t from the data set, and fit the model using the remaining data.
#   2. Then compute the error (e∗t = yt − ^yt) for the omitted observation. (This is not the same as the residual because the tth observation was not used in estimating the value of ^yt.)
#   3. Repeat step 1 for t = 1,…,T.
#   4. Compute the MSE from e∗1,…,e∗T. We shall call this the CV.

# Although this looks like a time-consuming procedure, there are fast methods of calculating CV, so that it takes no longer than fitting one model to the full data set.
# The equation for computing CV efficiently is given in Section 7.9.
# Under this criterion, the best model is the one with the smallest value of CV.


#** Akaike’s Information Criterion ----
# A closely-related method is Akaike’s Information Criterion, which we define as
#   AIC = Tlog(SSE/T) + 2(k+2),   where
#     T is the number of observations used for estimation and
#     k is the number of predictors in the model.
# Different computer packages use slightly different definitions for the AIC, although they should all lead to the same model being selected.
# The k+2 part of the equation occurs because there are k+2 parameters in the model: the k coefficients for the predictors, the intercept and the variance of the residuals.
# The idea here is to penalise the fit of the model (SSE) with the number of parameters that need to be estimated.

# The model with the minimum value of the AIC is often the best model for forecasting.
# For large values of T, minimising the AIC is equivalent to minimising the CV value.


#** Corrected Akaike’s Information Criterion ----
# For small values of T, the AIC tends to select too many predictors, and so a bias-corrected version of the AIC has been developed,
#   AICc = AIC + (2(k+2)(k+3) / T−k−3).
# As with the AIC, the AICc should be minimised.


#** Schwarz’s Bayesian Information Criterion ----
# A related measure is Schwarz’s Bayesian Information Criterion (usually abbreviated to BIC, SBIC or SC):
#   BIC = Tlog(SSE/T) + (k+2)log(T).
# As with the AIC, minimising the BIC is intended to give the best model. 
# The model chosen by the BIC is either the same as that chosen by the AIC, or one with fewer terms.
# This is because the BIC penalises the number of parameters more heavily than the AIC.
# For large values of T, minimising BIC is similar to leave-v-out cross-validation when
#   v = T[1 − 1/(log(T)−1)].


#** Which measure should we use? ----
# While ¯R2 is widely used, and has been around longer than the other measures, its tendency to select too many predictor variables makes it less suitable for forecasting.

# Many statisticians like to use the BIC because it has the feature that if there is a true underlying model, the BIC will select that model given enough data.
# However, in reality, there is rarely, if ever, a true underlying model, and even if there was a true underlying model, selecting that model will not necessarily give the best forecasts (because the parameter estimates may not be accurate).

# Consequently, we recommend that one of the AICc, AIC, or CV statistics be used, each of which has forecasting as their objective.
# If the value of T is large enough, they will all lead to the same model. 


#** Example: US consumption ----
# In the multiple regression example for forecasting US consumption we considered four predictors. With four predictors, there are 24=16 possible models. Now we can check if all four predictors are actually useful, or whether we can drop one or more of them. All 16 models were fitted and the results are summarised in Table below.

fit.consMR_all_modesl <- us_change %>%
  model(
    tslm_all = TSLM(Consumption ~ Income + Production + Unemployment + Savings),
    tslm_ipu = TSLM(Consumption ~ Income + Production + Unemployment),
    tslm_ips = TSLM(Consumption ~ Income + Production + Savings),
    tslm_ius = TSLM(Consumption ~ Income + Unemployment + Savings),
    tslm_pus = TSLM(Consumption ~ Production + Unemployment + Savings),
    tslm_ip = TSLM(Consumption ~ Income + Production),
    tslm_iu = TSLM(Consumption ~ Income + Unemployment),
    tslm_is = TSLM(Consumption ~ Income + Savings),
    tslm_pu = TSLM(Consumption ~ Production + Unemployment),
    tslm_ps = TSLM(Consumption ~ Production + Savings),
    tslm_us = TSLM(Consumption ~ Unemployment + Savings),
    tslm_none = TSLM(Consumption ~ NULL)
  )

glance(fit.consMR_all_modesl) %>%
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) %>%
  arrange(AICc)

# The results have been sorted according to the AICc. Therefore the best models are given at the top of the table, and the worst at the bottom of the table.

# The best model contains all four predictors. However, a closer look at the results reveals some interesting features. There is clear separation between the models in the first four rows and the ones below. This indicates that Income and Savings are both more important variables than Production and Unemployment. Also, the first three rows have almost identical values of CV, AIC and AICc. So we could possibly drop either the Production variable, or the Unemployment variable, and get similar forecasts. Note that Production and Unemployment are highly (negatively) correlated, as shown in Figure Section 7.1-Example, so most of the predictive information in Production is also contained in the Unemployment variable.


#** Best subset regression ----
# Where possible, all potential regression models should be fitted (as was done in the example above) and the best model should be selected based on one of the measures discussed.
# This is known as “best subsets” regression or “all possible subsets” regression.


#** Stepwise regression ----
# If there are a large number of predictors, it is not possible to fit all possible models. For example, 40 predictors leads to 2[^40] >  1 trillion possible models!
# Consequently, a strategy is required to limit the number of models to be explored.

# An approach that works quite well is backwards stepwise regression:
#   - Start with the model containing all potential predictors.
#   - Remove one predictor at a time. Keep the model if it improves the measure of predictive accuracy.
#   - Iterate until no further improvement.

# If the number of potential predictors is too large, then the backwards stepwise regression will not work and forward stepwise regression can be used instead.
# This procedure starts with a model that includes only the intercept. 
# Predictors are added one at a time, and the one that most improves the measure of predictive accuracy is retained in the model.
# The procedure is repeated until no further improvement can be achieved.

# Alternatively for either the backward or forward direction, a starting model can be one that includes a subset of potential predictors.
# In this case, an extra step needs to be included.
# For the backwards procedure we should also consider adding a predictor with each step, and for the forward procedure we should also consider dropping a predictor with each step.
# These are referred to as hybrid procedures.

# It is important to realise that any stepwise approach is not guaranteed to lead to the best possible model, but it almost always leads to a good model.


#** Beware of inference after selecting predictors ----
# We do not discuss statistical inference of the predictors in this book (e.g., looking at p-values associated with each predictor).
# If you do wish to look at the statistical significance of the predictors, beware that any procedure involving selecting predictors first will invalidate the assumptions behind the p-values.
# The procedures we recommend for selecting predictors are helpful when the model is used for forecasting;
#   they are not helpful if you wish to study the effect of any predictor on the forecast variable.


# _----


#* 7.6 Forecasting with regression -----------------------------------------
# Recall that predictions of y can be obtained using
#     ^y[t] = ^β[0] + ^β[1]x[1,t] + ^β[2]x[2,t] + ⋯ + ^β[k]x[k,t]
# which comprises the estimated coefficients and ignores the error in the regression equation.
# Plugging in the values of the predictor variables x1,t,…,xk,t for t=1,…,T returns the fitted (training set) values of y.
# What we are interested in here, however, is forecasting future values of y.


#** Ex-ante versus ex-post forecasts ----
# When using regression models for time series data, we need to distinguish between the different types of forecasts that can be produced, depending on what is assumed to be known when the forecasts are computed.

# Ex-ante forecasts
#   are those that are made using only the information that is available in advance.
#     example, ex-ante forecasts for the percentage change in US consumption for quarters following the end of the sample, should only use information that was available up to and including 2019 Q2.
#   These are genuine forecasts, made in advance using whatever information is available at the time.
#   Therefore in order to generate ex-ante forecasts, the model requires forecasts of the predictors.
#   To obtain these we can use one of the simple methods introduced in Section 5.2 or more sophisticated pure time series approaches that follow in Chapters 8 and 9.
#   Alternatively, forecasts from some other source, such as a government agency, may be available and can be used.

# Ex-post forecasts
#   are those that are made using later information on the predictors.
#     example, ex-post forecasts of consumption may use the actual observations of the predictors, once these have been observed.
#   These are not genuine forecasts, but are useful for studying the behaviour of forecasting models.

# The model from which ex-post forecasts are produced should not be estimated using data from the forecast period.
# That is, ex-post forecasts can
#   assume knowledge of the predictor variables (the x variables), but 
#   should not assume knowledge of the data that are to be forecast (the y variable).

# A comparative evaluation of ex-ante forecasts and ex-post forecasts can help to separate out the sources of forecast uncertainty. 
# This will show whether forecast errors have arisen due to poor forecasts of the predictor or due to a poor forecasting model.


#** Example: Australian quarterly beer production ----
# Normally, we cannot use actual future values of the predictor variables when producing ex-ante forecasts because their values will not be known in advance. 
# However, the special predictors introduced in Section 7.4 are all known in advance, as they are based on calendar variables (e.g., seasonal dummy variables or public holiday indicators) or deterministic functions of time (e.g. time trend). 
# In such cases, there is no difference between ex-ante and ex-post forecasts.

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))

fc_beer <- forecast(fit_beer)

fc_beer %>%
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )


#** Scenario based forecasting ----
# In this setting, the forecaster assumes possible scenarios for the predictor variables that are of interest.
#   example, a US policy maker may be interested in comparing the predicted change in consumption when there is a constant growth of 1% and 0.5% respectively for income and savings with no change in the employment rate, versus a respective decline of 1% and 0.5%, for each of the four quarters following the end of the sample. #   The resulting forecasts are calculated below and shown in below. 
# We should note that prediction intervals for scenario based forecasts do not include the uncertainty associated with the future values of the predictor variables. #   They assume that the values of the predictors are known in advance.

fit_consBest <- us_change %>%
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")


#** Building a predictive regression model ----
# The great advantage of regression models is that they can be used to capture important relationships between the forecast variable of interest and the predictor variables. 
# A major challenge however, is that in order to generate ex-ante forecasts, the model requires future values of each predictor. 
# If scenario based forecasting is of interest then these models are extremely useful. 
# However, if ex-ante forecasting is the main focus, obtaining forecasts of the predictors can be challenging (in many cases generating forecasts for the predictor variables can be more challenging than forecasting directly the forecast variable without using predictors).

# An alternative formulation is to use as predictors their lagged values. 
# Assuming that we are interested in generating a h-step ahead forecast we write 
#   y[t+h] = β[0] + β[1]x[1,t] + ⋯ + β[k]x[k,t] + ε[t+h]        for h = 1,2…. 
# The predictor set is formed by values of the xs that are observed h time periods prior to observing y. 
# Therefore when the estimated model is projected into the future, i.e., beyond the end of the sample T, all predictor values are available.

# Including lagged values of the predictors does not only make the model operational for easily generating forecasts, it also makes it intuitively appealing. 
#   example, the effect of a policy change with the aim of increasing production may not have an instantaneous effect on consumption expenditure. 
# It is most likely that this will happen with a lagging effect. 
# We touched upon this in Section 7.4 when briefly introducing distributed lags as predictors. 
# Several directions for generalising regression models to better incorporate the rich dynamics observed in time series are discussed in Section 10.


#** Prediction intervals ----
# With each forecast for the change in consumption in Figure above, 95% and 80% prediction intervals are also included. 
# The general formulation of how to calculate prediction intervals for multiple regression models is presented in Section 7.9. 
# As this involves some advanced matrix algebra we present here the case for calculating prediction intervals for a simple regression, where a forecast can be generated using the equation, 
#   ^y = ^β[0] + ^β[1]x

# Assuming that the regression errors are normally distributed, an approximate 95% prediction interval associated with this forecast is given by 
#   ^y ± 1.96^σe√1+1T+(x−¯x)2(T−1)s2x,      (7.4) 
#     where 
#     T is the total number of observations, 
#     ¯x is the mean of the observed x values, 
#     s[x] is the standard deviation of the observed x values and 
#     ^σe is the standard error of the regression given by Equation (7.3). 
# Similarly, an 80% prediction interval can be obtained by replacing 1.96 by 1.28. 
# Other prediction intervals can be obtained by replacing the 1.96 with the appropriate value given in Table 5.1. 
# If the fable package is used to obtain prediction intervals, more exact calculations are obtained (especially for small values of T) than what is given by Equation (7.4).

# Equation (7.4) shows that the prediction interval is wider when x is far from ¯x. 
# That is, we are more certain about our forecasts when considering values of the predictor variable close to its sample mean.


#** Example ----
# The estimated simple regression line in the US consumption example is 
#   ^y[t] = 0.54 + 0.27x[t]

# Assuming that for the next four quarters, personal income will increase by its historical mean value of ¯x=0.73%, consumption is forecast to increase by 0.74% and the corresponding 80% and 95% prediction intervals are [−0.02,1.5] and [−0.42,1.9] respectively (calculated using R). 
# If we assume an extreme increase of 5% in income, then the prediction intervals are considerably wider as shown in Figure below.

fit_cons <- us_change %>%
  model(TSLM(Consumption ~ Income))

new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) %>%
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) %>%
    mutate(Income = 12),
  names_to = "Scenario"
)

fcast <- forecast(fit_cons, new_cons)

us_change %>%
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(title = "US consumption", y = "% change")


# _----


#* 7.7 Nonlinear regression ------------------------------------------------
# Although the linear relationship assumed so far in this chapter is often adequate, there are many cases in which a nonlinear functional form is more suitable. 
# To keep things simple in this section we assume that we only have one predictor x.

# The simplest way of modelling a nonlinear relationship is to transform the forecast variable y and/or the predictor variable x before estimating a regression model. 
# While this provides a non-linear functional form, the model is still linear in the parameters. 
# The most commonly used transformation is the (natural) logarithm (see Section 3.1).

# A log-log functional form is specified as 
#   logy = β[0] + β[1]logx + ε
# In this model, the slope β1 can be interpreted as an elasticity: β[1] is the average percentage change in y resulting from a 1% increase in x. 
# Other useful forms can also be specified. 
#   The log-linear form is specified by only transforming the forecast variable and the linear-log form is obtained by transforming the predictor.

# Recall that in order to perform a logarithmic transformation to a variable, all of its observed values must be greater than zero. 
# In the case that variable x contains zeros, we use the transformation log(x+1); i.e., we add one to the value of the variable and then take logarithms. 
#   This has a similar effect to taking logarithms but avoids the problem of zeros. 
#   It also has the neat side-effect of zeros on the original scale remaining zeros on the transformed scale.

# There are cases for which simply transforming the data will not be adequate and a more general specification may be required. 
# Then the model we use is 
#   y = f(x) + ε
#     where f is a nonlinear function. 
# In standard (linear) regression, f(x)=β0+β1x. 
# In the specification of nonlinear regression that follows, we allow f to be a more flexible nonlinear function of x, compared to simply a logarithmic or other transformation.

# One of the simplest specifications is to make f piecewise linear. 
# That is, we introduce points where the slope of f can change. 
#   These points are called knots. 
# This can be achieved by letting x1,t=x and introducing variable x2,t such that 
#                     { 0       if x < c
#   x[2,t] = (x−c)+ = { x − c   if x ≥ c 
# The notation (x−c)+ means the value x−c if it is positive and 0 otherwise. 
# This forces the slope to bend at point c. 
# Additional bends can be included in the relationship by adding further variables of the above form.
  
# Piecewise linear relationships constructed in this way are a special case of regression splines. 
# In general, a linear regression spline is obtained using 
#   x[1] = x    x[2] = (x−c1)+    … x[k] = (x−ck−1)+
#       where c1,…,ck−1 are the knots (the points at which the line can bend). 
# Selecting the number of knots (k−1) and where they should be positioned can be difficult and somewhat arbitrary. 
# Some automatic knot selection algorithms are available, but are not widely used.


#** Forecasting with a nonlinear trend ----
# In Section 7.4 fitting a linear trend to a time series by setting x=t was introduced. 
# The simplest way of fitting a nonlinear trend is using quadratic or higher order trends obtained by specifying x1,t=t,x2,t=t2,….

# However, it is not recommended that quadratic or higher order trends be used in forecasting. 
# When they are extrapolated, the resulting forecasts are often unrealistic.

# A better approach is to use the piecewise specification introduced above and fit a piecewise linear trend which bends at some point in time. 
# We can think of this as a nonlinear trend constructed of linear pieces. 
# If the trend bends at time τ, then it can be specified by simply replacing x=t and c=τ above such that we include the predictors, 
#   x[1,t] = t 
#                     { 0     if t < τ
#   x[2,t] = (t−τ)+ = { t−τ   if t ≥ τ in the model. 
# If the associated coefficients of [x1,t] and x[2,t] are β[1] and β[2], then 
#   β1 gives the slope of the trend before time τ, while the 
#   slope of the line after time τ is given by β1+β2. 
# Additional bends can be included in the relationship by adding further variables of the form (t−τ)+ where τ is the “knot” or point in time at which the line should bend.


#** Example: Boston marathon winning times ----
# We will fit some trend models to the Boston marathon winning times for men. 
# First we extract the men’s data and convert the winning times to a numerical value. 
# The course was lengthened (from 24.5 miles to 26.2 miles) in 1924, which led to a jump in the winning times, so we only consider data from that date onwards.

boston_men <- boston_marathon %>%
  filter(Year >= 1924) %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)

p1 <- boston_men %>%
  autoplot(Minutes) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", title = "Boston marathon winning times")

p2 <- boston_men %>%
  model(TSLM(Minutes ~ trend())) %>%
  augment() %>%
  autoplot(.resid) +
  labs(title = "Residuals from a linear trend")

p1/p2


# Fitting an exponential trend (equivalent to a log-linear regression) to the data can be achieved by transforming the y variable so that the model to be fitted is, #   logy[t] = β[0] + β[1]t + ε[t]

# The fitted exponential trend and forecasts are shown in Figure below. 
# Although the exponential trend does not seem to fit the data much better than the linear trend, it perhaps gives a more sensible projection in that the winning times will decrease in the future but at a decaying rate rather than a fixed linear rate.

# The plot of winning times reveals three different periods. 
#   There is a lot of volatility in the winning times up to about 1950, with the winning times barely declining. 
#   After 1950 there is a clear decrease in times, 
#   followed by a flattening out after the 1980s, 
#   with the suggestion of an upturn towards the end of the sample. 
# To account for these changes, we specify the years 1950 and 1980 as knots. 
# We should warn here that subjective identification of knots can lead to over-fitting, which can be detrimental to the forecast performance of a model, and should be performed with caution.

fit_trends <- boston_men %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )

fc_trends <- fit_trends %>% forecast(h = 10)

boston_men %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(
    y = "Minutes",
    title = "Boston marathon winning times"
  )

# The best forecasts appear to come from the piecewise linear trend.


# _----


#* 7.8 Correlation, causation and forecasting ------------------------------
#** Correlation is not causation ----
# It is important not to confuse correlation with causation, or causation with forecasting. 
# A variable x may be useful for forecasting a variable y, but that does not mean x is causing y. 
# It is possible that x is causing y, but it may be that y is causing x, or that the relationship between them is more complicated than simple causality.

#   example, it is possible to model the number of drownings at a beach resort each month with the number of ice-creams sold in the same period. 
#   The model can give reasonable forecasts, not because ice-creams cause drownings, but because people eat more ice-creams on hot days when they are also more likely to go swimming. 
#   So the two variables (ice-cream sales and drownings) are correlated, but one is not causing the other. 
#   They are both caused by a third variable (temperature). 
#   This is an example of 
#     “confounding” — where an omitted variable causes changes in both the response variable and at least one predictor variables.

# confounder - describe a variable that is not included in our forecasting model when it influences both the response variable and at least one predictor variable. 
# Confounding makes it difficult to determine what variables are causing changes in other variables, but it does not necessarily make forecasting more difficult.

# Similarly, it is possible to forecast if it will rain in the afternoon by observing the number of cyclists on the road in the morning. 
#   When there are fewer cyclists than usual, it is more likely to rain later in the day. 
#   The model can give reasonable forecasts, not because cyclists prevent rain, but because people are more likely to cycle when the published weather forecast is for a dry day. 
#   In this case, there is a causal relationship, but in the opposite direction to our forecasting model. 
#   The number of cyclists falls because there is rain forecast. 
#   That is, y(rainfall) is affecting x(cyclists).

# It is important to understand that correlations are useful for forecasting, even
#   when there is no causal relationship between the two variables, or 
#   when the causality runs in the opposite direction to the model, or 
#   when there is confounding.

# However, often a better model is possible if a causal mechanism can be determined. 
#   A better model for drownings will probably include temperatures and visitor numbers and exclude ice-cream sales. 
#   A good forecasting model for rainfall will not include cyclists, but it will include atmospheric observations from the previous few days.


#** Forecasting with correlated predictors ----
# When two or more predictors are highly correlated it is always challenging to accurately separate their individual effects. 
#   Suppose we are forecasting monthly sales of a company for 2012, using data from 2000–2011. 
#   In January 2008, a new competitor came into the market and started taking some market share. 
#   At the same time, the economy began to decline. 
#   In your forecasting model, you include both competitor activity (measured using advertising time on a local television station) and the health of the economy (measured using GDP). 
#   It will not be possible to separate the effects of these two predictors because they are highly correlated.

# Having correlated predictors is not really a problem for forecasting, as we can still compute forecasts without needing to separate out the effects of the predictors. 
# However, it becomes a problem with scenario forecasting as the scenarios should take account of the relationships between predictors. 
# It is also a problem if some historical analysis of the contributions of various predictors is required.


#** Multicollinearity and forecasting ----
# multicollinearity - A closely related issue, which occurs when similar information is provided by two or more of the predictor variables in a multiple regression.

# It can occur when two predictors are highly correlated with each other (that is, they have a correlation coefficient close to +1 or -1). 
# In this case, knowing the value of one of the variables tells you a lot about the value of the other variable. 
# Hence, they are providing similar information. 
#   example, foot size can be used to predict height, but including the size of both left and right feet in the same model is not going to make the forecasts any better, although it won’t make them worse either.

# Multicollinearity can also occur when a linear combination of predictors is highly correlated with another linear combination of predictors. 
# In this case, knowing the value of the first group of predictors tells you a lot about the value of the second group of predictors. 
# Hence, they are providing similar information.

#   example of this problem is the dummy variable trap discussed in Section 7.4. 
#   Suppose you have quarterly data and use four dummy variables, d1, d2, d3 and d4. 
#   Then d4 = 1−d1−d2−d3, so there is perfect correlation between d4 and d1+d2+d3.

# In the case of perfect correlation (i.e., a correlation of +1 or -1, such as in the dummy variable trap), it is not possible to estimate the regression model.

# If there is high correlation (close to but not equal to +1 or -1), then the estimation of the regression coefficients is computationally difficult. 
# Most reputable statistical software will use algorithms to limit the effect of multicollinearity on the coefficient estimates, but you do need to be careful.

# When multicollinearity is present, the uncertainty associated with individual regression coefficients will be large. 
# This is because they are difficult to estimate. 
# Consequently, statistical tests (e.g., t-tests) on regression coefficients are unreliable. (In forecasting we are rarely interested in such tests.) 
# Also, it will not be possible to make accurate statements about the contribution of each separate predictor to the forecast.

# Forecasts will be unreliable if the values of the future predictors are outside the range of the historical values of the predictors.
#   example, suppose you have fitted a regression model with predictors x1 and x2 which are highly correlated with each other, and suppose that the values of x1 in the training data ranged between 0 and 100. 
#   Then forecasts based on x1>100 or x1<0 will be unreliable. 
# It is always a little dangerous when future values of the predictors lie much outside the historical range, but it is especially problematic when multicollinearity is present.

# Note that if you are using good statistical software, if you are not interested in the specific contributions of each predictor, and if the future values of your predictor variables are within their historical ranges, there is nothing to worry about — 
#   multicollinearity is not a problem except when there is perfect correlation.


# _----


#* 7.9 Matrix formulation --------------------------------------------------
# Warning: this is a more advanced, optional section and assumes knowledge of matrix algebra.

# Recall that multiple regression model can be written as 
#   y[t] = β[0] + β[1]x[1,t] + β[2]x[2,t] + ⋯ + β[k]x[k,t] + ε[t] where ε[t] has mean zero and variance σ[^2]. 
# This expresses the relationship between a single value of the forecast variable and the predictors.

# It can be convenient to write this in matrix form where all the values of the forecast variable are given in a single equation. 
# Let y=(y1,…,yT)′, ε=(ε1,…,εT)′, β=(β0,…,βk)′ and 
# 
#    ⎡  1   x[1,1]    x[2,1]    …   x[k,1]  ⎤
#    ⎢  1   x[1,2]    x[2,2]    …   x[k,2]  ⎥
# X= ⎢  ⋮     ⋮           ⋮             ⋮     ⎥
#    ⎢  1   x[1,T]    x[2,T]    …   x[k,T]  ⎦

# Then y = Xβ + ε. where ε has mean 0 and variance σ[^2]I. 
# Note that the X matrix has T rows reflecting the number of observations and k+1 columns reflecting the intercept which is represented by the column of ones plus the number of predictors.


#** Least squares estimation ----
# Least squares estimation is performed by minimising the expression ε′ε = (y−Xβ)′(y−Xβ). 
# It can be shown that this is minimised when β takes the value ^β = (X′X)[^−1]X′y 
#   This is sometimes known as the “normal equation.” 
# The estimated coefficients require the inversion of the matrix X′X. 
# If X is not of full column rank then matrix X′X is singular and the model cannot be estimated. 
#   This will occur, for example, if you fall for the “dummy variable trap,” i.e., having the same number of dummy variables as there are categories of a categorical predictor, as discussed in Section 7.4.

# The residual variance is estimated using 
#   ^σ[^2][e] = (1/T−k−1) * (y−X^β)′(y−X^β)


#** Fitted values and cross-validation ----
# The normal equation shows that the fitted values can be calculated using 
#   ^y = X^β = X(X′X)[^−1]X′y = Hy, 
#   where H = X(X′X)[^−1]X′ is known as the “hat-matrix” because it is used to compute ^y(“y-hat”).

# If the diagonal values of H are denoted by h1,…,hT, then the cross-validation statistic can be computed using 
#   CV = (1/T) * [T∑t=1](e[t]/(1−h[t]))[^2], 
#     where et is the residual obtained from fitting the model to all T observations. 
# Thus, it is not necessary to actually fit T separate models when computing the CV statistic.


#** Forecasts and prediction intervals ----
# Let x∗ be a row vector containing the values of the predictors (in the same format as X) for which we want to generate a forecast. 
# Then the forecast is given by 
#   ^y = x∗^β = x∗(X′X)[^−1]X′Y 
# and its estimated variance is given by 
#   ^σ[^2][e](1 + x∗(X′X)[^−1](x∗)′).
# A 95% prediction interval can be calculated (assuming normally distributed errors) as 
#   ^y ± 1.96^σ[e]√1 + x∗(X′X)[^−1](x∗)′.
# This takes into account the uncertainty due to the error term ε and the uncertainty in the coefficient estimates. 
# However, it ignores any errors in x∗. 
# Thus, if the future values of the predictors are uncertain, then the prediction interval calculated using this expression will be too narrow.


# _----


#* 7.10 Exercises ----------------------------------------------------------
#** 1 ----
jan14_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  index_by(Date) %>%
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

#*** 1.a ----
jan14_vic_elec %>%
  autoplot(Demand) +
  labs(title = "Daily Electricity Demand Trend")

jan14_vic_elec %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Temperature v/s Daily Electricity Demand")

# There is a positive relationship possibly because as Temperature rises there is no more usage of air conditioners.


#*** 1.b ----
demand_fit <- jan14_vic_elec %>%
  model(TSLM(Demand ~ Temperature))

demand_fit %>% gg_tsresiduals()
# There is a visible pattern in the residual plot.
# This is confirmed by the histogram, which is slightly right skewed.
# The ACF plot reveals no correlation.


#*** 1.c ----
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


#*** 1.d ----
demand_fc %>% hilo()


#*** 1.e ----
vic_elec %>%
  index_by(Date) %>%
  mutate(Demand ~ sum(Demand), Temperature = max(Temperature)) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# There is a non-linear relationship between Temperature and Demand.
# Upto 20 deg there is a reduction in Demand, followed by Increease.
# Therefore our model is overfitted for Jan 2014 Demand, and is not useful on a generic basis.


#** 2 ----
#*** 2.a ----
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


#*** 2.b ----
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


#*** 2.c ----
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


#*** 2.d ----
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


#** 3 ----
# logy = B0 + B1logx + E
# e[^logy] = e[^(B0 + B1logx + E)]
# y = e[^(B0 + B1logx + E)]

# dy/dx = d/dx * e[^(B0 + B1logx + E)]

# u = B1logx
# C = B0 + E
# y = e[C + u] = e[C].e[u]

# dy/dx = d/dx * e[C + u]
# dy/dx = (d/du * e[C].e[u]) * (d/dx * B1logx)
# dy/dx = e[C].e[u] * B1/x
# dy/dx = y * B1/x
# dy/dx * x/y = B1 

# therefore, coefficient B1 is the elasticity coefficient


#** 4 ----
#*** 4.a ----
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


#*** 4.b ----
# Sales in recent years have ranged from ~17,500 in Jan to ~75,000 in Dec
# even historically the range from Jan to Dec has been high
# to ensure that the Dec Sales does not dominate the regression model, it is essential to take logs

# Logarithms are useful because they are interpretable: changes in a log value are relative (or percentage) changes on the original scale


#*** 4.c ----
souvenirs <- souvenirs %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0))

souvenirs_fit <- souvenirs %>%
  model(TSLM(log(Sales) ~ trend() + season() + `Surfing Festival`))


#*** 4.d ----
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


#*** 4.e ----
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


#*** 4.f ----
report(souvenirs_fit)

# The +ve trend coeff means that it increases YoY but the rate is slow
# The +ve coeff for the seasonal variable means that the Jan sales are lowest, and the sales of other months are higher than Jan's for most years. The impact of Nov & Dec is especially high compared to the remaining months.
# The 0.501 coeff for Surfing Festival means that it has a high positive impact on the sales.


#*** 4.g ----
augment(souvenirs_fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)

# The p-value of the test is less than 0.001
# hence residuals can be distinguished from white noise.
# The residuals can be correlated with each other.


#*** 4.h ----
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


#*** 4.i ----
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


#** 5 ----
us_gasoline_2004 <- us_gasoline %>%
  filter(year(Week) <= 2004)


#*** 5.a ----
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


#*** 5.b ----
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


#*** 5.c ----
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


#*** 5.d ----
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


#** 6 ----
popu_afg_ge <- global_economy %>%
  filter(Code == "AFG") %>%
  select(Country, Code, Year, Population)


#*** 6.a ----
popu_afg_ge %>% autoplot(Population)
# During the period of 1980-1987/88 there is decline in the population.
# This is due to the effect of the Soviet-Afghan war which occured during 1980-1989.


#*** 6.b ----
fit_popu <- popu_afg_ge %>%
  model(
    linear = TSLM(Population ~ trend()),
    piecewise = TSLM(Population ~ trend(knots = c(1980, 1989)))
  )

popu_afg_ge %>%
  autoplot(Population) +
  geom_line(aes(y = .fitted, color = .model), data = fitted(fit_popu)) +
  labs(y = "Winning times", title = "Afghan Population")


#*** 6.c ----
popu_fc <- fit_popu %>% forecast(h = 5)

popu_afg_ge %>%
  autoplot(Population) +
  geom_line(aes(y = .fitted, color = .model), data = fitted(fit_popu)) +
  labs(y = "Winning times", title = "Afghan Population") +
  autolayer(popu_fc, alpha = 0.5, level = 95)

# The piecewise forecast closely follows the actual data. Hence the prediction interval also has a small range.
# The linear forecast doesn't follow the actual data. Hence the prediction interval is also very large.


#** 7 ----
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


# ____----


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
# (N,N) 	          Simple exponential smoothing
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


# _----


#* 8.8 Exercises -----------------------------------------------------------
#** 1 ----
aus_pigs <- aus_livestock %>%
  filter(Animal == "Pigs", State == "Victoria")


#*** 1.a ----
fit <- aus_pigs %>%
  model(ETS(Count ~ error() + trend("N") + season("N")))

tidy(fit)

fc <- fit %>%
  forecast(h = 4)

fc %>%
  autoplot(aus_pigs %>% filter(year(Month) >= 2010))


#*** 1.b ----
clipr::write_clip(augment(fit))
# std in excel(using STDEV.S()) == 9344.665793

s <- glance(fit)$sigma2 %>% sqrt()  # 9353.115

level_max_month <- components(fit) %>%
  filter(Month == max(Month)) %>%
  pull(level)

# first forecast
#   Excel std
level_max_month - 1.96*9344.665793  # 76871.01
level_max_month + 1.96*9344.665793  # 113502.1


#   R std
level_max_month - 1.96*s  # 76854.45
level_max_month + 1.96*s  # 113518.7


#   R fc
fc %>% filter(Month == min(Month)) %>% hilo() %>% pull(`95%`)   # [76854.79, 113518.3]95


# There is a slight difference between the 3 results.
# R std result might be different because 1.96 is a rounded figure rather than the full number.
# Excel std result might be different because STD calculation is different and also the above reason.


#** 2 ----
ses <- function(y, alpha, level) {
  level0 <- level
  rows <- nrow(y)
  level <- NA
  for (t in 1:rows) {
    if(t == 1)
      pre_level <- level0
    else
      pre_level <- level[t-1]
  
    level[t] <- (alpha * y[[t,2]]) + ((1 - alpha) * pre_level)
  }
  level <- level[rows]
  return(level)
}


# aus_pigs from Q1
aus_pigs %>%
  select(Month, Count) %>%
  ses(alpha = 0.3221247, level = 100646.6)

# Yes. The forecast is the same.


#** 3 ----
sse <- function(y, par) {
  # par1 is alpha; par2 is level0
  alpha <- par[1]
  level0 <- par[2]
  rows <- nrow(y)
  level <- yhat <- error <- NA
  for (t in 1:rows) {
    if(t == 1)
      pre_level <- level0
    else
      pre_level <- level[t-1]
    
    yhat[t] <- pre_level
    level[t] <- (alpha * y[[t,2]]) + ((1 - alpha) * pre_level)
    error[t] <- y[[t,2]] - yhat[t]
  }
  level <- level[rows]
  sse <- sum(error^2)
  return(sse)
}


# aus_pigs from Q1
aus_pigs %>%
  select(Month, Count) %>%
  ses(par = c(0.3221247, 100646.6))

optim_par <- optim(
  par = c(0, mean(range(aus_pigs$Count))), 
  fn = sse, 
  y = aus_pigs %>% select(Month, Count)
)


# The optim value varies from the values forecasted using ETS().
# This might be possibly because of the large range for aus_pigs$count.


#** 4 ----
ets_custom <- function(y){
  optim_par <- optim(
    par = c(0, mean(range(y[,2]))), 
    fn = sse, 
    y = y
  )
  fc <- ses(y = y, alpha = optim_par$par[1], level = optim_par$par[2])
  return(list(fc = fc, alpha = optim_par$par[1], level = optim_par$par[2]))
}

aus_pigs %>%
  select(Month, Count) %>%
  ets_custom()


# compare with ETS model
fc$.mean[1]; tidy(fit)$estimate[1]; tidy(fit)$estimate[2]


#** 5 ----
uk_exports <- global_economy %>%
  filter(Country == "United Kingdom")


#*** 5.a ----
uk_exports %>% autoplot(Exports)
# Rising trend, no clear seasonality, possible cyclicity present

uk_exports %>% gg_lag(Exports, geom = "point")
# Strong correlation at lag 1 which reduces to lag 2 and 3. No clear correlation after lag 3.

uk_exports %>% ACF(Exports) %>% autoplot()
# Large autocorrelations for nearby lags slowly reducing confirms the presence of trend.
# No seasonal spikes show lack of seasonality in the data.


#*** 5.b ----
fit <- uk_exports %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

fc <- fit %>% forecast(h = 5)

fc %>% autoplot(uk_exports)


#*** 5.c ----
fit_RMSE <- accuracy(fit)$RMSE


#*** 5.d ----
fit2 <- uk_exports %>%
  model(ETS(Exports ~ error("A") + trend("A") + season("N")))

fit2_RMSE <- accuracy(fit2)$RMSE

fit_RMSE; fit2_RMSE
# The acccuracy of the 2 models using RMSE is nearly the same.
# Based on the fit accuracy, either model can be used.


#*** 5.e ----
fc2 <- fit2 %>% forecast(h = 5)

p1 <- fc %>% autoplot(uk_exports) + theme(legend.position = "none")

p2 <- fc2 %>% autoplot(uk_exports) + labs(y = element_blank())

p1 + p2

# The forecasts also provide a similar range, although there is a clear presence of trend in the trended model.
# For short term forecasts(< 5 years), the trended model gives a slightly better forecast.


#*** 5.f ----
fc %>% filter(Year == min(Year)) %>% hilo() %>% pull(`95%`); fc$.mean[1] - 1.96*fit_RMSE; fc$.mean[1] + 1.96*fit_RMSE

fc2 %>% filter(Year == min(Year)) %>% hilo() %>% pull(`95%`); fc2$.mean[1] - 1.96*fit2_RMSE; fc2$.mean[1] + 1.96*fit2_RMSE

# The ranges for the manually calculated intervals are slightly narrower than the range produced from R.
# This might be due to the rounding of 1.96.


#** 6 ----
gdp_china <- global_economy %>%
  filter(Country == "China")

gdp_china %>% autoplot(GDP)

gdp_china %>% gg_lag(GDP, geom = "point")

gdp_china %>% ACF(GDP) %>% autoplot()


# Clear trend is visible, which starts exponentially increasing after 2020.
# No seasonality seems to be visible.


lambda <- gdp_china %>% features(GDP, features = guerrero) %>% pull(lambda_guerrero)

gdp_china %>% autoplot(GDP) + labs(title = "Original scale") +
  gdp_china %>% autoplot(box_cox(GDP, lambda)) + labs(title = "Box-Cox Transformation", y = element_blank())

fit <- gdp_china %>%
  model(
    Auto = ETS(GDP),
    AAN = ETS(GDP ~ error("A") + trend("A") + season("N")),
    AAdN = ETS(GDP ~ error("A") + trend("Ad") + season("N")),
    Box_Cox = ETS(box_cox(GDP, lambda)),
    Box_Cox_Ad = ETS(box_cox(GDP, lambda) ~ error("A") + trend("Ad") + season("N"))
  )

fc <- fit %>% forecast(h = 100)

fc %>%
  filter(Year <= 2025) %>%
  autoplot(gdp_china) +
  # autoplot(gdp_china, level = NULL) +
  facet_wrap(vars(.model), scales = "free_y")

# Forecast Analysis
# Auto, AAN, and AAdN give negative intervals after 2075. Negative values for GDP are not possible.
# The forecast intervals for all forecast are very wide, caused due to the exponential increase in the GDP post 2020.
#   Auto(MAN) - the point forecast is the trend continued in the future. Has the widest interval from the start, with the lower interval quickly moving towards the negative.
#   AAN - point forecast is similar to Auto.
#   AAdN - the forecast is slowly dampened.
#   Box_Cox - the point forecast is an exponential increase.
#   Box_Cox_Ad - the point forecast is an exponential increase, but the increase is slightly dampened. So the rate of increase is not as high as for Box_Cox.


accuracy(fit)
# All have similar error terms.
# The Box_Cox methods seem to have the lowest AIC values showing they are better models.
# However, over the 100 years forecast period their forecast doesn't seem possible. The AAdN model seems to give a better forecast over large forecast range.


#** 7 ----
aus_production %>% autoplot(Gas)
# Increasing trend with precense of seasonality. The increase in seasonal component YoY is clearly visible.

aus_production %>% gg_season(Gas)
# Seasonality increases from Q1 to Q3 and then dips in Q4.

aus_production %>% gg_subseries(Gas)
# Rates are increasing YoY each quarter.

aus_production %>% gg_lag(Gas)
aus_production %>% ACF(Gas) %>% autoplot()
# Strong lag clearly visible. Decreasing bar heights confirm presence of trend. Spikes every 4 quarters confirms presence of seasonality.


# Due to  increasing seasonal component YoY the multiplicative seasonality is necessary.

fit <- aus_production %>%
  model(
    AM = ETS(Gas ~ error() + trend("A") + season("M")),
    AdM = ETS(Gas ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(h = "2 years")

fc %>%
  autoplot(aus_production) +
  facet_wrap(vars(.model))

# Damping the trend seems to have no visible impact on the forecast.


#** 8 ----
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries %>% autoplot(Turnover)

# Comments from 2.10.6
# increasing trend with seasonality
# decreases in Feb, peaks in Dec
# peaks in Dec, starts increasing from Jan
# data is not random, with increasing trend showcasing high correlation with historical values
# trend and seasonlity are confirmed
# correlation with lag = 1, 12


#*** 8.a ----
# Due to presence of increasing seasonal component YoY multiplicative seasonality is necessary.


#*** 8.b ----
fit <- myseries %>%
  model(
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(h = "2 years")

fc %>% autoplot(myseries)

# Trend damping results in slightly lower point forecasts and wider prediction intervals.


#*** 8.c ----
fit %>% accuracy()

# The RMSE is similar for both models, with the value for AdM being slightly lower. Hence the AdM model is preferred.


#*** 8.d ----
fit %>% select(AdM) %>% gg_tsresiduals()

fit %>% augment() %>% filter(.model == "AdM") %>% features(.resid, ljung_box, lag = 24, dof = 0)
# Since this is seasonal data taking lag = 2m = 2*12 = 24

# There is clear visibility of autocorrelation visible in the lag plot.
# This is confirmed by the ljung_box test.


#*** 8.e ----
myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)

fit <- myseries_train %>%
  model(
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(new_data = myseries_test)

fc %>% accuracy(myseries)

fc %>% autoplot(myseries, level = NULL)

# Both AM and AdM beat the SNaive model.
#   This is because the SNaive model assumes no trend is present in the model.
# However, between AM and AdM, AM model performs better on the test set.
#   This is because the actual data continues following historical trends rather than plateauing out.

# This case shows that even though a model may perform better on the train set, all models should still be checked against the test set, and only the model which performs best on the test set should be taken, no matter it's performance on the train set.


#** 9 ----
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries %>% autoplot(Turnover)


myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)

lambda <- myseries_train %>% features(Turnover, features = guerrero) %>% pull(lambda_guerrero)

myseries_train %>%
  model(STL(box_cox(Turnover, lambda = lambda))) %>%
  components() %>%
  autoplot()

fit <- myseries_train %>%
  model(
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    STL = decomposition_model(
      STL(Turnover),
      ETS(season_adjust)
    )
  )

fc <- forecast(fit, new_data = myseries_test)

fc %>% accuracy(myseries)

fc %>% autoplot(myseries, level = NULL)


# For periods to close to start of test set, the forecasts from all 3 models are similar.
# For periods close to the end of the test set, the forecasts for STL model are lower than the actual Turnover, Box_Cox forecasts higher, while AM forecasts closest to the actual.
# Therefore, AM model is still better than STL decomposition (with or without Box_Cox transformation).


#** 10 ----
total_trips <- tourism %>%
  summarise(Trips = sum(Trips))


#*** 10.a ----
total_trips %>% autoplot(Trips)
# No visible trend or seasonality before 2010.
# Increasing trend starting from 2010 with possible seasonal pattern.

total_trips %>% gg_season(Trips)
# Highest Trips in Q1, before dropping in Q2 and Q3, and rising in Q4.

total_trips %>% gg_subseries(Trips)
# There is a clear increase YoY in the number of Trips in each quarter starting from 2010.


total_trips %>% gg_lag(Trips, geom = "point")
total_trips %>% ACF(Trips) %>% autoplot()
# Both Trend and Seasonality is present, with seasonal spikes at 4, 8 and 12 lags.
# Since this is quarterly data it essentially means that the Trips data has a decreasing (but significant) correlation with last 3 years.


#*** 10.b ----
total_trips %>%
  model(STL(Trips)) %>%
  components() %>%
  autoplot(season_adjust)


#*** 10.c ----
fit <- total_trips %>%
  model(
    decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("Ad"))
    )
  )

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)


#*** 10.d ----
fit <- total_trips %>%
  model(
    decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("A"))
    )
  )

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)


#*** 10.e ----
fit <- total_trips %>%
  model(
    decomposition_model(
      STL(Trips),
      ETS(season_adjust)
    )
  )

fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)


#*** 10.f ----
fit <- total_trips %>%
  model(
    Ad = decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("Ad"))
    ),
    A = decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error() + trend("A"))
    ),
    ETS = decomposition_model(
      STL(Trips),
      ETS(season_adjust)
    )
  )

fit %>% select(ETS) %>% report()

accuracy(fit)

# The A and ETS models are the same, and give slightly better result than Ad on MAPE and RMSSE.


#*** 10.g ----
fc <- forecast(fit, h = "2 years")

fc %>% autoplot(total_trips)

# Model A/ETS seem to be reasonable based on past trends.


#*** 10.h ----
fit %>% select(A) %>% gg_tsresiduals()

# The residual plot seems to show no visible pattern, and for the most part seem to be centered around 0.
# The histogram also shows a near normal plot.
# The correlogram shows a spike at lag 14. However, since there is only one such spike and it is far back, we can say that the residuals are not correlated.


#** 11 ----
aus_arrivals_nz <- aus_arrivals %>% filter(Origin == "NZ")


#*** 11.a ----
aus_arrivals_nz %>% autoplot(Arrivals)
# Increasing trend with seasonal patterns. Seasonal amplitude seems to be increasing.

aus_arrivals_nz %>% gg_season(Arrivals)
# Lowest arrivals in Q1, which decreases in Q2 and peaks in Q3, before falling in Q4.

aus_arrivals_nz %>% gg_subseries(Arrivals)
# The mean arrivals in each quarter validate above observation.
# There is YoY increase in each quarter, although the increase in Q1 is less than for other quarters.

aus_arrivals_nz %>% gg_lag(Arrivals, geom = "point")
aus_arrivals_nz %>% ACF(Arrivals, lag_max = 50) %>% autoplot()
# The lag plot and the correlogram shows a strong presence of autocorrelation.
# The trend and seasonal patterns are clearly visible.
# The seasonal patterns repeat every 4 quarters (1 year).
# The strong correlation goes back almost 7-8 years.


#*** 11.b ----
aus_arrivals_nz_test <- aus_arrivals_nz %>% slice(n() - 8:0)

aus_arrivals_nz_train <- aus_arrivals_nz %>% anti_join(aus_arrivals_nz_test)

fit <- aus_arrivals_nz_train %>%
  model(
    AM = ETS(Arrivals ~ error() + trend("A") + season("M")),
    AdM = ETS(Arrivals ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(new_data = aus_arrivals_nz_test)

fc %>% autoplot(aus_arrivals_nz, level = NULL)

accuracy(fc, aus_arrivals_nz)
# AM gives better results on all accuracy measures.

fit %>% select(AdM) %>% gg_tsresiduals()
# Residuals are centered around the mean, histogram is near normal, and the lag plot shows white noise.


#*** 11.c ----
# Due to presence of incresing seasonal component YoY multiplicative seasonality is necessary.


#*** 11.d ----
fit <- aus_arrivals_nz_train %>%
  model(
    ETS = ETS(Arrivals),
    ETS_A = ETS(log(Arrivals) ~ error() + trend() + season("M")),
    SNaive = SNAIVE(Arrivals),
    STL = decomposition_model(
      STL(log(Arrivals)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = aus_arrivals_nz_test)

fc %>% autoplot(aus_arrivals_nz, level = NULL)


#*** 11.e ----
accuracy(fc, aus_arrivals_nz)
# The ETS model gives the best forecast.

fit %>% select(ETS) %>% gg_tsresiduals()
# Residuals are almost centered around the mean, histogram has a slight night skew, and the lag plot shows white noise.


#*** 11.f ----
fit <- aus_arrivals_nz %>%
  stretch_tsibble(.init = 4) %>%
  model(
    ETS = ETS(Arrivals),
    ETS_A = ETS(log(Arrivals) ~ error() + trend() + season("M")),
    SNaive = SNAIVE(Arrivals),
    STL = decomposition_model(
      STL(log(Arrivals)),
      ETS(season_adjust)
    )
  )

fit %>% forecast(h = 1) %>% accuracy(aus_arrivals_nz)

# Using CV, ETS_A gives the best forecast.


#** 12 ----
aus_production %>% autoplot(Cement)

aus_production %>% gg_season(Cement)

aus_production %>% gg_subseries(Cement)

aus_production %>% gg_lag(Cement, geom = "point")

aus_production %>% ACF(Cement, lag_max = 70) %>% autoplot()

# Trend present, increasing for most part.
# Minor seasonality with changing amplitude.
# lag present up to 64 quarters.

# Therefore, an ETS model with (A,M) or (Ad,M) might be good.


#*** 12.a ----
fit <- aus_production %>%
  stretch_tsibble(.init = 5*4, .step = 1) %>%
  model(
    ETS_AM = ETS(Cement ~ error() + trend("A") + season("M")),
    ETS_AdM = ETS(Cement ~ error() + trend("Ad") + season("M")),
    SNaive = SNAIVE(Cement)
  )

fc <- fit %>% forecast(h = 4)


#*** 12.b ----
fc %>%
  accuracy(aus_production) %>%
  select(.model, RMSE) %>%
  mutate(MSE = RMSE*2) %>%
  print.data.frame()

# ETS_AM is a better model. This is expected based on the initial analysis done on the data.

 
#** 13 ----
#*** Beer and bricks production ----
# Beer
aus_production %>% autoplot(Beer)

test <- aus_production %>% slice(n() -  11:0)
train <- aus_production %>% anti_join(test)

lambda <- aus_production %>%
  features(Beer, features = guerrero) %>%
  pull(lambda_guerrero)

aus_production %>%
  model(STL(box_cox(Beer, lambda = lambda))) %>%
  components() %>%
  autoplot()

fit <- train %>%
  model(
    ETS = ETS(Beer),
    SNaive = SNAIVE(Beer),
    STL = decomposition_model(
      STL(box_cox(Beer, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(aus_production, level = NULL)

accuracy(fc, aus_production)
# ETS model is the best based on RMSE, RMSSE.


# Bricks
aus_production %>% autoplot(Bricks)

test <- aus_production %>% filter(!is.na(Bricks)) %>% slice(n() - 11:0)
train <- aus_production %>% filter(!is.na(Bricks)) %>% anti_join(test)

lambda <- aus_production %>%
  features(Bricks, features = guerrero) %>%
  pull(lambda_guerrero)

fit <- train %>%
  filter(!is.na(Bricks)) %>%
  model(
    ETS = ETS(Bricks),
    SNaive = SNAIVE(Bricks),
    STL = decomposition_model(
      STL(box_cox(Bricks, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(aus_production, level = NULL)

accuracy(fc, aus_production)
# ETS model is the best based on all parameters.


#*** diabetes(ATC2 == "A10") and corticosteroids (ATC2 == "H02") ----
PBS_A10_H02 <- PBS %>%
  filter(ATC2 %in% c("A10", "H02")) %>%
  group_by(ATC2) %>%
  summarise(Cost = sum(Cost))

PBS_A10_H02_test <- PBS_A10_H02 %>% filter(yearmonth(Month) >= max(yearmonth(Month)) - 35)
PBS_A10_H02_train <- PBS_A10_H02 %>% anti_join(PBS_A10_H02_test)

# diabetes (ATC2 == "H02")
PBS_A10_H02 %>% filter(ATC2 == "A10") %>% autoplot(Cost)

lambda <- PBS_A10_H02 %>% filter(ATC2 == "A10") %>% features(Cost, features = guerrero) %>% pull(lambda_guerrero)

PBS_A10_H02 %>% filter(ATC2 == "A10") %>%
  model(STL(Cost)) %>%
  components() %>%
  autoplot()

fit <- PBS_A10_H02_train %>%
  filter(ATC2 == "A10") %>%
  model(
    ETS = ETS(Cost),
    SNaive = SNAIVE(Cost),
    STL = decomposition_model(
      STL(box_cox(Cost, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = PBS_A10_H02_test %>% filter(ATC2 == "A10"))

fc %>% autoplot(PBS_A10_H02 %>% filter(ATC2 == "A10"), level = NULL)

accuracy(fc, PBS_A10_H02 %>% filter(ATC2 == "A10"))
# STL model is better.
# Probably because of the changing seasonal amplitude, first performing STL on the Box-Cox transformed data, and then using ETS only on the seasonally adjusted data will reduce the impact of the varying seasonal amplitude resulting in better forecast.


# corticosteroids (ATC2 == "H02")
PBS_A10_H02 %>% filter(ATC2 == "H02") %>% autoplot(Cost)

lambda <- PBS_A10_H02 %>% filter(ATC2 == "H02") %>% features(Cost, features = guerrero) %>% pull(lambda_guerrero)

PBS_A10_H02 %>% filter(ATC2 == "H02") %>%
  model(STL(Cost)) %>%
  components() %>%
  autoplot()

fit <- PBS_A10_H02_train %>%
  filter(ATC2 == "H02") %>%
  model(
    ETS = ETS(Cost),
    SNaive = SNAIVE(Cost),
    STL = decomposition_model(
      STL(box_cox(Cost, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = PBS_A10_H02_test %>% filter(ATC2 == "H02"))

fc %>% autoplot(PBS_A10_H02 %>% filter(ATC2 == "H02"), level = NULL)

accuracy(fc, PBS_A10_H02 %>% filter(ATC2 == "H02"))

# STL model is better. Probably same reason as above.


#*** Total food retailing turnover ----
food_turnover <- aus_retail %>% summarise(Turnover = sum(Turnover))

food_turnover %>% autoplot(Turnover)

test <- food_turnover %>% slice(n() - 35:0)
train <- food_turnover %>% anti_join(test)

lambda <- train %>% features(Turnover, features = guerrero) %>% pull(lambda_guerrero)

food_turnover %>%
  model(STL(box_cox(Turnover, lambda = lambda))) %>%
  components() %>%
  autoplot()

fit <- train %>%
  model(
    ETS = ETS(Turnover),
    SNaive = SNAIVE(Turnover),
    STL = decomposition_model(
      STL(box_cox(Turnover, lambda = lambda)),
      ETS(season_adjust)
    )
  )

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(food_turnover, level = NULL)

accuracy(fc, food_turnover)
# ETS is the best model.


#** 14 ----
#*** 14.a ----
#**** total number of trips across Australia ----
total_trips <- tourism %>% summarise(Trips = sum(Trips))

total_trips %>% autoplot(Trips)

total_trips %>%
  stretch_tsibble(.init = 4) %>%
  model(ETS(Trips)) %>%
  forecast(h = 4) %>%
  accuracy(total_trips)

test <- total_trips %>% slice(n() - 7:0)
train <- total_trips %>% anti_join(test)

fc <- train %>%
  model(ETS(Trips)) %>%
  forecast(new_data = test)

fc %>% autoplot(total_trips, level = NULL)
fc %>% accuracy(total_trips)

# The forecast is not very great, especially as the forecast range increases.


#**** closing prices for the four stocks ----
stock <- gafa_stock %>%
  group_by(Symbol) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE) %>%
  ungroup()

stock %>% autoplot(Close) +
  facet_wrap(vars(Symbol), scales = "free_y")

test <- stock %>% filter(day >= max(day) - 99)
train <- stock %>% anti_join(test)

fit <- train %>% model(ETS(Close))

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(stock, level = NULL) +
  facet_wrap(vars(Symbol), scales = "free_y")
fc %>% accuracy(stock)

# ETS gives a flat forecast which is incorrect.


#**** lynx series ----
pelt %>% autoplot(Lynx)

test <- pelt %>% slice(n() - 4:0)
train <- pelt %>% anti_join(test)

fit <- train %>% model(ETS(Lynx))

fc <- fit %>% forecast(new_data = test)

fc %>% autoplot(pelt, level = NULL)
fc %>% accuracy(pelt)

# ETS gives a flat forecast which is incorrect.


#*** 14.b ----
# ETS does not always give a food forecasts.
# None of the forecsats above work well.

# total number of trips across Australia
# Model - (M,N,M)
#   due to no clear trend, ETS sets the Trend parameter as None. However, after 2010 there seems to be an increasing trend.
#   the seasonal patterns are also not clear and the amplitude seems to be changing. Therefore, even if we take only data after 2010, then the ETS model sets the seasonal parameter as None (see below).

total_trips <- tourism %>%
  summarise(Trips = sum(Trips)) %>%
  filter(year(Quarter) >= 2010)

test <- total_trips %>% slice(n() - 7:0)
train <- total_trips %>% anti_join(test)

fc <- train %>% model(ETS(Trips)) %>% forecast(new_data = test)

fc %>% autoplot(total_trips, level = NULL)
fc %>% accuracy(total_trips)


# closing prices for the four stocks
# Model -
#   AAPL   <ETS(M,N,N)>
#   AMZN   <ETS(M,A,N)>
#   FB     <ETS(M,N,N)>
#   GOOG   <ETS(M,N,N)>
# Since stocks trading does not take place on all 7 days, and there is no clear seasonal patterns, the ETS model selects (N,N) for all stocks except AMZN, where it selects additive trend.
#   However, a similar trend seem to be visible in the other 4 stocks so not sure why it didn't use the same model for the other 3.


# lynx series
# Model - (A,N,N)
# Since this is a annual data, there are no seasonal patterns. Also, there is no trend even though is cyclicity.
# Hence, while the model chosen by ETS is correct, it fails to capture the data trends.
#   This is because ETS is meant to capture trend and seasonality only, and fails when there is only cyclical data with no trend.


#** 15 ----
# Holt-Winters’ multiplicative method
# The component form for the multiplicative method is:
#   ^y[t+h|t] = (ℓ[t] + hb[t]) * s[t+h−m(k+1)]
#   ℓ[t]      = α(y[t] / s[t−m]) + (1 − α)(ℓ[t−1] + b[t−1])
#   b[t]      = β∗ (ℓ[t] − ℓ[t−1]) + (1 − β∗)b[t−1]
#   s[t]      = γ(y[t] / (ℓ[t−1] + b[t−1])) + (1 − γ)s[t−m]

# 1 step ahead model
#   ^y[t+1|t] = (ℓ[t] + b[t]) * s[t+1−m]

# Specifying one-step-ahead training errors as relative errors such that
#   ε[t]      = ( ^y[t|t-1] - (ℓ[t-1] + b[t-1]) * s[t−m] ) / ( (ℓ[t-1] + b[t-1]) * s[t−m] )

# Substituting this into the error correction equations for Holt-Winters' linear method we obtain
#   y[t]      = ( (ℓ[t-1] + b[t-1]) * s[t−m] ) (1 + ε[t])

#   ℓ[t]      = α(y[t] / s[t−m]) + (1 − α)(ℓ[t−1] + b[t−1])
#             = α( ( ( (ℓ[t-1] + b[t-1]) * s[t−m] ) (1 + ε[t]) ) / s[t−m] ) + (1 − α)(ℓ[t−1] + b[t−1])
#             = α( ℓ[t-1] + b[t-1] )  (1 + ε[t]) + (1 − α)(ℓ[t−1] + b[t−1])
#             = α( ℓ[t-1] + b[t-1] ) + α( ℓ[t-1] + b[t-1] ) ε[t] + ( ℓ[t-1] + b[t-1] ) - α( ℓ[t-1] + b[t-1] )
#             = (ℓ[t-1] + b[t-1])(1 + αε[t])

#   b[t]      = β∗ (ℓ[t] − ℓ[t−1]) + (1 − β∗)b[t−1]
#             = β∗ ( (ℓ[t-1] + b[t-1])(1 + αε[t]) - ℓ[t−1] ) + (1 - β∗)b[t-1]
#             = β∗ ( (ℓ[t-1] + b[t-1]) + (ℓ[t-1] + b[t-1])αε[t] - ℓ[t−1] ) + (1 - β∗)b[t-1]
#             = β∗ ( b[t-1] + (ℓ[t-1] + b[t-1])αε[t] ) + b[t-1] - β∗b[t-1]
#             = β∗ ( ℓ[t-1] + b[t-1] )αε[t] + b[t-1]
#             = b[t-1] + β(ℓ[t-1] + b[t-1])ε[t]
# for simplicity we have set β = αβ*

#   s[t]      = γ(y[t] / (ℓ[t−1] + b[t−1])) + (1 − γ)s[t−m]
#             = γ( ( ( (ℓ[t-1] + b[t-1]) * s[t−m] ) (1 + ε[t]) ) / (ℓ[t-1] + b[t-1]) ) + (1 − γ)s[t−m]
#             = γs[t−m](1 + ε[t]) + (1 − γ)s[t−m]
#             = γs[t−m] + γs[t−m]ε[t] + s[t−m] - γs[t−m]
#             = s[t−m](1 + γε[t])

# After substituting we get the ETS(M,A,M) model.


#** 16 ----
# Skipped as this proof is given in a separate book.


#** 17 ----
# prediction interval can be written as
#   y[T+h|T] ± c^σh
#   ℓ[t+h-1] + ε[t] ± 1.96(σ[^2][1 + α[^2](h-1)])[^0.5]


# ____----


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


# _----


#* 9.11 Exercises ----------------------------------------------------------
#** 1 ----
#*** 1.a ----
# Since for all 3 series the lagged values are within the limits, all 3 are white noises.


#*** 1.b ----
# Depending on the range T of all Time series, the limits are calculated as ±2/√T.
# Therefore, the range for the 3 series are:
#   36 = ±0.3333333;        360 = ±0.1054093;         1000 = ±0.06324555

# The autocorrelations are different for each series, since that is dependent on the data in the series.
# Different white noise series can have different autocorrelations depending on their data.


#** 2 ----
gafa_stock %>%
  filter(Symbol == "AMZN") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day) %>%
  gg_tsdisplay(Close, plot_type = "partial", lag_max = 60)

# ACF plot shows that the first 60 lagged values have autocorrelations higher than the critical values, and are decreasing slowly.
# PACF plot shows a strong spike at lag 1 (near 1), meaning that stock price has strong correlation with the previous days price. There are also spikes at other lagged values, although these have no visible pattern.
# This shows that the series is non-stationary and should be differenced at least once, to remove effects of lagged values and stabilize the series.


#** 3 ----
#*** 3.a ----
global_economy %>%
  filter(Code == "TUR") %>%
  autoplot(GDP)

# The presence of trend, no seasonality and no large variances means that first difference should give a stationary series.

global_economy %>%
  filter(Code == "TUR") %>%
  mutate(diff_gdp = difference(GDP)) %>%
  gg_tsdisplay(diff_gdp, plot_type = "partial")

# Except the spike at lag 11 in the ACF and PACF plot, the rest of the plot shows that the first differenced data is a white noise.

global_economy %>%
  filter(Code == "TUR") %>%
  features(GDP, unitroot_ndiffs)
  
# Performing unit root test also tells us that one difference is required.


#*** 3.b ----
aus_accommodation %>%
  filter(State == "Tasmania") %>%
  autoplot(Takings)

# There is trend and seasonality present, with increasing variance in the seasonality.
# Therefore, a first difference for both seasonal and non-seasonal might be required, along with Box-Cox transformations.

lambda <- aus_accommodation %>%
  filter(State == "Tasmania") %>%
  features(Takings, guerrero) %>%
  pull(lambda_guerrero)


aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda)) %>%
  gg_tsdisplay(Takings, plot_type = "partial")

# ACF plot shows decreasing spikes with seasonal variation every 4 quarters.

aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda)) %>%
  features(Takings, unitroot_nsdiffs)

aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda) %>% difference(4)) %>%
  features(Takings, unitroot_ndiffs)


aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda) %>% difference(4)) %>%
  gg_tsdisplay(Takings, plot_type = "partial")

# Both ACF and PACF plots show there is still lags at 1, 3 and 4.
# Therefore, maybe one more difference is required.  


aus_accommodation %>%
  filter(State == "Tasmania") %>%
  transmute(Takings = box_cox(Takings, lambda) %>% difference(4)) %>%
  features(Takings, ljung_box)

# ljung_box test also has given significant p-value meaning that the data is still not white noise.

# However, since the KPSS test doesn't give further differences, it means that though the data is not white noise, it is stationary.


#*** 3.c ----
souvenirs %>% autoplot(Sales)

# Increasing trend and seasonality with changing variance.
# Box-Cox transformation followed by seasonal differencing and first differencing might be required.

lambda <- souvenirs %>% features(Sales, guerrero) %>% pull(lambda_guerrero)

souvenirs %>%
  transmute(Sales = box_cox(Sales, lambda)) %>%
  features(Sales, unitroot_nsdiffs)

souvenirs %>%
  transmute(Sales = box_cox(Sales, lambda)) %>%
  features(Sales, unitroot_ndiffs)

souvenirs %>%
  transmute(Sales = box_cox(Sales, lambda) %>% difference(12)) %>%
  gg_tsdisplay(Sales, plot_type = "partial")


# There are rapidly failing spikes at lag 1 and 2.
# However, unit test doesn't give suggest a first difference is required. 


#** 4 ----
# Data needs 1 seasonal difference after Box-Cox transformation (lambda = 0.00213)
# Model equation is:
#   w[t]  = (sign(y[t])|y[t]|^0.00213 - 1)/0.00213
#         = (y[t]^0.00213 - 1)/0.00213      # since sales values are positive we can drop sign and ||
#   y'[t] = w[t] - w[t-4]
#         = w[t] - B^4w[t]
#         = (1 - B^4)w[t]


#** 5 ----
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries %>% autoplot(Turnover)

# Trend, seasonality with changing variance.
# Might need seasonal and first difference after Box-Cox transformation.

lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

myseries %>%
  transmute(Turnover = box_cox(Turnover, lambda)) %>%
  features(Turnover, unitroot_nsdiffs)

myseries %>%
  transmute(Turnover = box_cox(Turnover, lambda) %>% difference(12)) %>%
  features(Turnover, unitroot_ndiffs)


myseries %>%
  transmute(Turnover = box_cox(Turnover, lambda) %>% difference(12) %>% difference(1)) %>%
  gg_tsdisplay(Turnover, plot_type = "partial")

# 1 seasonal difference after Box-Cox transformation.


#** 6 ----
#*** 6.a ----
y <- numeric(100)
e <- rnorm(100)       # since σ[^2] = 1

# since y1 = 0, so c = 0
for (i in 2:100) {
  y[i] <- 0.6*y[i-1] + e[i]
}

sim <- tsibble(idx = seq_len(100), y = y, index = idx)


#*** 6.b ----
autoplot(sim, y)

ar1generator <- function(phi1 = 0.6){
  y <- numeric(100)
  e <- rnorm(100)
  
  for (i in 2:100) {
    y[i] <- phi1*y[i-1] + e[i]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, phi = phi1, index = idx, key = phi)
  
  return(sim)
}

bind_rows(
  ar1generator(),
  ar1generator(0.3),
  ar1generator(0.9)
) %>%
  autoplot(y)

# As phi increases, the variation in y increases.


#*** 6.c ----
ma1generator <- function(theta1 = 0.6){
  y <- numeric(100)
  e <- rnorm(100)
  
  for (i in 2:100) {
    y[i] <- e[i] + theta1*e[i-1]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, theta1 = theta1, index = idx, key = theta1)
  
  return(sim)
}


#*** 6.d ----
bind_rows(
  ma1generator(),
  ma1generator(0.3),
  ma1generator(0.9)
) %>%
  autoplot(y)

# As theta increases, the variation in y increases.


#*** 6.e ----
arma11generator <- function(phi1 = 0.6, theta1 = 0.6){
  y <- numeric(100)
  e <- rnorm(100)
  
  for (i in 2:100) {
    y[i] <- phi1*y[i-1] + e[i] + theta1*e[i-1]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, phi1 = phi1, theta1 = theta1, index = idx, key = c(phi1, theta1))
  
  return(sim)
}

autoplot(arma11generator(), y)


#*** 6.f ----
ar2generator <- function(phi1 = -0.8, phi2 = 0.3){
  y <- numeric(100)
  e <- rnorm(100)
  
  y[2] <- phi1*y[i] + e[2]
  
  for (i in 3:100) {
    y[i] <- phi1*y[i-1] + phi2*y[i-2] + e[i]
  }
  
  sim <- tsibble(idx = seq_len(100), y = y, phi1 = phi1, phi2 = phi2, index = idx, key = c(phi1, phi2))
  
  return(sim)
}

autoplot(ar2generator(), y)


#*** 6.g ----
p1 <- autoplot(arma11generator(), y)
p2 <- autoplot(ar2generator(), y)

p1 / p2

# data from AR(2) increased with oscelations, and is non-stationary.
# AR(1) model is stationary.


#** 7 ----
#*** 7.a ----
aus_airpassengers %>% autoplot(Passengers)
# Annual data with increasing trend.

fit <- aus_airpassengers %>%
  model(ARIMA(Passengers))

fit
# Model chosen (0,2,1)

fit %>% gg_tsresiduals()
# residuals look like white noise

fit %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)


#*** 7.b ----
# y''[t]  = c + ε[t] + θ[1]ε[t−1] + θ[2]ε[t−2]
#         = c + ε[t] + θ[1]Bε[t] + θ[2]B[^2]ε[t]
#         = c + ε[t] (1 + θ[1]B + θ[2]B[^2])


#*** 7.c ----
aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima010 = ARIMA(Passengers ~ 1 + pdq(0,1,0))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# ARIMA(0,1,0) gives a damped forecast compared to ARIMA.


#*** 7.d ----
aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima010 = ARIMA(Passengers ~ 1 + pdq(0,1,0)),
    arima212 = ARIMA(Passengers ~ 1 + pdq(2,1,2))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# ARIMA(2,1,2) gives a forecast similar to ARIMA, but with a narrower confidence interval.


aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima010 = ARIMA(Passengers ~ 1 + pdq(0,1,0)),
    arima212 = ARIMA(Passengers ~ 0 + pdq(2,1,2))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# Removing the constant gives an error as the AR becomes non-stationary.


#*** 7.e ----
aus_airpassengers %>%
  model(
    arima = ARIMA(Passengers),
    arima_1 = ARIMA(Passengers ~ 1 + pdq(0,2,1))
  ) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# Introducing constant causes the model to give a quadratic or higher order polynomial trend.
#   This is generally discouraged.
# The trend is higher than for without constant, but the confidence interval is narrower.


#** 8 ----
gdp <- global_economy %>% filter(Code == "USA") %>% select(GDP)


#*** 8.a ----
gdp %>% autoplot(GDP)

# The data has increasing trend, with little to no variance.
# Box-Cox transformation is not required.


#*** 8.b ----
gdp %>%
  model(ARIMA(GDP))
# ARIMA(0,2,2)


#*** 8.c ----
gdp %>%
  gg_tsdisplay(GDP, plot_type = "partial")

gdp %>%
  gg_tsdisplay(difference(GDP), plot_type = "partial")

gdp %>%
  gg_tsdisplay(difference(difference(GDP)), plot_type = "partial")

gdp %>%
  model(
    arima = ARIMA(GDP),
    arima122 = ARIMA(GDP ~ pdq(1,2,2)),
    arima021 = ARIMA(GDP ~ pdq(0,2,1)),
    arima012 = ARIMA(GDP ~ pdq(0,1,2)),
    arima222 = ARIMA(GDP ~ pdq(2,2,2))
  ) %>%
  # accuracy() %>% arrange(RMSE)
  augment() %>%
  autoplot(.fitted) +
  autolayer(gdp, GDP)


#*** 8.d ----
# ARIMA(2,2,2) looks best.

gdp %>%
  model(ARIMA(GDP ~ pdq(2,2,2))) %>%
  gg_tsresiduals()

# The residuals show white noise, although the histogram shows it is not normal.


#*** 8.e ----
gdp %>%
  model(ARIMA(GDP ~ pdq(2,2,2))) %>%
  forecast(h = 10) %>%
  autoplot(gdp)

# The forecast looks reasonable.


#*** 8.f ----
gdp %>%
  model(
    arima = ARIMA(GDP),
    arima222 = ARIMA(GDP ~ pdq(2,2,2)),
    ets = ETS(GDP)
  ) %>%
  forecast(h = 10) %>%
  autoplot(gdp, level = NULL)

# ARIMA(2,2,2) seems to give a more reasonable forecast than ETS.
# ETS has a higher trend increase than the default ARIMA model.


#** 9 ----
#*** 9.a ----
aus_arrivals_jp <- aus_arrivals %>% filter(Origin == "Japan")

aus_arrivals_jp %>% autoplot(Arrivals)

# Increasing trend till 1995 before decreasing.
# Possible seasonality with changing variances.


#*** 9.b ----
aus_arrivals_jp %>% features(Arrivals, unitroot_nsdiffs)

aus_arrivals_jp %>% features(Arrivals %>% difference(lag = 4), unitroot_ndiffs)

aus_arrivals_jp <- aus_arrivals_jp %>%
  mutate(
    double_diff_arrivals = difference(Arrivals, lag = 4) %>% difference(lag = 1)
  )

# 1 seasonal + 1 first difference.


#*** 9.c ----
aus_arrivals_jp %>% gg_tsdisplay(double_diff_arrivals, plot_type = "partial")

# There are spikes at lags 1, 4, 6 and 10, with the spikes at lag 4 being extreme.


#*** 9.d ----
# There are decreasing spikes at multiples of 4. Additional spikes at lags 1 and 5.


#*** 9.e ----
# Possible model = ARIMA(1,1,1)(1,1,1)[4]


#*** 9.f ----
fit <- aus_arrivals_jp %>%
  model(
    arima = ARIMA(Arrivals),
    arima_111111 = ARIMA(Arrivals ~ pdq(1,1,1) + PDQ(1,1,1))
  )

fit %>% glance() %>% arrange(AICc)
# Both models have similar values, although the arima model is slightly better.


#*** 9.g ----
# Backshift operator
#   (1 − Φ[1]B[^4]) * (1 − B)(1 − B[^4]) * y[t] = (1 + θ[1]B) (1 + Θ[1]B[^4]) * ε[t]

#   (1 − Φ[1]B[^4]) * (1 − B[^4] − B + B[^4]) * y[t] = (1 + θ[1]B + Θ[1]B[^4] + Θ[1][^2]B[^5]) * ε[t]
#   (1 − Φ[1]B[^4] − B[^4] + Φ[1]B[^8] − B + Φ[1]B[^5] + B[^4] - Φ[1]B[^8]) * y[t] = (1 + θ[1]B + Θ[1]B[^4] + Θ[1][^2]B[^5]) * ε[t]
#   (1 − Φ[1]B[^4] − B + Φ[1]B[^5]) * y[t] = (1 + θ[1]B + Θ[1]B[^4] + Θ[1][^2]B[^5]) * ε[t]
#   y[t] - Φ[1]y[t-4] - y[t-1] + Φ[1]y[t-5] = ε[t] + θ[1]ε[t-1] + Θ[1]ε[t-4] + Θ[1][^2]ε[t-5]
#   y[t] = y[t-1] + Φ[1](y[t-4] - y[t-5]) + ε[t] + θ[1](ε[t-1] + ε[t-4]) + Θ[1][^2]ε[t-5]
# Without Backshift operator


#** 10 ----
us_emp_TP <- us_employment %>%
  filter(Title == "Total Private") %>%
  select(Month, Employed)

us_emp_TP %>% autoplot(Employed)


#*** 10.a ----
us_emp_TP %>%
  model(STL(Employed)) %>%
  components() %>%
  autoplot()

# Increasing trend with changing seasonal variation.


#*** 10.b ----
# The data needs transformation.

lambda <- us_emp_TP %>% features(Employed, guerrero) %>% pull(lambda_guerrero)

us_emp_TP <- us_emp_TP %>% mutate(Employed_bc = box_cox(Employed, lambda))


#*** 10.c ----
# Not stationary.

us_emp_TP %>% features(Employed_bc, unitroot_nsdiffs)

us_emp_TP %>% features(Employed_bc %>% difference(lag = 12), unitroot_ndiffs)


us_emp_TP %>% autoplot(difference(Employed_bc, lag = 12))

# 1 seasonal difference.

us_emp_TP <- us_emp_TP %>%
  mutate(Employed_bc_sdiff = difference(Employed_bc, lag = 12))


#*** 10.d ----
us_emp_TP %>% gg_tsdisplay(Employed_bc_sdiff, plot_type = "partial", lag_max = 36)

fit <- us_emp_TP %>%
  model(
    arima = ARIMA(box_cox(Employed, lambda)),
    arima_100010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0)),
    arima_200010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0)),
    arima_300010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0)),
    arima_400010 = ARIMA(box_cox(Employed, lambda) ~ pdq(1,0,0) + PDQ(0,1,0))
  )

fit %>% pivot_longer(everything())

fit %>% glance() %>% arrange(AICc)

# arima model is the best.


#*** 10.e ----
fit %>%
  select(arima) %>%
  gg_tsresiduals()

# The arima model has residuals which resemble white noise.
# However, there is a spike at lag 5, 7 and 18.


fit %>%
  select(arima_400010) %>%
  gg_tsresiduals()

# The next best model has spikes at multiple lags.


#*** 10.f ----
fit %>%
  select(arima) %>%
  forecast(h = "3 years") %>%
  hilo(level = 95)

fit %>%
  select(arima) %>%
  forecast(h = "3 years") %>%
  autoplot(us_emp_TP %>% filter(year(Month) >= 2018))

# The mean forecasted values are close to the actual values, and are in the range.
# Due to Covid 19 (black swan event), the data starting from Mar 2020 is different from the forecasted values.


#*** 10.g ----
fit %>%
  select(arima) %>%
  forecast(h = "10 years") %>%
  autoplot(us_emp_TP %>% filter(year(Month) >= 2018))

# Due to the black swan event, it is not possible to check the forecast for long ranges.
# 2-3 years is a decent range, especially as this gives a decent near forecast.
# If the historical pattern is consistant and is expected to continue in the future then even 5-6 years might be considered.


#** 11 ----
#*** 11.a ----
aus_production %>% autoplot(Electricity)
aus_production %>% model(STL(Electricity)) %>% components() %>% autoplot()
# There is changing variation in the seasonal pattern, so transformation is required.

lambda <- aus_production %>%
  features(Electricity, guerrero) %>%
  pull(lambda_guerrero)

aus_electricity <- aus_production %>%
  select(Quarter, Electricity) %>%
  mutate(Electricity_bc = box_cox(Electricity, lambda))

aus_electricity %>% autoplot(Electricity_bc)
aus_electricity %>% model(STL(Electricity_bc)) %>% components() %>% autoplot()


#*** 11.b ----
# The data is not stationary as it has trend and seasonality.

aus_electricity %>% features(Electricity_bc, unitroot_nsdiffs)
aus_electricity %>% features(Electricity_bc %>% difference(lag = 4), unitroot_ndiffs)

aus_electricity <- aus_electricity %>%
  mutate(Electricity_bc_ddiff = Electricity_bc %>% difference(lag = 3) %>% difference(lag = 1))

aus_electricity %>% autoplot(Electricity_bc_ddiff)

aus_electricity %>%
  filter(!is.na(Electricity_bc_ddiff)) %>%
  model(STL(Electricity_bc_ddiff)) %>%
  components() %>% autoplot()


#*** 11.c ----
aus_electricity %>%
  gg_tsdisplay(Electricity_bc_ddiff, plot_type = "partial", lag_max = 36)

# Possible Model = (1,1,1)(1,1,1)[4]

fit <- aus_electricity %>%
  model(
    arima = ARIMA(box_cox(Electricity, lambda)),
    arima_111111 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(1,1,1)),
    arima_111011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(0,1,1)),
    arima_011111 = ARIMA(box_cox(Electricity, lambda) ~ pdq(0,1,1) + PDQ(1,1,1)),
    arima_011011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima_111110 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(1,1,0)),
    arima_110111 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,0) + PDQ(1,1,1)),
    arima_110110 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,0) + PDQ(1,1,0)),
    arima_110112 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,0) + PDQ(1,1,2)),
    arima_112011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,2) + PDQ(0,1,1))
  )

fit %>% pivot_longer(everything())

fit %>% glance() %>% arrange(AIC)
# The best model is 111011.


#*** 11.d ----
fit %>%
  select(arima_111011) %>%
  gg_tsresiduals(lag = 36)

# The residuals resemble white noise. There are spikes at lag 6 and 22 but they are far back that they can be ignored.


#*** 11.e ----
fit %>%
  select(arima_111011) %>%
  forecast(h = "24 months") %>%
  autoplot(aus_electricity %>% filter(year(Quarter) >= 2000))


#*** 11.f ----
aus_electricity %>%
  model(
    arima_111011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(0,1,1)),
    ETS = ETS(box_cox(Electricity, lambda))
  ) %>%
  forecast(h = "24 months") %>%
  autoplot(aus_electricity %>% filter(year(Quarter) >= 2000))

# The arima model shows a slightly damped trend compared to the ETS model.


#** 12 ----
aus_electricity %>%
  model(
    arima_111011 = ARIMA(box_cox(Electricity, lambda) ~ pdq(1,1,1) + PDQ(0,1,1)),
    ETS = ETS(box_cox(Electricity, lambda)),
    arima_bc_sa = decomposition_model(
      STL(box_cox(Electricity, lambda)),
      ARIMA(season_adjust)
    ),
    ETS_bc_sa = decomposition_model(
      STL(box_cox(Electricity, lambda)),
      ETS(season_adjust)
    ),
    arima_sa = decomposition_model(
      STL(Electricity),
      ARIMA(season_adjust)
    ),
    ETS_sa = decomposition_model(
      STL(Electricity),
      ETS(season_adjust)
    )
  ) %>%
  forecast(h = "24 months") %>%
  autoplot(aus_electricity %>% filter(year(Quarter) >= 2000), level = NULL)

# Except arima_bc_sa and ETS_sa the remaining models gives similar forecasts and all look reasonable.
# Non seasonally adjusted models seems to give a slightly better result, although either approach can work.


#** 13 ----
#*** 13.a ----
fit <- tourism %>%
  filter(Region %in% c("Snowy Mountains", "Melbourne")) %>%
  model(ARIMA(Trips))


#*** 13.b ----
fc <- fit %>% forecast(h = 12)


#*** 13.c ----
fc %>%
  autoplot(tourism) +
  facet_grid(Region ~ Purpose, scales = "free_y")

# Snowy Mountains - Business, Other & Visiting have naive forecasts.
# Melbourne - Holiday & Other have MA(1) models and the forecasts are similar to naive forecasts.
# These do not capture any seasonal fluctuations, but the forecasts ranges look reasonable.

# The other forecasts look reasonable.


#** 14 ----
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

# 1 seasonal difference after Box-Cox transformation.

myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)


#*** 14.a ----
fit <- myseries_train %>%
  model(
    Arima = ARIMA(box_cox(Turnover, lambda)),
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )


#*** 14.b ----
fc <- fit %>% forecast(new_data = myseries_test)

fc %>% autoplot(myseries, level = NULL)

fc %>% accuracy(myseries) %>% arrange(RMSE)


# ETS AM model is better than ARIMA model.
# However, ARIMA is much better than the AdM and SNaive models.


#*** 14.c ----
fc <- fit %>% forecast(h = "10 years")

fc %>%
  hilo(level = 95) %>%
  select(.model, Month, .mean, "95%") %>%
  filter(year(Month) == 2019)

# The RMSE for year 2019 = 115.6, which is higher than for test set (87).
# The forecasts are all within the 95% confidence interval.
# However, considering that the model has built using data till 2010, this error is reasonable and it shows that the model is still good even after nearly a decade.


#** 15 ----
#*** 15.a ----
pelt %>% autoplot(Hare)


#*** 15.b ----
# Model - ARIMA(4,0,0)


#*** 15.c ----
pelt %>% gg_tsdisplay(Hare, plot_type = "partial")

# The ACF plot is sinusoidal and the PACF has spike at lag 4 but none after.
# Therefore the plot is correct.


#*** 15.d ----
# c=30993, ϕ1=0.82, ϕ2=−0.29, ϕ3=−0.01, and ϕ4=−0.22
# ε[t] = 0 (while forecasting future errors are replaced with 0).


# y[t]  = c + ϕ[1]y[t−1] + ϕ[2]y[t−2] + ϕ[3]y[t−3] + ϕ[4]y[t−4] + ε[t]
#       = 30993 + 0.82y[t−1] − 0.29y[t−2] − 0.01y[t−3] − 0.22y[t−4]

# y[1936] = 30993 + 0.82y[1935] − 0.29y[1934] − 0.01y[1933] − 0.22y[1932]
#         = 30993 + 0.82*15760 − 0.29*81660 − 0.01*89760 − 0.22*82110
#         = 1273

# Similarly 1937 = 6903, 1938 = 18161, 1939 = 40403


#*** 15.e ----
pelt %>%
  model(ARIMA(Hare ~ pdq(4,0,0))) %>%
  report()

# The forecasted numbers are slightly different as the manually calculated numbers are using rounded off coefficients rather than the actuals ones.
# Also, as the future forecasted numbers are based on the past forecasted values, the difference in the past values get compounded as the future values are calculated.


#** 16 ----
popu_switz <- global_economy %>%
  filter(Country == "Switzerland") %>%
  select(Year, Population) %>%
  mutate(Population = Population/1e6)


#*** 16.a ----
popu_switz %>% autoplot(Population)


#*** 16.b ----
# Model - ARIMA(3,1,0)


#*** 16.c ----
popu_switz %>% gg_tsdisplay(Population %>% difference(), plot_type = "partial")

# ACF plot is sinusoidal and the PACF has a spike at lag 3 and none after.
# Therefore the model is appropriate.


#*** 16.d ----
# c=0.0053, ϕ1=1.64, ϕ2=−1.17, and ϕ3=0.45

# y[t]  = c + y[t−1] + ϕ[1](y[t−1] − y[t−2]) + ϕ[2](y[t−2] − y[t−3]) + ϕ[3](y[t−3] − y[t−4]) + ε[t]
#       = 0.0053 + y[t−1] + 1.64*(y[t−1] − y[t−2]) − 1.17(y[t−2] − y[t−3]) + 0.45(y[t−3] − y[t−4])

# y[2018] = 0.0053 + y[2017] + 1.64*(y[2017] − y[2016]) − 1.17(y[2016] − y[2015]) + 0.45(y[2015] − y[2014])
#         = 0.0053 + 8.47 + 1.64*(8.47 − 8.37) − 1.17(8.37 − 8.28) + 0.45(8.28 − 8.19)
#         = 8.5745

# Similarly 2019 = 8.6747, 2020 = 8.7670


#*** 16.e ----
popu_switz %>%
  model(ARIMA(Population ~ pdq(3,1,0))) %>%
  forecast(h = 3) %>%
  hilo() %>%
  select(Year, .mean, "95%")

# 2018 = 8.5585, 2019 = 8.6475, 2020 = 8.7317

# The forecasted numbers are slightly lower as the manually calculated numbers are using rounded off coefficients rather than the actual ones.
# Also, as the future forecasted numbers are based on the past forecasted values, the differences in the past values get compounded as the future values are calculated.


#** 17 ----
#*** 17.a ----
y <- as_tsibble(Quandl::Quandl("PSE/ANNINC992I_PALL_IN"))

y <- y %>% mutate(Date = year(Date))


#*** 17.b ----
y %>% autoplot(Value)
# This is annual data, with increasing trend, which exponentially increases after 2000.
# Log transform might help stabalize the series.
# Also, first difference might be required.

y %>% autoplot(log(Value))
y %>% autoplot(log(Value) %>% difference())
# There is a slight trend still visible, so a second difference might be required.

y %>% features(log(Value), unitroot_ndiffs)

y %>%
  gg_tsdisplay(log(Value) %>% difference(differences = 2), plot_type = "partial")

# The PACF has a spike at lag 2 followed by a small spike at lag 5 and 8. A MA(2) might work.
# The ACF has a spike at lag 1 followed by a spike at lag 9. An AR(1) might work.

# Model - ARIMA(1,2,2)


#*** 17.c ----
fit <- y %>% model(ARIMA(log(Value) ~ pdq(1,2,2)))

fit %>% gg_tsresiduals()
# The residuals look like white noise.

augment(fit) %>% features(.innov, ljung_box, lag = 10, dof = 3)
# The test returns a large p-value proving that the residuals are white noise.


#*** 17.d ----
fit %>%
  forecast(h = 4) %>%
  autoplot(y)

# The forecast looks reasonable.


#*** 17.e ----
fit <- y %>% model(ETS(log(Value)))


#*** 17.f ----
fit %>% gg_tsresiduals()
# The residuals look like white noise.

augment(fit) %>% features(.innov, ljung_box, lag = 10, dof = 2)
# The test returns a large p-value proving that the residuals are white noise.


#*** 17.g ----
fit %>%
  forecast(h = 4) %>%
  autoplot(y)

# The forecast looks reasonable.


#*** 17.h ----
y %>%
  model(
    ARIMA(log(Value) ~ pdq(1,2,2)),
    ETS(log(Value))
  ) %>%
  forecast(h = 4) %>%
  autoplot(y %>% filter(Date >= 2010))

# The ETS model gives a slightly damped forecast compared to the ARIMA model, and therefore seems more reasonable.


# ____----


# Chapter 10 Dynamic regression modelsChapter 10 Dynamic regressio --------
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


# _----


#* 10.7 Exercises ----------------------------------------------------------
#** 1 ----
#*** 1.a ----
LakeHuron_ts <- LakeHuron %>%
  as_tsibble() %>%
  rename(Year = index, Level = value)


#*** 1.b ----
LakeHuron_ts %>% autoplot(Level)

fit <- LakeHuron_ts %>%
  model(ARIMA(Level ~ pdq(d = 0) + trend(knots = 1920)))

fit %>%
  augment() %>%
  pivot_longer(Level:.fitted) %>%
  autoplot(value) +
  scale_color_manual(labels = c(".fitted", "Level"), values = c("red", "blue"))


#*** 1.b ----
fit %>%
  forecast(h = "30 years") %>%
  autoplot(LakeHuron_ts)

# The extrapolated linear trend doesn't seem realistic.
# There will be annual fluctuations in the water level due to multiple factors including the amount of rainfall in the year.


#** 2 ----
souvenirs <- souvenirs %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0))


#*** 2.a ----
souvenirs_fit <- souvenirs %>%
  model(
    TSLM = TSLM(log(Sales) ~ trend() + season() + `Surfing Festival`),
    ARIMA = ARIMA(log(Sales) ~ trend() + season() + `Surfing Festival`)
  )

souvenirs_fit %>% select(TSLM) %>% coefficients()
souvenirs_fit %>% select(ARIMA) %>% coefficients()

# The parameters have changed slightly, with smaller coefficient values for the ARIMA model when compared to the TSLM model.


#*** 2.b ----
souvenirs_future_data <- new_data(souvenirs, 36) %>%
  mutate(`Surfing Festival` = if_else(year(Month) >= 1988 & month(Month) == 3, 1, 0))

souvenirs_fit %>%
  forecast(souvenirs_future_data) %>%
  autoplot(souvenirs %>% filter(Month >= yearmonth("1992 Jan")))

souvenirs_fit %>%
  forecast(souvenirs_future_data) %>%
  hilo(level = 95) %>%
  select(-"95%") %>%
  View()

# The ARIMA forecast, including the prediction interval, is similar to the TSLM model.


#*** 2.c ----
souvenirs_fit %>%
  select(ARIMA) %>%
  gg_tsresiduals()

souvenirs_fit %>%
  select(ARIMA) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 19, dof = 6)

# The residuals are white noise now.


#** 3 ----
vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(
    Temp2 = I(pmax(Temperature - 20, 0)),
    Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 25)))

fit %>% gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 9, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Temp2 = I(pmax(Temperature - 20, 0)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title = "Daily electricity demand: Victoria", y = "GW")

# To optimize the knots we can run multiple models with different knots and then compare which has the lowest AICc value.

fit <- vic_elec_daily %>%
  model(
    knot_21 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 21)),
    knot_22 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 22)),
    knot_23 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 23)),
    knot_24 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 24)),
    knot_25 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 25)),
    knot_26 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 26)),
    knot_27 = ARIMA(Demand ~ Temperature + Temp2 + (Day_Type == "Weekday") + trend(knots = 27))
  )

fit %>% glance() %>% arrange(AICc)

# Although the AICc values for all the models are nearly the same, the value for Knot 23 is the lowest, and is therefore the optimum value.


#** 4 ----
#*** 4.a ----
aus_accommodation <- aus_accommodation %>%
  mutate(Takings_CPI_adj = Takings / CPI * 100)

aus_accommodation %>%
  autoplot(Takings_CPI_adj) +
  facet_wrap(vars(State), scales = "free_y") +
  labs(x = "Quarter", y = "CPI adjusted Takings") +
  theme(legend.position = "none")


#*** 4.b ----
fit <- aus_accommodation %>%
  model(Takings_CPI_adj_ARIMA = ARIMA(Takings_CPI_adj ~ trend(knots = yearquarter("2008 Q1")) + season()))


#*** 4.c ----
for (i in fit$State) {
  fit %>%
    filter(State == i) %>%
    gg_tsresiduals() %>%
    print()
}

# The residuals for all models look like white noise.


#*** 4.d ----
#**** 4.d.1 ----
# Method 1: Forecasting mmodel in 4.b and CPI then combining to get Takings forecast.

future_data <- new_data(aus_accommodation, n = 18)

fit2 <- aus_accommodation %>%
  model(CPI_ARIMA = ARIMA(CPI))

fc2 <- fit2 %>%
  forecast(future_data) %>%
  rename(CPI_fc = .mean, CPI_model = .model)

fc <- fit %>%
  forecast(future_data) %>%
  rename(Takings_adj_fc = .mean, Takings_adj_model = .model)


# Combining done using below method.
# https://community.thinkwisesoftware.com/news-updates-21/time-series-forecasting-using-r-with-fable-651
fc_final <- fc %>%
  left_join(fc2, by = c("State", "Date")) %>%
  mutate(Takings = Takings_adj_fc * CPI_fc / 100) %>%
  select(State, Date, Takings) %>%
  mutate(
    .dist = distributional::dist_normal(
      mu = Takings,
      sigma = sqrt(
        purrr::map_dbl(fc$Takings_CPI_adj, function(.x) .x$sigma) +
        purrr::map_dbl(fc2$CPI, function(.x) .x$sigma)
      )
    ),
    .model = "Takings"
  ) %>%
  update_tsibble(key = c(State, .model)) %>%
  as_fable(response = .$Takings, distribution = .dist)

aus_accommodation %>%
  ggplot(aes(x = Date, y = Takings), scales = "free_y") +
  autolayer(fc_final %>% hilo(), .vars = Takings) +
  geom_line(aes(color = State)) +
  geom_line(aes(y = Takings), data = fc_final %>% hilo()) +
  geom_line(aes(y = `95%_lower`,alpha = 0.6), color = "lightblue",  data = fc_final %>% hilo() %>% unpack_hilo(c(`80%`, `95%`))) +
  geom_line(aes(y = `95%_upper`, alpha = 0.6), color = "lightblue", data = fc_final %>% hilo() %>% unpack_hilo(c(`80%`, `95%`))) +
  facet_wrap(vars(State), scales = "free_y") +
  theme(legend.position = "none")

# Note: Not confident about the method, although looking at the forecasts they look reasonable.


#**** 4.d.2 ----
# Method 2: Forecast Takings directly as a function of CPI.
#   This is how the author is forecasted using earlier packages.
#   https://robjhyndman.com/nyc2018/Day3.html
fit <- aus_accommodation %>%
  model(ARIMA(Takings ~ CPI + trend(knots = yearquarter("2008 Q1")) + season()))

future_data <- new_data(aus_accommodation, n = 18) %>%
  mutate(CPI = fc2$CPI_fc)

fc <- fit %>% forecast(future_data)

fc %>%
  autoplot(aus_accommodation) +
  facet_wrap(vars(State), scales = "free_y")

# Using CPI as a predictor, the forecast for Takings is similar to the previous forecast for most  States, but with much wider confidence interval.
#   Since Takings was adjusted using CPI in the previous forecast (resulting in a simpler trend) and the prediction was manually calculated, it is possible both reasons have resulted in a narrower prediction interval.

# Western Australia has a slightly different forecast.


#** 5 ----
#*** 5.a ----
us_gasoline %>%
  # filter(between(year(Week), 1999, 2009)) %>%
  filter(year(Week) >= 2009) %>%
  autoplot(Barrels)

fit <- us_gasoline %>%
  model(
    K_5 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 5)),
    K_10 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 10)),
    K_15 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 15)),
    K_20 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 20)),
    K_26 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 26)),
    K_11 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 11)),
    K_12 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 12)),
    K_13 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 13)),
    K_14 = TSLM(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 14))
  )

glance(fit) %>%
  select(.model, AIC:CV) %>%
  arrange(AICc, CV)

# The best mode with the lowest AICc and CV value is K_12.


#*** 5.b ----
fit <- us_gasoline %>%
  model(
    K_10 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 10) + PDQ(0,0,0)),
    K_15 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 15) + PDQ(0,0,0)),
    K_11 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 11) + PDQ(0,0,0)),
    K_12 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 12) + PDQ(0,0,0)),
    K_13 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 13) + PDQ(0,0,0)),
    K_14 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 14) + PDQ(0,0,0))
  )

glance(fit) %>%
  arrange(AICc)

# The best mode with the lowest AICc value is K_12.


#*** 5.c ----
fit %>%
  select(K_12) %>%
  gg_tsresiduals(lag = 105)

# lag
min(2*52, 1355/5)   # 104

# dof
fit %>%
  select(K_12) %>%
  coefficients() %>%
  # select(term) %>% print(n = 30)
  nrow()


fit %>%
  select(K_12) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 104, dof = 34)


# Although the ACF plot looks similar to white noise, ljung_box shows that the residuals are not white noise.

# Rerunning above test after creating new model by removing first few years of data.
fit <- us_gasoline %>%
  filter(year(Week) >= 2000) %>%
  model(K_12 = ARIMA(Barrels ~ trend(knots = c(yearweek("2007 W33"), yearweek("2014 W11"))) + fourier(K = 12) + PDQ(0,0,0), stepwise = FALSE, approximation = FALSE))

# Even though the model fails the ljung_box test, the p-value is close to 0.05 and the residuals graph looks sufficiently like white noise, and therefore choosing this model (<LM w/ ARIMA(2,0,4) errors>).


#*** 5.d ----
fit %>%
  forecast(h = "1 year") %>%
  autoplot(us_gasoline)


#** 6 ----
#*** 6.a ----
# Model - (0,1,1)(2,1,0)[12]


#*** 6.b ----
# β1 = 0.0077 increase in the electricity demand for each unit increase in heating degrees.
# β2 = 0.0208 increase in the electricity demand for each unit increase in cooling degrees.


#*** 6.c ----
# log(y[t]) = 0.0077*√x[1,t] + 0.0208*√x[2,t] + η[t]

# (1 − Φ[1]B[^12] − Φ[2]B[^24])(1 − B)(1 − B[^12]) η[t] = (1 + θ[1]B) ε[t]
# (1 − Φ[1]B[^12] − Φ[2]B[^24] − B + Φ[1]B[^13] + Φ[2]B[^25])(1 − B[^12]) η[t] = (1 + θ[1]B) ε[t]
# (1 − Φ[1]B[^12] − Φ[2]B[^24] − B + Φ[1]B[^13] + Φ[2]B[^25] − B[^12] + Φ[1]B[^24] + Φ[2]B[^36] + B[^13] - Φ[1]B[^25] - Φ[2]B[^37]) η[t] = (1 + θ[1]B) ε[t]
# (1 − B − B[^12](Φ[1] + 1) - B[^24](Φ[2] - Φ[1]) + B[^13](Φ[1] + 1) + B[^25](Φ[2] - Φ[1]) + Φ[2]B[^36] - Φ[2]B[^37]) η[t] = (1 + θ[1]B) ε[t]
# (1 − B - (Φ[1] + 1)(B[^12] -  B[^13]) - (Φ[2] - Φ[1])(B[^24] - B[^25]) + Φ[2](B[^36] - B[^37])) η[t] = (1 + θ[1]B) ε[t]
# η[t] = η[t-1] + (Φ[1] + 1)(η[t-12] -  η[t-13]) + (Φ[2] - Φ[1])(η[t-24] - η[t-25]) - Φ[2](η[t-36] - η[t-37]) + θ[1]ε[t-1] + ε[t]

# η[t] = η[t-1] + (1 - 0.5373)(η[t-12] -  η[t-13]) + (-0.4667 + 0.5373)(η[t-24] - η[t-25]) + 0.4667(η[t-36] - η[t-37]) - 0.5830ε[t-1] + ε[t]
# η[t] = η[t-1] + 0.4627(η[t-12] -  η[t-13]) + 0.0706(η[t-24] - η[t-25]) + 0.4667(η[t-36] - η[t-37]) - 0.5830ε[t-1] + ε[t]


#*** 6.d ----
# When we estimate the parameters from the model, we need to minimise the sum of squared ε[t] values.
# If we minimise the sum of squared η[t] values instead (which is what would happen if we estimated the regression model ignoring the autocorrelations in the errors), then several problems arise.
#   1. The estimated coefficients ^β[0], …, ^β[k] are no longer the best estimates, as some information has been ignored in the calculation;
#   2. Any statistical tests associated with the model (e.g., t-tests on the coefficients) will be incorrect.
#   3. The AICc values of the fitted models are no longer a good guide as to which is the best model for forecasting.
#   4. In most cases, the p-values associated with the coefficients will be too small, and so some predictor variables will appear to be important when they are not. This is known as “spurious regression.”

# Minimising the sum of squared ε[t] values avoids these problems.


#** 7 ----
set.seed(123)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries_train <- myseries %>% filter(year(Month) <= 2010)
myseries_test <- myseries %>% anti_join(myseries_train)


#*** 7.a ----
lambda <- myseries %>% features(Turnover, guerrero) %>% pull(lambda_guerrero)

fit <- myseries_train %>%
  model(
    K_1 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 1) + PDQ(0,0,0)),
    K_2 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 2) + PDQ(0,0,0)),
    K_3 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 3) + PDQ(0,0,0)),
    K_4 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 4) + PDQ(0,0,0)),
    K_5 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 5) + PDQ(0,0,0)),
    K_6 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0))
  )

fit %>%
  glance() %>%
  arrange(AICc)

# The best model with lowest AICc is K_6.


#*** 7.b ----
fit %>%
  select(K_6) %>%
  gg_tsresiduals()

# dof
fit %>%
  select(K_6) %>%
  coefficients() %>%
  nrow()

fit %>%
  select(K_6) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24, dof = 18)

# The spikes for lags 12, 18 and 24 are huge. It also fails the ljung-box test.


fit <- myseries_train %>%
  model(K_6 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0), stepwise = FALSE, approximation = FALSE))

# Even though the model fails the ljung-box test, this is the best model selected and the residual plot and the histogram look closer to white noise than for the first model.


#*** 7.c ----
fit <- myseries_train %>%
  model(
    ARIMA_K_6_313 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0) + pdq(3,1,3)),
    ARIMA_K_6_214 = ARIMA(box_cox(Turnover, lambda) ~ fourier(K = 6) + PDQ(0,0,0) + pdq(2,1,4)),
    Arima = ARIMA(box_cox(Turnover, lambda)),
    SNaive = SNAIVE(Turnover),
    AM = ETS(Turnover ~ error() + trend("A") + season("M")),
    AdM = ETS(Turnover ~ error() + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(myseries_test)

fc %>%
  filter(.model %in% c("ARIMA_K_6_313", "ARIMA_K_6_214", "ARIMA", "AM")) %>%
  autoplot(myseries, level = NULL)

fc %>% accuracy(myseries) %>% arrange(RMSE)

# Both, AM and ARIMA models, are better than the K_6 model.


# ____----


# Chapter 11 Forecasting hierarchical and grouped time series -------------
# Time series can often be naturally disaggregated by various attributes of interest.
#   example,
#     the total number of bicycles sold by a cycling manufacturer can be disaggregated by product type such as road bikes, mountain bikes and hybrids.
#     Each of these can be disaggregated into finer categories.
#       example hybrid bikes can be divided into city, commuting, comfort, and trekking bikes; and so on.
# These categories are nested within the larger group categories, and so the collection of time series follows a hierarchical aggregation structure.
#   Therefore we refer to these as “hierarchical time series.”

# Hierarchical time series often arise due to geographic divisions.
#   example, the total bicycle sales can be disaggregated by country, then within each country by state, within each state by region, and so on down to the outlet level.

# Alternative aggregation structures arise when attributes of interest are crossed rather than nested.
#   example, the bicycle manufacturer may be interested in attributes such as frame size, gender, price range, etc.
# Such attributes do not naturally disaggregate in a unique hierarchical manner as the attributes are not nested.
#   We refer to the resulting time series of crossed attributes as “grouped time series.”

# More complex structures arise when attributes of interest are both nested and crossed.
#   example,
#     it would be natural for the bicycle manufacturer to be interested in sales by product type and also by geographic division.
#     Then both the product groupings and the geographic hierarchy are mixed together.
# We introduce alternative aggregation structures in Section 11.1.

# Forecasts are often required for all disaggregate and aggregate series, and it is natural to want the forecasts to add up in the same way as the data.
#   example, forecasts of regional sales should add up to forecasts of state sales, which should in turn add up to give a forecast for national sales.

# In this chapter we discuss forecasting large collections of time series that aggregate in some way.
# The challenge is that we require forecasts that are coherent across the entire aggregation structure.
# That is, we require forecasts to add up in a manner that is consistent with the aggregation structure of the hierarchy or group that defines the collection of time series.


# _----


#* 11.1 Hierarchical and grouped time series -------------------------------
#** Hierarchical time series ----
# Figure below shows a simple hierarchical structure.
# At the top of the hierarchy is the “Total,” the most aggregate level of the data.
# The tth observation of the Total series is denoted by y[t] for t=1,…,T.
# The Total is disaggregated into two series, which in turn are divided into three and two series respectively at the bottom level of the hierarchy.
# Below the top level, we use y[j,t] to denote the [t]th observation of the series corresponding to node j.
#   example, y[A,t] denotes the tth observation of the series corresponding to node A, y[AB,t] denotes the tth observation of the series corresponding to node AB, and so on.

# Figure 11.1: A two level hierarchical tree diagram. 
#        Total
#       /     \
#      A       B
#    / | \     /\
#   AA AB AC  BA BB

# In this small example, the total number of series in the hierarchy is n=1+2+5=8, while the number of series at the bottom level is m=5.
# Note that n>m in all hierarchies.

# For any time t, the observations at the bottom level of the hierarchy will sum to the observations of the series above.
#   example,
#     y[t] = y[AA,t] + y[AB,t] + y[AC,t] + y[BA,t] + y[BB,t],                             (11.1)
#     y[A,t] = y[AA,t] + y[AB,t] + y[AC,t]      and     y[B,t] = y[BA,t] + y[BB,t].       (11.2)
#   Substituting (11.2) into (11.1), we also get y[t] = y[A,t] + y[B,t].


#** Example: Australian tourism hierarchy ----
# Australia is divided into six states and two territories, with each one having its own government and some economic and administrative autonomy.
#   For simplicity, we refer to both states and territories as “states.”
# Each of these states can be further subdivided into regions as shown in Figure 11.2 and Table 11.1.
# In total there are 76 such regions.
# Business planners and tourism authorities are interested in forecasts for the whole of Australia, for each of the states and territories, and also for the regions.

# The tourism tsibble contains data on quarterly domestic tourism demand, measured as the number of overnight trips Australians spend away from home.
# The key variables State and Region denote the geographical areas, while a further key Purpose describes the purpose of travel.
# For now, we will ignore the purpose of travel and just consider the geographic hierarchy.
# To make the graphs and tables simpler, we will recode State to use abbreviations.

tourism <- tsibble::tourism %>%
  mutate(State = recode(State,
    `New South Wales` = "NSW",
    `Northern Territory` = "NT",
    `Queensland` = "QLD",
    `South Australia` = "SA",
    `Tasmania` = "TAS",
    `Victoria` = "VIC",
    `Western Australia` = "WA"
    )
  )

# Using the aggregate_key() function, we can create the hierarchical time series with overnight trips in regions at the bottom level of the hierarchy, aggregated to states, which are aggregated to the national total.
# A hierarchical time series corresponding to the nested structure is created using a parent/child specification.

tourism_hts <- tourism %>%
  aggregate_key(State / Region, Trips = sum(Trips))

# The new tsibble now has some additional rows corresponding to state and national aggregations for each quarter.
# Figure below shows the aggregate total overnight trips for the whole of Australia as well as the states, revealing diverse and rich dynamics.
#   example,
#     there is noticeable national growth since 2010 and for some states such as the ACT, New South Wales, Queensland, South Australia, and Victoria.
#     There seems to be a significant jump for Western Australia in 2014.

tourism_hts %>%
  filter(is_aggregated(Region)) %>%
  autoplot(Trips) +
  labs(y = "Trips ('000)", title = "Australian tourism: national and states") +
  facet_wrap(vars(State), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")


tourism %>%
  filter(State %in% c("QLD", "NT", "VIC", "TAS")) %>%
  mutate(State = fct_relevel(factor(State), c("QLD", "VIC", "NT", "TAS"))) %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) %>%
  gg_season(Trips) +
  facet_wrap(vars(State), scales = "free_y")
# filter %in% not working with aggregated data.

# The seasonal pattern of the northern states, such as Queensland and the Northern Territory, leads to peak visits in winter (corresponding to Q3) due to the tropical climate and rainy summer months.
# In contrast, the southern states tend to peak in summer (corresponding to Q1).
# This is highlighted in the seasonal plots shown in Figure above for Queensland and the Northern Territory (shown in the left column) versus the most southern states of Victoria and Tasmania (shown in the right column).


tourism_hts %>%
  filter(!is_aggregated(Region)) %>%
  autoplot(Trips) +
  labs(y = "Trips ('000)", title = "Australian tourism: by regions nested within states") +
  facet_wrap(vars(State), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

# The plots in Figure above shows data for some selected regions.
# These help us visualise the diverse regional dynamics within each state, with some series showing strong trends or seasonality, some showing contrasting seasonality, while some series appear to be just noise.


#** Grouped time series ----
# With grouped time series, the data structure does not naturally disaggregate in a unique hierarchical manner.
# Figure below shows a simple grouped structure.
#   At the top of the grouped structure is the Total, the most aggregate level of the data, again represented by y[t].
#   The Total can be disaggregated by attributes (A, B) forming series y[A,t] and y[B,t], or by attributes (X, Y) forming series y[X,t] and y[Y,t].
#   At the bottom level, the data are disaggregated by both attributes.

#        Total                 Total
#       /     \               /     \
#      A       B             X       Y
#    /  \     /\           /  \     /\
#   AX  AY  BX BY         AX  BX  AY  BY

# This example shows that there are alternative aggregation paths for grouped structures.
# For any time t, as with the hierarchical structure,
#   y[t] = y[AX,t] + y[AY,t] + y[BX,t] + y[BY,t].
# However, for the first level of the grouped structure,
#   y[A,t] = y[AX,t] + y[AY,t]      y[B,t] = y[BX,t] + y[BY,t]      (11.3)
#   but also
#   y[X,t] = y[AX,t] + y[BX,t]      y[Y,t] = y[AY,t] + y[BY,t].     (11.4)

# Grouped time series can sometimes be thought of as hierarchical time series that do not impose a unique hierarchical structure, in the sense that the order by which the series can be grouped is not unique.


#** Example: Australian prison population ----
# In this example we consider the Australia prison population data introduced in Chapter 2.
# The top panel in Figure below shows the total number of prisoners in Australia over the period 2005Q1–2016Q4.
# This represents the top-level series in the grouping structure.
#   The panels below show the prison population disaggregated or grouped by (a) state (b) legal status (whether prisoners have already been sentenced or are in remand waiting for a sentence), and (c) gender.
# The three factors are crossed, but none are nested within the others.

p1 <- prison_gts %>%
  filter(is_aggregated(Gender), is_aggregated(Legal), is_aggregated(State)) %>%
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)", title = "Prison population: Total")

p2 <- prison_gts %>%
  filter(is_aggregated(Gender) + is_aggregated(Legal) + is_aggregated(State) == 2) %>%
  as_tibble() %>%
  pivot_longer(cols = !c(Quarter, Count)) %>%
  filter(!is_aggregated(value)) %>%
  mutate(value = fct_relevel(factor(value), c("Female", "Male", "Remanded", "Sentenced"))) %>%
  as_tsibble(index = Quarter, key = c(name, value)) %>%
  autoplot(Count) +
  facet_wrap(vars(name), scales = "free_y") +
  labs(x = "Quarter", y = "Number of prisoners ('000)") +
  guides(color = guide_legend(title = "Series"))

(p1 / p2) +
  plot_layout(guides = "collect")


# The following code, introduced in Section 2.1, builds a tsibble object for the prison data.

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date)  %>%
  as_tsibble(key = c(Gender, Legal, State, Indigenous), index = Quarter) %>%
  relocate(Quarter)


# We create a grouped time series using aggregate_key() with attributes or groupings of interest now being crossed using the syntax attribute1*attribute2 (in contrast to the parent/child syntax used for hierarchical time series).
# The following code builds a grouped tsibble for the prison data with crossed attributes: gender, legal status and state.

prison_gts <- prison %>%
  aggregate_key(Gender * Legal * State, Count = sum(Count)/1e3)


# Using is_aggregated() within filter() is helpful for exploring or plotting the main groups shown in the bottom panels of Figure below.
#   example, the following code plots the total numbers of female and male prisoners across Australia.

prison_gts %>%
  filter(!is_aggregated(Gender), is_aggregated(Legal), is_aggregated(State)) %>%
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")


# Plots of other group combinations can be obtained in a similar way.
# Figure below shows the Australian prison population grouped by all possible combinations of two attributes at a time: state and gender, state and legal status, and legal status and gender.
# The following code will reproduce the first plot in Figure below.

p1 <- prison_gts %>%
  filter(!is_aggregated(Gender), !is_aggregated(Legal), !is_aggregated(State)) %>%
  mutate(Gender = as.character(Gender)) %>%
  ggplot(aes(x = Quarter, y = Count, group = Gender, colour = Gender)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by state and gender", y = "Number of prisoners ('000)") +
  facet_wrap(~ as.character(State), nrow = 1, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2 <- prison_gts %>%
  filter(!is_aggregated(Gender), !is_aggregated(Legal), !is_aggregated(State)) %>%
  mutate(`Legal status` = as.character(Legal)) %>%
  ggplot(aes(x = Quarter, y = Count, colour = `Legal status`)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by state and legal status", y = "Number of prisoners ('000)") +
  scale_color_manual(values = c("Remanded" = "purple", "Sentenced" = "orange")) +
  facet_wrap(~ as.character(State), nrow = 1, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p3 <- prison_gts %>%
  filter(!is_aggregated(Gender), !is_aggregated(Legal), is_aggregated(State)) %>%
  mutate(Gender = as.character(Gender)) %>%
  ggplot(aes(x = Quarter, y = Count, group = Gender, colour = Gender)) +
  stat_summary(fun = sum, geom = "line") +
  facet_wrap(~ as.character(Legal), nrow = 1, scales = "free_y") +
  labs(title = "Prison population by legal status and gender", x = "Quarter") +
  theme(legend.position = "none")

p1 / p2 / p3


# Figure below shows the Australian adult prison population disaggregated by all three attributes: state, legal status and gender.
# These form the bottom-level series of the grouped structure.

prison_gts %>%
  filter(!is_aggregated(Gender), !is_aggregated(Legal), !is_aggregated(State)) %>%
  mutate(
    Gender = as.character(Gender),
    Legal = as.character(Legal),
    State = as.character(State)
  ) %>%
  ggplot(aes(x = Quarter, y = Count, colour = interaction(Legal, Gender, sep = " / "))) +
  stat_summary(fun = sum, geom = "line") +
  facet_wrap(vars(State), scales = "free_y", nrow = 1) +
  labs(x = "Quarter", y  = "Number of prisoners ('000)", title = "Australian prison population: bottom-level series", color = "Legal status & Gender")


#** Mixed hierarchical and grouped structure ----
# Often disaggregating factors are both nested and crossed.
#   example,
#     the Australian tourism data can also be disaggregated by the four purposes of travel: holiday, business, visiting friends and relatives, and other.
#     This grouping variable does not nest within any of the geographical variables.
#     In fact, we could consider overnight trips split by purpose of travel for the whole of Australia, and for each state, and for each region.
#   We describe such a structure as a “nested” geographic hierarchy “crossed” with the purpose of travel.
# Using aggregate_key() this can be specified by simply combining the factors.

tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

# The tourism_full tsibble contains 425 series, including the 85 series from the hierarchical structure, as well as another 340 series obtained when each series of the hierarchical structure is crossed with the purpose of travel.

tourism_full %>%
  filter(!is_aggregated(Purpose), is_aggregated(State), is_aggregated(Region)) %>%
  mutate(Purpose = as.character(Purpose)) %>%
  ggplot(aes(x = Quarter, y = Trips, color = Purpose)) +
  geom_line() +
  facet_wrap(vars(Purpose), scales = "free_y") +
  labs(x = "Quarter", y = "Trips ('000)", title = "Australian tourism: by purpose of travel")

tourism_full %>%
  filter(!is_aggregated(Purpose), !is_aggregated(State), is_aggregated(Region)) %>%
  mutate(
    Purpose = as.character(Purpose),
    State = as.character(State),
    State = recode(State,
                   `New South Wales` = "NSW",
                   `Northern Territory` = "NT",
                   `Queensland` = "QLD",
                   `South Australia` = "SA",
                   `Tasmania` = "TAS",
                   `Victoria` = "VIC",
                   `Western Australia` = "WA"
    )
  ) %>%
  ggplot(aes(x = Quarter, y = Trips, color = Purpose)) +
  geom_line() +
  facet_wrap(vars(State), scales = "free_y", nrow = 2) +
  labs(x = "Quarter", y = "Trips ('000)", title = "Australian tourism: by purpose of travel and state")

# Figures above 2 and above show the aggregate series grouped by purpose of travel, and the series grouped by purpose of travel and state, revealing further rich and diverse dynamics across these series.


# _----


#* 11.2 Single level approaches --------------------------------------------
# Traditionally, forecasts of hierarchical or grouped time series involved selecting one level of aggregation and generating forecasts for that level.
# These are then either aggregated for higher levels, or disaggregated for lower levels, to obtain a set of coherent forecasts for the rest of the structure.


#** The bottom-up approach ----
# A simple method for generating coherent forecasts is the “bottom-up” approach.
# This approach involves first generating forecasts for each series at the bottom level, and then summing these to produce forecasts for all the series in the structure.

# example,
#   for the hierarchy of Figure 11.1, we first generate h-step-ahead forecasts for each of the bottom-level series:
#     ^y[AA,h],  ^y[AB,h],  ^y[AC,h],  ^y[BA,h]  and  ^y[BB,h].
#     (We have simplified the previously used notation of ^y[T+h|T] for brevity.)

#   Summing these, we get h-step-ahead coherent forecasts for the rest of the series:
#     ~y[h]   = ^y[AA,h] + ^y[AB,h] + ^y[AC,h] + ^y[BA,h] + ^y[BB,h],
#     ~y[A,h] = ^y[AA,h] + ^y[AB,h] + ^y[AC,h],     and
#     ~y[B,h] = ^y[BA,h] + ^y[BB,h].

# (In this chapter, we will use the “tilde” notation to indicate coherent forecasts.)

# An advantage of this approach is that we are forecasting at the bottom level of a structure, and therefore no information is lost due to aggregation.
# On the other hand, bottom-level data can be quite noisy and more challenging to model and forecast.


#** Example: Generating bottom-up forecasts ----
# Suppose we want national and state forecasts for the Australian tourism data, but we aren’t interested in disaggregations using regions or the purpose of travel.
# So we first create a simple tsibble object containing only state and national trip totals for each quarter.

tourism_states <- tourism %>%
  aggregate_key(State, Trips = sum(Trips))

# We could generate the bottom-level state forecasts first, and then sum them to obtain the national forecasts.


fcasts_state <- tourism_states %>%
  filter(!is_aggregated(State)) %>%
  model(ets = ETS(Trips)) %>%
  forecast()

# Sum bottom-level forecasts to get top-level forecasts
fcasts_national <- fcasts_state %>%
  summarise(value = sum(Trips), .mean = mean(value))


# However, we want a more general approach that will work with all the forecasting methods discussed in this chapter.
# So we will use the reconcile() function to specify how we want to compute coherent forecasts.


tourism_states %>%
  model(ets = ETS(Trips)) %>%
  reconcile(bu = bottom_up(ets)) %>%
  forecast()


# The reconcile() step has created a new “model” to produce bottom-up forecasts.
# The fable object contains the ets forecasts as well as the coherent bu forecasts, for the 8 states and the national aggregate.
# At the state level, these forecasts are identical, but the national ets forecasts will be different from the national bu forecasts.

# For bottom-up forecasting, this is rather inefficient as we are not interested in the ETS model for the national total, and the resulting fable contains a lot of duplicates.
# But later we will introduce more advanced methods where we will need models for all levels of aggregation, and where the coherent forecasts are different from any of the original forecasts.


#** Workflow for forecasting aggregation structures ----
# The above code illustrates the general workflow for hierarchical and grouped forecasts.
# We use the following pipeline of functions.

data %>% aggregate_key() %>% model() %>% reconcile() %>% forecast()

# 1. Begin with a tsibble object (here labelled data) containing the individual bottom-level series.
# 2. Define in aggregate_key() the aggregation structure and build a tsibble object that also contains the aggregate series.
# 3. Identify a model() for each series, at all levels of aggregation.
# 4. Specify in reconcile() how the coherent forecasts are to be generated from the selected models.
# 5. Use the forecast() function to generate forecasts for the whole aggregation structure.


#** Top-down approaches ----
# Top-down approaches involve first generating forecasts for the Total series y[t], and then disaggregating these down the hierarchy.

# Let p1,…,pm denote a set of disaggregation proportions which determine how the forecasts of the Total series are to be distributed to obtain forecasts for each series at the bottom level of the structure.
#   example, for the hierarchy of Figure 11.1, using proportions p1,…,p5 we get
#   ~y[AA,t] = p[1]^y[t],   ~y[AB,t] = p[2]^y[t],   ~y[AC,t] = p[3]^y[t],   ~y[BA,t] = p[4]^y[t]   and      ~y[BB,t] = p[5]^y[t].

# Once the bottom-level h-step-ahead forecasts have been generated, these are aggregated to generate coherent forecasts for the rest of the series.

# Top-down forecasts can be generated using top_down() within the reconcile() function.

# There are several possible top-down methods that can be specified.
# The two most common top-down approaches specify disaggregation proportions based on the historical proportions of the data. 


#** Average historical proportions ----
# p[j] = (1/T) [T∑t=1](y[j,t] / y[t])         for j=1,…,m.

# Each proportion pj reflects the average of the historical proportions of the bottom-level series y[j,t] over the period t=1,…,T relative to the total aggregate y[t].

# This approach is implemented in the top_down() function by setting method = "average_proportions".


#** Proportions of the historical averages ----
# p[j] = [T∑t=1]( y[j,t]/ T ) / [T∑t=1]( y[t] / T )     for j=1,…,m.
# Each proportion p[j] captures the average historical value of the bottom-level series y[j,t] relative to the average value of the total aggregate y[t].

# This approach is implemented in the top_down() function by setting method = "proportion_averages".

# A convenient attribute of such top-down approaches is their simplicity.
# One only needs to model and generate forecasts for the most aggregated top-level series.
# In general, these approaches seem to produce quite reliable forecasts for the aggregate levels and they are useful with low count data.
# On the other hand, one disadvantage is the loss of information due to aggregation.
# Using such top-down approaches, we are unable to capture and take advantage of individual series characteristics such as time dynamics, special events, different seasonal patterns, etc.


#** Forecast proportions ----
# Because historical proportions used for disaggregation do not take account of how those proportions may change over time, top-down approaches based on historical proportions tend to produce less accurate forecasts at lower levels of the hierarchy than bottom-up approaches.
# To address this issue, proportions based on forecasts rather than historical data can be used.

# Consider a one level hierarchy.
# We first generate h-step-ahead forecasts for all of the series.
#   We don’t use these forecasts directly, and they are not coherent (they don’t add up correctly).
#   Let’s call these “initial” forecasts.
# We calculate the proportion of each h-step-ahead initial forecast at the bottom level, to the aggregate of all the h-step-ahead initial forecasts at this level.
#   We refer to these as the forecast proportions,
#     and we use them to disaggregate the top-level h-step-ahead initial forecast in order to generate coherent forecasts for the whole of the hierarchy.

# For a K-level hierarchy, this process is repeated for each node, going from the top to the bottom level.
# Applying this process leads to the following general rule for obtaining the forecast proportions: 
#   p[j] = [K−1] ∏ [ℓ=0]^y^(ℓ)[j,h] ^S^(ℓ+1)[j,h]
#     where
#       j=1,2,…,m,
#       ^y^(ℓ)[j,h] is the h-step-ahead initial forecast of the series that corresponds to the node which is ℓ levels above j, and
#       ^S^(ℓ)[j,h] is the sum of the h-step-ahead initial forecasts below the node that is ℓ levels above node j and are directly connected to that node.
# These forecast proportions disaggregate the h-step-ahead initial forecast of the Total series to get h-step-ahead coherent forecasts of the bottom-level series.

# We will use the hierarchy of Figure 11.1 to explain this notation and to demonstrate how this general rule is reached.
# Assume we have generated initial forecasts for each series in the hierarchy.
# Recall that for the top-level “Total” series, ~y[h] = ^y[h], for any top-down approach.
# Here are some examples using the above notation:
#   y^(1)[A,h]   = ^y^(1)[B,h]  = ^y[h] = ~y[h];
#   ^y^(1)[AA,h] = ^y^(1)[AB,h] = ^y^(1)[AC,h] = ^y[A,h];
#   ^y^(2)[AA,h] = ^y^(2)[AB,h] = ^y^(2)[AC,h] = ^y^(2)[BA,h] = ^y(2)[BB,h] = ^y[h] = ~y[h];
#   ^S^(1)[AA,h] = ^S^(1)[AB,h] = ^S^(1)[AC,h] = ^y[AA,h] + ^y[AB,h] + ^y[AC,h];
#   ^S^(2)[AA,h] = ^S^(2)[AB,h] = ^S^(2)[AC,h] = ^S^(1)[A,h] = ^S(1)[B,h] = ^S[h] = ^y[A,h] + ^y[B,h].

# Moving down the farthest left branch of the hierarchy, coherent forecasts are given by
#   ~y[A,h]  = ( ^y[A,h] / ^S^(1)[A,h] )~y[h] = ( ^y^(1)[AA,h] / ^S^(2)[AA,h] )~y[h]     and
#   ~y[AA,h] = ( ^y[AA,h] /  ^S^(1)[AA,h] )~y[A,h] = ( ^y[AA,h] / ^S^(1)[AA,h] ) ( ^y^(1)[AA,h] / ^S^(2)[AA,h] )~y[h].
# Consequently,
#   p[1] = (^y[AA,h] / ^S^(1)[AA,h] ) (^y^(1)[AA,h] / ^S^(2)[AA,h] ).

# The other proportions can be obtained similarly.

# This approach is implemented in the top_down() function by setting method = "forecast_proportions".
# Because this approach tends to work better than other top-down methods, it is the default choice in the top_down() function when no method argument is specified.

# One disadvantage of all top-down approaches, is that they do not produce unbiased coherent forecasts even if the base forecasts are unbiased.


#** Middle-out approach ----
# The middle-out approach combines bottom-up and top-down approaches.
# Again, it can only be used for strictly hierarchical aggregation structures.

# First, a “middle” level is chosen and forecasts are generated for all the series at this level.
# For the series above the middle level, coherent forecasts are generated using the bottom-up approach by aggregating the “middle-level” forecasts upwards.
# For the series below the “middle level,” coherent forecasts are generated using a top-down approach by disaggregating the “middle level” forecasts downwards.

# This approach is implemented in the middle_out() function by specifying the appropriate middle level via the level argument and selecting the top-down approach with the method argument.


# _----


#* 11.3 Forecast reconciliation --------------------------------------------
# Warning: the rest of this chapter is more advanced and assumes a knowledge of some basic matrix algebra.


#** Matrix notation ----
# Recall that Equations (11.1) and (11.2) represent how data, that adhere to the hierarchical structure of Figure 11.1, aggregate.
# Similarly (11.3) and (11.4) represent how data, that adhere to the grouped structure of Figure 11.6, aggregate.
# These equations can be thought of as aggregation constraints or summing equalities, and can be more efficiently represented using matrix notation.

# For any aggregation structure we construct an n×m matrix S (referred to as the “summing matrix”) which dictates the way in which the bottom-level series aggregate.

# For the hierarchical structure in Figure 11.1, we can write 
#     | y[t]    |         | 1   1   1   1   1 |
#     | y[A,t]  |         | 1   1   1   0   0 |   | y[AA,t] |
#     | y[B,t]  |         | 0   0   0   1   1 |   | y[AB,t] |
#     | y[AA,t] |   =     | 1   0   0   0   0 |   | y[AC,t] |
#     | y[AB,t] |         | 0   1   0   0   0 |   | y[BA,t] |
#     | y[AC,t] |         | 0   0   1   0   0 |   | y[BB,t] |
#     | y[BA,t] |         | 0   0   0   1   0 |   
#     | y[BB,t] |         | 0   0   0   0   1 |

# or in more compact notation
#   y[t] = Sb[t],                           (11.5)
#     where
#       y[t] is an n-dimensional vector of all the observations in the hierarchy at time t,
#       S is the summing matrix, and
#       b[t] is an m-dimensional vector of all the observations in the bottom level of the hierarchy at time t.
# Note that the first row in the summing matrix S represents Equation (11.1), the second and third rows represent (11.2).
# The rows below these comprise an m-dimensional identity matrix I[m] so that each bottom-level observation on the right hand side of the equation is equal to itself on the left hand side.

# Similarly for the grouped structure of Figure 11.6 we write 
#     | y[t]    |         | 1   1   1   1 |   
#     | y[A,t]  |         | 1   1   0   0 |
#     | y[B,t]  |         | 0   0   1   1 |   | y[AX,t] |
#     | y[X,t]  |         | 1   0   1   0 |   | y[AY,t] |
#     | y[Y,t]  |         | 0   1   0   1 |   | y[BX,t] |
#     | y[AX,t] |   =     | 1   0   0   0 |   | y[BY,t] |
#     | y[AY,t] |         | 0   1   0   0 |
#     | y[BX,t] |         | 0   0   0   1 |   
#     | y[BY,t] |         | 0   0   0   0 |   

# or
#   y[t] = Sb[t],         (11.6)
#     where
#       the second and third rows of S represent Equation (11.3) and the fourth and fifth rows represent (11.4).


#** Mapping matrices ----
# This matrix notation allows us to represent all forecasting methods for hierarchical or grouped time series using a common notation.

# Suppose we forecast all series ignoring any aggregation constraints.
# We call these the base forecasts and denote them by ^y[h] where h is the forecast horizon.
# They are stacked in the same order as the data y[t].

# Then all coherent forecasting approaches for either hierarchical or grouped structures can be represented as
#   ~y[h] = SG^y[h],          (11.7)
#     where
#       G is a matrix that maps the base forecasts into the bottom level, and
#       the summing matrix S sums these up using the aggregation structure to produce a set of coherent forecasts ~y[h].
#   (Actually, some recent nonlinear reconciliation methods require a slightly more complicated equation.
#    This equation is for general linear reconciliation methods.)

# The G matrix is defined according to the approach implemented.
#   example if the bottom-up approach is used to forecast the hierarchy of Figure 11.1, then 
#         | 0   0   0   1   0   0   0   0 |
#         | 0   0   0   0   1   0   0   0 |
#   G =   | 0   0   0   0   0   1   0   0 |
#         | 0   0   0   0   0   0   1   0 |
#         | 0   0   0   0   0   0   0   1 |

# Notice that G contains two partitions.
# The first three columns zero out the base forecasts of the series above the bottom level, while the m-dimensional identity matrix picks only the base forecasts of the bottom level.
# These are then summed by the S matrix.

# If any of the top-down approaches were used then 
#         | p0   0   0   0   0   0   0   0 |
#         | p2   0   0   0   0   0   0   0 |
#   G =   | p3   0   0   0   0   0   0   0 |
#         | p4   0   0   0   0   0   0   0 |
#         | p5   0   0   0   0   0   0   0 |

# The first column includes the set of proportions that distribute the base forecasts of the top level to the bottom level.
# These are then summed up by the S matrix.
# The rest of the columns zero out the base forecasts below the highest level of aggregation.

# For a middle out approach, the G matrix will be a combination of the above two.
# Using a set of proportions, the base forecasts of some pre-chosen level will be disaggregated to the bottom level, all other base forecasts will be zeroed out, and the bottom-level forecasts will then summed up the hierarchy via the summing matrix.


#** Forecast reconciliation ----
# Equation (11.7) shows that pre-multiplying any set of base forecasts with SG will return a set of coherent forecasts.

# The traditional methods considered so far are limited in that they only use base forecasts from a single level of aggregation which have either been aggregated or disaggregated to obtain forecasts at all other levels.
# Hence, they use limited information.
# However, in general, we could use other G matrices, and then SG combines and reconciles all the base forecasts in order to produce coherent forecasts.

# In fact, we can find the optimal G matrix to give the most accurate reconciled forecasts.


#** The MinT optimal reconciliation approach ----
# Wickramasuriya et al. (2019) found a G matrix that minimises the total forecast variance of the set of coherent forecasts, leading to the MinT (Minimum Trace) optimal reconciliation approach.

# Suppose we generate coherent forecasts using Equation (11.7).
# First we want to make sure we have unbiased forecasts.
# If the base forecasts ^y[h] are unbiased, then the coherent forecasts ~y[h] will be unbiased provided SGS = S.
#   this is equivalent to SG being a projection matrix onto the m-dimensional coherent subspace for which the aggregation constraints hold.
# This provides a constraint on the matrix G.
# Interestingly, no top-down method satisfies this constraint, so all top-down approaches result in biased coherent forecasts.

# Next we need to find the errors in our forecasts.
# Wickramasuriya et al. (2019) show that the variance-covariance matrix of the h-step-ahead coherent forecast errors is given by
#   V[h] = Var[y[T+h] − ~y[h]] = SGWhG′S′
#     where W[h] = Var[(y[T+h] − ^y[h])] is the variance-covariance matrix of the corresponding base forecast errors.

# The objective is to find a matrix G that minimises the error variances of the coherent forecasts.
# These error variances are on the diagonal of the matrix V[h], and so the sum of all the error variances is given by the trace of the matrix V[h].
# Wickramasuriya et al. (2019) show that the matrix G which minimises the trace of V[h] such that SGS = S, is given by
#   G = ( S′ W[^−1][h] S )[^−1] S′ W[^−1][h].

# Therefore, the optimally reconciled forecasts are given by
#   ~y[h] = S ( S′ W[^−1][h] S )[^−1] S′ W[^−1][h] ^y[h],         (11.8)

# We refer to this as the MinT (or Minimum Trace) optimal reconciliation approach.
# MinT is implemented by min_trace() within the reconcile() function.

# To use this in practice, we need to estimate W[h], the forecast error variance of the h-step-ahead base forecasts.
# This can be difficult, and so we provide four simplifying approximations that have been shown to work well in both simulations and in practice.

# 1. Set W[h] = k[h]I for all h, where k[h] > 0.
#     Note that kh is a proportionality constant.
#     It does not need to be estimated or specified here as it gets cancelled out in (11.8).
#   This is the most simplifying assumption to make, and means that G is independent of the data, providing substantial computational savings.
#   The disadvantage, however, is that this specification does not account for the differences in scale between the levels of the structure, or for relationships between series.

# Setting W[h] = k[h]I in (11.8) gives the ordinary least squares (OLS) estimator we introduced in Section 7.9 with X = S and y = ^y.
# Hence this approach is usually referred to OLS reconciliation.
# It is implemented in min_trace() by setting method = "ols".


# 2. Set W[h] = k[h]diag(^W[1]) for all h,
#   where
#     k[h] > 0,
#     ^W[1] = (1 / T)[T∑t=1]e[t]e′[t],    and
#     e[t] is an n-dimensional vector of residuals of the models that generated the base forecasts stacked in the same order as the data.

# This specification scales the base forecasts using the variance of the residuals and it is therefore referred to as the WLS (weighted least squares) estimator using variance scaling.
# The approach is implemented in min_trace() by setting method = "wls_var".


# 3. Set W[h] = k[h]Λ for all h,
#   where
#     k[h] > 0,
#     Λ = diag(S1), and
#     1 is a unit vector of dimension m (the number of bottom-level series).

# This specification assumes that the bottom-level base forecast errors each have variance k[h] and are uncorrelated between nodes.
# Hence each element of the diagonal Λ matrix contains the number of forecast error variances contributing to each node.
# This estimator only depends on the structure of the aggregations, and not on the actual data.
#   It is therefore referred to as structural scaling.
# Applying the structural scaling specification is particularly useful in cases where residuals are not available, and so variance scaling cannot be applied;
#   example, in cases where the base forecasts are generated by judgmental forecasting (Chapter 6).
# The approach is implemented in min_trace() by setting method = "wls_struct".


# 4. Set W[h] = k[h]W[1] for all h,   where k[h] > 0.
#   Here we only assume that the error covariance matrices are proportional to each other, and we directly estimate the full one-step covariance matrix W[1].
#   The most obvious and simple way would be to use the sample covariance.
#   This is implemented in min_trace() by setting method = "mint_cov".

# However, for cases where the number of bottom-level series m is large compared to the length of the series T, this is not a good estimator.
# Instead we use a shrinkage estimator which shrinks the sample covariance to a diagonal matrix.
# This is implemented in min_trace() by setting method = "mint_shrink".

# In summary, unlike any other existing approach, the optimal reconciliation forecasts are generated using all the information available within a hierarchical or a grouped structure.
# This is important, as particular aggregation levels or groupings may reveal features of the data that are of interest to the user and are important to be modelled.
# These features may be completely hidden or not easily identifiable at other levels.

# example,
#   consider the Australian tourism data introduced in Section 11.1, where the hierarchical structure followed the geographic division of a country into states and regions.
#   Some areas will be largely summer destinations, while others may be winter destinations.
#   We saw in Figure 11.4 the contrasting seasonal patterns between the northern and the southern states.
#   These differences will be smoothed at the country level due to aggregation.


# _----


#* 11.4 Forecasting Australian domestic tourism ----------------------------
# We will compute forecasts for the Australian tourism data that was described in Section 11.1.
# We use the data up to the end of 2015 as a training set, withholding the final two years (eight quarters, 2016Q1–2017Q4) as a test set for evaluation.
# The code below demonstrates the full workflow for generating coherent forecasts using the bottom-up, OLS and MinT methods.

tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full %>%
  filter(year(Quarter) <= 2015) %>%
  model(base = ETS(Trips)) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )

# Here, fit contains the base ETS model (discussed in Chapter 8) for each series in tourism_full, along with the three methods for producing coherent forecasts as specified in the reconcile() function.

fc <- fit %>% forecast(h = "2 years")

# Passing fit into forecast() generates base and coherent forecasts across all the series in the aggregation structure.
# Figures below and below 2 plot the four point forecasts for the overnight trips for the Australian total, the states, and the purposes of travel, along with the actual observations of the test set.

fc %>%
  filter(is_aggregated(Region), is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(State), scales = "free_y")

fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(Purpose), scales = "free_y")


# To make it easier to see the differences, we have included only the last five years of the training data, and have omitted the prediction intervals.
# In most panels, the increase in overnight trips, especially in the second half of the test set, is higher than what is predicted by the point forecasts.
# This is particularly noticeable for the mainland eastern states of ACT, New South Wales, Queensland and Victoria, and across all purposes of travel.

# The accuracy of the forecasts over the test set can be evaluated using the accuracy() function.
# We summarise some results using RMSE and MASE.
# The following code generates the accuracy measures for the aggregate series shown in the first row of the table.
# Similar code is used to evaluate forecasts for other levels.

acc_T <- fc %>%
  filter(is_aggregated(State), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase)) %>%
  mutate(Level = "Total")


acc_P <- fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase)) %>%
  mutate(Level = "Purpose")


acc_S <- fc %>%
  filter(!is_aggregated(State), is_aggregated(Purpose), is_aggregated(Region)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase)) %>%
  mutate(Level = "State")


acc_R <- fc %>%
  filter(!is_aggregated(Region), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase)) %>%
  mutate(Level = "Region")


acc_B <- fc %>%
  filter(!is_aggregated(Region), !is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase)) %>%
  mutate(Level = "Bottom")


acc_A <- fc %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase)) %>%
  mutate(Level = "All series")

bind_rows(acc_T, acc_P, acc_S, acc_R, acc_B, acc_A) %>%
  relocate(Level) %>%
  pivot_wider(names_from = .model, values_from = c(rmse, mase))


# The scales of the series at different levels of aggregation are quite different, due to aggregation.
# Hence, we need to be cautious when comparing or calculating scale dependent error measures, such as the RMSE, across levels as the aggregate series will dominate.
# Therefore, we compare error measures across each level of aggregation, before providing the error measures across all the series in the bottom-row.
# Notice, that the RMSE increases as we go from the bottom level to the aggregate levels above.

# Reconciling the base forecasts using OLS and MinT results in more accurate forecasts compared to the bottom-up approach.
# This result is commonly observed in applications as reconciliation approaches use information from all levels of the structure, resulting in more accurate coherent forecasts compared to the older traditional methods which use limited information.
# Furthermore, reconciliation usually improves the incoherent base forecasts for almost all levels.


# _----


#* 11.5 Reconciled distributional forecasts --------------------------------
# So far we have only discussed the reconciliation of point forecasts.
# However, we are usually also interested in the forecast distributions so that we can compute prediction intervals.

# Panagiotelis et al. (2020) present several important results for generating reconciled probabilistic forecasts.
# We focus here on two fundamental results that are implemented in the reconcile() function.
# 1. If the base forecasts are normally distributed, i.e.,
#     ^y[h] ∼ N(^μ[h], ^Σ[h]),
#   then the reconciled forecasts are also normally distributed,
#     ~y[h] ∼ N(SG^μ[h], SG^Σ[h]G′S′).

# 2. If it is unreasonable to assume normality for the base forecasts, we can use bootstrapping.
#   Bootstrapped prediction intervals were introduced in Section 5.5.
#   The same idea can be used here.
#   We can simulate future sample paths from the model(s) that produce the base forecasts, and then reconcile these sample paths.
#   Coherent prediction intervals can be computed from the reconciled sample paths.

#   Suppose that (^y[^1][h],…,^y[^B][h]) are a set of B simulated sample paths, generated independently from the models used to produce the base forecasts.
#   Then (SG^y[^1][h],…,SG^y[^B][h]) provides a set of reconciled sample paths, from which percentiles can be calculated in order to construct coherent prediction intervals.

#   To generate bootstrapped prediction intervals in this way, we simply set bootstrap = TRUE in the forecast() function.


# _----


#* 11.6 Forecasting Australian prison population ---------------------------
# Returning to the Australian prison population data (Section 11.1), we will compare the forecasts from bottom-up and MinT methods applied to base ETS models, using a test set comprising the final two years or eight quarters 2015Q1–2016Q4 of the available data.

fit <- prison_gts %>%
  filter(year(Quarter) <= 2014) %>%
  model(base = ETS(Count)) %>%
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )

fc <- fit %>% forecast(h = 8)

fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  autoplot(prison_gts, alpha = 0.7, level = 90) +
  labs(y = "Number of prisoners ('000)", title = "Australian prison population (total)")


# Figure above shows the three sets of forecasts for the aggregate Australian prison population.
# The base and bottom-up forecasts from the ETS models seem to underestimate the trend over the test period.
# The MinT approach combines information from all the base forecasts in the aggregation structure;
#   in this case, the base forecasts at the top level are adjusted upwards.

# The MinT reconciled prediction intervals are much tighter than the base forecasts, due to MinT being based on an estimator that minimizes variances.
# The base forecast distributions are also incoherent, and therefore carry with them the extra uncertainty of the incoherency error.

# We exclude the bottom-up forecasts from the remaining plots in order to simplify the visual exploration.
# However, we do revisit their accuracy in the evaluation results presented later.

# Figures below–below 3 show the MinT and base forecasts at various levels of aggregation.
# To make it easier to see the effect, we only show the last five years of training data.
# In general, MinT adjusts the base forecasts in the direction of the test set, hence improving the forecast accuracy.
# There is no guarantee that MinT reconciled forecasts will be more accurate than the base forecasts for every series, but they will be more accurate on average.

fc %>%
  filter(
    .model %in% c("base", "MinT"),
    !is_aggregated(State), is_aggregated(Legal), is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by state)", y = "Number of prisoners ('000)") +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Figure above shows forecasts for each of the eight states.
# There is a general upward trend during the test set period across all the states.
# However, there appears to be a relatively large and sudden surge in New South Wales and Tasmania, which means the test set observations are well outside the upper bound of the forecast intervals for both these states.
# Because New South Wales is the state with the largest prison population, this surge will have a substantial impact on the total.
# In contrast, Victoria shows a substantial dip in 2015Q2–2015Q3, before returning to an upward trend.
# This dip is not captured in any of the Victorian forecasts.

p1 <- fc %>%
  filter(
    .model %in% c("base", "MinT"),
    is_aggregated(State), !is_aggregated(Legal), is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by legal status)", y = "Number of prisoners ('000)") +
  facet_wrap(vars(Legal), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2 <- fc %>%
  filter(
    .model %in% c("base", "MinT"),
    is_aggregated(State), is_aggregated(Legal), !is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by gender)", y = "Number of prisoners ('000)") +
  facet_wrap(vars(Gender), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p1 + p2 + plot_layout(guides = "collect")


fc %>%
  filter(
    .model %in% c("base", "MinT"),
    as.character(State) %in% c("NSW", "QLD", "VIC", "WA"),
    !is_aggregated(State), !is_aggregated(Legal), !is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = NULL, y = "Number of prisoners ('000)") +
  facet_wrap(~ interaction(Gender, Legal, sep = " + ", lex.order = TRUE) + State, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Figure above shows the forecasts for some selected bottom-level series of the Australian prison population.
# The four largest states are represented across the columns, with legal status and gender down the rows.
# These allow for some interesting analysis and observations that have policy implications.
# The large increase observed across the states during the 2015Q1–2016Q4 test period appears to be driven by large increases in the remand prison population.
# These increases seem to be generally missed by both forecasts.
# In contrast to the other states, for New South Wales there is also a substantial increase in the sentenced prison population.
# In particular, the increase in numbers of sentenced males in NSW contributes substantially to the rise in state and national prison numbers.

# Using the accuracy() function, we evaluate the forecast accuracy across the grouped structure.
# The code below evaluates the forecast accuracy for only the top-level national aggregate of the Australian prison population time series.
# Similar code is used for the rest of the results

acc_T <- fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Total")

acc_S <- fc %>%
  filter(!is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "State")

acc_L <- fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), !is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Legal")

acc_G <- fc %>%
  filter(is_aggregated(State), !is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Gender")

acc_B <- fc %>%
  filter(!is_aggregated(State), !is_aggregated(Gender), !is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Bottom")

acc_A <- fc %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "All series")

bind_rows(acc_T, acc_S, acc_L, acc_G, acc_B, acc_A) %>%
  pivot_wider(names_from = .model, values_from = c(mase, sspc))


# We use scaled measures because the numbers of prisoners vary substantially across the groups.
# The MASE gives a scaled measure of point-forecast accuracy (see Section 5.8), while the CRPS skill score gives a scaled measure of distributional forecast accuracy (see Section 5.9).
# A low value of MASE indicates a good forecast, while a high value of the skill score indicates a good forecast.

# The results show that the MinT reconciled forecasts improve on the accuracy of the base forecasts and are also more accurate than the bottom-up forecasts.
# As the MinT optimal reconciliation approach uses information from all levels in the structure, it generates more accurate forecasts than the traditional approaches (such as bottom-up) which use limited information.


# _----


#* 11.7 Exercises ----------------------------------------------------------
#** 1 ----
PBS2 <- PBS %>%
  aggregate_key(ATC1/ATC2 * Concession * Type, Cost = sum(Cost)/1e6)


#*** 1.a ----
PBS2 %>%
  filter(!is_aggregated(ATC1), is_aggregated(ATC2), is_aggregated(Concession), is_aggregated(Type)) %>%
  autoplot(Cost) +
  facet_wrap(vars(ATC1), scales = "free_y") +
  labs(y = "Cost (millions)", title = "Cost by ATC1") +
  theme(legend.position = "none")

# The most have increasing trend and possible seasonality.
# Z and P (post 2005) show a decreasing trend, although they too might have seasonality.


PBS2 %>%
  filter(is_aggregated(ATC1), is_aggregated(ATC2), !is_aggregated(Concession), is_aggregated(Type)) %>%
  autoplot(Cost) +
  facet_wrap(vars(Concession), scales = "free_y") +
  labs(y = "Cost (millions)", title = "Cost by Concession") +
  theme(legend.position = "none")

# Increasing trend with possible seasonality.


PBS2 %>%
  filter(is_aggregated(ATC1), is_aggregated(ATC2), is_aggregated(Concession), !is_aggregated(Type)) %>%
  autoplot(Cost) +
  facet_wrap(vars(Type), scales = "free_y") +
  labs(y = "Cost (millions)", title = "Cost by Type") +
  theme(legend.position = "none")

# Increasing trend with possible seasonality for Co-payments.
# Increasing seasonal variance but no trend for Safety Net.


PBS2 %>%
  filter(!is_aggregated(ATC1), is_aggregated(ATC2), !is_aggregated(Concession), is_aggregated(Type)) %>%
  autoplot(Cost) +
  facet_wrap(~ Concession + ATC1, scales = "free_y") +
  labs(y = "Cost (millions)", title = "Cost by ATC1 & Concession") +
  theme(legend.position = "none")

# Most show a similar pattern to the simple plots.
# Concessional P & Z follow a similar plot as ATC1 above.
#   However, General Z & P follow a more damped pattern.
# Few other ATC1's also shows a slightly different pattern between Concessional and General.


PBS2 %>%
  filter(is_aggregated(ATC1), is_aggregated(ATC2), !is_aggregated(Concession), !is_aggregated(Type)) %>%
  autoplot(Cost) +
  facet_wrap(~ Concession + Type, scales = "free_y") +
  labs(y = "Cost (millions)", title = "Cost by Type & Concession") +
  theme(legend.position = "none")

# Shows a similar trend and seasonality as the Type plot above.
# Concessional Cost has more weightage than the General cost.


PBS2 %>%
  filter(!is_aggregated(ATC1), is_aggregated(ATC2), !is_aggregated(Concession), !is_aggregated(Type)) %>%
  autoplot(Cost) +
  facet_wrap(~ interaction(ATC1, Concession, sep = " + ") + Type, scales = "free_y") +
  labs(y = "Cost (millions)", title = NULL) +
  theme(legend.position = "none")

# For the most part trend is similar to above plots.
# Z + General Co-Payments has a different plot.


# ATC2 has missing values, therefore forecasting at ATC1 level. 


#*** 1.b ----
PBS2_train <- PBS2 %>%
  filter(Month <= yearmonth("2005 Jun"))

PBS2_test <- PBS2 %>%
  filter(is_aggregated(ATC2)) %>%
  filter(Month > yearmonth("2005 Jun"))

fit <- PBS2_train %>%
  filter(is_aggregated(ATC2)) %>%
  model(
    ets = ETS(Cost),
    arima = ARIMA(Cost),
    snaive = SNAIVE(Cost)
  )

fc <- fit %>% forecast(PBS2_test)

fc %>%
  filter(!is_aggregated(ATC1), is_aggregated(ATC2), is_aggregated(Concession), is_aggregated(Type)) %>%
  autoplot(
    PBS2 %>% filter(year(Month) >= 2000),
    alpha = 0.7, level = NULL
  ) +
  labs(y = "Cost (Millions)") +
  facet_wrap(~as.character(ATC1), scales = "free_y")

# For cases where there is major deviation from the past trend (e.g. P & Z) the forecast is not reasonable for any model.


#*** 1.c ----
fit2 <- fit %>%
  reconcile(
    ets_mint = min_trace(ets),
    arima_mint = min_trace(arima),
    snaive_mint = min_trace(snaive)
  )


#*** 1.d ----
acc_T <- fc %>%
  filter(is_aggregated(ATC1), is_aggregated(Concession), is_aggregated(Type)) %>%
  accuracy(data = PBS2, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Total")

acc_AT <- fc %>%
  filter(!is_aggregated(ATC1), is_aggregated(Concession), is_aggregated(Type)) %>%
  accuracy(data = PBS2, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "ATC1")

acc_C <- fc %>%
  filter(is_aggregated(ATC1), !is_aggregated(Concession), is_aggregated(Type)) %>%
  accuracy(data = PBS2, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Concession")

acc_Ty <- fc %>%
  filter(is_aggregated(ATC1), is_aggregated(Concession), !is_aggregated(Type)) %>%
  accuracy(data = PBS2, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Type")

acc_B <- fc %>%
  filter(!is_aggregated(ATC1), !is_aggregated(Concession), !is_aggregated(Type)) %>%
  accuracy(data = PBS2, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Bottom")

acc_A <- fc %>%
  accuracy(data = PBS2, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "All")

bind_rows(acc_T, acc_AT, acc_C, acc_Ty, acc_B, acc_A) %>%
  pivot_wider(names_from = .model, values_from = c(mase, sspc))

# Both arima and ets are better than SNaive at forecasting.


#*** 1.e ----
# SNaive forecasts by repeating the last seasonal values.
# Since the data is already aggregated at higher level, SNaive and reconciliation will give the same forecasts.


#** 2 ----
tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full %>%
  filter(year(Quarter) <= 2015) %>%
  model(base = ETS(Trips)) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fc <- fit %>% forecast(h = "2 years")

acc_T <- fc %>%
  filter(is_aggregated(State), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE, ss = skill_score(CRPS))
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Total")


acc_P <- fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE, ss = skill_score(CRPS))
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Purpose")


acc_S <- fc %>%
  filter(!is_aggregated(State), is_aggregated(Purpose), is_aggregated(Region)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE, ss = skill_score(CRPS))
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "State")


acc_R <- fc %>%
  filter(!is_aggregated(Region), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE, ss = skill_score(CRPS))
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Region")


acc_B <- fc %>%
  filter(!is_aggregated(Region), !is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE, ss = skill_score(CRPS))
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "Bottom")


acc_A <- fc %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE, ss = skill_score(CRPS))
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), sspc = mean(ss)*100) %>%
  mutate(Level = "All series")

bind_rows(acc_T, acc_P, acc_S, acc_R, acc_B, acc_A) %>%
  pivot_wider(names_from = .model, values_from = c(rmse, mase, sspc))

# Overall, ols model seems the best model based on skill score.


#** 3 ----
fit <- prison_gts %>%
  filter(year(Quarter) <= 2014) %>%
  model(base = ETS(Count)) %>%
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )

fc <- fit %>% forecast(h = 8, bootstrap = TRUE)

acc_T <- fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Total")

acc_S <- fc %>%
  filter(!is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "State")

acc_L <- fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), !is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Legal")

acc_G <- fc %>%
  filter(is_aggregated(State), !is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Gender")

acc_B <- fc %>%
  filter(!is_aggregated(State), !is_aggregated(Gender), !is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "Bottom")

acc_A <- fc %>%
  accuracy(data = prison_gts, measures = list(mase = MASE, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100) %>%
  mutate(Level = "All series")

bind_rows(acc_T, acc_S, acc_L, acc_G, acc_B, acc_A) %>%
  pivot_wider(names_from = .model, values_from = c(mase, sspc))

# Using bootstrapping has reduced the values for the CRPS skill score for all levels.
# Only the Total level seems to have had the least impact.


# ____----


# Chapter 12 Advanced forecasting methods ---------------------------------
# In this chapter, we briefly discuss four more advanced forecasting methods that build on the models discussed in earlier chapters.


# _----


#* 12.1 Complex seasonality ------------------------------------------------
# So far, we have mostly considered relatively simple seasonal patterns such as quarterly and monthly data.
# However, higher frequency time series often exhibit more complicated seasonal patterns.
#   example,
#     daily data may have a weekly pattern as well as an annual pattern.
#     Hourly data usually has three types of seasonality: a daily pattern, a weekly pattern, and an annual pattern.
#     Even weekly data can be challenging to forecast as there are not a whole number of weeks in a year, so the annual pattern has a seasonal period of 365.25/7≈52.179 on average.
# Most of the methods we have considered so far are unable to deal with these seasonal complexities.

# We don’t necessarily want to include all of the possible seasonal periods in our models — just the ones that are likely to be present in the data.
#   example,
#     if we have only 180 days of data, we may ignore the annual seasonality.
#     If the data are measurements of a natural phenomenon (e.g., temperature), we can probably safely ignore any weekly seasonality.

# Figure below shows the number of calls to a North American commercial bank per 5-minute interval between 7:00am and 9:05pm each weekday over a 33 week period.
# The lower panel shows the first four weeks of the same time series.
# There is a strong daily seasonal pattern with period 169 (there are 169 5-minute intervals per day), and a weak weekly seasonal pattern with period 169×5=845.
#   (Call volumes on Mondays tend to be higher than the rest of the week.)
# If a longer series of data were available, we may also have observed an annual seasonal pattern.

p1 <- bank_calls %>%
  fill_gaps() %>%
  autoplot(Calls) +
  labs(y = "Calls", title = "Five-minute call volume to bank")

p2 <- bank_calls %>%
  fill_gaps() %>%
  filter(DateTime <= min(DateTime) + dweeks(4)) %>%
  autoplot(Calls) +
  labs(y = "Calls", title = "Five-minute call volume over 4 weeks")

p1 / p2

# Apart from the multiple seasonal periods, this series has the additional complexity of missing values between the working periods.


#** STL with multiple seasonal periods ----
# The STL() function is designed to deal with multiple seasonality.
# It will return multiple seasonal components, as well as a trend and remainder component.
# In this case, we need to re-index the tsibble to avoid the missing values, and then explicitly give the seasonal periods.

calls <- bank_calls %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, regular = TRUE)

calls %>%
  model(
    STL(sqrt(Calls) ~ season(period = 169) + season(period = 5*169), robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")


# There are two seasonal patterns shown, one for the time of day (the third panel), and one for the time of week (the fourth panel).
# To properly interpret this graph, it is important to notice the vertical scales.
# In this case, the trend and the weekly seasonality have wider bars (and therefore relatively narrower ranges) compared to the other components, because there is little trend seen in the data, and the weekly seasonality is weak.

# The decomposition can also be used in forecasting, with each of the seasonal components forecast using a seasonal naïve method, and the seasonally adjusted data forecast using ETS.

# The code is slightly more complicated than usual because we have to add back the time stamps that were lost when we re-indexed the tsibble to handle the periods of missing observations.
# The square root transformation used in the STL decomposition has ensured the forecasts remain positive.


# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(sqrt(Calls) ~ season(period = 169) + season(period = 5*169), robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

fc <- calls %>%
  model(my_dcmp_spec) %>%
  forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
  new_data(n = 7 * 24 * 60 / 5) %>%
  mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) %>%
  mutate(t = row_number() + max(calls$t)) %>%
  left_join(fc, by = "t") %>%
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times %>%
  fill_gaps() %>%
  autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
  labs(y = "Calls", title = "Five-minute call volume to bank")


#** Dynamic harmonic regression with multiple seasonal periods ----
# With multiple seasonalities, we can use Fourier terms as we did in earlier chapters (see Sections 7.4 and 10.5).
# Because there are multiple seasonalities, we need to add Fourier terms for each seasonal period.
# In this case, the seasonal periods are 169 and 845, so the Fourier terms are of the form
#   sin(2πkt/169), cos(2πkt/169), sin(2πkt/845),      and     cos(2πkt/845),    for k=1,2,….
# As usual, the fourier() function can generate these for you.

# We will fit a dynamic harmonic regression model with an ARIMA error structure.
# The total number of Fourier terms for each seasonal period could be selected to minimise the AICc.
# However, for high seasonal periods, this tends to over-estimate the number of terms required, so we will use a more subjective choice with 10 terms for the daily seasonality and 5 for the weekly seasonality.
# Again, we will use a square root transformation to ensure the forecasts and prediction intervals remain positive.
# We set D=d=0 in order to handle the non-stationarity through the regression terms, and P=Q=0 in order to handle the seasonality through the regression terms.

fit <- calls %>%
  model(
    dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 169, K = 10) +
                  fourier(period = 5*169, K = 5))
  )

fc <- fit %>% forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
  new_data(n = 7 * 24 * 60 / 5) %>%
  mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) %>%
  mutate(t = row_number() + max(calls$t)) %>%
  left_join(fc, by = "t") %>%
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times %>%
  fill_gaps() %>%
  autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
  labs(y = "Calls", title = "Five-minute call volume to bank")


# This is a large model, containing 33 parameters: 4 ARMA coefficients, 20 Fourier coefficients for period 169, and 8 Fourier coefficients for period 845.
# Not all of the Fourier terms for period 845 are used because there is some overlap with the terms of period 169 (since 845=5×169).


#** Example: Electricity demand ----
# One common application of such models is electricity demand modelling.
# Figure below shows half-hourly electricity demand (MWh) in Victoria, Australia, during 2012–2014, along with temperatures (degrees Celsius) for the same period for Melbourne (the largest city in Victoria).

vic_elec %>%
  pivot_longer(Demand:Temperature, names_to = "Series") %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line() +
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(y = "")


# Plotting electricity demand against temperature (Figure below) shows that there is a nonlinear relationship between the two, with demand increasing for low temperatures (due to heating) and increasing for high temperatures (due to cooling).

elec <- vic_elec %>%
  mutate(
    DOW = wday(Date, label = TRUE),
    Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )

elec %>%
  ggplot(aes(x=Temperature, y=Demand, col=Working_Day)) +
  geom_point(alpha = 0.6) +
  labs(x="Temperature (degrees Celsius)", y="Demand (MWh)")


# We will fit a regression model with a piecewise linear function of temperature (containing a knot at 18 degrees), and harmonic regression terms to allow for the daily seasonal pattern.
# Again, we set the orders of the Fourier terms subjectively, while using the AICc to select the order of the ARIMA errors.

fit <- elec %>%
  model(
    ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
            Temperature + Cooling + Working_Day +
            fourier(period = "day", K = 10) +
            fourier(period = "week", K = 5) +
            fourier(period = "year", K = 3))
  )


# Forecasting with such models is difficult because we require future values of the predictor variables.
# Future values of the Fourier terms are easy to compute, but future temperatures are, of course, unknown.
#   If we are only interested in forecasting up to a week ahead, we could use temperature forecasts obtain from a meteorological model.
#   Alternatively, we could use scenario forecasting (Section 6.5) and plug in possible temperature patterns.
#   In the following example, we have used a repeat of the last two days of temperatures to generate future possible demand values.


elec_newdata <- new_data(elec, 2*48) %>%
  mutate(
    Temperature = tail(elec$Temperature, 2 * 48),
    Date = lubridate::as_date(Time),
    DOW = wday(Date, label = TRUE),
    Working_Day = (Date != "2015-01-01") & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )

fc <- fit %>%
  forecast(new_data = elec_newdata)

fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(title="Half hourly electricity demand: Victoria", y = "Demand (MWh)", x = "Time [30m]")


# Although the short-term forecasts look reasonable, this is a crude model for a complicated process.
# The residuals, plotted in Figure below, demonstrate that there is a lot of information that has not been captured with this model.

fit %>% gg_tsresiduals()

# More sophisticated versions of this model which provide much better forecasts are described in Hyndman & Fan (2010) and Fan & Hyndman (2012).


# _----


#* 12.2 Prophet model ------------------------------------------------------
# A recent proposal is the Prophet model, available via the fable.prophet package.
# This model was introduced by Facebook (S. J. Taylor & Letham, 2018), originally for forecasting daily data with weekly and yearly seasonality, plus holiday effects.
# It was later extended to cover more types of seasonal data.
# It works best with time series that have strong seasonality and several seasons of historical data.

# Prophet can be considered a nonlinear regression model (Chapter 7), of the form
#   y[t] = g(t) + s(t) + h(t) + ε[t],
#   where
#     g(t) describes a piecewise-linear trend (or “growth term”),
#     s(t) describes the various seasonal patterns,
#     h(t) captures the holiday effects, and
#     ε[t] is a white noise error term.

#   The knots (or changepoints) for the piecewise-linear trend are automatically selected if not explicitly specified.
#   Optionally, a logistic function can be used to set an upper bound on the trend.
#   The seasonal component consists of Fourier terms of the relevant periods.
#   By default, order 10 is used for annual seasonality and order 3 is used for weekly seasonality.
#   Holiday effects are added as simple dummy variables.
#   The model is estimated using a Bayesian approach to allow for automatic selection of the changepoints and other model characteristics.

# We illustrate the approach using two data sets: a simple quarterly example, and then the electricity demand data described in the previous section.


#** Example: Quarterly cement production ----
# For the simple quarterly example, we will repeat the analysis from Section 9.10 in which we compared an ARIMA and ETS model, but we will add in a prophet model for comparison.

library(fable.prophet)

cement <- aus_production %>%
  filter(year(Quarter) >= 1988)

train <- cement %>%
  filter(year(Quarter) <= 2007)

fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(period = 4, order = 2, type = "multiplicative"))
  )

# Note that the seasonal term must have the period fully specified for quarterly and monthly data, as the default values assume the data are observed at least daily.

fc <- fit %>% forecast(h = "2 years 6 months")

fc %>% autoplot(cement)


# In this example, the Prophet forecasts are worse than either the ETS or ARIMA forecasts.

fc %>% accuracy(cement)


#** Example: Half-hourly electricity demand ----
# We will fit a similar model to the dynamic harmonic regression (DHR) model from the previous section, but this time using a Prophet model.
# For daily and sub-daily data, the default periods are correctly specified, so that we can simply specify the period using a character string as follows.

fit <- elec %>%
  model(
    prophet(Demand ~ Temperature + Cooling + Working_Day +
              season(period = "day", order = 10) +
              season(period = "week", order = 5) +
              season(period = "year", order = 3))
  )

fit %>%
  components() %>%
  autoplot()


# Figure above shows the trend and seasonal components of the fitted model.

# The model specification is very similar to the DHR model in the previous section, although the result is different in several important ways.
# The Prophet model adds a piecewise linear time trend which is not really appropriate here as we don’t expect the long term forecasts to continue to follow the downward linear trend at the end of the series.

# There is also substantial remaining autocorrelation in the residuals,


fit %>% gg_tsresiduals()


# As a result, the prediction intervals are probably too narrow.
fc <- fit %>%
  forecast(new_data = elec_newdata)

fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")


# Prophet has the advantage of being much faster to estimate than the DHR models we have considered previously, and it is completely automated.
# However, it rarely gives better forecast accuracy than the alternative approaches, as these two examples have illustrated.


# _----


#** 12.3 Vector autoregressions ----
# One limitation of the models that we have considered so far is that they impose a unidirectional relationship — the forecast variable is influenced by the predictor variables, but not vice versa.
# However, there are many cases where the reverse should also be allowed for — where all variables affect each other.
# In Section 10.2, the changes in personal consumption expenditure (Ct) were forecast based on the changes in personal disposable income (It).
# However, in this case a bi-directional relationship may be more suitable: an increase in It will lead to an increase in Ct and vice versa.

#   example of such a situation occurred in Australia during the Global Financial Crisis of 2008–2009.
#   The Australian government issued stimulus packages that included cash payments in December 2008, just in time for Christmas spending.
#   As a result, retailers reported strong sales and the economy was stimulated.
#   Consequently, incomes increased.

# Such feedback relationships are allowed for in the vector autoregressive (VAR) framework.
# In this framework, all variables are treated symmetrically.
# They are all modelled as if they all influence each other equally.
# In more formal terminology, all variables are now treated as “endogenous.”
# To signify this, we now change the notation and write all variables as ys: y[1,t] denotes the tth observation of variable y[1], y[2,t] denotes the tth observation of variable y[2], and so on.

# A VAR model is a generalisation of the univariate autoregressive model for forecasting a vector of time series.
#   A more flexible generalisation would be a Vector ARMA process.
#   However, the relative simplicity of VARs has led to their dominance in forecasting. 
# It comprises one equation per variable in the system.
# The right hand side of each equation includes a constant and lags of all of the variables in the system.
# To keep it simple, we will consider a two variable VAR with one lag.
# We write a 2-dimensional VAR(1) model as
#   y[1,t] = c[1] + ϕ[11,1]y[1,t−1] + ϕ[12,1]y[2,t−1] + ε[1,t]      (12.1)
#   y[2,t] = c[2] + ϕ[21,1]y[1,t−1] + ϕ[22,1]y[2,t−1] + ε[2,t],     (12.2)
#     where ε[1,t] and ε[2,t] are white noise processes that may be contemporaneously correlated.
#   The coefficient ϕ[ii,ℓ] captures the influence of the ℓth lag of variable y[i] on itself, while the coefficient ϕ[ij,ℓ] captures the influence of the ℓth lag of variable y[j] on y[i].

# If the series are stationary, we forecast them by fitting a VAR to the data directly (known as a “VAR in levels”).
# If the series are non-stationary, we take differences of the data in order to make them stationary, then fit a VAR model (known as a “VAR in differences”).
# In both cases, the models are estimated equation by equation using the principle of least squares.
# For each equation, the parameters are estimated by minimising the sum of squared e[i,t] values.

# The other possibility, which is beyond the scope of this book and therefore we do not explore here, is that the series may be non-stationary but cointegrated,
#   which means that there exists a linear combination of them that is stationary.
# In this case, a VAR specification that includes an error correction mechanism (usually referred to as a vector error correction model) should be included, and alternative estimation methods to least squares estimation should be used.

# Forecasts are generated from a VAR in a recursive manner.
# The VAR generates forecasts for each variable included in the system.
#   To illustrate the process, assume that we have fitted the 2-dimensional VAR(1) model described in Equations (12.1)–(12.2), for all observations up to time T.
#   Then the one-step-ahead forecasts are generated by
#     ^y[1,T+1|T] = ^c[1] + ^ϕ[11,1]y[1,T] + ^ϕ[12,1]y[2,T]
#     ^y[2,T+1|T] = ^c[2] + ^ϕ[21,1]y[1,T] + ^ϕ[22,1]y[2,T].
#   This is the same form as (12.1)–(12.2), except that the errors have been set to zero and parameters have been replaced with their estimates.
#   For h=2, the forecasts are given by
#     ^y[1,T+2|T] = ^c[1] + ^ϕ[11,1]^y[1,T+1|T] + ^ϕ[12,1]^y[2,T+1|T]
#     ^y[2,T+2|T] = ^c[2] + ^ϕ[21,1]^y[1,T+1|T] + ^ϕ[22,1]^y[2,T+1|T].
#   Again, this is the same form as (12.1)–(12.2), except that the errors have been set to zero, the parameters have been replaced with their estimates, and the unknown values of y[1] and y[2] have been replaced with their forecasts.
#   The process can be iterated in this manner for all future time periods.

# There are two decisions one has to make when using a VAR to forecast, namely
#   how many variables (denoted by K) and
#   how many lags (denoted by p) should be included in the system.
# The number of coefficients to be estimated in a VAR is equal to K + pK[^2] (or 1 + pK per equation).
#   example, for a VAR with K=5 variables and p=3 lags, there are 16 coefficients per equation, giving a total of 80 coefficients to be estimated.
# The more coefficients that need to be estimated, the larger the estimation error entering the forecast.

# In practice, it is usual to keep K small and include only variables that are correlated with each other, and therefore useful in forecasting each other.
# Information criteria are commonly used to select the number of lags to be included.
# Care should be taken when using the AICc as it tends to choose large numbers of lags; instead, for VAR models, we often use the BIC instead.
#   A more sophisticated version of the model is a “sparse VAR” (where many coefficients are set to zero);
#   another approach is to use “shrinkage estimation” (where coefficients are smaller).

# A criticism that VARs face is that they are atheoretical; that is, they are not built on some economic theory that imposes a theoretical structure on the equations.
# Every variable is assumed to influence every other variable in the system, which makes a direct interpretation of the estimated coefficients difficult.
# Despite this, VARs are useful in several contexts:
#   1. forecasting a collection of related variables where no explicit interpretation is required;
#   2. testing whether one variable is useful in forecasting another (the basis of Granger causality tests);
#   3. impulse response analysis, where the response of one variable to a sudden but temporary change in another variable is analysed;
#   4. forecast error variance decomposition, where the proportion of the forecast variance of each variable is attributed to the effects of the other variables.


#** Example: A VAR model for forecasting US consumption ----
fit <- us_change %>%
  model(
    aicc = VAR(vars(Consumption, Income)),
    bic = VAR(vars(Consumption, Income), ic = "bic")
  )

glance(fit)


# A VAR(5) model is selected using the AICc (the default), while a VAR(1) model is selected using the BIC.
# This is not unusual — the BIC will always select a model that has fewer parameters than the AICc model as it imposes a stronger penalty for the number of parameters.

fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()


# We see that the residuals from the VAR(1) model (bic) have significant autocorrelation for Consumption, while the VAR(5) model has effectively captured all the information in the data.

# The forecasts generated by the VAR(5) model are plotted in Figure below.

fit %>%
  select(aicc) %>%
  forecast() %>%
  autoplot(us_change %>% filter(year(Quarter) > 2010))


# _----


#* 12.4 Neural network models ----------------------------------------------
# Artificial neural networks are forecasting methods that are based on simple mathematical models of the brain.
# They allow complex nonlinear relationships between the response variable and its predictors.


#** Neural network architecture ----
# A neural network can be thought of as a network of “neurons” which are organised in layers.
# The predictors (or inputs) form the bottom layer, and the forecasts (or outputs) form the top layer.
# There may also be intermediate layers containing “hidden neurons.”

# The simplest networks contain no hidden layers and are equivalent to linear regressions.
# Figure 12.15 shows the neural network version of a linear regression with four predictors.
# The coefficients attached to these predictors are called “weights.”
# The forecasts are obtained by a linear combination of the inputs.
# The weights are selected in the neural network framework using a “learning algorithm” that minimises a “cost function” such as the MSE.
# Of course, in this simple example, we can use linear regression which is a much more efficient method of training the model.

# Once we add an intermediate layer with hidden neurons, the neural network becomes non-linear.
# A simple example is shown in Figure 12.16.

# This is known as a multilayer feed-forward network,
#   where each layer of nodes receives inputs from the previous layers.
# The outputs of the nodes in one layer are inputs to the next layer.
# The inputs to each node are combined using a weighted linear combination.
# The result is then modified by a nonlinear function before being output.
#   example,
#     the inputs into each hidden neuron in Figure 12.16 are combined linearly to give
#       z[j] = b[j] + [4∑i=1]w[i,j]x[i].
#     In the hidden layer, this is then modified using a nonlinear function such as a sigmoid,
#       s(z) = 1 / (1 + e[^−z]), to give the input for the next layer.
# This tends to reduce the effect of extreme input values, thus making the network somewhat robust to outliers.

# The parameters b[1], b[2], b[3] and w[1,1],…, w[4,3] are “learned” (or estimated) from the data.
# The values of the weights are often restricted to prevent them from becoming too large.
#   The parameter that restricts the weights is known as the “decay parameter,” and is often set to be equal to 0.1.

# The weights take random values to begin with, and these are then updated using the observed data.
# Consequently, there is an element of randomness in the predictions produced by a neural network.
# Therefore, the network is usually trained several times using different random starting points, and the results are averaged.

# The number of hidden layers, and the number of nodes in each hidden layer, must be specified in advance.
# We will consider how these can be chosen using cross-validation later in this chapter.


#** Neural network autoregression ----
# With time series data, lagged values of the time series can be used as inputs to a neural network, just as we used lagged values in a linear autoregression model (Chapter 9).
# We call this a neural network autoregression or NNAR model.

# In this book, we only consider feed-forward networks with one hidden layer, and we use the notation NNAR(p,k) to indicate there are p lagged inputs and k nodes in the hidden layer.
#   example,
#     a NNAR(9,5) model is a neural network with the last nine observations (y[t−1], y[t−2],…, y[t−9]) used as inputs for forecasting the output y[t], and with five neurons in the hidden layer.
#     A NNAR(p,0) model is equivalent to an ARIMA(p,0,0) model, but without the restrictions on the parameters to ensure stationarity.

# With seasonal data, it is useful to also add the last observed values from the same season as inputs.
#   example, an NNAR(3,1,2)[12] model has inputs y[t−1], y[t−2], y[t−3] and y[t−12], and two neurons in the hidden layer.
# More generally, an NNAR(p,P,k)m model has inputs (y[t−1], y[t−2],…, y[t−p], y[t−m], yt−2m,…,yt−Pm) and k neurons in the hidden layer.
# A NNAR(p,P,0)m model is equivalent to an ARIMA(p,0,0)(P,0,0)m model but without the restrictions on the parameters that ensure stationarity.

# The NNETAR() function fits an NNAR(p,P,k)m model.
# If the values of p and P are not specified, they are selected automatically.
#   For non-seasonal time series, the default is the optimal number of lags (according to the AIC) for a linear AR(p) model.
#   For seasonal time series, the default values are P=1 and p is chosen from the optimal linear model fitted to the seasonally adjusted data.
# If k is not specified, it is set to k=(p+P+1)/2 (rounded to the nearest integer).

# When it comes to forecasting, the network is applied iteratively.
# For forecasting one step ahead, we simply use the available historical inputs.
# For forecasting two steps ahead, we use the one-step forecast as an input, along with the historical data.
# This process proceeds until we have computed all the required forecasts.


#** Example: Sunspots ----
# The surface of the sun contains magnetic regions that appear as dark spots.
# These affect the propagation of radio waves, and so telecommunication companies like to predict sunspot activity in order to plan for any future difficulties.
# Sunspots follow a cycle of length between 9 and 14 years.
# In Figure below, forecasts from an NNAR(9,5) are shown for the next 30 years.
# We have used a square root transformation to ensure the forecasts stay positive.

sunspots <- sunspot.year %>% as_tsibble()

fit <- sunspots %>%
  model(NNETAR(sqrt(value)))
  
fc <- fit %>% forecast(h = 30)

fc %>%
  autoplot(sunspots) +
  labs(x = "Year", y = "Counts", title = "Yearly sunspots")


# Here, the last 9 observations are used as predictors, and there are 5 neurons in the hidden layer.
# The cyclicity in the data has been modelled well.
# We can also see the asymmetry of the cycles has been captured by the model, where the increasing part of the cycle is steeper than the decreasing part of the cycle.
# This is one difference between a NNAR model and a linear AR model — while linear AR models can model cyclicity, the modelled cycles are always symmetric.


#** Prediction intervals ----
# Unlike most of the methods considered in this book, neural networks are not based on a well-defined stochastic model, and so it is not straightforward to derive prediction intervals for the resultant forecasts.
# However, we can still compute prediction intervals using simulation where future sample paths are generated using bootstrapped residuals (as described in Section 5.5).

# The neural network fitted to the sunspot data can be written as
#   y[t] = f(y[t−1]) + ε[t]
#     where
#     y[t−1] = (y[t−1], y[t−2],…, y[t−9])′ is a vector containing lagged values of the series, and
#     f is a neural network with 5 hidden nodes in a single layer.
#   The error series {εt} is assumed to be homoscedastic (and possibly also normally distributed).

# We can simulate future sample paths of this model iteratively, by randomly generating a value for ε[t], either from a normal distribution, or by resampling from the historical values.
# So if ε∗[T+1] is a random draw from the distribution of errors at time T+1, then
#   y∗[T+1] = f(y[T]) + ε∗[T+1]
#     is one possible draw from the forecast distribution for y[T+1].
# Setting y∗[T+1] = (y∗[T+1],y[T],…,y[T−7])′, we can then repeat the process to get
#   y[∗T+2] = f(y[∗T+1]  )+ ε∗[T+2].

# In this way, we can iteratively simulate a future sample path.
# By repeatedly simulating sample paths, we build up knowledge of the distribution for all future values based on the fitted neural network.

# Here is a simulation of 9 possible future sample paths for the sunspot data.
# Each sample path covers the next 30 years after the observed data.

fit %>%
  generate(times = 9, h = 30) %>%
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")


# If we do this many times, we can get a good picture of the forecast distributions.
# This is how the forecast() function produces prediction intervals for NNAR models.
# The times argument in forecast() controls how many simulations are done (default 1000).
# By default, the errors are drawn from a normal distribution.
# The bootstrap argument allows the errors to be “bootstrapped” (i.e., randomly drawn from the historical errors).


# _----


#* 12.5 Bootstrapping and bagging ------------------------------------------
#** Bootstrapping time series ----
# In the preceding section, and in Section 5.5, we bootstrap the residuals of a time series in order to simulate future values of a series using a model.

# More generally, we can generate new time series that are similar to our observed series, using another type of bootstrap.

# First, the time series is transformed if necessary, and then decomposed into trend, seasonal and remainder components using STL.
# Then we obtain shuffled versions of the remainder component to get bootstrapped remainder series.
# Because there may be autocorrelation present in an STL remainder series, we cannot simply use the re-draw procedure that was described in Section 5.5.
# Instead, we use a “blocked bootstrap,”
#   where contiguous sections of the time series are selected at random and joined together.
# These bootstrapped remainder series are added to the trend and seasonal components, and the transformation is reversed to give variations on the original time series.

# Consider the quarterly cement production in Australia from 1988 Q1 to 2010 Q2.
# First we check, see Figure below that the decomposition has adequately captured the trend and seasonality, and that there is no obvious remaining signal in the remainder series.

cement <- aus_production %>%
  filter(year(Quarter) >= 1988) %>%
  select(Quarter, Cement)

cement_stl <- cement %>%
  model(stl = STL(Cement))

cement_stl %>%
  components() %>%
  autoplot()


# Now we can generate several bootstrapped versions of the data.
# Usually, generate() produces simulations of the future from a model.
# But here we want simulations for the period of the historical data.
# So we use the new_data argument to pass in the original data so that the same time periods are used for the simulated data.
# We will use a block size of 8 to cover two years of data.

cement_stl %>%
  generate(new_data = cement, times = 10, bootstrap_block_size = 8) %>%
  autoplot(.sim) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: Bootstrapped series", y="Tonnes ('000)")


#** Bagged forecasts ----
# One use for these bootstrapped time series is to improve forecast accuracy.
# If we produce forecasts from each of the additional time series, and average the resulting forecasts, we get better forecasts than if we simply forecast the original time series directly.
# This is called “bagging” which stands for “bootstrap aggregating.”

# We demonstrate the idea using the cement data.
# First, we simulate many time series that are similar to the original data, using the block-bootstrap described above.

sim <- cement_stl %>%
  generate(new_data = cement, times = 100, bootstrap_block_size = 8) %>%
  select(-.model, -Cement)

# For each of these series, we fit an ETS model.
# A different ETS model may be selected in each case, although it will most likely select the same model because the series are similar.
# However, the estimated parameters will be different, so the forecasts will be different even if the selected model is the same.
# This is a time-consuming process as there are a large number of series.

ets_forecasts <- sim %>%
  model(ets = ETS(.sim)) %>%
  forecast(h = 12)

ets_forecasts %>%
  update_tsibble(key = .rep) %>%
  autoplot(.mean) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: bootstrapped forecasts", y = "Tonnes ('000)")


# Finally, we average these forecasts for each time period to obtained the “bagged forecasts” for the original data.

bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))

cement %>%
  model(ets = ETS(Cement)) %>%
  forecast(h = 12) %>%
  autoplot(cement) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = "Cement production in Australia", y = "Tonnes ('000)")

# On average, bagging gives better forecasts than just applying ETS() directly.
# Of course, it is slower because a lot more computation is required.


# _----


#* 12.6 Exercises ----------------------------------------------------------
#** 1 ----
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


#*** 1.a ----
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


#*** 1.b ----
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


#*** 1.c ----
fc <- bind_rows(
  fit %>% forecast(h = "1 week"),
  fit2 %>% forecast(h = "1 week")
)

fc %>%
  filter(.model %in% c("dhr_K1053", "stl")) %>%
  autoplot(sth_cross_ped %>% filter(yearmonth(Date_Time) >= yearmonth("2016 Dec")))

# Both models give comparable forecasts, although the mean forecasts give lower values than the historical data.
# There is room for improving the model, as neither model is very accurate.


#** 2 ----
us_gasoline_2004 <- us_gasoline %>%
  filter(year(Week) <= 2004)

us_gasoline_2004 %>%
  filter(year(Week) <= 1995) %>%
  autoplot(Barrels)


#*** 2.a ----
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


#*** 2.b ----
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


#*** 2.c ----
# Due to presence of varying cyclicity along with the trend, the regression model is the best option.
# We can try modeling using Neural Network, as it can run non-linear regression model which might capture the cyclicity better.


#** 3 ----
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


# ____----


# Chapter 13 Some practical forecasting issues ----------------------------
# In this final chapter, we address many practical issues that arise in forecasting, and discuss some possible solutions.


# _----


#* 13.1 Weekly, daily and sub-daily data -----------------------------------
# Weekly, daily and sub-daily data can be challenging for forecasting, although for different reasons.


#** Weekly data ----
# Weekly data is difficult to work with because the seasonal period (the number of weeks in a year) is both large and non-integer.
# The average number of weeks in a year is 52.18.
#   Most of the methods we have considered require the seasonal period to be an integer.
# Even if we approximate it by 52, most of the methods will not handle such a large seasonal period efficiently.

# The simplest approach is to use an STL decomposition along with a non-seasonal method applied to the seasonally adjusted data (as discussed in Chapter 3).
#   Here is an example using weekly data on US finished motor gasoline products supplied (in millions of barrels per day) from February 1991 to May 2005.

my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)

us_gasoline %>%
  model(stl_ets = my_dcmp_spec) %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day", title = "Weekly US gasoline production")


# An alternative approach is to use a dynamic harmonic regression model, as discussed in Section 10.5.
#   In the following example, the number of Fourier terms was selected by minimising the AICc.
#     The order of the ARIMA model is also selected by minimising the AICc, although that is done within the ARIMA() function.
#   We use PDQ(0,0,0) to prevent ARIMA() trying to handle the seasonality using seasonal ARIMA components.

gas_dhr <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

gas_dhr %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day", title = "Weekly US gasoline production")


# The fitted model has 6 pairs of Fourier terms and can be written as
#   y[t] = b[t] + [6∑j=1] [α[j]sin(2πjt/52.18) + β[j]cos(2πjt/52.18)] + η[t]
#     where η[t] is an ARIMA(0,1,1) process.
# Because η[t] is non-stationary, the model is actually estimated on the differences of the variables on both sides of this equation.
# There are 12 parameters to capture the seasonality, while the total number of degrees of freedom is 14 (the other two coming from the MA parameter and the drift parameter).

# The STL approach is preferable when the seasonality changes over time.
# The dynamic harmonic regression approach is preferable if there are covariates that are useful predictors as these can be added as additional regressors.


#** Daily and sub-daily data ----
# Daily and sub-daily (such as hourly) data are challenging for a different reason — they often involve multiple seasonal patterns, and so we need to use a method that handles such complex seasonality.

# Of course, if the time series is relatively short so that only one type of seasonality is present, then it will be possible to use one of the single-seasonal methods we have discussed in previous chapters (e.g., ETS or a seasonal ARIMA model).
# But when the time series is long enough so that some of the longer seasonal periods become apparent, it will be necessary to use STL, dynamic harmonic regression or Prophet, as discussed in Section 12.1.

# However, these methods only allow for regular seasonality.
# Capturing seasonality associated with moving events such as Easter, Id, or the Chinese New Year is more difficult.
# Even with monthly data, this can be tricky as the festivals can fall in either March or April (for Easter), in January or February (for the Chinese New Year), or at any time of the year (for Id).

# The best way to deal with moving holiday effects is to include dummy variables in the model.
# This can be done within the ARIMA() or prophet() functions, for example, but not within ETS().
# In fact, prophet() has a holiday() special to easily incorporate holiday effects.


# _----


#* 13.2 Time series of counts ----------------------------------------------
# All of the methods discussed in this book assume that the data have a continuous sample space.
# But often data comes in the form of counts.
#   example, we may wish to forecast the number of customers who enter a store each day.
#   We could have 0,1,2,…, customers, but we cannot have 3.45693 customers.

# In practice, this rarely matters provided our counts are sufficiently large.
# If the minimum number of customers is at least 100, then the difference between a continuous sample space [100,∞) and the discrete sample space {100,101,102,…} has no perceivable effect on our forecasts.
# However, if our data contains small counts (0,1,2,…), then we need to use forecasting methods that are more appropriate for a sample space of non-negative integers.

# Such models are beyond the scope of this book.
# However, there is one simple method which gets used in this context, that we would like to mention.
# It is “Croston’s method,” named after its British inventor, John Croston, and first described in Croston (1972).
# Actually, this method does not properly deal with the count nature of the data either, but it is used so often, that it is worth knowing about it.

# With Croston’s method, we construct two new series from our original time series by noting which time periods contain zero values, and which periods contain non-zero values.
# Let q[i] be the ith non-zero quantity, and let a[i] be the time between q[i−1] and q[i].
# Croston’s method involves separate simple exponential smoothing forecasts on the two new series a and q.
# Because the method is usually applied to time series of demand for items, q is often called the “demand” and a the “inter-arrival time.”

# If ^q[i+1|i] and ^a[i+1|i] are the one-step forecasts of the (i+1)th demand and inter-arrival time respectively, based on data up to demand i, then Croston’s method gives
#   ^q[i+1|i] = (1 − α[q])^q[i|i−1] + α[q]q[i],     (13.1)
#   ^a[i+1|i] = (1 − α[a])^a[i|i−1] + α[a]a[i].     (13.2)
# The smoothing parameters α[a] and α[q] take values between 0 and 1.
# Let j be the time for the last observed positive observation.
# Then the h-step ahead forecast for the demand at time T+h, is given by the ratio
#   ^y[T+h|T] = ^q[j+1|j] / ^a[j+1|j].        (1)

# There are no algebraic results allowing us to compute prediction intervals for this method, because the method does not correspond to any statistical model (Shenstone & Hyndman, 2005).

# The CROSTON() function produces forecasts using Croston’s method.
# The two smoothing parameters α[a] and α[q] are estimated from the data.
#   This is different from the way Croston envisaged the method being used.
#   He would simply use α[a] = α[q] = 0.1, and set a[0] and q[0] to be equal to the first observation in each of the series.


#** Example: Pharmaceutical sales ----
# Figure below shows the numbers of scripts sold each month for immune sera and immunoglobulin products in Australia.
# The data contain small counts, with many months registering no sales at all, and only small numbers of items sold in other months.

j06 <- PBS %>%
  filter(ATC2 == "J06") %>%
  summarise(Scripts = sum(Scripts))

j06 %>% autoplot(Scripts) +
  labs(y="Number of scripts",
       title = "Sales for immune sera and immunoglobulins")


# Tables below and below 2 shows the first 10 non-zero demand values, with their corresponding inter-arrival times.

j06 %>% head(n = 12)

j06 %>%
  head(n = 12) %>%
  filter(Scripts > 0) %>%
  mutate(time = Month - lag(Month))


# In this example, the smoothing parameters are estimated to be α[a] = 0.08, α[q] = 0.71, ^q[1|0] = 4.17, and ^a[1|0] = 3.52.
# The final forecasts for the two series are ^q[T+1|T] = 2.419 and ^a[T+1|T] = 2.484.
# So the forecasts are all equal to ^y[T+h|T] = 2.419 / 2.484 = 0.974.

# In practice, fable does these calculations for you:
j06 %>%
  fill_gaps() %>%
  fill(Scripts, .direction = "down") %>%
  model(CROSTON(Scripts)) %>%
  # coefficients()
  forecast(h = 6)


# The Scripts column repeats the mean rather than provide a full distribution, because there is no underlying stochastic model.

# Forecasting models that deal more directly with the count nature of the data, and allow for a forecasting distribution, are described in Christou & Fokianos (2015).


# _----


#* 13.3 Ensuring forecasts stay within limits ------------------------------
# It is common to want forecasts to be positive, or to require them to be within some specified range [a,b].
# Both of these situations are relatively easy to handle using transformations.


#** Positive forecasts ----
# To impose a positivity constraint, we can simply work on the log scale.
#   example,
#     consider the real price of a dozen eggs (1900-1993; in cents) shown in Figure below.
#     Because of the log transformation, the forecast distributions are constrained to stay positive, and so they will become progressively more skewed as the mean decreases.

egg_prices <- prices %>% filter(!is.na(eggs))

egg_prices %>%
  model(ETS(log(eggs) ~ trend("A"))) %>%
  forecast(h = 50) %>%
  autoplot(egg_prices) +
  labs(title = "Annual egg prices", y = "$US (in cents adjusted for inflation) ")


#** Forecasts constrained to an interval ----
# To see how to handle data constrained to an interval, imagine that the egg prices were constrained to lie within a = 50 and b = 400.
# Then we can transform the data using a scaled logit transform which maps (a,b) to the whole real line:
#   y = log(x−a / b−x), where x is on the original scale and y is the transformed data.
# To reverse the transformation, we will use
#   x = ((b−a)e[^y] / 1+e[^y]) + a.
# This is not a built-in transformation, so we will need to first setup the transformation functions.

scaled_logit <- function(x, lower = 0, upper = 1) {
  log((x - lower) / (upper - x))
}

inv_scaled_logit <- function(x, lower = 0, upper = 1) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

my_scaled_logit <- new_transformation(
  scaled_logit, inv_scaled_logit)

egg_prices %>%
  model( ETS(my_scaled_logit(eggs, lower = 50, upper = 400) ~ trend("A")) ) %>%
  forecast(h = 50) %>%
  autoplot(egg_prices) +
  labs(title = "Annual egg prices", y = "$US (in cents adjusted for inflation) ")


# The bias-adjustment is automatically applied here, and the prediction intervals from these transformations have the same coverage probability as on the transformed scale, because quantiles are preserved under monotonically increasing transformations.

# The prediction intervals lie above 50 due to the transformation.
# As a result of this artificial (and unrealistic) constraint, the forecast distributions have become extremely skewed.


# _----


#* 13.4 Forecast combinations ----------------------------------------------
# An easy way to improve forecast accuracy is to use several different methods on the same time series, and to average the resulting forecasts.
#   Over 50 years ago, John Bates and Clive Granger wrote a famous paper (Bates & Granger, 1969), showing that combining forecasts often leads to better forecast accuracy.
#   Twenty years later, Clemen (1989) wrote
#     The results have been virtually unanimous: combining multiple forecasts leads to increased forecast accuracy.
#     In many cases one can make dramatic performance improvements by simply averaging the forecasts.

# While there has been considerable research on using weighted averages, or some other more complicated combination approach, using a simple average has proven hard to beat.

# Here is an example using monthly revenue from take-away food in Australia, from April 1982 to December 2018.
# We use forecasts from the following models: ETS, STL-ETS, and ARIMA; and we compare the results using the last 5 years (60 months) of observations.

auscafe <- aus_retail %>%
  filter(stringr::str_detect(Industry, "Takeaway")) %>%
  summarise(Turnover = sum(Turnover))

train <- auscafe %>%
  filter(year(Month) <= 2013)

STLF <- decomposition_model(
  STL(log(Turnover) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)

cafe_models <- train %>%
  model(
    ets = ETS(Turnover),
    stlf = STLF,
    arima = ARIMA(log(Turnover))
  ) %>%
  mutate(combination = (ets + stlf + arima) / 3)

cafe_fc <- cafe_models %>%
  forecast(h = "5 years")


# Notice that we form a combination in the mutate() function by simply taking a linear function of the estimated models.
# This very simple syntax will automatically handle the forecast distribution appropriately by taking account of the correlation between the forecast errors of the models that are included.
# However, to keep the next plot simple, we will omit the prediction intervals.

cafe_fc %>%
  autoplot(auscafe %>% filter(year(Month) > 2008), level = NULL) +
  labs(y = "$ billion", title = "Australian monthly expenditure on eating out")

cafe_fc %>%
  accuracy(auscafe) %>%
  arrange(RMSE)

# ARIMA does particularly well with this series, while the combination approach does even better (based on most measures including RMSE and MAE).
# For other data, ARIMA may be quite poor, while the combination approach is usually not far off, or better than, the best component method.


#** Forecast combination distributions ----
# The cafe_fc object contains forecast distributions, from which any prediction interval can usually be computed.
# Let’s look at the intervals for the first period.

cafe_fc %>% filter(Month == min(Month))


# The first three are a mixture of normal and transformed normal distributions.
# The package does not yet combine such diverse distributions, so the combination output is simply the mean instead.

# However, if we work with simulated sample paths, it is possible to create forecast distributions for the combination forecast as well.

cafe_futures <- cafe_models %>%
  # Generate 1000 future sample paths
  generate(h = "5 years", times = 1000) %>%
  # Compute forecast distributions from future sample paths
  as_tibble() %>%
  group_by(Month, .model) %>%
  summarise( dist = distributional::dist_sample(list(.sim)) ) %>%
  ungroup() %>%
  # Create fable object
  as_fable(index = Month, key = .model, distribution = dist, response = "Turnover")


# Forecast distributions for h=1
cafe_futures %>% filter(Month == min(Month))


# Now all four models and the combination are stored as empirical distributions, and we can plot prediction intervals for the combination forecast, as shown in Figure below.

cafe_futures %>%
  filter(.model == "combination") %>%
  autoplot(auscafe %>% filter(year(Month) > 2008)) +
  labs(y = "$ billion", title = "Australian monthly expenditure on eating out")


# To check the accuracy of the 95% prediction intervals, we can use a Winkler score (defined in Section 5.9.

cafe_futures %>%
  accuracy(auscafe, measures = interval_accuracy_measures, level = 95) %>%
  arrange(winkler)

# Lower is better, so the combination forecast is again better than any of the component models.


# _----


#* 13.5 Prediction intervals for aggregates --------------------------------
# A common problem is to forecast the aggregate of several time periods of data, using a model fitted to the disaggregated data.
#   example,
#     we may have monthly data but wish to forecast the total for the next year.
#     Or we may have weekly data, and want to forecast the total for the next four weeks.

# If the point forecasts are means, then adding them up will give a good estimate of the total.
# But prediction intervals are more tricky due to the correlations between forecast errors.

# A general solution is to use simulations.
# Here is an example using ETS models applied to Australian take-away food sales, assuming we wish to forecast the aggregate revenue in the next 12 months.

fit <- auscafe %>%
  # Fit a model to the data
  model(ETS(Turnover))

futures <- fit %>%
  # Simulate 10000 future sample paths, each of length 12
  generate(times = 10000, h = 12) %>%
  # Sum the results for each sample path
  as_tibble() %>%
  group_by(.rep) %>%
  summarise(.sim = sum(.sim)) %>%
  # Store as a distribution
  summarise(total = distributional::dist_sample(list(.sim)))


# We can compute the mean of the simulations, along with prediction intervals:

futures %>%
  mutate(
    mean = mean(total),
    pi80 = hilo(total, 80),
    pi95 = hilo(total, 95)
  )


# As expected, the mean of the simulated data is close to the sum of the individual forecasts.

forecast(fit, h = 12) %>%
  as_tibble() %>%
  summarise(total = sum(.mean))


# _----


#* 13.6 Backcasting --------------------------------------------------------
# Sometimes it is useful to “backcast” a time series — that is, forecast in reverse time.
# Although there are no in-built R functions to do this, it is easy to implement by creating a new time index.

# Suppose we want to extend our Australian takeaway to the start of 1981 (the actual data starts in April 1982).

backcasts <- auscafe %>%
  mutate(reverse_time = rev(row_number())) %>%
  update_tsibble(index = reverse_time) %>%
  model(ets = ETS(Turnover ~ season(period = 12))) %>%
  forecast(h = 15) %>%
  mutate(Month = auscafe$Month[1] - (1:15)) %>%
  as_fable(index = Month, response = "Turnover", distribution = "Turnover")

backcasts %>%
  autoplot(auscafe %>% filter(year(Month) < 1990)) +
  labs(title = "Backcasts of Australian food expenditure", y = "$ (billions)")


# Most of the work here is in re-indexing the tsibble object and then re-indexing the fable object.


# _----


#* 13.7 Very long and very short time series -------------------------------
#** Forecasting very short time series ----
# We often get asked how few data points can be used to fit a time series model.
# As with almost all sample size questions, there is no easy answer.
# It depends on the number of model parameters to be estimated and the amount of randomness in the data.
# The sample size required increases with the number of parameters to be estimated, and the amount of noise in the data.

# Some textbooks provide rules-of-thumb giving minimum sample sizes for various time series models.
# These are misleading and unsubstantiated in theory or practice.
# Further, they ignore the underlying variability of the data and often overlook the number of parameters to be estimated as well.
# There is, for example, no justification for the magic number of 30 often given as a minimum for ARIMA modelling.
# The only theoretical limit is that we need more observations than there are parameters in our forecasting model.
# However, in practice, we usually need substantially more observations than that.

# Ideally, we would test if our chosen model performs well out-of-sample compared to some simpler approaches.
# However, with short series, there is not enough data to allow some observations to be withheld for testing purposes, and even time series cross validation can be difficult to apply.
# The AICc is particularly useful here, because it is a proxy for the one-step forecast out-of-sample MSE.
# Choosing the model with the minimum AICc value allows both the number of parameters and the amount of noise to be taken into account.

# What tends to happen with short series is that the AICc suggests simple models because anything with more than one or two parameters will produce poor forecasts due to the estimation error.
# We will fit an ARIMA model to the annual series from the M3-competition with fewer than 20 observations.
# First we need to create a tsibble, containing the relevant series.

m3totsibble <- function(z) {
  bind_rows(
    as_tsibble(z$x) %>% mutate(Type = "Training"),
    as_tsibble(z$xx) %>% mutate(Type = "Test")
  ) %>%
    mutate(
      st = z$st,
      type = z$type,
      period = z$period,
      description = z$description,
      sn = z$sn,
    ) %>%
    as_tibble()
}

short <- M3 %>%
  # Mcomp::M3 %>%    # Mcomp::M3 is a mcomp class object. so using the package.
  subset("yearly") %>%
  purrr::map_dfr(m3totsibble) %>%
  group_by(sn) %>%
  mutate(n = max(row_number())) %>%
  filter(n <= 20) %>%
  ungroup() %>%
  as_tsibble(index = index, key = c(sn, period, st))


# Now we can apply an ARIMA model to each series.

short_fit <- short %>%
  model(arima = ARIMA(value))


# Of the 152 series, 21 had models with zero parameters (white noise and random walks), 86 had models with one parameter, 31 had models with two parameters, 13 had models with three parameters, and only 1 series had a model with four parameters.


#** Forecasting very long time series ---- 
# Most time series models do not work well for very long time series.
# The problem is that real data do not come from the models we use.
# When the number of observations is not large (say up to about 200) the models often work well as an approximation to whatever process generated the data.
# But eventually we will have enough data that the difference between the true process and the model starts to become more obvious.
# An additional problem is that the optimisation of the parameters becomes more time consuming because of the number of observations involved.

# What to do about these issues depends on the purpose of the model.
# A more flexible and complicated model could be used, but this still assumes that the model structure will work over the whole period of the data.
# A better approach is usually to allow the model itself to change over time.
# ETS models are designed to handle this situation by allowing the trend and seasonal terms to evolve over time.
# ARIMA models with differencing have a similar property.
# But dynamic regression models do not allow any evolution of model components.

# If we are only interested in forecasting the next few observations, one simple approach is to throw away the earliest observations and only fit a model to the most recent observations.
# Then an inflexible model can work well because there is not enough time for the relationships to change substantially.

# example,
#   we fitted a dynamic harmonic regression model to 26 years of weekly gasoline production in Section 13.1.
#   It is, perhaps, unrealistic to assume that the seasonal pattern remains the same over nearly three decades.
#   So we could simply fit a model to the most recent years instead.


# _----


#* 13.8 Forecasting on training and test sets ------------------------------
# Typically, we compute one-step forecasts on the training data (the “fitted values”) and multi-step forecasts on the test data.
# However, occasionally we may wish to compute multi-step forecasts on the training data, or one-step forecasts on the test data.


#** Multi-step forecasts on training data ----
# We normally define fitted values to be one-step forecasts on the training set (see Section 5.3), but a similar idea can be used for multi-step forecasts.
# We will illustrate the method using an ARIMA model for the Australian take-away food expenditure.
# The last five years are used for a test set, and the forecasts are plotted in Figure below.

training <- auscafe %>% filter(year(Month) <= 2013)

test <- auscafe %>% filter(year(Month) > 2013)

cafe_fit <- training %>%
  model(ARIMA(log(Turnover)))

cafe_fit %>%
  forecast(h = 60) %>%
  autoplot(auscafe) +
  labs(title = "Australian food expenditure", y = "$ (billions)")


# The fitted() function has an h argument to allow for h-step “fitted values” on the training set.
# Figure below is a plot of 12-step (one year) forecasts on the training set.
# Because the model involves both seasonal (lag 12) and first (lag 1) differencing, it is not possible to compute these forecasts for the first few observations.

fits12 <- fitted(cafe_fit, h = 12)

training %>%
  autoplot(Turnover) +
  autolayer(fits12, .fitted, col = "#D55E00") +
  labs(title = "Australian food expenditure", y = "$ (billions)")


#** One-step forecasts on test data ----
# It is common practice to fit a model using training data, and then to evaluate its performance on a test data set.
# The way this is usually done means the comparisons on the test data use different forecast horizons.
# In the above example, we have used the last sixty observations for the test data, and estimated our forecasting model on the training data.
# Then the forecast errors will be for 1-step, 2-steps, …, 60-steps ahead.
# The forecast variance usually increases with the forecast horizon, so if we are simply averaging the absolute or squared errors from the test set, we are combining results with different variances.

# One solution to this issue is to obtain 1-step errors on the test data.
# That is, we still use the training data to estimate any parameters, but when we compute forecasts on the test data, we use all of the data preceding each observation (both training and test data).
# So our training data are for times 1,2,…,T−60.
# We estimate the model on these data, but then compute ^yT−60+h|T−61+h, for h=1,…,T−1.
# Because the test data are not used to estimate the parameters, this still gives us a “fair” forecast.

# Using the same ARIMA model used above, we now apply the model to the test data.

cafe_fit %>%
  refit(test) %>%
  accuracy()


# Note that model is not re-estimated in this case.
# Instead, the model obtained previously (and stored as cafe_fit) is applied to the test data.
# Because the model was not re-estimated, the “residuals” obtained here are actually one-step forecast errors.
# Consequently, the results produced from the accuracy() command are actually on the test set (despite the output saying “Training set”).
# This approach can be used to compare one-step forecasts from different models.


# _----


#* 13.9 Dealing with outliers and missing values ---------------------------
# Real data often contains missing values, outlying observations, and other messy features.
# Dealing with them can sometimes be troublesome.


#** Outliers ----
# Outliers are observations that are very different from the majority of the observations in the time series.
# They may be errors, or they may simply be unusual.
#   (See Section 7.3 for a discussion of outliers in a regression context.)
# None of the methods we have considered in this book will work well if there are extreme outliers in the data.
# In this case, we may wish to replace them with missing values, or with an estimate that is more consistent with the majority of the data.

# Simply replacing outliers without thinking about why they have occurred is a dangerous practice.
# They may provide useful information about the process that produced the data, which should be taken into account when forecasting.
# However, if we are willing to assume that the outliers are genuinely errors, or that they won’t occur in the forecasting period, then replacing them can make the forecasting task easier.

# Figure below shows the number of visitors to the Adelaide Hills region of South Australia.
# There appears to be an unusual observation in 2002 Q4.

tourism %>%
  filter( Region == "Adelaide Hills", Purpose == "Visiting" ) %>%
  autoplot(Trips) +
  labs(title = "Quarterly overnight trips to Adelaide Hills", y = "Number of trips")


# One useful way to find outliers is to apply STL() to the series with the argument robust=TRUE.
# Then any outliers should show up in the remainder series.
# The data in Figure above have almost no visible seasonality, so we will apply STL without a seasonal component by setting period=1.

ah_decomp <- tourism %>%
  filter( Region == "Adelaide Hills", Purpose == "Visiting" ) %>%
  # Fit a non-seasonal STL decomposition
  model( stl = STL(Trips ~ season(period = 1), robust = TRUE) ) %>%
  components()

ah_decomp %>% autoplot()


# In the above example the outlier was easy to identify.
# In more challenging cases using a boxplot of the remainder series would be useful.
# We can identify as outliers those that are greater than 1.5 interquartile ranges (IQRs) from the central 50% of the data.
# If the remainder was normally distributed, this would show 7 in every 1000 observations as “outliers.”
# A stricter rule is to define outliers as those that are greater than 3 interquartile ranges (IQRs) from the central 50% of the data, which would make only 1 in 500,000 normally distributed observations to be outliers.
# This is the rule we prefer to use.

outliers <- ah_decomp %>%
  filter(
    remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
    remainder > quantile(remainder, 0.75) + 3*IQR(remainder)
  )


# This finds the one outlier that we suspected from Figure above 2.
# Something similar could be applied to the full data set to identify unusual observations in other series.


#** Missing values ----
# Missing data can arise for many reasons, and it is worth considering whether the missingness will induce bias in the forecasting model.
#   example,
#     suppose we are studying sales data for a store, and missing values occur on public holidays when the store is closed.
#     The following day may have increased sales as a result.
#     If we fail to allow for this in our forecasting model, we will most likely under-estimate sales on the first day after the public holiday, but over-estimate sales on the days after that.
#     One way to deal with this kind of situation is to use a dynamic regression model, with dummy variables indicating if the day is a public holiday or the day after a public holiday.
# No automated method can handle such effects as they depend on the specific forecasting context.

# In other situations, the missingness may be essentially random.
#   example,
#     someone may have forgotten to record the sales figures, or the data recording device may have malfunctioned.
# If the timing of the missing data is not informative for the forecasting problem, then the missing values can be handled more easily.

# Finally, we might remove some unusual observations, thus creating missing values in the series.

# Some methods allow for missing values without any problems.
#   example,
#     the naïve forecasting method continues to work, with the most recent non-missing value providing the forecast for the future time periods.
#     Similarly, the other benchmark methods introduced in Section 5.2 will all produce forecasts when there are missing values present in the historical data.
#     The fable functions for ARIMA models, dynamic regression models and NNAR models will also work correctly without causing errors.
# However, other modelling functions do not handle missing values including ETS() and STL().

# When missing values cause errors, there are at least two ways to handle the problem.
#   First, we could just take the section of data after the last missing value, assuming there is a long enough series of observations to produce meaningful forecasts.
#   Alternatively, we could replace the missing values with estimates by first fitting an ARIMA model, and then using the model to interpolate the missing observations.

# We will replace the outlier identified in Figure above by an estimate using an ARIMA model.

ah_miss <- tourism %>%
  filter(
    Region == "Adelaide Hills",
    Purpose == "Visiting"
  ) %>%
  # Remove outlying observations
  anti_join(outliers) %>%
  # Replace with missing values
  fill_gaps()

ah_fill <- ah_miss %>%
  # Fit ARIMA model to the data containing missing values
  model(ARIMA(Trips)) %>%
  # Estimate Trips for all periods
  interpolate(ah_miss)

ah_fill %>%
  # Only show outlying periods
  right_join(outliers %>% select(-Trips))


# The interpolate() function uses the ARIMA model to estimate any missing values in the series.
# In this case, the outlier of 81.1 has been replaced with 8.5.
# The resulting series is shown in Figure below.

# The ah_fill data could now be modeled with a function that does not allow missing values.

ah_fill %>%
  autoplot(Trips) +
  autolayer(ah_fill %>% filter_index("2002 Q3"~"2003 Q1"), Trips, colour="#D55E00") +
  labs(title = "Quarterly overnight trips to Adelaide Hills", y = "Number of trips")


# ____----

