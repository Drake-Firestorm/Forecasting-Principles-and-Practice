# Chapter 2 Time series graphics ----
#* 2.1 tsibble objects ----
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


#* 2.2 Time plots ---------
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


#* 2.3 Time series patterns ----------
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


#* 2.4 Seasonal plots -----
# A seasonal plot allows the underlying seasonal pattern to be seen more clearly, and is especially useful in identifying years in which the pattern changes.

a10 %>% gg_season(Cost, labels = "both") +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


#** Multiple seasonal periods ----
vic_elec %>% gg_season(Demand, period="day") + theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period="week") + theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period="year")


# _----


#* 2.5 Seasonal subseries plots ------
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


#* 2.6 Scatterplots -------
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


#* 2.7 Lag plots ----------
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


#* 2.8 Autocorrelation ----
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


#* 2.9 White noise --------
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