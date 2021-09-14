#* 1. ----
# Use the help function to explore what the series gafa_stock, PBS, vic_elec and pelt represent.

#** 1.a ----
# Use autoplot() to plot some of the series in these data sets.
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
# What is the time interval of each series?
gafa_stock; PBS; vic_elec; pelt;
frequency(gafa_stock); frequency(PBS); frequency(vic_elec); frequency(pelt)
# daily;                monthly;        30 min;             annual


#** 1.c ----
# Use filter() to find what days corresponded to the peak closing price for each of the four stocks in gafa_stock.
gafa_stock %>%
  group_by(Symbol) %>%
  filter(Close == max(Close)) %>%
  select(Symbol, Date)


#* 2. ----
# Download the file tute1.csv from the book website, open it in Excel (or some other spreadsheet application), and review its contents.
# You should find four columns of information.
# Columns B through D each contain a quarterly series, labelled Sales, AdBudget and GDP.
# Sales contains the quarterly sales for a small company over the period 1981-2005.
# AdBudget is the advertising budget and GDP is the gross domestic product.
# All series have been adjusted for inflation.

#** 2.a ----
# You can read the data into R with the following script:
# tute1 <- readr::read_csv("tute1.csv")
# View(tute1)
tute1 <- readr::read_csv("tute1.csv")


#** 2.b ----
# Convert the data to time series
# mytimeseries <- tute1 %>%
#   mutate(Quarter = yearmonth(Quarter)) %>%
#   as_tsibble(index = Quarter)
mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)


#** 2.c ----
# Construct time series plots of each of the three series
# mytimeseries %>%
#   pivot_longer(-Quarter) %>%
#   ggplot(aes(x = Quarter, y = value, colour = name)) +
#   geom_line() +
#   facet_grid(name ~ ., scales = "free_y")
# Check what happens when you don’t include facet_grid().
mytimeseries %>%
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>%
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
    geom_line() +
    facet_grid(vars(Key), scales = "free_y")  # removing this plots all on the same plot



#* 3 ----
#** 3.a ----
# Download tourism.xlsx from the book website and read it into R using readxl::read_excel().

# read_excel = Note that the Excel spreadsheet must be local (a URL does not work)
# below method is an alternate for making this work
httr::GET("http://otexts.com/fpp3/extrafiles/tourism.xlsx", httr::write_disk(td <- tempfile(fileext = ".xlsx")))
tourism_url <- readxl::read_xlsx(path = td, sheet = "Sheet1")
# tourism_url <- readxl::read_excel(path = td, sheet = "Sheet1")


#** 3.b ----
# Find what combination of Region and Purpose had the maximum number of overnight trips on average.
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


#* 4 ----
# Create time plots of the following four time series: Bricks from aus_production, Lynx from pelt, Close from gafa_stock, Demand from vic_elec.
#    Use ? (or help()) to find out about the data in each series.
#    For the last plot, modify the axis labels and title.
aus_production %>% autoplot(Bricks)

pelt %>% autoplot(Lynx)

gafa_stock %>% autoplot(Close)

vic_elec %>%
  autoplot(Demand) +
  xlab("Time (30 min)") + ylab("Demand (MW)") +
  ggtitle("Australian Electricity Demand")


#* 5 ----
# The aus_arrivals data set comprises quarterly international arrivals to Australia from Japan, New Zealand, UK and the US.
#    Use autoplot(), gg_season() and gg_subseries() to compare the differences between the arrivals from these four countries.
#    Can you identify any unusual observations?

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


#* 6 ----
# Monthly Australian retail data is provided in aus_retail. Select one of the time series as follows (but choose your own seed value):
# set.seed(12345678)
# myseries <- aus_retail %>%
#   filter(`Series ID` == sample(aus_retail$`Series ID`,1))
# Explore your chosen retail time series using the following functions:
#   autoplot(), gg_season(), gg_subseries(), gg_lag(), ACF() %>% autoplot()
# Can you spot any seasonality, cyclicity and trend? What do you learn about the series?

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


#* 7 ----
# Use the following graphics functions: autoplot(), gg_season(), gg_subseries(), gg_lag(), ACF() and explore features from the following time series:
# “Total Private” Employed from us_employment, Bricks from aus_production, Hare from pelt, “H02” Cost from PBS, and us_gasoline.
#    Can you spot any seasonality, cyclicity and trend?
#    What do you learn about the series?
#    What can you say about the seasonal patterns?
#    Can you identify any unusual years?


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


#* 8 ----
# The following time plots and ACF plots correspond to four different time series.
# Your task is to match each time plot in the first row with one of the ACF plots in the second row.

# visual check
# 1 - B; 2 - A; 3 - D; 4 - C


#* 9 ----
# The aus_livestock data contains the monthly total number of pigs slaughtered in Victoria, Australia, from Jul 1972 to Dec 2018.
# Use filter() to extract pig slaughters in Victoria between 1990 and 1995.
# Use autoplot() and ACF() for this data. How do they differ from white noise?
# If a longer period of data is used, what difference does it make to the ACF?
aus_livestock_pigs_victoria_90_95 <- 
  aus_livestock %>%
  filter(Animal == "Pigs", States == "Vicotria", year(Month) >= 1990, year(Month) <= 1995)

aus_livestock_pigs_victoria_90_95 %>% autoplot()

aus_livestock_pigs_victoria_90_95 %>% ACF() %>% autoplot()
# increasing trend; with lag correltaion spike at 3, 6, 12 mark


#* 10 ----
#** 10.a ----
# Use the following code to compute the daily changes in Google closing stock prices.
#   dgoog <- gafa_stock %>%
#     filter(Symbol == "GOOG", year(Date) >= 2018) %>%
#     mutate(trading_day = row_number()) %>%
#     update_tsibble(index = trading_day, regular = TRUE) %>%
#     mutate(diff = difference(Close))

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
# Why was it necessary to re-index the tsibble?
# this is to account for the weekend/market holidays which breaks the trend and might give incorrect result when annalysing visually
# check 10.c


#** 10.c ----
# Plot these differences and their ACF.
dgoog %>% autoplot(diff)
# no trend and seasonality
# dgoog_org_index %>% autoplot(diff)

dgoog %>% ACF(diff) %>% autoplot()
# no correlation
# dgoog_org_index %>% ACF(diff) %>% autoplot()
#   throws error due to missing dates


#** 10.d ----
# Do the changes in the stock prices look like white noise?
# the change in prices looks like white noise
