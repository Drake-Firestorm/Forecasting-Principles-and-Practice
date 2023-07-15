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
