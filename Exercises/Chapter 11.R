#* 1 ----
# Consider the PBS data which has aggregation structure ATC1/ATC2 * Concession * Type.

PBS2 <- PBS %>%
  aggregate_key(ATC1/ATC2 * Concession * Type, Cost = sum(Cost)/1e6)


#** 1.a ----
# Produce plots of the aggregated Scripts data by Concession, Type and ATC1.

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


#** 1.b ----
# Forecast the PBS Scripts data using ETS, ARIMA and SNAIVE models, applied to all but the last three years of data.

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


#** 1.c ----
# Reconcile each of the forecasts using MinT.

fit2 <- fit %>%
  reconcile(
    ets_mint = min_trace(ets),
    arima_mint = min_trace(arima),
    snaive_mint = min_trace(snaive)
  )


#** 1.d ----
# Which type of model works best on the test set?

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


#** 1.e ----
# Why doesn’t the reconciliation make any difference to the SNAIVE forecasts?

#   SNaive forecasts by repeating the last seasonal values.
#   Since the data is already aggregated at higher level, SNaive and reconciliation will give the same forecasts.


#* 2 ----
# Repeat the tourism example from Section 11.4, but also evaluate the forecast distribution accuracy using CRPS skill scores.
# Which method does best on this measure?

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


#* 3 ----
# Repeat the prison example from Section 11.6, but using a bootstrap to generate the forecast distributions rather than assuming normality.
# Does it make much difference to the CRPS skill scores?

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
