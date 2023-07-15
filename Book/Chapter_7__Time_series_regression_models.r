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
#   Monday 	      1 	  0 	  0 	  0 	  0 	  0
#   Tuesday 	  0 	  1 	  0 	  0 	  0 	  0
#   Wednesday 	  0 	  0 	  1 	  0 	  0 	  0
#   Thursday 	  0 	  0 	  0 	  1 	  0 	  0
#   Friday 	      0 	  0 	  0 	  0 	  1 	  0
#   Saturday 	  0 	  0 	  0 	  0 	  0 	  1
#   Sunday 	      0 	  0 	  0 	  0 	  0 	  0
#   Monday 	      1 	  0 	  0 	  0 	  0 	  0
#     ⋮ 	       ⋮ 	   ⋮ 	    ⋮ 	    ⋮ 	    ⋮        ⋮


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


# ____----
