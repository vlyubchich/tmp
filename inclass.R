
# ACF ----

## Sunspots ----

data("sunspot.year")
sunspot.year

par(mfrow = c(1, 2))
acf(sunspot.year)
pacf(sunspot.year)

library(forecast)
tsdisplay(sunspot.year)



# Smoothing ----
library(forecast)
?Nile
Nile
ma3 <- ma(Nile, order = 3)
ma10 <- ma(Nile, order = 10)

plot.ts(Nile)
lines(ma3)

autoplot(Nile) + 
  autolayer(ma3, lwd = 2) + 
  autolayer(ma10, lwd = 1.5)

## Exponential smoothing ----

is.ts(co2)

# Plot
plot(co2)

# Apply appropriate exp.smoothing
m = HoltWinters(co2)

# Predictions 2 years ahead
?predict.HoltWinters
predict(m, n.ahead = 24,
        prediction.interval = TRUE)
# ?predict.lm


# AR ----

## Assuming consecutive measurements ----

x = beaver1$temp

# plot TS
plot.ts(x)

# plot ACF and PACF
par(mfrow = c(1, 2))
acf(x)
pacf(x)

# Suggest AR(p)
# AR(1)

# Estimate AR(p)
m = ar(x, demean = TRUE)
print(m)

# X(t) = 0.83 X(t-1) 
# var(e) = 0.01201
# mean(X) = 0

library(forecast)
m2 = auto.arima(x, d = 0)
m2

# ARMA ----

X = beaver1$temp.x

# Identify ARMA(p, q) model
library(forecast)
X1 <- ts(X)
tsdisplay(X1)
acf(X)
pacf(X)

ARMA(0, 0)
# Estimate the model
m = arima(X, order = c(1,0,0))   # AR(1)

# Evaluate the model
acf(m$residuals)
pacf(m$residuals)

shapiro.test(m$residuals)

# Forecast 5 steps ahead
?predict.Arima
predict(m, n.ahead = 5)


# GARCH ----
library(forecast)
library(FinTS)
library(dplyr)
X = FinTS::ibm %>% 
  group_by(date.time) %>% 
  summarise(price = mean(price))
x = X$price

# plot price 
# and see if there are any ARCH effects
par(mfrow = c(1, 2))
plot.ts(x)
lr = diff(log(x)) * 100 # log returns (%)
plot.ts(lr)

# ACF of log returns and squared log returns
acf(lr)
acf(lr^2)

# Model autocorrelations before modeling any ARCH effects
m_arma = auto.arima(lr, allowmean = FALSE)
m_arma
lr_resid <- m_arma$residuals
# detect ARCH effects visually
acf(lr_resid)
acf(lr_resid^2)

# test for ARCH effects
FinTS::ArchTest(lr_resid, lags = 10)

# Model using fGarch (NAs in the output) 
library(fGarch)
set.seed(123)
m_garch = fGarch::garchFit(formula = ~arma(0, 1) + garch(1, 1),
                     algorithm = "lbfgsb",
                     data = lr)
m_garch
par(mfrow = c(1, 2))
fGarch::plot(m, which = c(9, 11))


# Model using rugarch
library(rugarch)
spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1), include.mean = FALSE))
m_rugarch = ugarchfit(spec, data = lr)
m_rugarch
rugarch::plot(m_rugarch, which = 9)
rugarch::plot(m_rugarch, which = 11)
