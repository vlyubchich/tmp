
# Clean environment
rm(list = ls())

# Packages ----
# data manipulation
library(dplyr)
library(tidyr)
library(readxl)

# data visualization
library(ggplot2)
theme_set(theme_light())
library(patchwork)

# modeling
library(forecast)

# Data ----

# Read data
D <- read_xlsx("CBL PIER Running Summary_2015_2024_cln.xlsx",
               sheet = "CHL") %>%
    rename(Date = `SAMPLE DATE`,
           CHL = `ACTIVE CHL-A mg/L`) %>%
    # convert Date column to Date format
    mutate(Date = as.Date(Date)) %>%
    # extract Year, Month, and Day of the Year (DoY) from Date
    mutate(Year = as.numeric(format(Date, "%Y")),
           Month = as.numeric(format(Date, "%m")),
           DoY = as.numeric(format(Date, "%j")))

# Summarize by Year and Month
D_monthly <- D %>%
    group_by(Year, Month) %>%
    summarise(CHL = mean(CHL, na.rm = TRUE)) %>%
    ungroup() %>%
    # create a Date column for the first day of each month
    mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    # order chronologically
    arrange(Date)

# Check that there are no missing months in the data
all_months <- seq.Date(from = min(D_monthly$Date),
                       to = max(D_monthly$Date),
                       by = "month")
missing_months <- setdiff(all_months, D_monthly$Date)
missing_months

# Add the missing months into the data frame with NA values for CHL
if (length(missing_months) > 0) {
    missing_data <- data.frame(Date = missing_months,
                               Year = as.numeric(format(missing_months, "%Y")),
                               Month = as.numeric(format(missing_months, "%m")),
                               CHL = NA)
    D_monthly <- bind_rows(D_monthly, missing_data) %>%
        arrange(Date)
}


## Data interpolation ----

# Interpolate missing CHL values using linear interpolation
D_monthly <- D_monthly %>%
    mutate(CHL_interp = as.numeric(forecast::na.interp(CHL)))

# Show the interpolated values (visual check)
ggplot(D_monthly, aes(x = Date, y = CHL_interp)) +
    geom_line() +
    geom_point(fill = "white", pch = 21) +
    geom_point(data = D_monthly %>% filter(is.na(CHL)),
               aes(y = CHL_interp),
               fill = "tomato", pch = 21) +
    labs(x = "Date", y = "Active chlorophyll-a (µg/L)")

# Format the CHL column as a ts object
CHLts <- ts(D_monthly$CHL_interp,
            start = c(min(D_monthly$Year), min(D_monthly$Month)),
            frequency = 12)


# EDA ----

# Time series plot of monthly CHLts, mu g/L
p0 <- autoplot(CHLts) +
    labs(x = "Date", y = "Active chlorophyll-a (µg/L)")

# ACF and PACF plots
p1 <- forecast::ggAcf(CHLts, lag.max = 37) +
    labs(x = "Lag (months)", y = "ACF", title = "")
p2 <- forecast::ggPacf(CHLts, lag.max = 37) +
    labs(x = "Lag (months)", y = "PACF", title = "")

# Combine plots
p0 / (p1 + p2) +
    plot_annotation(tag_levels = "A")


# Model cross-validation for time series, using forecast::tsCV() ----

## Define prediction functions ----
# AR model
ar_pred <- function(x, h) {
    ar.order <- stats::ar(x, aic = TRUE)$order
    fit <- Arima(x, order = c(ar.order, 0, 0))
    forecast(fit, h = h)
}

# SARIMA model
sarima_pred <- function(x, h) {
    fit <- auto.arima(x, seasonal = TRUE)
    forecast(fit, h = h)
}

# Naive model, see ?forecast::naive
naive_pred <- function(x, h) {
    fit <- naive(x, h = h)
    forecast(fit, h = h)
}


## Perform cross-validation ----

# Set forecast horizon
h <- 12
initial_window <- 7 * 12 # 7 years

# Perform CV for each model
cv_ar     <- tsCV(CHLts, ar_pred, h = h, window = initial_window)
cv_sarima <- tsCV(CHLts, sarima_pred, h = h, initial = initial_window)
cv_naive  <- tsCV(CHLts, naive_pred, h = h, initial = initial_window)

## Calculate CV errors ----

# Calculate RMSE for each model and forecast horizon
rmse_ar <- sqrt(colMeans(cv_ar^2, na.rm = TRUE))
rmse_sarima <- sqrt(colMeans(cv_sarima^2, na.rm = TRUE))
rmse_naive <- sqrt(colMeans(cv_naive^2, na.rm = TRUE))

# Create a data frame for plotting
cv_results <- data.frame(
    Horizon = 1:h,
    AR = rmse_ar,
    SARIMA = rmse_sarima,
    Naive = rmse_naive
) %>%
    pivot_longer(cols = -Horizon, names_to = "Model", values_to = "RMSE")

## Plot CV results ----
ggplot(cv_results, aes(x = Horizon, y = RMSE, color = Model)) +
    geom_line() +
    geom_point() +
    labs(x = "Forecast horizon (months)", y = "Cross-validation RMSE (µg/L)") +
    theme(legend.position = "bottom")


# Refit the best model on the whole dataset ----
