
# Packages ----
library(daymetr)

library(dplyr)
library(tidyr)

library(ggplot2)
theme_set(theme_light())

# Data ----

# Coordinates of Solomons, MD
loc <- tibble(lat = 38.336431, lon = -76.464102)

# Download daily data from https://daymet.ornl.gov/
df0 <- download_daymet(
  lat = loc$lat,
  lon = loc$lon,
  start = 2000,
  internal = TRUE,
  simplify = TRUE
)

# Reformat the data
df0 <- df0 %>%
  mutate(Date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))

df <- df0 %>%
  select(Date, measurement, value) %>%
  pivot_wider(names_from = measurement,
              values_from = value) %>%
  mutate(tavg = (tmax..deg.c. + tmin..deg.c.)/2,
         Year = as.numeric(format(Date, "%Y")),
         Month = as.numeric(format(Date, "%m")))

# Plots ----
df0 %>% ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_wrap(~measurement,
             scales = "free",
             ncol = 2)

tmax_ts = ts(df$tmax..deg.c.,
             start = c(2000, 1),
             frequency = 365)
plot.ts(tmax_ts)
acf(tmax_ts, lag.max = 400)

# Cross-validation ----

d_train <- window(tmax_ts, end = c(2019, 365))
d_train <- window(tmax_ts, end = c(2019, 365))
d_test <- window(tmax_ts, start = c(2020, 1))

times <- time(tmax_ts)
sin2 = sin(2 * pi * times)
cos2 = cos(2 * pi * times)

Dtrain <- tibble(tmax = d_train,
                 sin2 = sin2[1:length(d_train)],
                 cos2 = cos2[1:length(d_train)])
Dtest <- tibble(tmax = d_test,
                sin2 = tail(sin2, length(d_test)),
                cos2 = tail(cos2, length(d_test)))

m0 <- lm(tmax ~ sin2 + cos2, data = Dtrain)

?predict.lm
CONF = 0.95 # confidence level
pred <- predict(m0, newdata = Dtest,
                level = CONF,
                interval = "prediction") %>%
  as_tibble()

# Check if the observations in the testing set are covered by
# the interval
covered = d_test > pred$lwr & d_test < pred$upr

# Calculate empirical coverage
coverage = mean(covered)

# Note that the coverage closer to the nominal is better,
# not the highest coverage.
# You might want to calculate absolute differences
# for easier comparison.
abs(coverage - CONF) # smaller is better


