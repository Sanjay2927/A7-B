# A7-B- PART A:
# Load required libraries
library(quantmod)
library(rugarch)
library(FinTS)
library(xts)
library(ggplot2)

# Load your dataset
df <- read.csv("Taiwan Semiconductor Stock Price History.csv", stringsAsFactors = FALSE)

# Ensure Date is in Date format and sorted
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df <- df[order(df$Date), ]

# Use only Date and Adjusted Close columns (or Close if Adj Close not present)
df <- df[, c("Date", "Adj.Close")]
colnames(df) <- c("Date", "Price")

# Remove commas or any non-numeric characters (if present)
df$Price <- gsub(",", "", df$Price)
df$Price <- as.numeric(df$Price)

# Convert to xts time series object
price_xts <- xts(df$Price, order.by = df$Date)

# Compute daily log returns
returns <- na.omit(dailyReturn(price_xts, type = "log"))
colnames(returns) <- "LogReturns"

# Plot price and returns
par(mfrow = c(2, 1))
plot(price_xts, main = "Taiwan Semiconductor Price", col = "blue")
plot(returns, main = "Daily Log Returns", col = "darkred")

# ARCH Test to check for ARCH effects
ArchTest(returns, lags = 12)

# GARCH(1,1) specification with Student-t distribution
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "std"
)

# Fit the GARCH model
fit_garch <- ugarchfit(spec = spec_garch, data = returns)
show(fit_garch)

# Plot conditional volatility
volatility_series <- sigma(fit_garch)
plot(volatility_series, main = "Estimated Conditional Volatility", col = "darkgreen")

# Forecast next 60 days volatility and return
forecast_60 <- ugarchforecast(fit_garch, n.ahead = 60)

# Extract forecasted return (mu) and sigma
mu_forecast <- fitted(forecast_60)
sigma_forecast <- sigma(forecast_60)
shape <- coef(fit_garch)["shape"]  # For Student-t distribution

# Compute VaR at 1% and 5%
VaR_1pct <- mu_forecast + sigma_forecast * qdist("std", p = 0.01, mu = 0, sigma = 1, shape = shape)
VaR_5pct <- mu_forecast + sigma_forecast * qdist("std", p = 0.05, mu = 0, sigma = 1, shape = shape)

# Create forecast DataFrame
forecast_dates <- seq(max(df$Date) + 1, by = "days", length.out = 60)
VaR_forecast_df <- data.frame(
  Date = forecast_dates,
  MeanReturn = as.numeric(mu_forecast),
  Volatility = as.numeric(sigma_forecast),
  VaR_1pct = as.numeric(VaR_1pct),
  VaR_5pct = as.numeric(VaR_5pct)
)

# Plot forecasted VaR and return
ggplot(VaR_forecast_df, aes(x = Date)) +
  geom_line(aes(y = VaR_1pct, color = "1% VaR")) +
  geom_line(aes(y = VaR_5pct, color = "5% VaR")) +
  geom_line(aes(y = MeanReturn, color = "Mean Return"), linetype = "dashed") +
  labs(title = "60-Day Ahead Forecasted VaR", y = "Return", x = "Date") +
  scale_color_manual(values = c("1% VaR" = "red", "5% VaR" = "blue", "Mean Return" = "black")) +
  theme_minimal()
