library(mgcv)
library(lmtest)

# Simulated time series data
set.seed(123)
time <- 1:100
x <- rnorm(100)
y <- 5 + sin(2 * pi * time / 50) + 0.5 * x + arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = 100)

data <- data.frame(y = y, x = x, time = time)

# Fit GAMM models
model0 <- gamm(y ~ s(x), data = data)
model1 <- gamm(y ~ s(x) + s(time), data = data)
model2 <- gamm(y ~ s(x), correlation = corAR1(form = ~ time), data = data)

summary(model0)
summary(model1)
summary(model2)

# Residual diagnostics
acf(residuals(model0$lme, type = "normalized"), main = "ACF - Model 0")
acf(residuals(model1$lme, type = "normalized"), main = "ACF - Model 1")
acf(residuals(model2$lme, type = "normalized"), main = "ACF - Model 2")

pacf(residuals(model0$lme, type = "normalized"), main = "PACF - Model 0")
pacf(residuals(model1$lme, type = "normalized"), main = "PACF - Model 1")
pacf(residuals(model2$lme, type = "normalized"), main = "PACF - Model 2")

# Durbin-Watson Test
dwtest(residuals(model0$lme, type = "normalized") ~ 1)
dwtest(residuals(model1$lme, type = "normalized") ~ 1)
dwtest(residuals(model2$lme, type = "normalized") ~ 1)

# Residual plots
plot(data$time, residuals(model0$lme, type = "normalized"), type = "l", main = "Residuals - Model 0", ylab = "Residuals")
plot(data$time, residuals(model1$lme, type = "normalized"), type = "l", main = "Residuals - Model 1", ylab = "Residuals")
plot(data$time, residuals(model2$lme, type = "normalized"), type = "l", main = "Residuals - Model 2", ylab = "Residuals")

# Compare models using AIC
AIC(model0$lme, model1$lme, model2$lme)

# Compare models using ANOVA (nested models only)
anova(model0$lme, model1$lme, model2$lme)


acf(residuals(model0$lme, type = "normalized"), main = "ACF - Model 0")
pacf(residuals(model0$lme, type = "normalized"), main = "PACF - Model 0")
dwtest(residuals(model0$lme, type = "normalized") ~ 1)
plot(data$time, residuals(model0$lme, type = "normalized"), type = "l", main = "Residuals - Model 0", ylab = "Residuals")
AIC(model0$lme, model1$lme, model2$lme)

## APELAFICO
# Since the apelafico data has simultaneous time series data from multiple stations, you cannot just add time, 
# but you have to take the station into account too

# For model 1 like: s(time, by = station)
# For model 2 like: correlation = corAR1(form = ~ time | station)