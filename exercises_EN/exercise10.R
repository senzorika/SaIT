# ============================================================
# Exercise 10: Survival analysis and sensory shelf life
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise10.html
# ============================================================

# ------------------------------------------------------------------------
# Survival analysis (Kaplan-Meier model) (Vietoris, 2013)
# + predictive modelling
# ------------------------------------------------------------------------

# estimating sensory shelf life using non-parametric Kaplan-Meier survival analysis
# Five assessors evaluated yogurt samples stored for 0, 4, 8, 12, 24, 36 and 48 hours at room temperature.
# The result is a scale of (rejection / would not eat) and (acceptance / would eat). What is the estimated
# sensory shelf life of the yogurts, i.e. when will the yogurt become sensorially unacceptable?
# ----------------------------------------------------------------------------------------
# load the survival analysis package
library(survival)
time <- c(0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48)
event <- c(
  TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
  TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE
)

# create the survival object and set the basic parameters
food_shelf_life <- Surv(time, event)
food_shelf_life

fit <- survfit(food_shelf_life ~ 1, conf.int = FALSE)
# results and visualisation
fit
summary(fit)
plot(fit, main = "Sensory stability of product (A)", xlab = "time (h)", col = "blue")
abline(h = 0.5, col = "red")


# -----------------------------------------------------
# + LINEAR REGRESSION (Vietoris, 2020)
# -----------------------------------------------------
# we will predict the cut-off point for the yogurts from the previous example
# where x is time (hours) and y is the survival value
y <- fit$surv
x <- fit$time
linear <- data.frame(x, y)
linear

# linear model: time as a function of survival, x = a*y + b
regression <- lm(x ~ y)
regression

# and now we write our first function ever :)
model <- function(estimate) {
  regression$coefficients[2] * estimate + regression$coefficients[1]
}

# we still need the coefficient of determination (R2) of the model and we are done :)
reg <- lm(x ~ y)
summary(reg)

# check the linear model: how many hours is the cut-off point (value = 0.5)
model(0.5)
abline(v = model(0.5), col = "red")

# add some visuals :)
abline(v = 0, col = "green")
rect(0, 1, model(0.5), 0, density = 5, col = "green", border = "transparent")
text(model(0.5), 0.52, round(model(0.5), digit = 2), pos = 4)


# -----------------------------------------------------
# + INTERVAL-CENSORED DATA AND THE WEIBULL MODEL (Hough, 2010; ISO 16779)
# -----------------------------------------------------
# The example above treats each of the 35 ratings as a separate observation. In a consumer
# study, however, each consumer evaluates all storage times and the exact rejection time is
# unknown - we only know between which two times it happened (interval censoring):
#   accepted at 12 h, rejected at 24 h  -> interval (12, 24)
#   rejected already at 4 h             -> interval (NA, 4)   left-censored
#   accepted everything up to 48 h      -> interval (48, NA)  right-censored
# 42 consumers, yogurt stored for 4, 8, 12, 24, 36 and 48 hours:
lower <- c(rep(NA, 2), rep(4, 2), rep(8, 5), rep(12, 9), rep(24, 11), rep(36, 7), rep(48, 6))
upper <- c(rep(4, 2), rep(8, 2), rep(12, 5), rep(24, 9), rep(36, 11), rep(48, 7), rep(NA, 6))
intervals <- Surv(lower, upper, type = "interval2")
intervals

# parametric estimate: Weibull distribution of the rejection time
weibull <- survreg(intervals ~ 1, dist = "weibull")
summary(weibull)
eta <- exp(coef(weibull)) # scale parameter (h)
beta <- 1 / weibull$scale # shape parameter; beta > 1 = the risk of rejection grows with time
c(eta = unname(eta), beta = beta)

# time at which 10, 25 and 50 % of consumers reject the product
percentiles <- predict(weibull,
  newdata = data.frame(x = 1), type = "quantile",
  p = c(0.10, 0.25, 0.50), se.fit = TRUE
)
estimate <- as.numeric(percentiles$fit)
se <- as.numeric(percentiles$se.fit)
data.frame(
  rejection = c("10 %", "25 %", "50 %"), time_h = round(estimate, 1),
  lower_95 = round(estimate - 1.96 * se, 1), upper_95 = round(estimate + 1.96 * se, 1)
)

# non-parametric (Turnbull) estimate and the Weibull curve in one plot
plot(survfit(intervals ~ 1),
  conf.int = FALSE, xlab = "time (h)", ylab = "share of consumers accepting",
  main = "Sensory shelf life - interval-censored data"
)
curve(exp(-(x / eta)^beta), from = 0, to = 60, add = TRUE, col = "blue", lwd = 2)
abline(h = 0.5, col = "red", lty = 2)


# -----------------------------------------------------
# + ACCELERATED STORAGE: Q10 AND THE ARRHENIUS EQUATION
# -----------------------------------------------------
# median sensory shelf life (days) found at three storage temperatures
temperature <- c(5, 15, 25) # °C
shelf_life <- c(28, 13, 6) # days

# Q10: how many times the shelf life shortens when the temperature rises by 10 °C
Q10 <- (shelf_life[1] / shelf_life[3])^(10 / (temperature[3] - temperature[1]))
Q10

# Arrhenius: ln(k) = ln(A) - Ea / (R * T), rate of change k ~ 1 / shelf life, T in kelvin
T_K <- temperature + 273.15
arrhenius <- lm(log(1 / shelf_life) ~ I(1 / T_K))
Ea <- -coef(arrhenius)[2] * 8.314 / 1000 # activation energy in kJ/mol
unname(Ea)

# predicted shelf life at 8 °C (retail refrigerator)
1 / exp(predict(arrhenius, newdata = data.frame(T_K = 8 + 273.15)))


# TASK1:
# =========
# The producer wants at most 25 % of consumers to reject the yogurt at the time of consumption.
# Which shelf life do you recommend according to the Weibull model? Compare it with the median.

# TASK2:
# =========
# Compute Q10 from the pair 5 and 15 °C and from the pair 15 and 25 °C. Is Q10 constant?
# Check the result in the calculator (in Slovak): https://senzorika.github.io/SAP/kapitoly/10_shelf_life.html#kalkulator
