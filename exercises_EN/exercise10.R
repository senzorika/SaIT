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
