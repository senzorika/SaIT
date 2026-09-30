# ============================================================
# Exercise 23: Wald's sequential analysis
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise23.html
# ============================================================

# install.packages("sensR")
library(sensR)

#---------------------------------------------------------------------------------------
# 1. Boundaries of the sequential test (SPRT, ISO 16820)
#---------------------------------------------------------------------------------------
# A triangle test evaluated after every answer:
# H0: p = p0 (guessing), H1: p = p1 (the difference we want to detect)
p0 <- 1 / 3
p1 <- 0.5
alpha <- 0.05 # risk of declaring a difference that does not exist
beta <- 0.20 # risk of missing an existing difference

D <- log(p1 / p0) + log((1 - p0) / (1 - p1))
slope <- log((1 - p0) / (1 - p1)) / D
upper <- function(n) log((1 - beta) / alpha) / D + slope * n # above it: difference established
lower <- function(n) -log((1 - alpha) / beta) / D + slope * n # below it: difference not established
round(c(slope = slope, upper_0 = upper(0), lower_0 = lower(0)), 3)
c(upper(20), lower(20)) # at n = 20: at least 13 correct -> difference, at most 6 -> no difference

#---------------------------------------------------------------------------------------
# 2. Course of the test - a decision after every assessor
#---------------------------------------------------------------------------------------
answers <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1) # 1 = correct answer
n <- seq_along(answers)
correct <- cumsum(answers)
state <- ifelse(correct >= upper(n), "difference",
  ifelse(correct <= lower(n), "no difference", "continue")
)
course <- data.frame(n, correct, lower = round(lower(n), 2), upper = round(upper(n), 2), state)
course
end <- which(state != "continue")[1] # the first assessor at which the test stops
course[end, ]

plot(n, correct,
  type = "s", lwd = 2, ylim = c(-2, 16),
  xlab = "number of assessors", ylab = "cumulative number of correct answers", main = "Sequential triangle test"
)
abline(a = upper(0), b = slope, col = "red", lwd = 2)
abline(a = lower(0), b = slope, col = "darkgreen", lwd = 2)
points(end, correct[end], pch = 19, col = "red", cex = 1.5)
legend("topleft",
  legend = c("difference established", "difference not established"),
  col = c("red", "darkgreen"), lwd = 2, bty = "n"
)

#---------------------------------------------------------------------------------------
# 3. How many assessors does the sequential test save?
#---------------------------------------------------------------------------------------
sequential_test <- function(p, max_n = 500) {
  correct <- 0
  for (i in 1:max_n) {
    correct <- correct + rbinom(1, 1, p)
    if (correct >= upper(i)) {
      return(c(n = i, difference = 1))
    }
    if (correct <= lower(i)) {
      return(c(n = i, difference = 0))
    }
  }
  c(n = max_n, difference = NA)
}
set.seed(23)
sim_H1 <- replicate(5000, sequential_test(p1)) # the difference really exists
sim_H0 <- replicate(5000, sequential_test(p0)) # the assessors are only guessing
rbind(
  H1 = c(mean_n = mean(sim_H1["n", ]), share_difference = mean(sim_H1["difference", ])),
  H0 = c(mean_n = mean(sim_H0["n", ]), share_difference = mean(sim_H0["difference", ]))
)
quantile(sim_H1["n", ], c(0.5, 0.9, 0.99)) # the test can also drag on

# classical test with a fixed number of assessors at the same risks (pc = 0.5 -> pd = 0.25)
discrimSS(pdA = (p1 - p0) / (1 - p0), target.power = 1 - beta, alpha = alpha, pGuess = p0)


# TASK1:
# =========
# Compute the boundaries for a duo-trio test (p0 = 1/2, p1 = 0.7, alpha = 0.05, beta = 0.10).
# After how many assessors can the test stop at the earliest with "difference" and with "no difference"?

# TASK2:
# =========
# The first 12 answers in a triangle test: 0 1 0 0 1 0 0 0 1 0 0 0. How does the test end?

# TASK3:
# =========
# Change p1 to 0.45 (a smaller difference). How do the boundaries and the mean number of assessors change?
# Check the boundaries in the chapter (in Slovak): https://senzorika.github.io/SAP/kapitoly/06_skalovanie.html#wald
