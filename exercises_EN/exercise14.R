# ============================================================
# Exercise 14: Statistical power and panel size
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise14.html
# ============================================================

# install.packages(c("sensR", "pwr"))
library(sensR)
library(pwr)

#---------------------------------------------------------------------------------------
# 1. How many consumers are needed? (open question from exercise 5a)
#---------------------------------------------------------------------------------------
# Paired preference test: 34 of 60 (57%) preferred sample A - the result was not significant.
# What is the power of the test if the true preference is 57%?
power.prop.test(n = 60, p1 = 34 / 60, p2 = 0.5) # approximate (normal approximation)
discrimPwr(pdA = 2 * 34 / 60 - 1, sample.size = 60, pGuess = 1 / 2) # exact binomial calculation

# How many consumers are needed for 80% power?
discrimSS(pdA = 2 * 34 / 60 - 1, target.power = 0.80, pGuess = 1 / 2)

#---------------------------------------------------------------------------------------
# 2. Discrimination tests: panel size from d'
#---------------------------------------------------------------------------------------
# We want to detect a difference of d' = 1 with alpha = 0.05 and 80% power
d.primeSS(1, target.power = 0.80, method = "triangle")
d.primeSS(1, target.power = 0.80, method = "duotrio")
d.primeSS(1, target.power = 0.80, method = "threeAFC")
d.primeSS(1, target.power = 0.80, method = "tetrad")

# Power of a triangle test with 100 assessors for various d'
d_values <- seq(0, 2, by = 0.1)
power <- sapply(d_values, function(d) d.primePwr(d, sample.size = 100, method = "triangle"))
plot(d_values, power,
  type = "l", lwd = 2, col = "red", ylim = c(0, 1),
  xlab = "d'", ylab = "power", main = "Triangle, n = 100"
)
abline(h = 0.8, lty = 2)

#---------------------------------------------------------------------------------------
# 3. Scale data: t-test and ANOVA
#---------------------------------------------------------------------------------------
# Paired t-test: we want to detect a difference of 0.5 points, SD of the differences is 1 point
power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.80, type = "paired")

# Independent samples (two groups of consumers)
power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.80, type = "two.sample")

# One-way ANOVA for 4 products, medium effect (Cohen's f = 0.25)
pwr.anova.test(k = 4, f = 0.25, sig.level = 0.05, power = 0.80)


# TASK1:
# =========
# You plan a triangle test to detect a recipe change with d' = 0.8.
# How many assessors do you need for 80% and 90% power? Compare with the tetrad test.

# TASK2:
# =========
# Only 24 assessors are available. What is the smallest difference (in points) a paired t-test
# detects with 80% power if the SD of the differences is 1.2 points? (hint: power.t.test without delta)
