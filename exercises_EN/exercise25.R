# ============================================================
# Exercise 25: Sensory claims - superiority and parity
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise25.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Superiority: "consumers prefer A to B"
#---------------------------------------------------------------------------------------
# Paired preference test, 300 consumers: 172 preferred A, 128 preferred B
A <- 172
B <- 128
superiority <- binom.test(A, A + B, p = 0.5) # two-sided - we do not know the winner in advance
superiority

# "no preference" answers: 150 x A, 110 x B, 40 x no preference
# how to handle them must be decided BEFORE the test, not by what gives the nicer result
binom.test(150, 150 + 110, p = 0.5) # a) drop them
binom.test(150 + 40 / 2, 300, p = 0.5) # b) split them equally

#---------------------------------------------------------------------------------------
# 2. Parity: "liked as much as B"
#---------------------------------------------------------------------------------------
# A non-significant difference is NOT proof of parity. Parity is supported by an equivalence test:
# the (1 - 2*alpha) confidence interval of the share must lie entirely within a pre-set band 50 % +- margin.
parity <- function(x, n, margin = 0.10, alpha = 0.05) {
  ci <- binom.test(x, n, conf.level = 1 - 2 * alpha)$conf.int
  data.frame(
    share = x / n, lower = ci[1], upper = ci[2],
    parity = ci[1] > 0.5 - margin & ci[2] < 0.5 + margin
  )
}
parity(172, 300) # 57 % - parity is not supported
parity(154, 300) # 51 % with 300 consumers
parity(31, 60) # 52 % with 60 consumers - the same share, but the interval is too wide

# power of the parity test: probability of establishing parity when the true preference is 50 : 50
parity_power <- function(n, margin = 0.10, alpha = 0.05) {
  x <- 0:n
  sum(dbinom(x, n, 0.5)[sapply(x, function(i) parity(i, n, margin, alpha)$parity)])
}
n_values <- c(60, 100, 150, 200, 300, 400, 500)
power <- sapply(n_values, parity_power)
data.frame(n = n_values, power = round(power, 2))
plot(n_values, power,
  type = "b", pch = 19, ylim = c(0, 1),
  xlab = "number of consumers", ylab = "power of the parity test", main = "Parity: margin +- 10 pp, alpha = 0.05"
)
abline(h = 0.8, lty = 2)

#---------------------------------------------------------------------------------------
# 3. "Unsurpassed": product A is not liked less than B
#---------------------------------------------------------------------------------------
# one-sided test: H0: share of A <= 50 % - margin, H1: the share of A is higher
binom.test(154, 300, p = 0.5 - 0.10, alternative = "greater")

#---------------------------------------------------------------------------------------
# 4. Attribute claim: "intense flavour" (mean of at least 7 on a 10-point scale)
#---------------------------------------------------------------------------------------
# 12 trained assessors, mean 7.5, standard deviation 1.2
set.seed(25)
intensity <- as.numeric(7.5 + 1.2 * scale(rnorm(12)))
c(mean = mean(intensity), sd = sd(intensity))
t.test(intensity)$conf.int # 95 % confidence interval of the mean
# the claim is supported only if the lower limit of the interval is at least 7
t.test(intensity, mu = 7, alternative = "greater")

# how many assessors are needed to separate a mean of 7.5 from the limit of 7 with 80 % power?
power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.05, power = 0.80, type = "one.sample", alternative = "one.sided")


# TASK1:
# =========
# In a test with 250 consumers, 140 preferred product A and 110 product B. Is the claim
# "consumers prefer A" supported? Is parity within +- 10 pp supported?

# TASK2:
# =========
# How many consumers are needed to establish parity with 80 % power for a margin of +- 10 pp,
# and how many for a stricter margin of +- 5 pp?

# TASK3:
# =========
# Check the results of parts 1 and 2 in the calculator (in Slovak): https://senzorika.github.io/SAP/kapitoly/09_claims.html#kalkulator
# Why is the statement "no significant difference was found" not enough to support the claim "tastes the same"?
