# ============================================================
# Exercise 22: Detection threshold - 3-AFC and the BET method
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise22.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Best Estimate Threshold (BET)
#---------------------------------------------------------------------------------------
# Ascending series of sucrose concentrations (g/l). A 3-AFC test at each concentration:
# two samples of water and one with the substance, 1 = the assessor picked the right sample.
concentrations <- c(0.5, 1, 2, 4, 8, 16)
answers <- rbind(
  A1 = c(0, 0, 1, 1, 1, 1),
  A2 = c(0, 1, 0, 1, 1, 1),
  A3 = c(0, 0, 0, 1, 1, 1),
  A4 = c(1, 1, 1, 1, 1, 1),
  A5 = c(0, 0, 1, 0, 0, 1),
  A6 = c(0, 0, 0, 0, 1, 1)
)
colnames(answers) <- concentrations
answers

# individual threshold = geometric mean of the highest missed and the next higher concentration
bet <- function(x, conc) {
  k <- length(conc)
  misses <- which(x == 0)
  if (length(misses) == 0) {
    return(conc[1] / sqrt(conc[2] / conc[1])) # correct everywhere: half a step below the series
  }
  last <- max(misses)
  if (last == k) {
    return(conc[k] * sqrt(conc[k] / conc[k - 1])) # miss at the highest: half a step above the series
  }
  sqrt(conc[last] * conc[last + 1])
}
thresholds <- apply(answers, 1, bet, conc = concentrations)
round(thresholds, 2)

# group threshold = geometric mean of the individual thresholds
group_threshold <- exp(mean(log(thresholds)))
group_threshold
sd(log10(thresholds)) # variability of sensitivity within the panel (in log10 units)

#---------------------------------------------------------------------------------------
# 2. Psychometric function - threshold from the proportion of correct answers
#---------------------------------------------------------------------------------------
# 30 assessors at each concentration. In 3-AFC one third guess correctly without perceiving
# anything, so P(correct) = 1/3 + 2/3 * P(detection); threshold = concentration detected by 50 % of the panel.
set.seed(22)
n <- 30
p_detection <- plogis(1.8 * (log2(concentrations) - log2(3)))
correct <- rbinom(length(concentrations), n, 1 / 3 + 2 / 3 * p_detection)
data.frame(concentrations, correct, proportion = round(correct / n, 2))

# parameter estimation by maximum likelihood
neg_loglik <- function(par) {
  p <- 1 / 3 + 2 / 3 * plogis(par[1] + par[2] * log2(concentrations))
  -sum(dbinom(correct, n, p, log = TRUE))
}
fit <- optim(c(0, 1), neg_loglik)
model_threshold <- 2^(-fit$par[1] / fit$par[2]) # 50 % detection, i.e. 2/3 correct answers
model_threshold

plot(concentrations, correct / n,
  log = "x", pch = 19, ylim = c(0, 1),
  xlab = "concentration (g/l)", ylab = "proportion of correct answers", main = "3-AFC psychometric function"
)
curve(1 / 3 + 2 / 3 * plogis(fit$par[1] + fit$par[2] * log2(x)), add = TRUE, col = "blue", lwd = 2)
abline(h = 1 / 3, lty = 3) # chance level
abline(h = 2 / 3, lty = 2, col = "red") # 50 % detection
abline(v = model_threshold, lty = 2, col = "red")


# TASK1:
# =========
# Which assessor is the most sensitive and which the least? For which assessors is the threshold
# estimate unreliable and how would you change the concentration series?

# TASK2:
# =========
# Assessor A5 answered 0, 0, 1, 0, 0, 1. Why does the correct answer at 2 g/l not count?
# What is the probability that an assessor who perceives nothing guesses three 3-AFC tests in a row?

# TASK3:
# =========
# Check the individual thresholds in the calculator (in Slovak): https://senzorika.github.io/SAP/kapitoly/01_uvod_vnimanie_chuti.html#kalkulator
# Compare the BET group threshold with the threshold from the psychometric function - why may they differ?
