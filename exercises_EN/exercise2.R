# ============================================================
# Exercise 2: BCG matrix and confidence interval
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise02.html
# ============================================================

# Modified BCG matrix in sensory analysis
# =============================================================
# definition of the basic variables for the chart

product <- c("A", "B", "C", "D", "E", "F")
price <- c(7.40, 8.51, 7.62, 5.54, 7.20, 6.62)
quality <- c(4.5, 7.2, 8.5, 4.8, 7.5, 7.1)
results <- data.frame(product, price, quality)

# mean price and mean quality
mean(price)
mean(quality)

# draw the chart including the split of the area into quadrants

plot(price, quality)
abline(v = mean(price), col = "red")
abline(h = mean(quality), col = "red")
text(price, quality, product, pos = 4)

# Confidence interval in sensory analysis
# =====================================================
# taste scores from 7 assessors
x <- c(10, 9, 11, 10, 10, 9, 10)

# number of elements of the vector (number of assessors n)
length(x)

# number of distinct values (categories) of the vector
length(unique(x))

# 95% confidence interval (for small n we use the t-distribution instead of 1.96)
delta <- (sd(x) / sqrt(length(x))) * qt(0.975, df = length(x) - 1)
ci <- c(mean(x) - delta, mean(x) + delta)

# plot of the confidence interval
plot(x, type = "b")
abline(h = mean(x), col = "green", lty = 3)
abline(h = mean(x) - delta, col = "red", lty = 3)
abline(h = mean(x) + delta, col = "red", lty = 3)
