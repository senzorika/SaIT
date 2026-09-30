# ============================================================
# Exercise 21: Sample codes and the Williams design of serving order
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise21.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Random three-digit sample codes
#---------------------------------------------------------------------------------------
# Labels such as A, B, C or 1, 2, 3 suggest an order and quality - samples are therefore
# coded with random three-digit numbers.
set.seed(21)
samples <- c("A", "B", "C", "D")
codes <- sample(100:999, length(samples)) # sampling without replacement
names(codes) <- samples
codes

#---------------------------------------------------------------------------------------
# 2. Williams design - balanced serving order
#---------------------------------------------------------------------------------------
# Every sample appears equally often in every position and equally often follows every other
# sample (balance for first-order carry-over).
# Even number of samples k -> k orders, odd number -> 2k orders (the square + its mirror image).
williams <- function(k) {
  first <- numeric(k) # first order: 0, 1, k-1, 2, k-2, ...
  low <- seq(2, k, by = 2)
  first[low] <- seq_along(low)
  if (k > 2) {
    high <- seq(3, k, by = 2)
    first[high] <- k - seq_along(high)
  }
  square <- (outer(0:(k - 1), first, "+") %% k) + 1 # the other orders are shifts by 1
  if (k %% 2 == 1) square <- rbind(square, square[, k:1])
  square
}

orders <- williams(length(samples))
matrix(samples[orders], nrow = nrow(orders))

# checking the balance
carry_over <- function(p) table(previous = samples[p[, -ncol(p)]], following = samples[p[, -1]])
table(sample = samples[orders], position = col(orders)) # every sample once in every position
carry_over(orders) # every ordered pair exactly once

#---------------------------------------------------------------------------------------
# 3. Serving plan for the panel
#---------------------------------------------------------------------------------------
# 12 assessors: every Williams order is used 3 times, the assignment to assessors is random
n <- 12
rows <- sample(rep(seq_len(nrow(orders)), length.out = n))
plan <- matrix(codes[orders[rows, ]],
  nrow = n,
  dimnames = list(paste0("A", 1:n), paste0("position_", seq_len(ncol(orders))))
)
plan
# write.csv(plan, "serving_plan.csv") # a sheet for sample preparation

#---------------------------------------------------------------------------------------
# 4. Comparison with a completely random order
#---------------------------------------------------------------------------------------
# A random order is balanced on average, but with a small panel some combinations
# tend to occur more often than others.
random <- t(replicate(n, sample(length(samples))))
table(sample = samples[random], position = col(random))
carry_over(random)


# TASK1:
# =========
# Prepare a serving plan for 5 samples and 20 assessors. How many different orders does
# the Williams design for 5 samples have and how many times is each of them used?

# TASK2:
# =========
# You have 6 samples but only 15 assessors. Is the plan fully balanced? Check it with the table
# of positions and the carry-over table. How many assessors would be needed?

# TASK3:
# =========
# Compare the result with the calculator (in Slovak): https://senzorika.github.io/SAP/kapitoly/02_laboratorium.html#kalkulator
# What if an assessor can taste only 3 of the 6 samples? (see exercise 5d - incomplete blocks)
