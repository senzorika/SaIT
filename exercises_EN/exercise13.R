# ============================================================
# Exercise 13: Thurstonian model and d' (sensR)
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise13.html
# ============================================================

# install.packages("sensR")
library(sensR)

#---------------------------------------------------------------------------------------
# 1. From the proportion of correct answers to d'
#---------------------------------------------------------------------------------------
# Triangle test: 42 of 100 assessors identified the odd sample (task 2 of exercise 5a)
triangle <- discrim(42, 100, method = "triangle")
triangle
# pc - proportion correct, pd - proportion of "true discriminators", d.prime - Thurstonian d'

# the same result expressed for other methods: d' does not depend on the method, pc does
rescale(d.prime = coef(triangle)["d-prime", "Estimate"], method = "duotrio")
rescale(d.prime = coef(triangle)["d-prime", "Estimate"], method = "twoAFC")

#---------------------------------------------------------------------------------------
# 2. Psychometric functions - how fast pc grows with d' in each test
#---------------------------------------------------------------------------------------
curve(psyfun(x, method = "twoAFC"),
  from = 0, to = 4, lwd = 2, col = "darkgreen",
  xlab = "d'", ylab = "pc (proportion correct)", ylim = c(0, 1)
)
curve(psyfun(x, method = "threeAFC"), add = TRUE, lwd = 2, col = "blue")
curve(psyfun(x, method = "duotrio"), add = TRUE, lwd = 2, col = "orange")
curve(psyfun(x, method = "triangle"), add = TRUE, lwd = 2, col = "red")
legend("bottomright",
  legend = c("2-AFC", "3-AFC", "duo-trio", "triangle"),
  col = c("darkgreen", "blue", "orange", "red"), lwd = 2, bty = "n"
)

#---------------------------------------------------------------------------------------
# 3. Comparing methods for the same difference between samples (d' = 1)
#---------------------------------------------------------------------------------------
methods <- c("twoAFC", "threeAFC", "duotrio", "triangle", "tetrad")
pc_at_d1 <- sapply(methods, function(m) psyfun(1, method = m))
round(pc_at_d1, 3)

#---------------------------------------------------------------------------------------
# 4. Similarity test - are the samples "similar enough"?
#---------------------------------------------------------------------------------------
# Ingredient substitution: we want to show that the difference is smaller than pd0 = 0.2
discrim(38, 100, method = "triangle", test = "similarity", pd0 = 0.2)

# distributions of both samples according to the Thurstonian model
plot(triangle)

#---------------------------------------------------------------------------------------
# 5. A - not A (ISO 8588) and same-different: tests with a response bias
#---------------------------------------------------------------------------------------
# A - not A: every assessor receives one sample and says whether it is "A".
# 50 assessors received sample A, 50 received "not A"; the answer "A" was given 34 times for A and 20 times for "not A".
# The chance level here is not 1/2 - it depends on how willingly the assessors say "A".
# That is why a 2 x 2 table is used instead of the binomial test:
answers <- matrix(c(34, 16, 20, 30),
  nrow = 2, byrow = TRUE,
  dimnames = list(sample = c("A", "not A"), answer = c("A", "not A"))
)
answers
chisq.test(answers, correct = FALSE)
a_not_a <- AnotA(x1 = 34, n1 = 50, x2 = 20, n2 = 50) # d' = z(hit) - z(false alarm) and Fisher's test
a_not_a
qnorm(34 / 50) - qnorm(20 / 50) # the same "by hand"

# Same-different: the assessor receives a pair and says whether the samples are the same or different.
# 50 same pairs: 32 "same", 18 "different"; 50 different pairs: 20 "same", 30 "different"
same_different <- samediff(nsamesame = 32, ndiffsame = 18, nsamediff = 20, ndiffdiff = 30)
summary(same_different) # delta = d', tau = criterion (how large a difference is already called "different")

#---------------------------------------------------------------------------------------
# 6. Replicated discrimination tests - the beta-binomial model
#---------------------------------------------------------------------------------------
# 24 assessors performed 4 triangle tests each. 96 answers are not 96 independent trials:
# some assessors perceive the difference, others do not (overdispersion).
set.seed(3)
discriminator <- rbinom(24, 1, 0.5) # half of the assessors really perceive the difference
correct <- rbinom(24, 4, ifelse(discriminator == 1, 0.8, 1 / 3))
replicates <- cbind(correct, total = 4)
replicates

binom.test(sum(correct), 96, p = 1 / 3, alternative = "greater") # naive: everything is pooled
bb <- betabin(replicates, method = "triangle")
summary(bb) # gamma = degree of overdispersion (0 = none, 1 = maximum)
# if the overdispersion is significant, the naive binomial test understates the uncertainty


# TASK1:
# =========
# In a duo-trio test, 36 of 50 assessors correctly matched the reference sample.
# Compute d' and compare it with the triangle test result above.
# Which of the two products differs more from the control?

# TASK2:
# =========
# Compute how many correct answers out of 60 would be needed in 2-AFC, 3-AFC and the triangle
# for d' to be 1.5 (hint: psyfun()).

# TASK3:
# =========
# In an A - not A test, 80 assessors received sample A and 40 received "not A". The answer "A" was given
# 64 times for sample A and 30 times for sample "not A". Is the difference significant? What would the
# conclusion be if you (wrongly) used a binomial test of 74 correct answers out of 120 with p = 1/2?
