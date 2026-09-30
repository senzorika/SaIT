# ============================================================
# Exercise 5a: Normality and two-sample comparison
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise05a.html
# ============================================================

# A group of assessors rated 5 kinds of cheese with the following results.
# Dataset: cheeses
A <- c(7.5, 6.9, 7, 7, 6.9, 7.1, 7.2, 7.5, 6.9)
B <- c(7.5, 8.1, 7.5, 7.4, 7.1, 7.5, 7.2, 7.2, 6.9)
C <- c(7, 6.1, 6.7, 6.1, 6.9, 7.1, 7.2, 7.2, 6.9)
D <- c(7.5, 6.9, 7, 7, 6.9, 7.1, 7.2, 7.2, 6.9)
E <- c(6.8, 6.9, 6.9, 7, 7, 7, 7.1, 7.1, 7.2)
tab <- data.frame(A, B, C, D, E)
boxplot(A, B, C, D, E)
boxplot(tab)

#---------------------------------------------------------------------------------------
# normality check
#---------------------------------------------------------------------------------------
qqnorm(A)
qqline(A)
plot(density(A))
shapiro.test(A)


#---------------------------------------------------------------------------------------
# PAIRWISE COMPARISON
#---------------------------------------------------------------------------------------
# dependent (paired) samples
t.test(A, E, paired = TRUE)
wilcox.test(A, E, paired = TRUE)
# independent samples
t.test(A, E)
wilcox.test(A, E) # Mann-Whitney test


#---------------------------------------------------------------------------------------
# BINOMIAL TEST
#---------------------------------------------------------------------------------------
# 60 consumers took part in a paired preference test and 34 of them marked sample A (improved recipe)
# as better tasting. Is this number sufficient to accept the hypothesis that the samples differ?
# If not, how many consumers are needed to confirm the alternative hypothesis?
# (panel size calculation: exercise 14)

binom.test(34, 60, p = 0.5) # paired preference: two-sided test (we do not know in advance which sample wins)
# discrimination tests (triangle p = 1/3, duo-trio p = 1/2) are one-sided:
# binom.test(x, n, p = 1/3, alternative = "greater")

#---------------------------------------------------------------------------------------
# CHI-SQUARE TEST
#---------------------------------------------------------------------------------------
# 90 respondents answered a questionnaire about goulash and goulash seasoning mix...

likert <- c("strongly.agree", "agree", "dont.know", "disagree", "strongly.disagree")
question1 <- c(15, 20, 20, 10, 25) # Dark beer is suitable for goulash... :)
question2 <- c(40, 25, 15, 5, 5) # I have eaten goulash at least once in my life...
question3 <- c(82, 2, 2, 2, 2) # Goulash comes from Hungary...
results <- data.frame(likert, question1, question2, question3)
chisq.test(question1)
chisq.test(question2)
chisq.test(question3)

#---------------------------------------------------------------------------------------
# MCNEMAR TEST
#---------------------------------------------------------------------------------------
# Find out whether consumers react the same way to the old and the new (modified) recipe

rum <- matrix(c(40, 8, 24, 28), nrow = 2, dimnames = list("old recipe" = c("bought", "not bought"), "new recipe" = c("bought", "not bought")))
mcnemar.test((rum), correct = FALSE)


# TASK1:
# =========
# Analyse the data and test the hypothesis that there is a statistically significant difference
# between 2 cheese samples (the best one and the original one (E)).

# TASK2:
# =========
# Find out whether a statistically significant difference was detected in an adulteration test if
# out of 100 triangle tests 42 assessors identified the odd sample...
# What would the result be if the test were carried out as a paired test?

# TASK3:
# =========
# A retail chain offers these kinds of yogurt with the following customer preferences
# (strawberry(25), chocolate(34), vanilla(17), blueberry(7)).
# Using an appropriate statistical method, find out whether there is a significant preference for any kind.
