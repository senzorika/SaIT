# ============================================================
# Exercise 24: Conjoint analysis
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise24.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Product profiles - full factorial design
#---------------------------------------------------------------------------------------
# A yogurt is described by three attributes. The consumer does not rate attributes one by one,
# but whole profiles.
profiles <- expand.grid(
  fat = c("0.1 %", "3.5 %"),
  flavour = c("strawberry", "vanilla", "chocolate"),
  price = c("0.59 EUR", "0.79 EUR", "0.99 EUR")
)
nrow(profiles) # 2 x 3 x 3 = 18 profiles
head(profiles)

#---------------------------------------------------------------------------------------
# 2. Ratings - simulation of 40 consumers (9-point purchase intent scale)
#---------------------------------------------------------------------------------------
# two segments: "price" (price-sensitive) and "taste" (want a full-fat chocolate yogurt)
set.seed(24)
n <- 40
segment <- rep(c("price", "taste"), each = n / 2)
utility <- function(profile, seg) {
  u_fat <- c("0.1 %" = -0.3, "3.5 %" = 0.3) * ifelse(seg == "taste", 3, 1)
  u_flavour <- c(strawberry = 0.4, vanilla = -0.6, chocolate = 0.2) + c(0, 0, ifelse(seg == "taste", 1, 0))
  u_price <- c("0.59 EUR" = 1, "0.79 EUR" = 0, "0.99 EUR" = -1) * ifelse(seg == "price", 1.8, 0.4)
  u_fat[as.character(profile$fat)] + u_flavour[as.character(profile$flavour)] + u_price[as.character(profile$price)]
}
ratings <- do.call(rbind, lapply(1:n, function(i) {
  score <- 5 + utility(profiles, segment[i]) + rnorm(nrow(profiles), 0, 0.8)
  data.frame(respondent = factor(i), profiles, rating = pmin(9, pmax(1, round(score))))
}))
head(ratings)

#---------------------------------------------------------------------------------------
# 3. Part-worth utilities - regression on the attribute levels
#---------------------------------------------------------------------------------------
# sum contrasts: the utilities of the levels of one attribute sum to zero
contrasts_sum <- list(fat = "contr.sum", flavour = "contr.sum", price = "contr.sum")
model <- lm(rating ~ fat + flavour + price, data = ratings, contrasts = contrasts_sum)
part_worths <- dummy.coef(model)[c("fat", "flavour", "price")]
lapply(part_worths, round, 2)

# relative importance of an attribute = range of its utilities / sum of the ranges
ranges <- sapply(part_worths, function(u) diff(range(u)))
round(100 * ranges / sum(ranges), 1)

barplot(unlist(part_worths),
  las = 2, cex.names = 0.7, col = rep(c("grey70", "orange", "steelblue"), c(2, 3, 3)),
  ylab = "part-worth utility", main = "Conjoint - part-worth utilities"
)
abline(h = 0)

#---------------------------------------------------------------------------------------
# 4. Individual utilities and segmentation
#---------------------------------------------------------------------------------------
individual <- t(sapply(levels(ratings$respondent), function(r) {
  m <- lm(rating ~ fat + flavour + price, data = subset(ratings, respondent == r), contrasts = contrasts_sum)
  unlist(dummy.coef(m)[c("fat", "flavour", "price")])
}))
round(head(individual), 2)

# cluster analysis of the individual utilities (see exercise 8)
clusters <- kmeans(individual, centers = 2, nstart = 25)
table(cluster = clusters$cluster, true_segment = segment)
round(clusters$centers, 2)

#---------------------------------------------------------------------------------------
# 5. Market simulation - which concept would consumers choose?
#---------------------------------------------------------------------------------------
concepts <- data.frame(
  fat = c("3.5 %", "0.1 %", "3.5 %"),
  flavour = c("chocolate", "strawberry", "strawberry"),
  price = c("0.99 EUR", "0.59 EUR", "0.79 EUR"),
  row.names = c("Premium", "Light", "Classic")
)
# total utility of a concept for every respondent = sum of their part-worth utilities
total <- sapply(rownames(concepts), function(k) {
  individual[, paste0("fat.", concepts[k, "fat"])] +
    individual[, paste0("flavour.", concepts[k, "flavour"])] +
    individual[, paste0("price.", concepts[k, "price"])]
})
# share of first choice: every respondent "buys" the concept with the highest utility
round(100 * prop.table(table(colnames(total)[apply(total, 1, which.max)])), 1)


# TASK1:
# =========
# Which attribute is the most important for consumers in the whole sample and which in the individual clusters?
# (hint: compute the relative importance from the cluster centres)

# TASK2:
# =========
# Build the yogurt with the highest total utility for each cluster. By how much does the utility drop
# if you raise the price from 0.59 to 0.99 EUR?

# TASK3:
# =========
# Add a fourth concept "Promo" (3.5 %, chocolate, 0.59 EUR). How do the shares of first choice change?
# Compare with the TURF analysis in exercise 11a - which question does TURF answer and which does conjoint?
# Theory of the method (in Slovak): https://senzorika.github.io/SAP/kapitoly/08_spotrebitelska_veda.html
