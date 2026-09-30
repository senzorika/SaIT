# ============================================================
# Exercise 12: JAR scale and radar chart
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise12.html
# ============================================================

#------------------------------------------------------------
# JAR scale for product optimisation, Vietoris 2020
#------------------------------------------------------------

# load the required packages
library(grid)
library(lattice)
library(latticeExtra)
library(HH)

# product attributes in a matrix
product <- matrix(c(
  3, 2, 10, 2, 0,
  3, 2, 12, 0, 0,
  1, 3, 9, 0, 1,
  3, 9, 2, 1, 2,
  0, 1, 8, 3, 1,
  4, 2, 10, 1, 0
), ncol = 5, byrow = TRUE)

rownames(product) <- c("appearance", "odour", "texture", "taste", "aftertaste", "overall impression")
colnames(product) <- c("- -", "-", "JAR", "+", "+ +")
likert(product, main = "Selected product", sub = "")


#----------------------------------------------------
# Penalty analysis: what does a deviation from JAR "cost"?
#----------------------------------------------------
# 60 consumers rated a yogurt: overall liking (9-point hedonic scale)
# and three attributes on a JAR scale (1 = much too little ... 3 = just about right ... 5 = much too much)
set.seed(12)
n <- 60
jar <- data.frame(
  sweetness = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.15, 0.50, 0.20, 0.10)),
  sourness = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.10, 0.70, 0.10, 0.05)),
  thickness = sample(1:5, n, replace = TRUE, prob = c(0.15, 0.30, 0.45, 0.07, 0.03))
)
# liking drops when the yogurt is too sweet or too thin
liking <- 7.5 - 1.2 * pmax(jar$sweetness - 3, 0) - 0.9 * pmax(3 - jar$thickness, 0) + rnorm(n, 0, 0.8)
liking <- pmin(9, pmax(1, round(liking)))

# for each attribute: share of the "too little" / "too much" groups and the mean drop versus the JAR group
penalty <- do.call(rbind, lapply(names(jar), function(attribute) {
  group <- cut(jar[[attribute]], breaks = c(0, 2, 3, 5), labels = c("too little", "JAR", "too much"))
  group_mean <- tapply(liking, group, mean)
  share <- 100 * prop.table(table(group))
  data.frame(
    attribute = attribute, group = c("too little", "too much"),
    share = as.numeric(share[c("too little", "too much")]),
    mean_drop = as.numeric(group_mean["JAR"] - group_mean[c("too little", "too much")])
  )
}))
penalty$weighted_drop <- penalty$share / 100 * penalty$mean_drop
penalty

# penalty plot: the critical groups are top right (many consumers and a large drop)
plot(penalty$share, penalty$mean_drop,
  pch = 19, col = ifelse(penalty$share >= 20, "red", "grey40"),
  xlim = c(0, 60), ylim = range(c(0, penalty$mean_drop), na.rm = TRUE) + c(-0.3, 0.3),
  xlab = "share of consumers (%)", ylab = "drop in mean liking", main = "Penalty analysis"
)
text(penalty$share, penalty$mean_drop, paste(penalty$attribute, "-", penalty$group), pos = 4, cex = 0.8)
abline(v = 20, lty = 2) # 20 % rule: smaller groups are not interpreted
abline(h = 0, lty = 3)

# is the drop significant? (only for groups with a share of at least 20 %)
t.test(liking[jar$sweetness > 3], liking[jar$sweetness == 3])
t.test(liking[jar$thickness < 3], liking[jar$thickness == 3])


#----------------------------------------------------
# radar (spider) chart visualisation (profilogram)
#----------------------------------------------------

# load the package
library(fmsb)

# product dataset (A-D)
A <- c(5.2, 4.5, 3.8, 8, 8)
B <- c(5, 4, 3.8, 8, 8)
C <- c(7, 4.8, 5.8, 8, 8)
D <- c(1, 2, 3, 4, 5)
data <- t(data.frame(A, B, C, D))
colnames(data) <- c("Appearance", "Texture", "Odour", "Taste", "Aftertaste")
rownames(data) <- paste("Product", LETTERS[1:4], sep = "_")
data <- rbind(rep(9, 5), rep(0, 5), data)
data <- data.frame(data)
colours <- c(2:6)

# draw and set up the radar chart (try playing with the parameters :)
radarchart(data, axistype = 0, vlcex = 0.8, plwd = 2, plty = 1, pcol = colours)
legend(x = 1, y = 1.2, legend = rownames(data[-c(1, 2), ]), bty = "n", pch = 20, col = colours, text.col = "black", cex = 0.7, pt.cex = 3)


# TASK1:
# =========
# Which attribute of the yogurt should be adjusted first, and in which direction? Justify it with
# the group share, the mean drop and the weighted drop. Which groups fail the 20 % rule?

# TASK2:
# =========
# Check the calculation for sweetness in the penalty analysis calculator (in Slovak):
# https://senzorika.github.io/SAP/kapitoly/08_spotrebitelska_veda.html#kalkulator
