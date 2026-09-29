# ============================================================
# Exercise 15: Sensory panel performance
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise15.html
# ============================================================

# install.packages("SensoMineR")
library(SensoMineR)

# Dataset: 29 assessors, 6 chocolates, 2 sessions (replicates), 14 descriptors
data(chocolates)
str(sensochoc)
sensochoc$Panelist <- factor(sensochoc$Panelist)
sensochoc$Session <- factor(sensochoc$Session)

#---------------------------------------------------------------------------------------
# 1. Performance of the whole panel - ANOVA for each descriptor
#---------------------------------------------------------------------------------------
perf <- panelperf(sensochoc,
  firstvar = 5,
  formul = "~Product+Panelist+Session+Product:Panelist+Product:Session+Panelist:Session"
)
# p-values of the effects for each descriptor, sorted by product discrimination
coltable(magicsort(perf$p.value, sort.mat = perf$p.value[, 1], bycol = FALSE, method = "median"),
  main.title = "Panel performance (p-values)"
)
# Product - the panel discriminates the products (we want a small p)
# Product:Panelist - assessors disagree (we want a large p)
# Session - shift between replicates (we want a large p)

#---------------------------------------------------------------------------------------
# 2. Performance of individual assessors
#---------------------------------------------------------------------------------------
indiv <- paneliperf(sensochoc,
  formul = "~Product+Panelist+Product:Panelist",
  formul.j = "~Product", col.j = 1, firstvar = 5, lastvar = 12,
  synthesis = FALSE, graph = FALSE
)
# p-values of product discrimination for each assessor
coltable(magicsort(indiv$prob.ind, method = "median"), main.title = "Discrimination - assessors")
# agreement of the assessor with the panel (correlation)
coltable(magicsort(indiv$agree, sort.mat = indiv$agree, method = "median"),
  main.title = "Agreement with the panel", level.lower = 0.2, col.lower = "grey"
)

#---------------------------------------------------------------------------------------
# 3. The same indicators "by hand" for one descriptor (CocoaA - cocoa aroma)
#---------------------------------------------------------------------------------------
means <- tapply(sensochoc$CocoaA, list(sensochoc$Product, sensochoc$Panelist), mean)
panel <- rowMeans(means)

# agreement: correlation of the assessor with the panel mean
agreement <- apply(means, 2, function(h) cor(h, panel))
sort(round(agreement, 2))

# repeatability: correlation between session 1 and 2 of the same assessor
s1 <- tapply(
  sensochoc$CocoaA[sensochoc$Session == 1],
  list(sensochoc$Product[sensochoc$Session == 1], sensochoc$Panelist[sensochoc$Session == 1]), mean
)
s2 <- tapply(
  sensochoc$CocoaA[sensochoc$Session == 2],
  list(sensochoc$Product[sensochoc$Session == 2], sensochoc$Panelist[sensochoc$Session == 2]), mean
)
repeatability <- sapply(colnames(s1), function(h) cor(s1[, h], s2[, h]))

plot(agreement, repeatability[names(agreement)],
  pch = 19, col = "steelblue",
  xlab = "agreement with the panel (r)", ylab = "repeatability (r)", xlim = c(-1, 1), ylim = c(-1, 1),
  main = "Assessors - CocoaA"
)
text(agreement, repeatability[names(agreement)], names(agreement), pos = 3, cex = 0.7)
abline(h = 0.5, v = 0.5, lty = 2, col = "red")


# TASK1:
# =========
# Which three descriptors does the panel discriminate best, and for which do assessors agree least?

# TASK2:
# =========
# Select the assessors whose agreement and repeatability for CocoaA are both below 0.5.
# How does the product p-value in the ANOVA change if you leave them out of the data?
