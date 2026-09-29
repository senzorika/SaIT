# ============================================================
# Exercise 12: JAR scale and radar chart
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise12.html
# ============================================================

#------------------------------------------------------------
# JAR scale for product optimisation, Vietoris 2020
#------------------------------------------------------------

# load the required packages
require(grid)
require(lattice)
require(latticeExtra)
require(HH)

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
