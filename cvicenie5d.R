# ============================================================
# Cvičenie 5d: Durbinov test a neúplné bloky
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie05d.html
# ============================================================

# Durbin test priklad, VV 2020
# nacitanie kniznice (PMCMR bol z CRAN odstraneny, nahradou je PMCMRplus)
library(PMCMRplus)

# priklad kompletneho bloku
y <- matrix(
  c(
    3.88, 5.64, 5.76, 4.25, 5.91, 4.33, 30.58, 30.14, 16.92,
    23.19, 26.74, 10.91, 25.24, 33.52, 25.45, 18.85, 20.45,
    26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 30.72,
    32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23,
    26.65
  ),
  nrow = 6, ncol = 6,
  dimnames = list(1:6, LETTERS[1:6])
)
print(y)
friedman.test(y)
durbinTest(y)


## Priklad nekompletneho bloku - Durbin test
y <- matrix(
  c(
    2, NA, NA, NA, 3, NA, 3, 3, 3, NA, NA, NA, 3, NA, NA,
    1, 2, NA, NA, NA, 1, 1, NA, 1, 1,
    NA, NA, NA, NA, 2, NA, 2, 1, NA, NA, NA, NA,
    3, NA, 2, 1, NA, NA, NA, NA, 3, NA, 2, 2
  ),
  ncol = 7, nrow = 7, byrow = FALSE,
  dimnames = list(1:7, LETTERS[1:7])
)
print(y)
durbinTest(y)
durbinAllPairsTest(y, p.adjust.method = "none")
