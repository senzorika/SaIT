# ============================================================
# Cvičenie 23: Waldova sekvenčná analýza
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie23.html
# ============================================================

# install.packages("sensR")
library(sensR)

#---------------------------------------------------------------------------------------
# 1. Hranice sekvencneho testu (SPRT, ISO 16820)
#---------------------------------------------------------------------------------------
# Trojuholnikovy test vyhodnocovany po kazdej odpovedi:
# H0: p = p0 (hadanie), H1: p = p1 (rozdiel, ktory chceme odhalit)
p0 <- 1 / 3
p1 <- 0.5
alfa <- 0.05 # riziko, ze vyhlasime rozdiel, ktory neexistuje
beta <- 0.20 # riziko, ze existujuci rozdiel neodhalime

D <- log(p1 / p0) + log((1 - p0) / (1 - p1))
sklon <- log((1 - p0) / (1 - p1)) / D
horna <- function(n) log((1 - beta) / alfa) / D + sklon * n # nad nou: rozdiel preukazany
dolna <- function(n) -log((1 - alfa) / beta) / D + sklon * n # pod nou: rozdiel nepreukazany
round(c(sklon = sklon, horna_0 = horna(0), dolna_0 = dolna(0)), 3)
c(horna(20), dolna(20)) # pri n = 20: aspon 13 spravnych -> rozdiel, najviac 6 -> bez rozdielu

#---------------------------------------------------------------------------------------
# 2. Priebeh testu - rozhodnutie po kazdom hodnotitelovi
#---------------------------------------------------------------------------------------
odpovede <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1) # 1 = spravna odpoved
n <- seq_along(odpovede)
spravne <- cumsum(odpovede)
stav <- ifelse(spravne >= horna(n), "rozdiel",
  ifelse(spravne <= dolna(n), "bez rozdielu", "pokracovat")
)
priebeh <- data.frame(n, spravne, dolna = round(dolna(n), 2), horna = round(horna(n), 2), stav)
priebeh
koniec <- which(stav != "pokracovat")[1] # prvy hodnotitel, pri ktorom test konci
priebeh[koniec, ]

plot(n, spravne,
  type = "s", lwd = 2, ylim = c(-2, 16),
  xlab = "počet hodnotiteľov", ylab = "kumulatívny počet správnych odpovedí", main = "Sekvenčný trojuholníkový test"
)
abline(a = horna(0), b = sklon, col = "red", lwd = 2)
abline(a = dolna(0), b = sklon, col = "darkgreen", lwd = 2)
points(koniec, spravne[koniec], pch = 19, col = "red", cex = 1.5)
legend("topleft",
  legend = c("rozdiel preukázaný", "rozdiel nepreukázaný"),
  col = c("red", "darkgreen"), lwd = 2, bty = "n"
)

#---------------------------------------------------------------------------------------
# 3. Kolko hodnotitelov sekvencny test usetri?
#---------------------------------------------------------------------------------------
sekvencny_test <- function(p, max_n = 500) {
  spravne <- 0
  for (i in 1:max_n) {
    spravne <- spravne + rbinom(1, 1, p)
    if (spravne >= horna(i)) {
      return(c(n = i, rozdiel = 1))
    }
    if (spravne <= dolna(i)) {
      return(c(n = i, rozdiel = 0))
    }
  }
  c(n = max_n, rozdiel = NA)
}
set.seed(23)
sim_H1 <- replicate(5000, sekvencny_test(p1)) # rozdiel naozaj existuje
sim_H0 <- replicate(5000, sekvencny_test(p0)) # hodnotitelia len hadaju
rbind(
  H1 = c(priemerne_n = mean(sim_H1["n", ]), podiel_rozdiel = mean(sim_H1["rozdiel", ])),
  H0 = c(priemerne_n = mean(sim_H0["n", ]), podiel_rozdiel = mean(sim_H0["rozdiel", ]))
)
quantile(sim_H1["n", ], c(0.5, 0.9, 0.99)) # test sa moze aj natiahnut

# klasicky test s pevnym poctom hodnotitelov pri rovnakych rizikach (pc = 0.5 -> pd = 0.25)
discrimSS(pdA = (p1 - p0) / (1 - p0), target.power = 1 - beta, alpha = alfa, pGuess = p0)


# ULOHA1:
# =========
# Vypocitajte hranice pre duo-trio test (p0 = 1/2, p1 = 0.7, alfa = 0.05, beta = 0.10).
# Po kolkych hodnotiteloch najskor moze test skoncit zaverom "rozdiel" a po kolkych "bez rozdielu"?

# ULOHA2:
# =========
# Prvych 12 odpovedi v trojuholnikovom teste: 0 1 0 0 1 0 0 0 1 0 0 0. Ako test dopadne?

# ULOHA3:
# =========
# Zmente p1 na 0.45 (mensi rozdiel). Ako sa zmenia hranice a priemerny pocet hodnotitelov?
# Hranice si overte v kapitole: https://senzorika.github.io/SAP/kapitoly/06_skalovanie.html#wald
