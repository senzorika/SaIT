# ============================================================
# Cvičenie 12: JAR škála a radarový graf
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie12.html
# ============================================================

#------------------------------------------------------------
# JAR skala pre optimalizaciu produktov, Vietoris 2020
#------------------------------------------------------------

# nacitanie potrebnych kniznic
library(grid)
library(lattice)
library(latticeExtra)
library(HH)

# produkt a jeho atributy v matici
produkt <- matrix(c(
  3, 2, 10, 2, 0,
  3, 2, 12, 0, 0,
  1, 3, 9, 0, 1,
  3, 9, 2, 1, 2,
  0, 1, 8, 3, 1,
  4, 2, 10, 1, 0
), ncol = 5, byrow = TRUE)

rownames(produkt) <- c("vzhľad", "pach", "textúra", "chuť", "dochuť", "celkový dojem")
colnames(produkt) <- c("- -", "-", "JAR", "+", "+ +")
likert(produkt, main = "Vybraný produkt", sub = "")


#----------------------------------------------------
# Penalty analyza: kolko "stoji" odchylka od JAR?
#----------------------------------------------------
# 60 spotrebitelov hodnotilo jogurt: celkova prijemnost (9-bodova hedonicka skala)
# a tri znaky na JAR skale (1 = prilis malo ... 3 = akurat ... 5 = prilis vela)
set.seed(12)
n <- 60
jar <- data.frame(
  sladkost = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.15, 0.50, 0.20, 0.10)),
  kyslost = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.10, 0.70, 0.10, 0.05)),
  hustota = sample(1:5, n, replace = TRUE, prob = c(0.15, 0.30, 0.45, 0.07, 0.03))
)
# prijemnost klesa, ak je jogurt prilis sladky alebo prilis riedky
prijemnost <- 7.5 - 1.2 * pmax(jar$sladkost - 3, 0) - 0.9 * pmax(3 - jar$hustota, 0) + rnorm(n, 0, 0.8)
prijemnost <- pmin(9, pmax(1, round(prijemnost)))

# pre kazdy znak: podiel skupin "prilis malo" / "prilis vela" a pokles priemeru oproti skupine JAR
penalty <- do.call(rbind, lapply(names(jar), function(znak) {
  skupina <- cut(jar[[znak]], breaks = c(0, 2, 3, 5), labels = c("prilis malo", "JAR", "prilis vela"))
  priemer <- tapply(prijemnost, skupina, mean)
  podiel <- 100 * prop.table(table(skupina))
  data.frame(
    znak = znak, skupina = c("prilis malo", "prilis vela"),
    podiel = as.numeric(podiel[c("prilis malo", "prilis vela")]),
    pokles = as.numeric(priemer["JAR"] - priemer[c("prilis malo", "prilis vela")])
  )
}))
penalty$vazeny_pokles <- penalty$podiel / 100 * penalty$pokles
penalty

# penalty graf: kriticke su skupiny vpravo hore (vela spotrebitelov a velky pokles)
plot(penalty$podiel, penalty$pokles,
  pch = 19, col = ifelse(penalty$podiel >= 20, "red", "grey40"),
  xlim = c(0, 60), ylim = range(c(0, penalty$pokles), na.rm = TRUE) + c(-0.3, 0.3),
  xlab = "podiel spotrebitelov (%)", ylab = "pokles priemernej prijemnosti", main = "Penalty analyza"
)
text(penalty$podiel, penalty$pokles, paste(penalty$znak, "-", penalty$skupina), pos = 4, cex = 0.8)
abline(v = 20, lty = 2) # pravidlo 20 %: mensie skupiny sa nevyhodnocuju
abline(h = 0, lty = 3)

# je pokles preukazny? (len pre skupiny s podielom aspon 20 %)
t.test(prijemnost[jar$sladkost > 3], prijemnost[jar$sladkost == 3])
t.test(prijemnost[jar$hustota < 3], prijemnost[jar$hustota == 3])


#----------------------------------------------------
# vizualizacia pavucinoveho grafu (profilogram)
#----------------------------------------------------

# nacitanie kniznice
library(fmsb)

# dataset produktov (A-D)
A <- c(5.2, 4.5, 3.8, 8, 8)
B <- c(5, 4, 3.8, 8, 8)
C <- c(7, 4.8, 5.8, 8, 8)
D <- c(1, 2, 3, 4, 5)
data <- t(data.frame(A, B, C, D))
colnames(data) <- c("Vzhľad", "Textúra", "Pach", "Chuť", "Dochuť")
rownames(data) <- paste("Produkt", LETTERS[1:4], sep = "_")
data <- rbind(rep(9, 5), rep(0, 5), data)
data <- data.frame(data)
farby <- c(2:6)

# vykreslenie a nastavenie radaroveho grafu (skuste sa s parametrami pohrat :)
radarchart(data, axistype = 0, vlcex = 0.8, plwd = 2, plty = 1, pcol = farby)
legend(x = 1, y = 1.2, legend = rownames(data[-c(1, 2), ]), bty = "n", pch = 20, col = farby, text.col = "black", cex = 0.7, pt.cex = 3)


# ULOHA1:
# =========
# Ktory znak jogurtu treba upravit ako prvy a ktorym smerom? Zdovodnite podielom skupiny,
# poklesom priemeru a vazenym poklesom. Ktore skupiny nesplnaju pravidlo 20 %?

# ULOHA2:
# =========
# Overte si vypocet pre sladkost v kalkulatore penalty analyzy:
# https://senzorika.github.io/SAP/kapitoly/08_spotrebitelska_veda.html#kalkulator
