# ============================================================
# Cvičenie 18: Temporálne metódy – TDS a TCATA
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie18.html
# ============================================================

# Zber dat: aplikacie Senzometricke_appky/TDS_app.R a TCATA_app.R

#---------------------------------------------------------------------------------------
# 1. TDS (Temporal Dominance of Sensations) - simulacia 30 hodnotitelov, 60 s, 5 atributov
#---------------------------------------------------------------------------------------
set.seed(18)
atributy <- c("sladky", "kysly", "horky", "ovocny", "adstringentny")
cas <- 0:60
n <- 30
# vaha atributu v case (sladkost na zaciatku, horkost a adstringencia na konci)
vaha <- cbind(
  sladky = dnorm(cas, 8, 8), kysly = dnorm(cas, 18, 8), horky = dnorm(cas, 40, 12),
  ovocny = dnorm(cas, 22, 10), adstringentny = dnorm(cas, 52, 10)
)
colnames(vaha) <- atributy
tds <- t(sapply(1:n, function(h) apply(vaha, 1, function(w) sample(atributy, 1, prob = w + 1e-4))))
dim(tds) # hodnotitelia x casove body, v bunke je dominantny atribut

# miera dominancie: podiel hodnotitelov, ktori dany atribut oznacili ako dominantny
dominancia <- sapply(atributy, function(a) colMeans(tds == a))

# hladina nahody P0 = 1/k a hranica vyznamnosti (Pineau et al., 2009)
P0 <- 1 / length(atributy)
hranica <- P0 + 1.645 * sqrt(P0 * (1 - P0) / n)

farby <- c("orange", "gold3", "brown", "purple", "darkgreen")
matplot(cas, dominancia,
  type = "l", lty = 1, lwd = 2, col = farby, ylim = c(0, 1),
  xlab = "cas (s)", ylab = "miera dominancie", main = "TDS krivky"
)
abline(h = P0, lty = 3)
abline(h = hranica, lty = 2, col = "red")
legend("topright", legend = atributy, col = farby, lwd = 2, bty = "n")

#---------------------------------------------------------------------------------------
# 2. TCATA (Temporal Check-All-That-Apply) - dva produkty
#---------------------------------------------------------------------------------------
# kazdy hodnotitel moze mat zaskrtnutych viac atributov naraz
sim_tcata <- function(posun) {
  sapply(atributy, function(a) {
    w <- vaha[, a] / max(vaha[, a])
    if (a == "horky") w <- pmin(1, w * posun)
    colMeans(matrix(rbinom(n * length(cas), 1, 0.8 * w), nrow = n))
  })
}
tcata_A <- sim_tcata(1)
tcata_B <- sim_tcata(0.4) # produkt B je menej horky

matplot(cas, tcata_A,
  type = "l", lty = 1, lwd = 2, col = farby, ylim = c(0, 1),
  xlab = "cas (s)", ylab = "podiel citacii", main = "TCATA - produkt A (plne) vs. B (ciarkovane)"
)
matlines(cas, tcata_B, lty = 2, lwd = 2, col = farby)
legend("topright", legend = atributy, col = farby, lwd = 2, bty = "n")

# kedy sa produkty preukazne lisia v horkosti? (Fisherov test v kazdom casovom bode)
p_horky <- sapply(seq_along(cas), function(t) {
  x <- round(c(tcata_A[t, "horky"], tcata_B[t, "horky"]) * n)
  fisher.test(matrix(c(x, n - x), nrow = 2))$p.value
})
cas[p_horky < 0.05]


# ULOHA1:
# =========
# V ktorom casovom intervale je sladkost preukazne dominantna? Kedy dominanciu preberie horkost?

# ULOHA2:
# =========
# Zmerajte TDS vlastneho produktu v aplikacii TDS_app.R, nacitajte exportovane data a nakreslite TDS krivky.
