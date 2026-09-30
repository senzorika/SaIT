# ============================================================
# Cvičenie 22: Prah citlivosti – 3-AFC a metóda BET
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie22.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Metoda najlepsieho odhadu (BET, Best Estimate Threshold)
#---------------------------------------------------------------------------------------
# Vzostupny rad koncentracii sacharozy (g/l). Pri kazdej koncentracii skuska 3-AFC:
# dve vzorky vody a jedna s latkou, 1 = hodnotitel urcil vzorku s latkou spravne.
koncentracie <- c(0.5, 1, 2, 4, 8, 16)
odpovede <- rbind(
  H1 = c(0, 0, 1, 1, 1, 1),
  H2 = c(0, 1, 0, 1, 1, 1),
  H3 = c(0, 0, 0, 1, 1, 1),
  H4 = c(1, 1, 1, 1, 1, 1),
  H5 = c(0, 0, 1, 0, 0, 1),
  H6 = c(0, 0, 0, 0, 1, 1)
)
colnames(odpovede) <- koncentracie
odpovede

# individualny prah = geometricky priemer najvyssej nerozpoznanej a nasledujucej vyssej koncentracie
bet <- function(x, konc) {
  k <- length(konc)
  chyby <- which(x == 0)
  if (length(chyby) == 0) {
    return(konc[1] / sqrt(konc[2] / konc[1])) # vsade spravne: pol kroka pod radom
  }
  posledna <- max(chyby)
  if (posledna == k) {
    return(konc[k] * sqrt(konc[k] / konc[k - 1])) # chyba pri najvyssej: pol kroka nad radom
  }
  sqrt(konc[posledna] * konc[posledna + 1])
}
prahy <- apply(odpovede, 1, bet, konc = koncentracie)
round(prahy, 2)

# skupinovy prah = geometricky priemer individualnych prahov
skupinovy_prah <- exp(mean(log(prahy)))
skupinovy_prah
sd(log10(prahy)) # variabilita citlivosti v paneli (v log10 jednotkach)

#---------------------------------------------------------------------------------------
# 2. Psychometricka funkcia - prah z podielu spravnych odpovedi
#---------------------------------------------------------------------------------------
# 30 hodnotitelov pri kazdej koncentracii. Pri 3-AFC uhadne tretina aj bez vnemu, preto
# P(spravne) = 1/3 + 2/3 * P(detekcia); prah = koncentracia, ktoru deteguje 50 % panelu.
set.seed(22)
n <- 30
p_detekcia <- plogis(1.8 * (log2(koncentracie) - log2(3)))
spravne <- rbinom(length(koncentracie), n, 1 / 3 + 2 / 3 * p_detekcia)
data.frame(koncentracie, spravne, podiel = round(spravne / n, 2))

# odhad parametrov metodou maximalnej vierohodnosti
neg_loglik <- function(par) {
  p <- 1 / 3 + 2 / 3 * plogis(par[1] + par[2] * log2(koncentracie))
  -sum(dbinom(spravne, n, p, log = TRUE))
}
odhad <- optim(c(0, 1), neg_loglik)
prah_model <- 2^(-odhad$par[1] / odhad$par[2]) # detekcia 50 %, t. j. 2/3 spravnych odpovedi
prah_model

plot(koncentracie, spravne / n,
  log = "x", pch = 19, ylim = c(0, 1),
  xlab = "koncentrácia (g/l)", ylab = "podiel správnych odpovedí", main = "Psychometrická funkcia 3-AFC"
)
curve(1 / 3 + 2 / 3 * plogis(odhad$par[1] + odhad$par[2] * log2(x)), add = TRUE, col = "blue", lwd = 2)
abline(h = 1 / 3, lty = 3) # uroven nahody
abline(h = 2 / 3, lty = 2, col = "red") # 50 % detekcia
abline(v = prah_model, lty = 2, col = "red")


# ULOHA1:
# =========
# Ktory hodnotitel je najcitlivejsi a ktory najmenej? Pri ktorych hodnotiteloch je odhad prahu
# nespolahlivy a ako by ste rad koncentracii upravili?

# ULOHA2:
# =========
# Hodnotitel H5 odpovedal 0, 0, 1, 0, 0, 1. Preco sa spravna odpoved pri 2 g/l nepocita?
# Aka je pravdepodobnost, ze hodnotitel bez vnemu uhadne tri skusky 3-AFC po sebe?

# ULOHA3:
# =========
# Overte individualne prahy v kalkulatore: https://senzorika.github.io/SAP/kapitoly/01_uvod_vnimanie_chuti.html#kalkulator
# Porovnajte skupinovy prah metodou BET s prahom z psychometrickej funkcie - preco sa nemusia zhodovat?
