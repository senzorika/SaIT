# ============================================================
# Cvičenie 10: Analýza prežitia a senzorická trvanlivosť
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie10.html
# ============================================================

# ------------------------------------------------------------------------
# Analyza prezitia (Survival analysis, Kaplan-Meier model) (Vietoris,2013)
# + predikcne modelovanie
# ------------------------------------------------------------------------

# odhad senzorickej trvanlivosti pomocou neparametrickej analyzy prezitia podla Kaplan-Meiera
# Piati hodnotitelia analyzovali vzorky jogurtu skladovane 0,4,8,12,24,36 a 48 hodin pri izbovej teplote.
# Vysledkom je skala (zamietnutia/nezjedol) a (akceptacie/zjedol). Aka je odhadovana senzoricka trvanlivost
# jogurtov, resp. kedy uz bude jogurt neakceptovatelny po senzorickej stranke?
# ----------------------------------------------------------------------------------------
# Nacitanie kniznice pre analyzu prezitia
library(survival)
time <- c(0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48, 0, 4, 8, 12, 24, 36, 48)
event <- c(
  TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
  TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE
)

# vytvorenie datasetu a nastavenie zakladnych parametrov
foodshelflife <- Surv(time, event)
foodshelflife

fit <- survfit(foodshelflife ~ 1, conf.int = FALSE)
# vysledky a vizualizacia grafu
fit
summary(fit)
plot(fit, main = "Senzorická stabilita produktu (A)", xlab = " čas(h)", col = "blue")
abline(h = 0.5, col = "red")


# -----------------------------------------------------
# + LINEARNA REGRESIA (Vietoris,2020)
# -----------------------------------------------------
# ideme predikovat cut-off point pre jogurty z predchadzajuceho prikladu
# kde x su casove data (hodiny) a y (je survival hodnota)
y <- fit$surv
x <- fit$time
linear <- data.frame(x, y)
linear

# linearny model: cas ako funkcia prezivania, x = a*y + b
regresia <- lm(x ~ y)
regresia

# a ideme programovat nasu prvu funkciu v zivote :)
model <- function(odhad) {
  regresia$coefficients[2] * odhad + regresia$coefficients[1]
}

# a je este potrebne zistit koeficient determinacie (r2) celeho modelu a sme za vodou :)
reg <- lm(x ~ y)
summary(reg)

# overenie linearneho modelu, ideme zistit kolko hodin je cut-off point (hodnota=0.5)
model(0.5)
abline(v = model(0.5), col = "red")

# dokreslime si nejaky vizual :)
abline(v = 0, col = "green")
rect(0, 1, model(0.5), 0, density = 5, col = "green", border = "transparent")
text(model(0.5), 0.52, round(model(0.5), digit = 2), pos = 4)


# -----------------------------------------------------
# + INTERVALOVO CENZUROVANE DATA A WEIBULLOV MODEL (Hough, 2010; ISO 16779)
# -----------------------------------------------------
# Priklad vyssie berie kazde z 35 hodnoteni ako samostatne pozorovanie. V spotrebitelskej
# studii vsak kazdy spotrebitel hodnoti vsetky casy skladovania a presny cas odmietnutia
# nepozname - vieme len, medzi ktorymi dvoma casmi nastal (intervalova cenzura):
#   akceptoval pri 12 h, odmietol pri 24 h -> interval (12, 24)
#   odmietol uz pri 4 h                    -> interval (NA, 4)   lava cenzura
#   akceptoval vsetko do 48 h              -> interval (48, NA)  prava cenzura
# 42 spotrebitelov, jogurt skladovany 4, 8, 12, 24, 36 a 48 hodin:
dolna <- c(rep(NA, 2), rep(4, 2), rep(8, 5), rep(12, 9), rep(24, 11), rep(36, 7), rep(48, 6))
horna <- c(rep(4, 2), rep(8, 2), rep(12, 5), rep(24, 9), rep(36, 11), rep(48, 7), rep(NA, 6))
intervaly <- Surv(dolna, horna, type = "interval2")
intervaly

# parametricky odhad: Weibullovo rozdelenie casu odmietnutia
weibull <- survreg(intervaly ~ 1, dist = "weibull")
summary(weibull)
eta <- exp(coef(weibull)) # parameter mierky (h)
beta <- 1 / weibull$scale # parameter tvaru; beta > 1 = riziko odmietnutia s casom rastie
c(eta = unname(eta), beta = beta)

# cas, ked produkt odmietne 10, 25 a 50 % spotrebitelov
percentily <- predict(weibull,
  newdata = data.frame(x = 1), type = "quantile",
  p = c(0.10, 0.25, 0.50), se.fit = TRUE
)
odhad <- as.numeric(percentily$fit)
se <- as.numeric(percentily$se.fit)
data.frame(
  odmietnutie = c("10 %", "25 %", "50 %"), cas_h = round(odhad, 1),
  dolna_95 = round(odhad - 1.96 * se, 1), horna_95 = round(odhad + 1.96 * se, 1)
)

# neparametricky (Turnbullov) odhad a Weibullova krivka v jednom grafe
plot(survfit(intervaly ~ 1),
  conf.int = FALSE, xlab = "čas (h)", ylab = "podiel akceptujúcich",
  main = "Senzorická trvanlivosť – intervalovo cenzurované dáta"
)
curve(exp(-(x / eta)^beta), from = 0, to = 60, add = TRUE, col = "blue", lwd = 2)
abline(h = 0.5, col = "red", lty = 2)


# -----------------------------------------------------
# + ZRYCHLENE SKLADOVANIE: Q10 A ARRHENIUSOVA ROVNICA
# -----------------------------------------------------
# median senzorickej trvanlivosti (dni) zisteny pri troch teplotach skladovania
teplota <- c(5, 15, 25) # °C
trvanlivost <- c(28, 13, 6) # dni

# Q10: kolkokrat sa trvanlivost skrati pri zvyseni teploty o 10 °C
Q10 <- (trvanlivost[1] / trvanlivost[3])^(10 / (teplota[3] - teplota[1]))
Q10

# Arrhenius: ln(k) = ln(A) - Ea / (R * T), rychlost zmeny k ~ 1 / trvanlivost, T v kelvinoch
T_K <- teplota + 273.15
arrhenius <- lm(log(1 / trvanlivost) ~ I(1 / T_K))
Ea <- -coef(arrhenius)[2] * 8.314 / 1000 # aktivacna energia v kJ/mol
unname(Ea)

# predikcia trvanlivosti pri 8 °C (chladnicka v obchode)
1 / exp(predict(arrhenius, newdata = data.frame(T_K = 8 + 273.15)))


# ULOHA1:
# =========
# Vyrobca chce, aby jogurt v case spotreby odmietlo najviac 25 % spotrebitelov.
# Aku dobu spotreby odporucite podla Weibullovho modelu? Porovnajte s medianom.

# ULOHA2:
# =========
# Vypocitajte Q10 z dvojice teplot 5 a 15 °C a z dvojice 15 a 25 °C. Je Q10 konstantne?
# Vysledok si overte v kalkulatore: https://senzorika.github.io/SAP/kapitoly/10_shelf_life.html#kalkulator
