# ============================================================
# Cvičenie 20: Kontrolné prípadové štúdie II
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie20.html
# ============================================================
# Riesenie (R skript + kratky slovny zaver ku kazdej studii) poslite na: senzorickelaboratoriumfbp@gmail.com
# Predmet spravy: SaIT - cvicenie 20 - Meno Priezvisko

#---------------------------------------------------------------------------------------
# PRIPADOVA STUDIA A (lahka): Prichute a sladkost
#---------------------------------------------------------------------------------------
# Mliekaren planuje novu radu ochuteneho mlieka. 120 spotrebitelov vybralo najoblubenejsiu
# z troch prichuti. Tí, co vybrali jahodovu, ju potom hodnotili na JAR skale sladkosti.
#
# Ulohy:
# 1. Je preferencia prichuti rovnomerna? Vyberte a vypocitajte test.
# 2. Vypocitajte podiely JAR kategorii a nakreslite graf.
# 3. Odporucte, ci a ako upravit sladkost jahodoveho mlieka.

prichute <- c(jahoda = 58, broskyna = 41, mango = 21)
jar_sladkost <- c("--" = 2, "-" = 5, "JAR" = 25, "+" = 17, "++" = 9)


#---------------------------------------------------------------------------------------
# PRIPADOVA STUDIA B (stredna): Senzoricka trvanlivost salatu
#---------------------------------------------------------------------------------------
# 20 spotrebitelov hodnotilo cerstvy zeleninovy salat skladovany pri 8 °C kazdych 12 hodin.
# Zaznamenal sa cas (h), kedy salat prvy raz odmietli. Spotrebitelia, ktori ho neodmietli
# do konca testu (72 h), maju odmietnutie = 0.
#
# Ulohy:
# 1. Zostrojte Kaplan-Meierovu krivku akceptacie.
# 2. Urcte median senzorickej trvanlivosti.
# 3. Vyrobca chce, aby produkt v case spotreby odmietlo najviac 25 % spotrebitelov.
#    Aku dobu spotreby odporucite?

salat <- data.frame(
  cas = c(36, 48, 48, 60, 24, 48, 72, 60, 36, 48, 60, 72, 24, 48, 72, 36, 48, 72, 60, 48),
  odmietnutie = c(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
)


#---------------------------------------------------------------------------------------
# PRIPADOVA STUDIA C (tazka): Audit panelu a nova receptura
#---------------------------------------------------------------------------------------
# Deskriptivny panel (10 hodnotitelov) hodnotil horkost 5 tmavych cokolad (P1 - P5)
# v dvoch opakovaniach na 10-bodovej skale. Pred rozhodnutim o novej recepture chce
# vedenie vediet, ci sa na panel da spolahnut.
#
# Ulohy:
# 1. Vyhodnotte vykonnost panelu (rozlisovanie produktov, zhoda, opakovatelnost).
# 2. Najdite hodnotitela, ktory panel najviac zhorsuje, a zdovodnite to.
# 3. Vyhodnotte rozdiely medzi produktmi zmiesanym modelom (hodnotitel = nahodny efekt)
#    s tymto hodnotitelom aj bez neho. Ktore produkty sa lisia?
# 4. Nova receptura P6 ma byt o nieco menej horka ako P3; ocakavany rozdiel je d' = 0.9.
#    Kolko hodnotitelov treba na trojuholnikovy test so silou 80 %? Je lepsia tetrada?

set.seed(2020)
produkty <- paste0("P", 1:5)
skutocna_horkost <- c(P1 = 4, P2 = 5, P3 = 6.5, P4 = 5, P5 = 3.5)
panel <- expand.grid(
  hodnotitel = factor(paste0("H", 1:10), levels = paste0("H", 1:10)),
  produkt = factor(produkty), opakovanie = factor(1:2)
)
posun <- rnorm(10, 0, 1)
panel$horkost <- skutocna_horkost[as.character(panel$produkt)] +
  posun[as.integer(panel$hodnotitel)] + rnorm(nrow(panel), 0, 0.7)
slaby <- panel$hodnotitel == "H7"
panel$horkost[slaby] <- 5 + rnorm(sum(slaby), 0, 1.8)
panel$horkost <- pmin(10, pmax(0, round(panel$horkost, 1)))
head(panel)
