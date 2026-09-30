# ============================================================
# Cvičenie 24: Conjoint analýza
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie24.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Profily produktu - uplny faktorovy plan
#---------------------------------------------------------------------------------------
# Jogurt opisuju tri atributy. Spotrebitel nehodnoti atributy jednotlivo, ale cele profily.
profily <- expand.grid(
  tuk = c("0.1 %", "3.5 %"),
  prichut = c("jahoda", "vanilka", "cokolada"),
  cena = c("0.59 EUR", "0.79 EUR", "0.99 EUR")
)
nrow(profily) # 2 x 3 x 3 = 18 profilov
head(profily)

#---------------------------------------------------------------------------------------
# 2. Hodnotenia - simulacia 40 spotrebitelov (9-bodova skala ochoty kupit)
#---------------------------------------------------------------------------------------
# dva segmenty: "cena" (citlivi na cenu) a "chut" (chcu plnotucny cokoladovy jogurt)
set.seed(24)
n <- 40
segment <- rep(c("cena", "chut"), each = n / 2)
uzitocnost <- function(profil, seg) {
  u_tuk <- c("0.1 %" = -0.3, "3.5 %" = 0.3) * ifelse(seg == "chut", 3, 1)
  u_prichut <- c(jahoda = 0.4, vanilka = -0.6, cokolada = 0.2) + c(0, 0, ifelse(seg == "chut", 1, 0))
  u_cena <- c("0.59 EUR" = 1, "0.79 EUR" = 0, "0.99 EUR" = -1) * ifelse(seg == "cena", 1.8, 0.4)
  u_tuk[as.character(profil$tuk)] + u_prichut[as.character(profil$prichut)] + u_cena[as.character(profil$cena)]
}
hodnotenia <- do.call(rbind, lapply(1:n, function(i) {
  skore <- 5 + uzitocnost(profily, segment[i]) + rnorm(nrow(profily), 0, 0.8)
  data.frame(respondent = factor(i), profily, hodnotenie = pmin(9, pmax(1, round(skore))))
}))
head(hodnotenia)

#---------------------------------------------------------------------------------------
# 3. Ciastkove uzitocnosti (part-worths) - regresia na urovne atributov
#---------------------------------------------------------------------------------------
# sumove kontrasty: uzitocnosti urovni jedneho atributu sa scitaju na nulu
kontrasty <- list(tuk = "contr.sum", prichut = "contr.sum", cena = "contr.sum")
model <- lm(hodnotenie ~ tuk + prichut + cena, data = hodnotenia, contrasts = kontrasty)
ciastkove <- dummy.coef(model)[c("tuk", "prichut", "cena")]
lapply(ciastkove, round, 2)

# relativna dolezitost atributu = rozpatie jeho uzitocnosti / sucet rozpati
rozpatie <- sapply(ciastkove, function(u) diff(range(u)))
round(100 * rozpatie / sum(rozpatie), 1)

barplot(unlist(ciastkove),
  las = 2, cex.names = 0.7, col = rep(c("grey70", "orange", "steelblue"), c(2, 3, 3)),
  ylab = "čiastková užitočnosť", main = "Conjoint – čiastkové užitočnosti"
)
abline(h = 0)

#---------------------------------------------------------------------------------------
# 4. Individualne uzitocnosti a segmentacia
#---------------------------------------------------------------------------------------
individualne <- t(sapply(levels(hodnotenia$respondent), function(r) {
  m <- lm(hodnotenie ~ tuk + prichut + cena, data = subset(hodnotenia, respondent == r), contrasts = kontrasty)
  unlist(dummy.coef(m)[c("tuk", "prichut", "cena")])
}))
round(head(individualne), 2)

# zhlukova analyza individualnych uzitocnosti (pozri cvicenie 8)
zhluky <- kmeans(individualne, centers = 2, nstart = 25)
table(zhluk = zhluky$cluster, skutocny_segment = segment)
round(zhluky$centers, 2)

#---------------------------------------------------------------------------------------
# 5. Simulacia trhu - ktory koncept by si spotrebitelia vybrali?
#---------------------------------------------------------------------------------------
koncepty <- data.frame(
  tuk = c("3.5 %", "0.1 %", "3.5 %"),
  prichut = c("cokolada", "jahoda", "jahoda"),
  cena = c("0.99 EUR", "0.59 EUR", "0.79 EUR"),
  row.names = c("Premium", "Light", "Klasik")
)
# celkova uzitocnost konceptu pre kazdeho respondenta = sucet jeho ciastkovych uzitocnosti
celkova <- sapply(rownames(koncepty), function(k) {
  individualne[, paste0("tuk.", koncepty[k, "tuk"])] +
    individualne[, paste0("prichut.", koncepty[k, "prichut"])] +
    individualne[, paste0("cena.", koncepty[k, "cena"])]
})
# podiel prvej volby: kazdy respondent si "kupi" koncept s najvyssou uzitocnostou
round(100 * prop.table(table(colnames(celkova)[apply(celkova, 1, which.max)])), 1)


# ULOHA1:
# =========
# Ktory atribut je pre spotrebitelov najdolezitejsi v celej vzorke a ktory v jednotlivych zhlukoch?
# (pomocka: relativnu dolezitost vypocitajte zo stredov zhlukov)

# ULOHA2:
# =========
# Zostavte jogurt s najvyssou celkovou uzitocnostou pre kazdy zhluk. O kolko klesne uzitocnost,
# ak cenu zvysite z 0.59 na 0.99 EUR?

# ULOHA3:
# =========
# Pridajte stvrty koncept "Akcia" (3.5 %, cokolada, 0.59 EUR). Ako sa zmenia podiely prvej volby?
# Porovnajte s TURF analyzou v cviceni 11a - na aku otazku odpoveda TURF a na aku conjoint?
# Teoria metody: https://senzorika.github.io/SAP/kapitoly/08_spotrebitelska_veda.html
