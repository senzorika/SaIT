# ============================================================
# Cvičenie 15: Výkonnosť senzorického panelu
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie15.html
# ============================================================

# install.packages("SensoMineR")
library(SensoMineR)

# Dataset: 29 hodnotitelov, 6 cokolad, 2 opakovania (sessions), 14 deskriptorov
data(chocolates)
str(sensochoc)
sensochoc$Panelist <- factor(sensochoc$Panelist)
sensochoc$Session <- factor(sensochoc$Session)

#---------------------------------------------------------------------------------------
# 1. Vykonnost celeho panelu - ANOVA pre kazdy deskriptor
#---------------------------------------------------------------------------------------
vykon <- panelperf(sensochoc,
  firstvar = 5,
  formul = "~Product+Panelist+Session+Product:Panelist+Product:Session+Panelist:Session"
)
# p-hodnoty efektov pre kazdy deskriptor, zoradene podla rozlisovania produktov
coltable(magicsort(vykon$p.value, sort.mat = vykon$p.value[, 1], bycol = FALSE, method = "median"),
  main.title = "Vykonnost panelu (p-hodnoty)"
)
# Product - panel rozlisuje produkty (chceme male p)
# Product:Panelist - hodnotitelia sa nezhoduju (chceme velke p)
# Session - posun medzi opakovaniami (chceme velke p)

#---------------------------------------------------------------------------------------
# 2. Vykonnost jednotlivych hodnotitelov
#---------------------------------------------------------------------------------------
indiv <- paneliperf(sensochoc,
  formul = "~Product+Panelist+Product:Panelist",
  formul.j = "~Product", col.j = 1, firstvar = 5, lastvar = 12,
  synthesis = FALSE, graph = FALSE
)
# p-hodnoty rozlisovania produktov pre kazdeho hodnotitela
coltable(magicsort(indiv$prob.ind, method = "median"), main.title = "Rozlisovanie - hodnotitelia")
# zhoda hodnotitela s panelom (korelacia)
coltable(magicsort(indiv$agree, sort.mat = indiv$agree, method = "median"),
  main.title = "Zhoda s panelom", level.lower = 0.2, col.lower = "grey"
)

#---------------------------------------------------------------------------------------
# 3. Rovnake ukazovatele "rucne" pre jeden deskriptor (CocoaA - kakaova arona)
#---------------------------------------------------------------------------------------
priemery <- tapply(sensochoc$CocoaA, list(sensochoc$Product, sensochoc$Panelist), mean)
panel <- rowMeans(priemery)

# zhoda: korelacia hodnotitela s priemerom panelu
zhoda <- apply(priemery, 2, function(h) cor(h, panel))
sort(round(zhoda, 2))

# opakovatelnost: korelacia medzi 1. a 2. opakovanim u toho isteho hodnotitela
s1 <- tapply(
  sensochoc$CocoaA[sensochoc$Session == 1],
  list(sensochoc$Product[sensochoc$Session == 1], sensochoc$Panelist[sensochoc$Session == 1]), mean
)
s2 <- tapply(
  sensochoc$CocoaA[sensochoc$Session == 2],
  list(sensochoc$Product[sensochoc$Session == 2], sensochoc$Panelist[sensochoc$Session == 2]), mean
)
opakovatelnost <- sapply(colnames(s1), function(h) cor(s1[, h], s2[, h]))

plot(zhoda, opakovatelnost[names(zhoda)],
  pch = 19, col = "steelblue",
  xlab = "zhoda s panelom (r)", ylab = "opakovatelnost (r)", xlim = c(-1, 1), ylim = c(-1, 1),
  main = "Hodnotitelia - CocoaA"
)
text(zhoda, opakovatelnost[names(zhoda)], names(zhoda), pos = 3, cex = 0.7)
abline(h = 0.5, v = 0.5, lty = 2, col = "red")


# ULOHA1:
# =========
# Ktore tri deskriptory panel rozlisuje najlepsie a pri ktorych sa hodnotitelia najmenej zhoduju?

# ULOHA2:
# =========
# Vyberte hodnotitelov, ktori maju pre CocoaA zhodu aj opakovatelnost pod 0.5.
# Ako sa zmeni p-hodnota produktu v ANOVA, ak ich z dat vynechate?
