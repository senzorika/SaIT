# ============================================================
# Cvičenie 21: Kódy vzoriek a Williamsov dizajn poradia
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie21.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Nahodne trojciferne kody vzoriek
#---------------------------------------------------------------------------------------
# Oznacenie A, B, C alebo 1, 2, 3 naznacuje poradie a kvalitu - vzorky sa preto koduju
# nahodnymi trojcifernymi cislami.
set.seed(21)
vzorky <- c("A", "B", "C", "D")
kody <- sample(100:999, length(vzorky)) # vyber bez opakovania
names(kody) <- vzorky
kody

#---------------------------------------------------------------------------------------
# 2. Williamsov dizajn - vyvazene poradie podavania
#---------------------------------------------------------------------------------------
# Kazda vzorka je rovnako casto na kazdej pozicii a rovnako casto nasleduje po kazdej inej
# vzorke (vyvazenie prenosoveho efektu 1. radu).
# Parny pocet vzoriek k -> k poradi, neparny pocet -> 2k poradi (stvorec + jeho zrkadlovy obraz).
williams <- function(k) {
  prvy <- numeric(k) # prve poradie: 0, 1, k-1, 2, k-2, ...
  nizke <- seq(2, k, by = 2)
  prvy[nizke] <- seq_along(nizke)
  if (k > 2) {
    vysoke <- seq(3, k, by = 2)
    prvy[vysoke] <- k - seq_along(vysoke)
  }
  stvorec <- (outer(0:(k - 1), prvy, "+") %% k) + 1 # dalsie poradia vzniknu posunom o 1
  if (k %% 2 == 1) stvorec <- rbind(stvorec, stvorec[, k:1])
  stvorec
}

poradia <- williams(length(vzorky))
matrix(vzorky[poradia], nrow = nrow(poradia))

# kontrola vyvazenia
prenos <- function(p) table(predchadzajuca = vzorky[p[, -ncol(p)]], nasledujuca = vzorky[p[, -1]])
table(vzorka = vzorky[poradia], pozicia = col(poradia)) # kazda vzorka 1x na kazdej pozicii
prenos(poradia) # kazda dvojica za sebou prave 1x

#---------------------------------------------------------------------------------------
# 3. Plan podavania pre panel
#---------------------------------------------------------------------------------------
# 12 hodnotitelov: kazde Williamsovo poradie sa pouzije 3x, priradenie hodnotitelom je nahodne
n <- 12
riadky <- sample(rep(seq_len(nrow(poradia)), length.out = n))
plan <- matrix(kody[poradia[riadky, ]],
  nrow = n,
  dimnames = list(paste0("H", 1:n), paste0("pozicia_", seq_len(ncol(poradia))))
)
plan
# write.csv(plan, "plan_podavania.csv") # podklad pre pripravu vzoriek

#---------------------------------------------------------------------------------------
# 4. Porovnanie s uplne nahodnym poradim
#---------------------------------------------------------------------------------------
# Nahodne poradie je v priemere vyvazene, ale pri malom paneli byvaju niektore kombinacie
# castejsie nez ine.
nahodne <- t(replicate(n, sample(length(vzorky))))
table(vzorka = vzorky[nahodne], pozicia = col(nahodne))
prenos(nahodne)


# ULOHA1:
# =========
# Pripravte plan podavania pre 5 vzoriek a 20 hodnotitelov. Kolko roznych poradi ma
# Williamsov dizajn pre 5 vzoriek a kolkokrat sa kazde pouzije?

# ULOHA2:
# =========
# Mate 6 vzoriek, ale len 15 hodnotitelov. Je plan uplne vyvazeny? Overte tabulkou pozicii
# a tabulkou prenosu. Kolko hodnotitelov by bolo treba?

# ULOHA3:
# =========
# Vysledok porovnajte s kalkulatorom: https://senzorika.github.io/SAP/kapitoly/02_laboratorium.html#kalkulator
# Co ak hodnotitel zvladne ochutnat len 3 zo 6 vzoriek? (pozri cvicenie 5d - neuplne bloky)
