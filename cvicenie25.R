# ============================================================
# Cvičenie 25: Senzorické tvrdenia – nadradenosť a parita
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie25.html
# ============================================================

#---------------------------------------------------------------------------------------
# 1. Nadradenost: "spotrebitelia uprednostnuju A pred B"
#---------------------------------------------------------------------------------------
# Parovy preferencny test, 300 spotrebitelov: 172 preferovalo A, 128 preferovalo B
A <- 172
B <- 128
nadradenost <- binom.test(A, A + B, p = 0.5) # obojstranne - vopred nevieme, kto vyhra
nadradenost

# odpovede "bez preferencie": 150 x A, 110 x B, 40 x bez preferencie
# sposob spracovania treba urcit PRED testom, nie podla toho, co vyjde lepsie
binom.test(150, 150 + 110, p = 0.5) # a) vylucit
binom.test(150 + 40 / 2, 300, p = 0.5) # b) rozdelit rovnomerne

#---------------------------------------------------------------------------------------
# 2. Parita: "rovnako oblubeny ako B"
#---------------------------------------------------------------------------------------
# Nepreukazny rozdiel NIE JE dokaz parity. Paritu podlozi test ekvivalencie: (1 - 2*alfa)
# interval spolahlivosti podielu musi cely lezat vo vopred urcenom pasme 50 % +- pasmo.
parita <- function(x, n, pasmo = 0.10, alfa = 0.05) {
  is <- binom.test(x, n, conf.level = 1 - 2 * alfa)$conf.int
  data.frame(
    podiel = x / n, dolna = is[1], horna = is[2],
    parita = is[1] > 0.5 - pasmo & is[2] < 0.5 + pasmo
  )
}
parita(172, 300) # 57 % - parita nie je podlozena
parita(154, 300) # 51 % pri 300 spotrebiteloch
parita(31, 60) # 52 % pri 60 spotrebiteloch - rovnaky podiel, ale interval je prilis siroky

# sila testu parity: pravdepodobnost, ze paritu preukazeme, ak je skutocna preferencia 50 : 50
sila_parity <- function(n, pasmo = 0.10, alfa = 0.05) {
  x <- 0:n
  sum(dbinom(x, n, 0.5)[sapply(x, function(i) parita(i, n, pasmo, alfa)$parita)])
}
n_hodnoty <- c(60, 100, 150, 200, 300, 400, 500)
sila <- sapply(n_hodnoty, sila_parity)
data.frame(n = n_hodnoty, sila = round(sila, 2))
plot(n_hodnoty, sila,
  type = "b", pch = 19, ylim = c(0, 1),
  xlab = "počet spotrebiteľov", ylab = "sila testu parity", main = "Parita: pásmo ± 10 p. b., α = 0,05"
)
abline(h = 0.8, lty = 2)

#---------------------------------------------------------------------------------------
# 3. "Neprekonany": produkt A nie je menej oblubeny nez B
#---------------------------------------------------------------------------------------
# jednostranny test: H0: podiel A <= 50 % - pasmo, H1: podiel A je vyssi
binom.test(154, 300, p = 0.5 - 0.10, alternative = "greater")

#---------------------------------------------------------------------------------------
# 4. Atributove tvrdenie: "intenzivna chut" (priemer aspon 7 na 10-bodovej skale)
#---------------------------------------------------------------------------------------
# 12 trenovanych hodnotitelov, priemer 7.5, smerodajna odchylka 1.2
set.seed(25)
intenzita <- as.numeric(7.5 + 1.2 * scale(rnorm(12)))
c(priemer = mean(intenzita), sd = sd(intenzita))
t.test(intenzita)$conf.int # 95 % interval spolahlivosti priemeru
# tvrdenie je podlozene, len ak dolna hranica intervalu je aspon 7
t.test(intenzita, mu = 7, alternative = "greater")

# kolko hodnotitelov treba, aby sa priemer 7.5 odlisil od hranice 7 so silou 80 %?
power.t.test(delta = 0.5, sd = 1.2, sig.level = 0.05, power = 0.80, type = "one.sample", alternative = "one.sided")


# ULOHA1:
# =========
# V teste 250 spotrebitelov preferovalo 140 produkt A a 110 produkt B. Je podlozene tvrdenie
# "spotrebitelia uprednostnuju A"? Je podlozena parita v pasme +- 10 p. b.?

# ULOHA2:
# =========
# Kolko spotrebitelov treba na preukazanie parity so silou 80 % pri pasme +- 10 p. b.
# a kolko pri prisnejsom pasme +- 5 p. b.?

# ULOHA3:
# =========
# Overte vysledky casti 1 a 2 v kalkulatore: https://senzorika.github.io/SAP/kapitoly/09_claims.html#kalkulator
# Preco vyrok "rozdiel nebol preukazny" nestaci ako podklad pre tvrdenie "chuti rovnako"?
