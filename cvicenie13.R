# ============================================================
# Cvičenie 13: Thurstonov model a d' (sensR)
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie13.html
# ============================================================

# install.packages("sensR")
library(sensR)

#---------------------------------------------------------------------------------------
# 1. Od podielu spravnych odpovedi k d'
#---------------------------------------------------------------------------------------
# Trojuholnikovy test: zo 100 hodnotitelov urcilo odlisnu vzorku 42 (uloha 2 z cvicenia 5a)
triangel <- discrim(42, 100, method = "triangle")
triangel
# pc - podiel spravnych odpovedi, pd - podiel "skutocnych rozlisovatelov", d.prime - Thurstonovo d'

# rovnaky vysledok v inych metodach: d' je nezavisle od metody, pc nie
rescale(d.prime = coef(triangel)["d-prime", "Estimate"], method = "duotrio")
rescale(d.prime = coef(triangel)["d-prime", "Estimate"], method = "twoAFC")

#---------------------------------------------------------------------------------------
# 2. Psychometricke funkcie - ako rychlo rastie pc s d' v jednotlivych testoch
#---------------------------------------------------------------------------------------
curve(psyfun(x, method = "twoAFC"),
  from = 0, to = 4, lwd = 2, col = "darkgreen",
  xlab = "d'", ylab = "pc (podiel spravnych odpovedi)", ylim = c(0, 1)
)
curve(psyfun(x, method = "threeAFC"), add = TRUE, lwd = 2, col = "blue")
curve(psyfun(x, method = "duotrio"), add = TRUE, lwd = 2, col = "orange")
curve(psyfun(x, method = "triangle"), add = TRUE, lwd = 2, col = "red")
legend("bottomright",
  legend = c("2-AFC", "3-AFC", "duo-trio", "triangel"),
  col = c("darkgreen", "blue", "orange", "red"), lwd = 2, bty = "n"
)

#---------------------------------------------------------------------------------------
# 3. Porovnanie metod pri rovnakom rozdiele medzi vzorkami (d' = 1)
#---------------------------------------------------------------------------------------
metody <- c("twoAFC", "threeAFC", "duotrio", "triangle", "tetrad")
pc_pri_d1 <- sapply(metody, function(m) psyfun(1, method = m))
round(pc_pri_d1, 3)

#---------------------------------------------------------------------------------------
# 4. Test podobnosti (similarity) - su vzorky "dostatocne rovnake"?
#---------------------------------------------------------------------------------------
# Nahrada suroviny: chceme preukazat, ze rozdiel je mensi nez pd0 = 0.2
discrim(38, 100, method = "triangle", test = "similarity", pd0 = 0.2)

# graf rozdeleni pre obe vzorky podla Thurstonovho modelu
plot(triangel)

#---------------------------------------------------------------------------------------
# 5. A - nie A (ISO 8588) a same-different: testy s odpovedovou tendenciou
#---------------------------------------------------------------------------------------
# A - nie A: kazdy hodnotitel dostane jednu vzorku a povie, ci je to "A".
# 50 hodnotitelov dostalo vzorku A, 50 vzorku "nie A"; odpoved "A" zaznela 34x pri A a 20x pri "nie A".
# Nahodna uroven tu nie je 1/2 - zavisi od toho, ako ochotne hodnotitelia hovoria "A".
# Preto sa nepouziva binomicky test, ale tabulka 2 x 2:
odpovede <- matrix(c(34, 16, 20, 30),
  nrow = 2, byrow = TRUE,
  dimnames = list(vzorka = c("A", "nie A"), odpoved = c("A", "nie A"))
)
odpovede
chisq.test(odpovede, correct = FALSE)
a_nie_a <- AnotA(x1 = 34, n1 = 50, x2 = 20, n2 = 50) # d' = z(hit) - z(false alarm) a Fisherov test
a_nie_a
qnorm(34 / 50) - qnorm(20 / 50) # to iste "rucne"

# Same-different: hodnotitel dostane par a povie, ci su vzorky rovnake alebo rozne.
# 50 rovnakych parov: 32x "rovnake", 18x "rozne"; 50 roznych parov: 20x "rovnake", 30x "rozne"
rovnake_rozne <- samediff(nsamesame = 32, ndiffsame = 18, nsamediff = 20, ndiffdiff = 30)
summary(rovnake_rozne) # delta = d', tau = kriterium (ako velky rozdiel uz hodnotitel nazve "rozne")

#---------------------------------------------------------------------------------------
# 6. Opakovane rozlisovacie testy - beta-binomicky model
#---------------------------------------------------------------------------------------
# 24 hodnotitelov urobilo po 4 trojuholnikove testy. 96 odpovedi nie je 96 nezavislych pokusov:
# niektori hodnotitelia rozdiel vnimaju, ini nie (naddisperzia).
set.seed(3)
rozlisuje <- rbinom(24, 1, 0.5) # polovica hodnotitelov rozdiel naozaj vnima
spravne <- rbinom(24, 4, ifelse(rozlisuje == 1, 0.8, 1 / 3))
opakovania <- cbind(spravne, celkom = 4)
opakovania

binom.test(sum(spravne), 96, p = 1 / 3, alternative = "greater") # naivne: vsetko sa scita
bb <- betabin(opakovania, method = "triangle")
summary(bb) # gamma = miera naddisperzie (0 = ziadna, 1 = maximalna)
# ak je naddisperzia preukazna, naivny binomicky test podhodnocuje neistotu


# ULOHA1:
# =========
# Pri duo-trio teste urcilo spravne referencnu vzorku 36 z 50 hodnotitelov.
# Vypocitajte d' a porovnajte ho s vysledkom trojuholnikoveho testu vyssie.
# Ktory z oboch produktov sa od kontroly lisi viac?

# ULOHA2:
# =========
# Vypocitajte, kolko spravnych odpovedi zo 60 by bolo treba v 2-AFC, 3-AFC a triangli,
# aby d' vyslo 1.5 (pomocka: psyfun()).

# ULOHA3:
# =========
# V teste A - nie A dostalo 80 hodnotitelov vzorku A a 40 vzorku "nie A". Odpoved "A" zaznela
# 64x pri vzorke A a 30x pri vzorke "nie A". Je rozdiel preukazny? Aky by bol zaver, keby ste
# (nespravne) pouzili binomicky test 74 spravnych odpovedi zo 120 s p = 1/2?
