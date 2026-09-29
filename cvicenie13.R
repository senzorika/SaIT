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


# ULOHA1:
# =========
# Pri duo-trio teste urcilo spravne referencnu vzorku 36 z 50 hodnotitelov.
# Vypocitajte d' a porovnajte ho s vysledkom trojuholnikoveho testu vyssie.
# Ktory z oboch produktov sa od kontroly lisi viac?

# ULOHA2:
# =========
# Vypocitajte, kolko spravnych odpovedi zo 60 by bolo treba v 2-AFC, 3-AFC a triangli,
# aby d' vyslo 1.5 (pomocka: psyfun()).
