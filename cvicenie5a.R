# ============================================================
# Cvičenie 5a: Normalita a porovnanie dvoch vzoriek
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie05a.html
# ============================================================

# Skupina hodnotitelov hodnotila 5 druhov syra a dosiahli nasledovne vysledky.
# Dataset: syry
A <- c(7.5, 6.9, 7, 7, 6.9, 7.1, 7.2, 7.5, 6.9)
B <- c(7.5, 8.1, 7.5, 7.4, 7.1, 7.5, 7.2, 7.2, 6.9)
C <- c(7, 6.1, 6.7, 6.1, 6.9, 7.1, 7.2, 7.2, 6.9)
D <- c(7.5, 6.9, 7, 7, 6.9, 7.1, 7.2, 7.2, 6.9)
E <- c(6.8, 6.9, 6.9, 7, 7, 7, 7.1, 7.1, 7.2)
tabulka <- data.frame(A, B, C, D, E)
boxplot(A, B, C, D, E)
boxplot(tabulka)

#---------------------------------------------------------------------------------------
# overenie normality
#---------------------------------------------------------------------------------------
qqnorm(A)
qqline(A)
plot(density(A))
shapiro.test(A)


#---------------------------------------------------------------------------------------
# PAROVE POROVNANIE DVOJIC
#---------------------------------------------------------------------------------------
# zavisle vybery
t.test(A, E, paired = TRUE)
wilcox.test(A, E, paired = TRUE)
# nezavisle vybery
t.test(A, E)
wilcox.test(A, E) # Mann-Whitneyho test


#---------------------------------------------------------------------------------------
# BINOMICKY TEST
#---------------------------------------------------------------------------------------
# V parovom preferencnom teste sa zucastnilo 60 laikov a 34 oznacilo vzorku A (vylepsena receptura)
# ako chutovo lepsiu. Je toto mnozstvo dostacujuce na prijatie hypotezy, ze medzi vzorkami je rozdiel?
# V pripade, ze nie, kolko laikov treba na potvrdenie alternativnej hypotezy?
# (vypocet velkosti panelu: cvicenie 14)

binom.test(34, 60, p = 0.5) # pre parovy test; pre trojuholnikovy test p=1/3

#---------------------------------------------------------------------------------------
# CHI-KVADRAT TEST
#---------------------------------------------------------------------------------------
# 90 respondentov odpovedalo na dotaznik ohladne gulasu a gulasovej zmesy...

likert <- c("velmi.suhlasim", "suhlasim", "neviem", "nesuhlasim", "velmi.nesuhlasim")
otazka1 <- c(15, 20, 20, 10, 25) # Cierne pivo je vhodne do gulasu... :)
otazka2 <- c(40, 25, 15, 5, 5) # Minimalne raz v zivote som gulas jedol...
otazka3 <- c(82, 2, 2, 2, 2) # Gulas pochadza z Madarska...
vysledky <- data.frame(likert, otazka1, otazka2, otazka3)
chisq.test(otazka1)
chisq.test(otazka2)
chisq.test(otazka3)

#---------------------------------------------------------------------------------------
# MCNEMAR TEST
#---------------------------------------------------------------------------------------
# Zistite, ci spotrebitelia reaguju rovnako na staru a novu recepturu po modifikacii

rum <- matrix(c(40, 8, 24, 28), nrow = 2, dimnames = list("stara receptura" = c("kupil(a)", "nekupil(a)"), "nova receptura" = c("kupil(a)", "nekupil(a)")))
mcnemar.test((rum), correct = FALSE)


# ULOHA1:
# =========
# Analyzujte data a overte hypotezu, ze medzi 2 vzorkami syra (najlepsim a povodnym (E)) je statisticky preukazny rozdiel.

# ULOHA2:
# =========
# Zistite, ci pri falsovani vzoriek a naslednej detekcii bol zisteny statisticky preukazny rozdiel ak
# zo 100 podanych trianglov urcilo spravnu vzorku 42 hodnotitelov...
# Ako by dopadol tento test, keby sa robil ako parovy?

# ULOHA3:
# =========
# V obchodnom retazci su dostupne tieto druhy jogurtov s nasledovnymi preferenciami zakaznikov  (jahodovy(25), cokoladovy(34), vanilkovy(17), cucoriedkovy(7))
# Zistite pomocou adekvatnej statistickej metody, ci je preukazna preferencia vybraneho druhu jogurtu?
