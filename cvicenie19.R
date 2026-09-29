# ============================================================
# Cvičenie 19: Kontrolné prípadové štúdie I
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie19.html
# ============================================================
# Riesenie (R skript + kratky slovny zaver) poslite na: senzorickelaboratoriumfbp@gmail.com
# Predmet spravy: SaIT - cvicenie 19 - Meno Priezvisko

#---------------------------------------------------------------------------------------
# PRIPADOVA STUDIA A (velmi lahka): Novy dodavatel kakaa
#---------------------------------------------------------------------------------------
# Cokoladovna zvazuje prechod na lacnejsieho dodavatela kakaa. Senzoricke laboratorium
# urobilo trojuholnikovy test: 48 hodnotitelov, 21 z nich spravne urcilo odlisnu vzorku.
#
# Ulohy:
# 1. Formulujte nulovu a alternativnu hypotezu.
# 2. Vyberte a vypocitajte vhodny test (alfa = 0.05).
# 3. Napiste jednu vetu zaveru pre vedenie. Mozno na zaklade vysledku tvrdit, ze cokolady su rovnake?

spravne <- 21
hodnotitelia <- 48


#---------------------------------------------------------------------------------------
# PRIPADOVA STUDIA B (stredna): Styri receptury jogurtu
#---------------------------------------------------------------------------------------
# Vyvojove oddelenie pripravilo 4 receptury jogurtu (A - D). Kazdy z 12 hodnotitelov
# ochutnal vsetky 4 vzorky v nahodnom poradi a hodnotil celkovu prijemnost na 9-bodovej
# hedonickej skale (1 = extremne nechutne, 9 = extremne chutne).
#
# Ulohy:
# 1. Zobrazte data vhodnym grafom a popiste, co z neho vidno.
# 2. Urcte dizajn experimentu (zavisle / nezavisle vybery) a overte predpoklady.
# 3. Vyberte a vypocitajte globalny test.
# 4. Ak je rozdiel preukazny, zistite post-hoc testom, ktore receptury sa lisia.
# 5. Odporucte vedeniu jednu recepturu a zdovodnite to (2 - 3 vety).

jogurty <- data.frame(
  hodnotitel = factor(rep(paste0("H", 1:12), times = 4), levels = paste0("H", 1:12)),
  receptura = factor(rep(c("A", "B", "C", "D"), each = 12)),
  prijemnost = c(
    6, 5, 7, 6, 5, 6, 7, 5, 6, 6, 5, 7, # A
    6, 6, 7, 5, 6, 7, 6, 6, 5, 7, 6, 6, # B
    4, 3, 5, 4, 3, 4, 5, 3, 4, 2, 4, 5, # C
    8, 7, 8, 7, 8, 9, 7, 8, 7, 8, 8, 6 # D
  )
)
head(jogurty)
