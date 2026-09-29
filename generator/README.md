# Generátor samostatného cvičenia

Pre týždne, keď vyučujúci nie je prítomný. Podľa sylabu predmetu (ZS 2025/26) vygeneruje pre zvolený týždeň:

- **HTML stránku** – skrátená teória týždňa, kľúčové pojmy, odkazy na podrobnú teóriu (s riešenými príkladmi), prezentáciu a datasety, zadania úloh s pomôckami a tlačidlo na odoslanie riešenia e-mailom. R skript je vložený priamo v stránke (tlačidlo „Stiahnuť R skript“), stačí teda poslať jeden súbor.
- **R skript pre študentov** – študent zadá svoje ID (napr. číslo z AIS) a z neho sa vygenerujú jeho vlastné dáta. Každý študent má iné čísla, takže riešenia sa nedajú odpísať.
- **Kľúč pre vyučujúceho** (voliteľne) – CSV so správnymi výsledkami pre zadané ID študentov.

## Použitie

V R (pracovný priečinok = koreň repozitára):

```r
source("generator/generator.R")

generuj_cvicenie(5)                        # týždeň 5, slovensky
generuj_cvicenie(5, jazyk = "en")          # anglicky
generuj_cvicenie(5, studenti = c(123456, 234567, 345678))   # + KLUC_tyzden05.csv

kluc_cvicenia(5, studenti = c(123456, 234567))              # len kľúč ako data.frame
```

Výstup vznikne v `generator/vystup/tyzdenNN_sk/`. Študentom pošlite len súbor `cvicenie_tyzdenNN_sk.html` (alebo ho nahrajte do LMS). **Súbor `KLUC_*.csv` neposielajte** – priečinok `vystup/` je preto v `.gitignore`.

Potrebné balíky: `sensR` (týždne 4, 5, 9, 11), `survival` (týždeň 10, je súčasťou R).

## Týždne a úlohy

| týždeň | téma | úlohy |
|:-:|---|---|
| 1 | Úvod do senzorickej analýzy | základné štatistiky, plán experimentu |
| 2 | R a RStudio | plán experimentu, základné štatistiky |
| 3 | Dátové operácie a vizualizácia | interval spoľahlivosti, BCG matica, základné štatistiky |
| 4 | Prípadová štúdia | rozlišovací test, Friedman / ANOVA s blokom |
| 5 | Porovnanie dvoch produktov | párový test, rozlišovací test (d′), χ² test |
| 6 | Porovnanie 3+ produktov | nezávislé skupiny (KW / ANOVA), bloky (Friedman) |
| 7 | Viacrozmerné metódy | korelácia a regresia, PCA, zhluková analýza |
| 8 | Marketingové metódy | TURF, JAR, korešpondenčná analýza |
| 9 | Príprava na čiastkovú skúšku | rozlišovací test, nezávislé skupiny, korelácia |
| 10 | Prípadové štúdie | trvanlivosť (Kaplan-Meier), JAR, výkonnosť hodnotiteľov |
| 11 | Metódy pre semestrálny projekt | CATA (Cochranov Q), TDS krivky, veľkosť panelu |

V 12. a 13. týždni (prezentácie, zápočet) sa cvičenie negeneruje.

## Úpravy

- `tyzdne.R` – termíny, témy, text teórie, výber úloh a datasetov pre každý týždeň.
- `ulohy.R` – zásobník úloh: zadanie, pomôcka, kód generujúci dáta a funkcia, ktorá vypočíta správny výsledok do kľúča.
- Každá úloha má vlastný seed (`ID * 1000 + týždeň * 10 + číslo úlohy`), takže náhodné funkcie, ktoré študent použije vo svojom riešení, neovplyvnia dáta ďalších úloh.
