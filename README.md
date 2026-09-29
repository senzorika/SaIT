# SaIT – Senzometria v R / Sensometrics in R

[Slovensky](#slovensky) · [English](#english)

---

## Slovensky

Materiály k cvičeniam zo senzorickej analýzy a senzometrie v jazyku R. Ku každému cvičeniu je R skript (slovenská a anglická verzia) a teoretická stránka s grafmi, ktorá vysvetľuje metódu, jej použitie a čítanie výstupu.

**Začnite tu:** [teória – prehľad všetkých stránok](https://senzorika.github.io/SaIT/teoria/index.html)

> Teoretické stránky sa zobrazujú cez GitHub Pages. GitHub pri otvorení `.html` súboru priamo v repozitári ukáže iba zdrojový kód. Záložná možnosť bez Pages: [raw.githack.com](https://raw.githack.com/senzorika/SaIT/master/teoria/index.html).

### Cvičenia

| # | Téma | Teória | Skript SK | Skript EN |
|---|------|--------|-----------|-----------|
| 1 | R, RStudio a spolupráca | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie01.html) | [cvicenie1.txt](cvicenie1.txt) | [exercise1.txt](exercises_EN/exercise1.txt) |
| 2 | BCG matica a interval spoľahlivosti | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie02.html) | [cvicenie2.txt](cvicenie2.txt) | [exercise2.txt](exercises_EN/exercise2.txt) |
| 3 | Práca s dátami v R | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie03.html) | [cvicenie3.txt](cvicenie3.txt) | [exercise3.txt](exercises_EN/exercise3.txt) |
| 4 | Grafy a vizualizácia dát | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie04.html) | [cvicenie4.txt](cvicenie4.txt) | [exercise4.txt](exercises_EN/exercise4.txt) |
| 5a | Normalita a porovnanie dvoch vzoriek | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie05a.html) | [cvicenie5a.txt](cvicenie5a.txt) | [exercise5a.txt](exercises_EN/exercise5a.txt) |
| 5b | Porovnanie viacerých vzoriek (KW, Friedman, ANOVA) | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie05b.html) | [cvicenie5b.txt](cvicenie5b.txt) | [exercise5b.txt](exercises_EN/exercise5b.txt) |
| 5c | Import dát z Excelu a postup analýzy | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie05c.html) | [cvicenie5c.txt](cvicenie5c.txt) | [exercise5c.txt](exercises_EN/exercise5c.txt) |
| 5d | Durbinov test a neúplné bloky | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie05d.html) | [cvicenie5d.txt](cvicenie5d.txt) | [exercise5d.txt](exercises_EN/exercise5d.txt) |
| 6 | Korelácia a lineárna regresia | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie06.html) | [cvicenie6.txt](cvicenie6.txt) | [exercise6.txt](exercises_EN/exercise6.txt) |
| 7 | PCA a faktorová analýza | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie07.html) | [cvicenie7.txt](cvicenie7.txt) | [exercise7.txt](exercises_EN/exercise7.txt) |
| 8 | Zhluková analýza | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie08.html) | [cvicenie8.txt](cvicenie8.txt) | [exercise8.txt](exercises_EN/exercise8.txt) |
| 9 | Korešpondenčná analýza | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie09.html) | [cvicenie9.txt](cvicenie9.txt) | [exercise9.txt](exercises_EN/exercise9.txt) |
| 10 | Analýza prežitia a senzorická trvanlivosť | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie10.html) | [cvicenie10.txt](cvicenie10.txt) | [exercise10.txt](exercises_EN/exercise10.txt) |
| 11a | TURF analýza a mapa preferencií | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie11a.html) | [cvicenie11a.txt](cvicenie11a.txt) | [exercise11a.txt](exercises_EN/exercise11a.txt) |
| 11b | Text mining a analýza sentimentu | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie11b.html) | [cvicenie11b.txt](cvicenie11b.txt) | [exercise11b.txt](exercises_EN/exercise11b.txt) |
| 12 | JAR škála a radarový graf | [teória](https://senzorika.github.io/SaIT/teoria/cvicenie12.html) | [cvicenie12.txt](cvicenie12.txt) | [exercise12.txt](exercises_EN/exercise12.txt) |

### Ďalší obsah repozitára

- [datasety/](datasety/) – datasety k praktickým úlohám (napr. [001_datasety.txt](datasety/001_datasety.txt) k cvičeniu 5b)
- [Senzometricke_appky/](Senzometricke_appky/) – interaktívne Shiny aplikácie (PCA, TDS, TCATA, NPS, LDA…)
- [English/](English/) – rozšírené anglické materiály a prezentácie (dvojvýberové testy, viacrozmerné metódy, spotrebiteľské preferencie)

### Ako začať

1. Nainštalujte [R](https://cran.r-project.org/) a potom [RStudio](https://posit.co/download/rstudio-desktop/).
2. Otvorte skript cvičenia v RStudiu a spúšťajte ho po riadkoch (`Ctrl + Enter`).
3. Chýbajúce balíky doinštalujte cez `install.packages("nazov")` (zoznam je v [cvicenie1.txt](cvicenie1.txt)).

### Užitočné odkazy

- [R](https://cran.r-project.org/) a [RStudio](https://posit.co/download/rstudio-desktop/) – štatistické prostredie a editor
- [OpenCode](https://opencode.ai/) – open-source AI asistent na programovanie v termináli (pomoc pri písaní a vysvetľovaní R kódu)
- [White Noise](https://www.whitenoise.chat/) – súkromný šifrovaný messenger postavený na protokole Nostr (komunikácia v tíme)
- [GitHub](https://github.com/) – verziovanie skriptov a dát
- [Open Food Facts](https://world.openfoodfacts.org/) – otvorená databáza potravín (cvičenie 4)

---

## English

Course materials for sensory analysis and sensometrics exercises in R. Each exercise has an R script (in Slovak and English) and a theory page with graphics that explains the method, when to use it and how to read the output.

**Start here:** [theory – overview of all pages](https://senzorika.github.io/SaIT/teoria/index.html) (the theory pages are in Slovak)

> The theory pages are served via GitHub Pages; opening an `.html` file directly in the repository shows only its source code. Fallback without Pages: [raw.githack.com](https://raw.githack.com/senzorika/SaIT/master/teoria/index.html).

### Exercises

| # | Topic | Theory (SK) | Script SK | Script EN |
|---|-------|-------------|-----------|-----------|
| 1 | R, RStudio and collaboration tools | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie01.html) | [cvicenie1.txt](cvicenie1.txt) | [exercise1.txt](exercises_EN/exercise1.txt) |
| 2 | BCG matrix and confidence interval | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie02.html) | [cvicenie2.txt](cvicenie2.txt) | [exercise2.txt](exercises_EN/exercise2.txt) |
| 3 | Working with data in R | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie03.html) | [cvicenie3.txt](cvicenie3.txt) | [exercise3.txt](exercises_EN/exercise3.txt) |
| 4 | Charts and data visualisation | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie04.html) | [cvicenie4.txt](cvicenie4.txt) | [exercise4.txt](exercises_EN/exercise4.txt) |
| 5a | Normality and two-sample comparison | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie05a.html) | [cvicenie5a.txt](cvicenie5a.txt) | [exercise5a.txt](exercises_EN/exercise5a.txt) |
| 5b | Comparing several samples (KW, Friedman, ANOVA) | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie05b.html) | [cvicenie5b.txt](cvicenie5b.txt) | [exercise5b.txt](exercises_EN/exercise5b.txt) |
| 5c | Importing Excel data and the analysis workflow | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie05c.html) | [cvicenie5c.txt](cvicenie5c.txt) | [exercise5c.txt](exercises_EN/exercise5c.txt) |
| 5d | Durbin test and incomplete blocks | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie05d.html) | [cvicenie5d.txt](cvicenie5d.txt) | [exercise5d.txt](exercises_EN/exercise5d.txt) |
| 6 | Correlation and linear regression | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie06.html) | [cvicenie6.txt](cvicenie6.txt) | [exercise6.txt](exercises_EN/exercise6.txt) |
| 7 | PCA and factor analysis | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie07.html) | [cvicenie7.txt](cvicenie7.txt) | [exercise7.txt](exercises_EN/exercise7.txt) |
| 8 | Cluster analysis | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie08.html) | [cvicenie8.txt](cvicenie8.txt) | [exercise8.txt](exercises_EN/exercise8.txt) |
| 9 | Correspondence analysis | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie09.html) | [cvicenie9.txt](cvicenie9.txt) | [exercise9.txt](exercises_EN/exercise9.txt) |
| 10 | Survival analysis and sensory shelf life | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie10.html) | [cvicenie10.txt](cvicenie10.txt) | [exercise10.txt](exercises_EN/exercise10.txt) |
| 11a | TURF analysis and preference mapping | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie11a.html) | [cvicenie11a.txt](cvicenie11a.txt) | [exercise11a.txt](exercises_EN/exercise11a.txt) |
| 11b | Text mining and sentiment analysis | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie11b.html) | [cvicenie11b.txt](cvicenie11b.txt) | [exercise11b.txt](exercises_EN/exercise11b.txt) |
| 12 | JAR scale and radar chart | [theory](https://senzorika.github.io/SaIT/teoria/cvicenie12.html) | [cvicenie12.txt](cvicenie12.txt) | [exercise12.txt](exercises_EN/exercise12.txt) |

### Other contents

- [datasety/](datasety/) – datasets for the practical tasks (e.g. [001_datasety.txt](datasety/001_datasety.txt) for exercise 5b)
- [Senzometricke_appky/](Senzometricke_appky/) – interactive Shiny apps (PCA, TDS, TCATA, NPS, LDA…)
- [English/](English/) – extended English materials and presentations (two-sample tests, multivariate methods, consumer preference)

### Getting started

1. Install [R](https://cran.r-project.org/) and then [RStudio](https://posit.co/download/rstudio-desktop/).
2. Open an exercise script in RStudio and run it line by line (`Ctrl + Enter`).
3. Install missing packages with `install.packages("name")` (see the list in [exercise1.txt](exercises_EN/exercise1.txt)).

### Useful links

- [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) – statistical environment and editor
- [OpenCode](https://opencode.ai/) – open-source AI coding agent for the terminal (help with writing and explaining R code)
- [White Noise](https://www.whitenoise.chat/) – private end-to-end encrypted messenger built on the Nostr protocol (team communication)
- [GitHub](https://github.com/) – versioning of scripts and data
- [Open Food Facts](https://world.openfoodfacts.org/) – open food products database (exercise 4)
