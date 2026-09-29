<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/banner-dark.svg">
    <img src="assets/banner-light.svg" alt="SaIT – Senzometria v R / Sensometrics in R" width="100%">
  </picture>
</p>

<p align="center">
  <a href="https://cran.r-project.org/"><img src="https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3?logo=r&logoColor=white" alt="R"></a>
  <a href="https://posit.co/download/rstudio-desktop/"><img src="https://img.shields.io/badge/RStudio-IDE-75AADB?logo=rstudioide&logoColor=white" alt="RStudio"></a>
  <img src="https://img.shields.io/badge/cvi%C4%8Denia%20%7C%20exercises-22-0f7b6c" alt="22 exercises">
  <img src="https://img.shields.io/badge/jazyk%20%7C%20language-SK%20%C2%B7%20EN-d9622b" alt="SK · EN">
  <a href="https://senzorika.github.io/SaIT/teoria/index.html"><img src="https://img.shields.io/badge/te%C3%B3ria%20%7C%20theory-GitHub%20Pages-3a5fcd?logo=github" alt="Theory"></a>
</p>

<p align="center"><b><a href="#-slovensky">🇸🇰 Slovensky</a></b> &nbsp;|&nbsp; <b><a href="#-english">🇬🇧 English</a></b></p>

---

## 🇸🇰 Slovensky

Materiály k cvičeniam zo **senzorickej analýzy a senzometrie** v jazyku R. Ku každému cvičeniu patrí R skript (slovenská aj anglická verzia) a **teoretická stránka s grafmi**: čo metóda robí, kedy ju použiť, ako čítať výstup a na čo si dať pozor.

<p align="center"><a href="https://senzorika.github.io/SaIT/teoria/index.html"><b>📖 Otvoriť teóriu</b></a> &nbsp;·&nbsp; <a href="#-cvičenia">🧪 Cvičenia</a> &nbsp;·&nbsp; <a href="#-ako-začať">🚀 Ako začať</a> &nbsp;·&nbsp; <a href="#-užitočné-odkazy">🔗 Odkazy</a></p>

### 🧭 Mapa kurzu

```mermaid
flowchart LR
    B0["🧰 Základy práce v R<br/>cvičenia 1, 2, 3, 4"]
    B1["🧪 Testovanie hypotéz<br/>cvičenia 5a, 5b, 5c, 5d"]
    B2["📈 Vzťahy a modelovanie<br/>cvičenia 6, 10"]
    B3["🗺️ Viacrozmerné metódy<br/>cvičenia 7, 8, 9"]
    B4["🎯 Rozlišovacie testy a panel<br/>cvičenia 13, 14, 15, 16"]
    B5["🛒 Spotrebiteľský výskum<br/>cvičenia 11a, 11b, 12"]
    B6["⏱️ Rýchle a temporálne metódy<br/>cvičenia 17, 18"]
    B0 --> B1
    B1 --> B2
    B1 --> B3
    B1 --> B4
    B2 --> B5
    B3 --> B5
    B4 --> B5
    B5 --> B6
    classDef blk fill:#d6efe9,stroke:#0f7b6c,color:#1d1f24
    class B0,B1,B2,B3,B4,B5,B6 blk
```

### 🧪 Cvičenia

#### 🧰 Základy práce v R

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **1** | **R, RStudio a spolupráca**<br><sub>Pracovné prostredie pre senzometriu: štatistický jazyk R, vývojové prostredie RStudio, balíky a nástroje na tímovú prácu.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie01.html) | [`cvicenie1.R`](cvicenie1.R) | [`exercise1.R`](exercises_EN/exercise1.R) |
| **2** | **BCG matica a interval spoľahlivosti**<br><sub>Ako umiestniť produkty do mapy cena × kvalita a ako vyjadriť neistotu priemerného hodnotenia panelu.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie02.html) | [`cvicenie2.R`](cvicenie2.R) | [`exercise2.R`](exercises_EN/exercise2.R) |
| **3** | **Práca s dátami v R**<br><sub>Skaláry, vektory, matice a dátové rámce – stavebné kamene každej senzorickej analýzy.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie03.html) | [`cvicenie3.R`](cvicenie3.R) | [`exercise3.R`](exercises_EN/exercise3.R) |
| **4** | **Grafy a vizualizácia dát**<br><sub>Ktorý graf zvoliť, ako ho čítať a čo z neho vyčítať skôr, než siahneme po štatistickom teste.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie04.html) | [`cvicenie4.R`](cvicenie4.R) | [`exercise4.R`](exercises_EN/exercise4.R) |

#### 🧪 Testovanie hypotéz

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **5a** | **Normalita a porovnanie dvoch vzoriek**<br><sub>Overenie normality, t-test verzus Wilcoxon, binomický test v rozlišovacích skúškach, χ² test a McNemarov test.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie05a.html) | [`cvicenie5a.R`](cvicenie5a.R) | [`exercise5a.R`](exercises_EN/exercise5a.R) |
| **5b** | **Porovnanie viacerých vzoriek**<br><sub>Kruskal-Wallis, Friedman, analýza rozptylu (ANOVA) a viacnásobné porovnania.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie05b.html) | [`cvicenie5b.R`](cvicenie5b.R) | [`exercise5b.R`](exercises_EN/exercise5b.R) |
| **5c** | **Import dát z Excelu a postup analýzy**<br><sub>Načítanie online súboru, dlhý formát dát a kompletný postup: normalita → test → post-hoc.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie05c.html) | [`cvicenie5c.R`](cvicenie5c.R) | [`exercise5c.R`](exercises_EN/exercise5c.R) |
| **5d** | **Durbinov test a neúplné bloky**<br><sub>Keď hodnotiteľ nemôže ochutnať všetky vzorky: vyvážené neúplné blokové usporiadanie.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie05d.html) | [`cvicenie5d.R`](cvicenie5d.R) | [`exercise5d.R`](exercises_EN/exercise5d.R) |

#### 📈 Vzťahy a modelovanie

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **6** | **Korelácia a lineárna regresia**<br><sub>Sila a smer vzťahu medzi inštrumentálnymi a senzorickými znakmi a jednoduchý predikčný model.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie06.html) | [`cvicenie6.R`](cvicenie6.R) | [`exercise6.R`](exercises_EN/exercise6.R) |
| **10** | **Analýza prežitia a senzorická trvanlivosť**<br><sub>Kaplan-Meierov odhad pravdepodobnosti akceptácie produktu v čase a určenie cut-off bodu.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie10.html) | [`cvicenie10.R`](cvicenie10.R) | [`exercise10.R`](exercises_EN/exercise10.R) |

#### 🗺️ Viacrozmerné metódy

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **7** | **PCA a faktorová analýza**<br><sub>Zníženie rozmernosti dát: hlavné komponenty, Kaiserovo kritérium a skryté faktory v spotrebiteľských odpovediach.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie07.html) | [`cvicenie7.R`](cvicenie7.R) | [`exercise7.R`](exercises_EN/exercise7.R) |
| **8** | **Zhluková analýza**<br><sub>Hierarchické zhlukovanie (Ward), dendrogram a metóda k-priemerov pre segmentáciu produktov.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie08.html) | [`cvicenie8.R`](cvicenie8.R) | [`exercise8.R`](exercises_EN/exercise8.R) |
| **9** | **Korešpondenčná analýza**<br><sub>Mapa vzťahov medzi kategóriami kontingenčnej tabuľky – produkty a cieľové skupiny.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie09.html) | [`cvicenie9.R`](cvicenie9.R) | [`exercise9.R`](exercises_EN/exercise9.R) |

#### 🎯 Rozlišovacie testy a panel

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **13** | **Thurstonov model a d′**<br><sub>Rozlišovacie testy inak: d′ ako miera rozdielu nezávislá od metódy, psychometrické funkcie a test podobnosti.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie13.html) | [`cvicenie13.R`](cvicenie13.R) | [`exercise13.R`](exercises_EN/exercise13.R) |
| **14** | **Sila testu a veľkosť panelu**<br><sub>Koľko hodnotiteľov treba? Sila testu, veľkosť vzorky pre rozlišovacie testy, t-test a ANOVA.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie14.html) | [`cvicenie14.R`](cvicenie14.R) | [`exercise14.R`](exercises_EN/exercise14.R) |
| **15** | **Výkonnosť senzorického panelu**<br><sub>Rozlišovanie, zhoda a opakovateľnosť – ako overiť, že panelu a jednotlivým hodnotiteľom možno veriť.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie15.html) | [`cvicenie15.R`](cvicenie15.R) | [`exercise15.R`](exercises_EN/exercise15.R) |
| **16** | **Zmiešané modely**<br><sub>Hodnotiteľ ako náhodný efekt: prečo a ako nahradiť dvojfaktorovú ANOVA modelom lmer.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie16.html) | [`cvicenie16.R`](cvicenie16.R) | [`exercise16.R`](exercises_EN/exercise16.R) |

#### 🛒 Spotrebiteľský výskum

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **11a** | **TURF analýza a mapa preferencií**<br><sub>Optimálna kombinácia ingrediencií (reach & frequency) a prepojenie senzorického profilu s hedonikou.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie11a.html) | [`cvicenie11a.R`](cvicenie11a.R) | [`exercise11a.R`](exercises_EN/exercise11a.R) |
| **11b** | **Text mining a analýza sentimentu**<br><sub>Od voľného textu recenzií k frekvenciám slov, word cloudu a emóciám podľa lexikónu NRC.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie11b.html) | [`cvicenie11b.R`](cvicenie11b.R) | [`exercise11b.R`](exercises_EN/exercise11b.R) |
| **12** | **JAR škála a radarový graf**<br><sub>Just-About-Right hodnotenie pre optimalizáciu receptúry a profilogram senzorických vlastností.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie12.html) | [`cvicenie12.R`](cvicenie12.R) | [`exercise12.R`](exercises_EN/exercise12.R) |

#### ⏱️ Rýchle a temporálne metódy

| # | Téma | Teória | Skript SK | Script EN |
|:-:|------|:------:|:---------:|:---------:|
| **17** | **CATA a napping**<br><sub>Rýchle metódy profilovania so spotrebiteľmi: začiarkavacie otázky a projektívne mapovanie.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie17.html) | [`cvicenie17.R`](cvicenie17.R) | [`exercise17.R`](exercises_EN/exercise17.R) |
| **18** | **Temporálne metódy – TDS a TCATA**<br><sub>Ako sa vnem mení v čase: dominancia vnemov a časové CATA krivky.</sub> | [📖](https://senzorika.github.io/SaIT/teoria/cvicenie18.html) | [`cvicenie18.R`](cvicenie18.R) | [`exercise18.R`](exercises_EN/exercise18.R) |

### 🚀 Ako začať

1. Nainštalujte [R](https://cran.r-project.org/) a potom [RStudio](https://posit.co/download/rstudio-desktop/).
2. Otvorte skript cvičenia v RStudiu a spúšťajte ho po riadkoch (<kbd>Ctrl</kbd> + <kbd>Enter</kbd>).
3. Chýbajúce balíky doinštalujte cez `install.packages("nazov")` – zoznam je v [`cvicenie1.R`](cvicenie1.R).
4. Pred každým cvičením si prečítajte teóriu (📖) – vysvetľuje aj výstupy, ktoré skript vypíše.

> [!NOTE]
> Teoretické stránky sa zobrazujú cez **GitHub Pages**. Pri otvorení `.html` súboru priamo v repozitári GitHub ukáže iba zdrojový kód. Záložná možnosť: [raw.githack.com](https://raw.githack.com/senzorika/SaIT/master/teoria/index.html).

### 📦 Ďalší obsah repozitára

| Priečinok | Obsah |
|---|---|
| 📊 [`datasety/`](datasety/) | datasety k praktickým úlohám (napr. [`001_datasety.txt`](datasety/001_datasety.txt) k cvičeniu 5b) |
| 🖥️ [`Senzometricke_appky/`](Senzometricke_appky/) | interaktívne Shiny aplikácie (PCA, TDS, TCATA, NPS, LDA…) |
| 🇬🇧 [`English/`](English/) | rozšírené anglické materiály a prezentácie |
| 🌐 [`exercises_EN/`](exercises_EN/) · [`teoria/`](teoria/) · [`theory_EN/`](theory_EN/) | anglické skripty, zdrojové súbory teórie SK / EN |

### 🔗 Užitočné odkazy

| | Nástroj | Na čo |
|:-:|---|---|
| 📐 | [R](https://cran.r-project.org/) · [RStudio](https://posit.co/download/rstudio-desktop/) | štatistické prostredie a editor |
| 🤖 | [OpenCode](https://opencode.ai/) | open-source AI asistent na programovanie v termináli – pomoc pri písaní a vysvetľovaní R kódu |
| 💬 | [White Noise](https://www.whitenoise.chat/) | súkromný šifrovaný messenger na protokole Nostr – komunikácia v tíme |
| 🗂️ | [GitHub](https://github.com/) | verziovanie skriptov a dát |
| 🥫 | [Open Food Facts](https://world.openfoodfacts.org/) | otvorená databáza potravín (cvičenie 4) |

---

## 🇬🇧 English

Course materials for **sensory analysis and sensometrics** exercises in R. Every exercise has an R script (in English and Slovak) and a **theory page with graphics**: what the method does, when to use it, how to read the output and what to watch out for.

<p align="center"><a href="https://senzorika.github.io/SaIT/theory_EN/index.html"><b>📖 Open the theory</b></a> &nbsp;·&nbsp; <a href="#-exercises">🧪 Exercises</a> &nbsp;·&nbsp; <a href="#-getting-started">🚀 Getting started</a> &nbsp;·&nbsp; <a href="#-useful-links">🔗 Links</a></p>

### 🧭 Course map

```mermaid
flowchart LR
    B0["🧰 Getting started with R<br/>exercises 1, 2, 3, 4"]
    B1["🧪 Hypothesis testing<br/>exercises 5a, 5b, 5c, 5d"]
    B2["📈 Relationships and modelling<br/>exercises 6, 10"]
    B3["🗺️ Multivariate methods<br/>exercises 7, 8, 9"]
    B4["🎯 Discrimination tests and the panel<br/>exercises 13, 14, 15, 16"]
    B5["🛒 Consumer research<br/>exercises 11a, 11b, 12"]
    B6["⏱️ Rapid and temporal methods<br/>exercises 17, 18"]
    B0 --> B1
    B1 --> B2
    B1 --> B3
    B1 --> B4
    B2 --> B5
    B3 --> B5
    B4 --> B5
    B5 --> B6
    classDef blk fill:#d6efe9,stroke:#0f7b6c,color:#1d1f24
    class B0,B1,B2,B3,B4,B5,B6 blk
```

### 🧪 Exercises

#### 🧰 Getting started with R

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **1** | **R, RStudio and collaboration**<br><sub>The working environment for sensometrics: the R language, the RStudio IDE, packages and tools for teamwork.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise01.html) | [`exercise1.R`](exercises_EN/exercise1.R) | [`cvicenie1.R`](cvicenie1.R) |
| **2** | **BCG matrix and confidence interval**<br><sub>How to place products on a price × quality map and how to express the uncertainty of a panel mean.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise02.html) | [`exercise2.R`](exercises_EN/exercise2.R) | [`cvicenie2.R`](cvicenie2.R) |
| **3** | **Working with data in R**<br><sub>Scalars, vectors, matrices and data frames – the building blocks of every sensory analysis.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise03.html) | [`exercise3.R`](exercises_EN/exercise3.R) | [`cvicenie3.R`](cvicenie3.R) |
| **4** | **Charts and data visualisation**<br><sub>Which chart to choose, how to read it and what it tells us before we reach for a statistical test.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise04.html) | [`exercise4.R`](exercises_EN/exercise4.R) | [`cvicenie4.R`](cvicenie4.R) |

#### 🧪 Hypothesis testing

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **5a** | **Normality and two-sample comparison**<br><sub>Normality checks, t-test versus Wilcoxon, the binomial test in discrimination tests, the χ² test and McNemar’s test.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise05a.html) | [`exercise5a.R`](exercises_EN/exercise5a.R) | [`cvicenie5a.R`](cvicenie5a.R) |
| **5b** | **Comparing several samples**<br><sub>Kruskal-Wallis, Friedman, analysis of variance (ANOVA) and multiple comparisons.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise05b.html) | [`exercise5b.R`](exercises_EN/exercise5b.R) | [`cvicenie5b.R`](cvicenie5b.R) |
| **5c** | **Importing Excel data and the analysis workflow**<br><sub>Loading an online file, the long data format and the complete workflow: normality → test → post-hoc.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise05c.html) | [`exercise5c.R`](exercises_EN/exercise5c.R) | [`cvicenie5c.R`](cvicenie5c.R) |
| **5d** | **Durbin test and incomplete blocks**<br><sub>When an assessor cannot taste every sample: the balanced incomplete block design.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise05d.html) | [`exercise5d.R`](exercises_EN/exercise5d.R) | [`cvicenie5d.R`](cvicenie5d.R) |

#### 📈 Relationships and modelling

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **6** | **Correlation and linear regression**<br><sub>Strength and direction of the relationship between instrumental and sensory attributes, and a simple predictive model.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise06.html) | [`exercise6.R`](exercises_EN/exercise6.R) | [`cvicenie6.R`](cvicenie6.R) |
| **10** | **Survival analysis and sensory shelf life**<br><sub>Kaplan-Meier estimate of the probability of product acceptance over time and finding the cut-off point.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise10.html) | [`exercise10.R`](exercises_EN/exercise10.R) | [`cvicenie10.R`](cvicenie10.R) |

#### 🗺️ Multivariate methods

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **7** | **PCA and factor analysis**<br><sub>Reducing dimensionality: principal components, the Kaiser criterion and hidden factors in consumer answers.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise07.html) | [`exercise7.R`](exercises_EN/exercise7.R) | [`cvicenie7.R`](cvicenie7.R) |
| **8** | **Cluster analysis**<br><sub>Hierarchical clustering (Ward), the dendrogram and k-means for product segmentation.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise08.html) | [`exercise8.R`](exercises_EN/exercise8.R) | [`cvicenie8.R`](cvicenie8.R) |
| **9** | **Correspondence analysis**<br><sub>A map of associations between the categories of a contingency table – products and target groups.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise09.html) | [`exercise9.R`](exercises_EN/exercise9.R) | [`cvicenie9.R`](cvicenie9.R) |

#### 🎯 Discrimination tests and the panel

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **13** | **Thurstonian model and d′**<br><sub>Discrimination tests revisited: d′ as a method-independent measure of difference, psychometric functions and the similarity test.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise13.html) | [`exercise13.R`](exercises_EN/exercise13.R) | [`cvicenie13.R`](cvicenie13.R) |
| **14** | **Statistical power and panel size**<br><sub>How many assessors are needed? Power, sample size for discrimination tests, the t-test and ANOVA.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise14.html) | [`exercise14.R`](exercises_EN/exercise14.R) | [`cvicenie14.R`](cvicenie14.R) |
| **15** | **Sensory panel performance**<br><sub>Discrimination, agreement and repeatability – how to check that the panel and each assessor can be trusted.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise15.html) | [`exercise15.R`](exercises_EN/exercise15.R) | [`cvicenie15.R`](cvicenie15.R) |
| **16** | **Mixed models**<br><sub>The assessor as a random effect: why and how to replace two-way ANOVA with an lmer model.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise16.html) | [`exercise16.R`](exercises_EN/exercise16.R) | [`cvicenie16.R`](cvicenie16.R) |

#### 🛒 Consumer research

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **11a** | **TURF analysis and preference mapping**<br><sub>The optimal combination of ingredients (reach & frequency) and linking the sensory profile with hedonic data.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise11a.html) | [`exercise11a.R`](exercises_EN/exercise11a.R) | [`cvicenie11a.R`](cvicenie11a.R) |
| **11b** | **Text mining and sentiment analysis**<br><sub>From free-text reviews to word frequencies, a word cloud and emotions from the NRC lexicon.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise11b.html) | [`exercise11b.R`](exercises_EN/exercise11b.R) | [`cvicenie11b.R`](cvicenie11b.R) |
| **12** | **JAR scale and radar chart**<br><sub>Just-About-Right evaluation for recipe optimisation and a profile chart of sensory attributes.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise12.html) | [`exercise12.R`](exercises_EN/exercise12.R) | [`cvicenie12.R`](cvicenie12.R) |

#### ⏱️ Rapid and temporal methods

| # | Topic | Theory | Script EN | Skript SK |
|:-:|------|:------:|:---------:|:---------:|
| **17** | **CATA and napping**<br><sub>Rapid profiling methods with consumers: check-all-that-apply questions and projective mapping.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise17.html) | [`exercise17.R`](exercises_EN/exercise17.R) | [`cvicenie17.R`](cvicenie17.R) |
| **18** | **Temporal methods – TDS and TCATA**<br><sub>How perception changes over time: dominance of sensations and temporal CATA curves.</sub> | [📖](https://senzorika.github.io/SaIT/theory_EN/exercise18.html) | [`exercise18.R`](exercises_EN/exercise18.R) | [`cvicenie18.R`](cvicenie18.R) |

### 🚀 Getting started

1. Install [R](https://cran.r-project.org/) and then [RStudio](https://posit.co/download/rstudio-desktop/).
2. Open an exercise script in RStudio and run it line by line (<kbd>Ctrl</kbd> + <kbd>Enter</kbd>).
3. Install missing packages with `install.packages("name")` – see the list in [`exercise1.R`](exercises_EN/exercise1.R).
4. Read the theory (📖) before each exercise – it also explains the output the script prints.

> [!NOTE]
> The theory pages are served via **GitHub Pages**; opening an `.html` file directly in the repository shows only its source code. Fallback: [raw.githack.com](https://raw.githack.com/senzorika/SaIT/master/theory_EN/index.html).

### 📦 Other contents

| Folder | Contents |
|---|---|
| 📊 [`datasety/`](datasety/) | datasets for the practical tasks (e.g. [`001_datasety.txt`](datasety/001_datasety.txt) for exercise 5b) |
| 🖥️ [`Senzometricke_appky/`](Senzometricke_appky/) | interactive Shiny apps (PCA, TDS, TCATA, NPS, LDA…) |
| 🇬🇧 [`English/`](English/) | extended English materials and presentations |
| 🌐 [`exercises_EN/`](exercises_EN/) · [`teoria/`](teoria/) · [`theory_EN/`](theory_EN/) | English scripts, theory source files SK / EN |

### 🔗 Useful links

| | Tool | What for |
|:-:|---|---|
| 📐 | [R](https://cran.r-project.org/) · [RStudio](https://posit.co/download/rstudio-desktop/) | statistical environment and editor |
| 🤖 | [OpenCode](https://opencode.ai/) | open-source AI coding agent for the terminal – help with writing and explaining R code |
| 💬 | [White Noise](https://www.whitenoise.chat/) | private end-to-end encrypted messenger built on Nostr – team communication |
| 🗂️ | [GitHub](https://github.com/) | versioning of scripts and data |
| 🥫 | [Open Food Facts](https://world.openfoodfacts.org/) | open food products database (exercise 4) |
