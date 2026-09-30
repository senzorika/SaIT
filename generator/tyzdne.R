# Week plans according to the syllabus "Senzometrika a IT v Potravinárstve" (ZS 2025/26).
# tasks = ids from ULOHY (their order also fixes the order of the random data in the student script)

DATASETY <- list(
  senzorika = c(url = "https://raw.githubusercontent.com/senzorika/SaIT/master/datasety/senzorika.txt", sk = "senzorický profil 16 pízz (13 deskriptorov)", en = "sensory profile of 16 pizzas (13 descriptors)"),
  hedonika = c(url = "https://raw.githubusercontent.com/senzorika/SaIT/master/datasety/hedonika.txt", sk = "hedonické hodnotenia k profilu pízz", en = "hedonic ratings for the pizza profile"),
  cokolada = c(url = "https://raw.githubusercontent.com/senzorika/SaIT/master/datasety/cokolada.txt", sk = "mesačná spotreba čokolády", en = "monthly chocolate consumption"),
  potraviny = c(url = "https://raw.githubusercontent.com/senzorika/SaIT/master/datasety/potraviny.xlsx", sk = "hodnotenia 4 výrobkov (Excel)", en = "ratings of 4 products (Excel)"),
  datasety001 = c(url = "https://github.com/senzorika/SaIT/blob/master/datasety/001_datasety.txt", sk = "cvičné dáta na výber testu", en = "practice data for choosing a test"),
  sensochoc = c(url = "https://rdrr.io/cran/SensoMineR/man/chocolates.html", sk = "balík SensoMineR – dataset chocolates", en = "SensoMineR package – chocolates dataset")
)

TYZDNE <- list(
  "1" = list(
    date = "22.9.2025",
    title = tr("Úvod do senzorickej analýzy", "Introduction to sensory analysis"),
    lecture = tr("Úvod do senzorickej analýzy a spracovania jej výsledkov", "Introduction to sensory analysis and processing of its results"),
    theory = tr(
      "Senzorická analýza meria vlastnosti potravín ľudskými zmyslami. Hodnotiteľ je merací prístroj, ktorý sa unaví, adaptuje a používa škálu po svojom – preto výsledky vždy opisujeme nielen priemerom, ale aj variabilitou. Základom je dobre navrhnutý experiment: kto hodnotí (panel, spotrebitelia), čo hodnotí (vzorky, poradie) a na akej škále.",
      "Sensory analysis measures food properties with the human senses. The assessor is a measuring instrument that gets tired, adapts and uses the scale in their own way – so results are always described not only by the mean but also by their variability. The basis is a well-designed experiment: who rates (panel, consumers), what is rated (samples, order) and on which scale."
    ),
    points = tr(c("priemer, medián, smerodajná odchýlka", "histogram a rozdelenie hodnôt", "plán experimentu: každý hodnotiteľ × každá vzorka"),
                c("mean, median, standard deviation", "histogram and distribution of values", "experiment design: every assessor × every sample")),
    exercises = c("01", "03"), deck = c("01_uvod", "01_introduction"),
    tasks = c("zaklady", "dizajn"), datasets = c("cokolada")
  ),
  "2" = list(
    date = "29.9.2025",
    title = tr("Štatistický balík R a RStudio", "The R statistical package and RStudio"),
    lecture = tr("Úvod do potravinárskej štatistiky, základné pojmy", "Introduction to food statistics, basic terms"),
    theory = tr(
      "R je jazyk na štatistické výpočty, RStudio je editor okolo neho. Kód píšeme do skriptu, aby sa analýza dala zopakovať. Dáta ukladáme do data.frame: jeden riadok = jedno hodnotenie, stĺpce = premenné. Balíky rozširujú R o špecializované metódy – inštalujú sa raz, načítavajú pri každom spustení.",
      "R is a language for statistical computing, RStudio is the editor around it. We write code in a script so the analysis can be repeated. Data are stored in a data.frame: one row = one rating, columns = variables. Packages extend R with specialised methods – installed once, loaded at every start."
    ),
    points = tr(c("vektor, matica, data.frame", "`expand.grid()`, `sample()`, `set.seed()`", "uloženie dát do CSV"), c("vector, matrix, data.frame", "`expand.grid()`, `sample()`, `set.seed()`", "saving data to CSV")),
    exercises = c("01", "03"), deck = c("01_uvod", "01_introduction"),
    tasks = c("dizajn", "zaklady"), datasets = c("cokolada")
  ),
  "3" = list(
    date = "6.10.2025",
    title = tr("Základné dátové operácie a vizualizácia dát", "Basic data operations and data visualisation"),
    lecture = tr("Metodológia I.", "Methodology I"),
    theory = tr(
      "Priemer panelu je len odhad skutočného hodnotenia – jeho neistotu vyjadruje interval spoľahlivosti x̄ ± t · s/√n; pri malom paneli používame t-rozdelenie, nie 1,96. Graf volíme podľa otázky: rozdelenie (histogram, boxplot), porovnanie produktov (boxplot, stĺpce), vzťah dvoch premenných (bodový graf). BCG matica delí produkty podľa priemernej ceny a kvality do štyroch kvadrantov.",
      "The panel mean is only an estimate of the true rating – its uncertainty is expressed by the confidence interval x̄ ± t · s/√n; for a small panel we use the t-distribution, not 1.96. The chart is chosen by the question: distribution (histogram, box plot), comparing products (box plot, bars), relationship of two variables (scatter plot). The BCG matrix splits products into four quadrants by mean price and quality."
    ),
    points = tr(c("interval spoľahlivosti s t-rozdelením", "boxplot a histogram", "BCG matica cena × kvalita"), c("confidence interval with the t-distribution", "box plot and histogram", "BCG matrix price × quality")),
    exercises = c("02", "03", "04"), deck = c("01_uvod", "01_introduction"),
    tasks = c("interval", "bcg", "zaklady"), datasets = c("cokolada", "potraviny")
  ),
  "4" = list(
    date = "13.10.2025",
    title = tr("Prípadová štúdia – zadanie a riešenie", "Case study – assignment and solution"),
    lecture = tr("Software a informačné systémy v potravinárstve", "Software and information systems in the food industry"),
    theory = tr(
      "Prípadová štúdia spája celý postup: otázka → dizajn → dáta → graf → test → záver pre vedenie. Pri rozlišovacom teste porovnávame počet správnych odpovedí s pravdepodobnosťou uhádnutia (triangel 1/3, duo-trio 1/2). Pri hodnotení viacerých receptúr tými istými hodnotiteľmi je hodnotiteľ blok – použijeme Friedmanov test alebo ANOVA s blokom.",
      "A case study joins the whole workflow: question → design → data → plot → test → conclusion for management. In a discrimination test we compare the number of correct answers with the guessing probability (triangle 1/3, duo-trio 1/2). When several recipes are rated by the same assessors, the assessor is a block – we use the Friedman test or ANOVA with a block."
    ),
    points = tr(c("hypotézy a p-hodnota", "binomický test a d′", "Friedman / ANOVA s blokom, post-hoc", "záver zrozumiteľný pre vedenie"), c("hypotheses and the p-value", "binomial test and d′", "Friedman / ANOVA with a block, post-hoc", "a conclusion management understands")),
    exercises = c("05a", "05b", "19"), deck = c("02_testovanie_hypotez", "02_hypothesis_testing"),
    tasks = c("binom", "bloky"), datasets = c("datasety001")
  ),
  "5" = list(
    date = "20.10.2025",
    title = tr("Porovnanie dvoch produktov, binomický test", "Comparing two products, binomial test"),
    lecture = tr("Metodológia II.", "Methodology II"),
    theory = tr(
      "Pri dvoch vzorkách rozhoduje typ dát a dizajn. Spojité párové dáta: párový t-test (normálne rozdiely) alebo Wilcoxonov test. Rozlišovacie testy: binomický test s pravdepodobnosťou uhádnutia, veľkosť rozdielu vyjadrí d′. Početnosti v kategóriách: χ² test. Nepreukázaný rozdiel neznamená rovnaké vzorky – môže ísť o malú silu testu.",
      "With two samples, the data type and design decide. Continuous paired data: paired t-test (normal differences) or the Wilcoxon test. Discrimination tests: the binomial test with the guessing probability, the size of the difference is expressed by d′. Counts in categories: the χ² test. A difference not shown does not mean the samples are the same – the test may have low power."
    ),
    points = tr(c("normalita rozdielov (Shapiro-Wilk)", "párový t-test vs. Wilcoxon", "binomický test, d′", "χ² test dobrej zhody"), c("normality of differences (Shapiro-Wilk)", "paired t-test vs. Wilcoxon", "binomial test, d′", "χ² goodness-of-fit test")),
    exercises = c("05a", "13", "14"), deck = c("02_testovanie_hypotez", "02_hypothesis_testing"),
    tasks = c("parovy", "binom", "chi"), datasets = c("datasety001")
  ),
  "6" = list(
    date = "27.10.2025",
    title = tr("Porovnanie troch a viacerých produktov", "Comparing three or more products"),
    lecture = tr("Štatistické metódy v senzorickej analýze", "Statistical methods in sensory analysis"),
    theory = tr(
      "Pri viacerých vzorkách najprv urobíme jeden globálny test a až pri preukaznom výsledku post-hoc porovnania s korekciou – séria t-testov by zvýšila riziko falošného poplachu. Nezávislé skupiny: ANOVA alebo Kruskal-Wallis. Každý hodnotiteľ hodnotí všetko: ANOVA s blokom hodnotiteľ alebo Friedman; pri neúplných blokoch Durbin.",
      "With several samples we first run one global test and only if it is significant do post-hoc comparisons with a correction – a series of t-tests would increase the risk of a false alarm. Independent groups: ANOVA or Kruskal-Wallis. Everyone rates everything: ANOVA with an assessor block or Friedman; with incomplete blocks Durbin."
    ),
    points = tr(c("ANOVA vs. Kruskal-Wallis", "Friedman a bloky", "Tukey HSD, Bonferroni, Nemenyi"), c("ANOVA vs. Kruskal-Wallis", "Friedman and blocks", "Tukey HSD, Bonferroni, Nemenyi")),
    exercises = c("05b", "05c", "05d"), deck = c("02_testovanie_hypotez", "02_hypothesis_testing"),
    tasks = c("nezavisle", "bloky"), datasets = c("potraviny", "datasety001")
  ),
  "7" = list(
    date = "3.11.2025",
    title = tr("Viacrozmerné metódy v potravinárstve", "Multivariate methods in the food industry"),
    lecture = tr("Metodológia III.", "Methodology III"),
    theory = tr(
      "Korelácia meria silu a smer vzťahu (pre škály Spearman), regresia ho opisuje priamkou a umožňuje predikciu. PCA nahradí veľa korelovaných znakov niekoľkými komponentmi – ponechávame tie s vlastným číslom nad 1. Zhluková analýza hľadá skupiny podobných produktov; podobnosť určuje výška spojenia v dendrograme.",
      "Correlation measures the strength and direction of a relationship (Spearman for scales), regression describes it with a line and allows prediction. PCA replaces many correlated attributes with a few components – we keep those with an eigenvalue above 1. Cluster analysis looks for groups of similar products; similarity is given by the merge height in the dendrogram."
    ),
    points = tr(c("Spearmanova korelácia, lineárna regresia", "PCA, Kaiserovo kritérium, biplot", "hierarchické zhlukovanie (Ward)"), c("Spearman correlation, linear regression", "PCA, Kaiser criterion, biplot", "hierarchical clustering (Ward)")),
    exercises = c("06", "07", "08"), deck = c("05_viacrozmerne_metody", "05_multivariate_methods"),
    tasks = c("korelacia", "pca", "zhluky"), datasets = c("senzorika")
  ),
  "8" = list(
    date = "10.11.2025",
    title = tr("Marketingové metódy – spotrebiteľský výskum", "Marketing methods – consumer research"),
    lecture = tr("Marketingové metódy využívané v potravinárstve", "Marketing methods used in the food industry"),
    theory = tr(
      "Spotrebiteľský výskum odpovedá na otázky, ktoré panel nevyrieši. TURF hľadá kombináciu položiek, ktorá osloví najviac rôznych zákazníkov (reach). JAR škála ukazuje, ktorým smerom upraviť receptúru – pod 70 % odpovedí „akurát“ je znak kandidátom na zmenu. Korešpondenčná analýza zobrazí, ktorá cieľová skupina volí ktorý produkt.",
      "Consumer research answers questions a panel cannot. TURF looks for the combination of items that reaches the most different customers (reach). A JAR scale shows in which direction to adjust the recipe – below 70% “just about right” answers an attribute is a candidate for change. Correspondence analysis shows which target group chooses which product."
    ),
    points = tr(c("TURF: reach a frequency", "JAR škála a smer úpravy", "korešpondenčná analýza kontingenčnej tabuľky"), c("TURF: reach and frequency", "JAR scale and direction of adjustment", "correspondence analysis of a contingency table")),
    exercises = c("11a", "12", "09"), deck = c("06_spotrebitelsky_vyskum", "06_consumer_research"),
    tasks = c("turf", "jar", "korespondencna"), datasets = c("hedonika")
  ),
  "9" = list(
    date = "17.11.2025",
    title = tr("Príprava na čiastkovú skúšku", "Preparation for the partial exam"),
    lecture = tr("Metodológia IV.", "Methodology IV"),
    theory = tr(
      "Opakovanie celého postupu analýzy: rozpoznať typ dát a dizajn, overiť predpoklady, zvoliť test, správne ho zapísať v R a sformulovať záver. Najčastejšie chyby: zámena párových a nezávislých dát, obrátený vzorec (správne hodnota ~ skupina), výklad p ≥ 0,05 ako „vzorky sú rovnaké“ a chýbajúci post-hoc test.",
      "Revision of the whole analysis workflow: recognise the data type and design, check the assumptions, choose the test, write it correctly in R and formulate the conclusion. The most common mistakes: confusing paired and independent data, a reversed formula (correct is value ~ group), reading p ≥ 0.05 as “the samples are the same” and a missing post-hoc test."
    ),
    points = tr(c("výber testu podľa dizajnu", "interpretácia p-hodnoty", "korelácia a regresia"), c("choosing a test by design", "interpreting the p-value", "correlation and regression")),
    exercises = c("05a", "05b", "06", "19"), deck = c("02_testovanie_hypotez", "02_hypothesis_testing"),
    tasks = c("binom", "nezavisle", "korelacia"), datasets = c("datasety001")
  ),
  "10" = list(
    date = "24.11.2025",
    title = tr("Prípadové štúdie – riešenie zadaní", "Case studies – solving assignments"),
    lecture = tr("FoodTech a finančný ekosystém v potravinárstve", "FoodTech and the financial ecosystem in the food industry"),
    theory = tr(
      "Senzorická trvanlivosť sa vyhodnocuje analýzou prežitia: udalosť je odmietnutie produktu, spotrebitelia bez odmietnutia sú cenzúrovaní. Kaplan-Meierova krivka ukazuje podiel akceptujúcich v čase, medián je čas, keď krivka klesne pod 0,5. Výkonnosť panelu posudzujeme podľa rozlišovania, zhody a opakovateľnosti; slabý hodnotiteľ zhoršuje F produktu.",
      "Sensory shelf life is evaluated by survival analysis: the event is rejection of the product, consumers without rejection are censored. The Kaplan-Meier curve shows the share of accepting consumers over time; the median is the time when the curve drops below 0.5. Panel performance is judged by discrimination, agreement and repeatability; a weak assessor lowers the product F."
    ),
    points = tr(c("Kaplan-Meier, cenzúrované dáta", "JAR a odporúčanie úpravy", "zhoda a opakovateľnosť hodnotiteľov"), c("Kaplan-Meier, censored data", "JAR and adjustment recommendation", "agreement and repeatability of assessors")),
    exercises = c("10", "12", "15", "20"), deck = c("06_spotrebitelsky_vyskum", "06_consumer_research"),
    tasks = c("trvanlivost", "jar", "panel"), datasets = c("sensochoc")
  ),
  "11" = list(
    date = "1.12.2025",
    title = tr("Metódy pre semestrálny projekt", "Methods for the semester project"),
    lecture = tr("Metodológia V.", "Methodology V"),
    theory = tr(
      "Pre skupinové projekty sa hodia rýchle a temporálne metódy. CATA: spotrebitelia začiarkujú atribúty, rozdiely testuje Cochranov Q test. TDS: v každej sekunde jeden dominantný vnem; atribút je preukazne dominantný nad hranicou P₀ + 1,645·√(P₀(1 − P₀)/n). Pred experimentom si naplánujte veľkosť panelu – citlivejšie metódy (tetráda, 2-AFC) šetria hodnotiteľov.",
      "Rapid and temporal methods suit group projects. CATA: consumers tick attributes, differences are tested with Cochran’s Q test. TDS: one dominant sensation every second; an attribute is significantly dominant above the limit P₀ + 1.645·√(P₀(1 − P₀)/n). Plan the panel size before the experiment – more sensitive methods (tetrad, 2-AFC) save assessors."
    ),
    points = tr(c("CATA a Cochranov Q test", "TDS krivky a hranica významnosti", "veľkosť panelu (d′, sila testu)"), c("CATA and Cochran’s Q test", "TDS curves and the significance limit", "panel size (d′, power)")),
    exercises = c("17", "18", "14"), deck = c("07_rychle_temporalne_metody", "07_rapid_temporal_methods"),
    tasks = c("cata", "tds", "sila"), datasets = c()
  )
)
