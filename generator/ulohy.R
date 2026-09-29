# Task bank for the exercise generator.
# Every task has: bilingual title / text / hint, the theory pages it relies on, R code that
# generates the student's personal data (run after set.seed), and a key function that
# computes the correct results from the same data (used for the teacher's answer key).

tr <- function(sk, en) list(sk = sk, en = en)

ULOHY <- list()

ULOHY$zaklady <- list(
  title = tr("Základné štatistiky hodnotenia", "Basic statistics of a rating"),
  text = tr(
    "Vektor `ratings` obsahuje hodnotenia chuti jedného produktu na 9-bodovej škále. Vypočítajte priemer, medián a smerodajnú odchýlku, nakreslite histogram a zistite, koľko hodnotiteľov dalo 7 a viac bodov.",
    "The vector `ratings` contains taste ratings of one product on a 9-point scale. Compute the mean, median and standard deviation, draw a histogram and count how many assessors gave 7 or more points."
  ),
  hint = tr("`mean()`, `median()`, `sd()`, `hist()`, `sum(ratings >= 7)` – cvičenie 3.", "`mean()`, `median()`, `sd()`, `hist()`, `sum(ratings >= 7)` – exercise 3."),
  theory = c("03", "04"),
  code = "ratings <- sample(3:9, sample(12:20, 1), replace = TRUE, prob = c(1, 2, 3, 5, 6, 4, 2))",
  key = function(e) with(e, list(n = length(ratings), mean = mean(ratings), median = median(ratings), sd = sd(ratings), n_7plus = sum(ratings >= 7)))
)

ULOHY$dizajn <- list(
  title = tr("Plán senzorického experimentu", "Sensory experiment design"),
  text = tr(
    "V objektoch `n_assessors` a `products` je počet hodnotiteľov a zoznam vzoriek. Vytvorte data.frame s plánom, kde každý hodnotiteľ hodnotí každú vzorku v náhodnom poradí (stĺpce hodnotiteľ, vzorka, poradie). Koľko riadkov má plán? Uložte ho do súboru `plan.csv`.",
    "The objects `n_assessors` and `products` hold the number of assessors and the list of samples. Create a data.frame with a plan in which every assessor rates every sample in random order (columns assessor, sample, order). How many rows does the plan have? Save it as `plan.csv`."
  ),
  hint = tr("`expand.grid()`, `sample()`, `write.csv()` – cvičenie 3.", "`expand.grid()`, `sample()`, `write.csv()` – exercise 3."),
  theory = c("03"),
  code = "n_assessors <- sample(5:9, 1)\nproducts <- LETTERS[1:sample(3:5, 1)]",
  key = function(e) with(e, list(n_assessors = n_assessors, n_products = length(products), rows = n_assessors * length(products)))
)

ULOHY$interval <- list(
  title = tr("Interval spoľahlivosti priemeru", "Confidence interval of the mean"),
  text = tr(
    "Vektor `taste` obsahuje hodnotenia chuti od panelu. Vypočítajte priemer a 95 % interval spoľahlivosti (t-rozdelenie) a znázornite ho v grafe. Prečo nepoužijeme číslo 1,96?",
    "The vector `taste` contains taste ratings from a panel. Compute the mean and the 95% confidence interval (t-distribution) and show it in a plot. Why don't we use 1.96?"
  ),
  hint = tr("`qt(0.975, df = n - 1)`, `sd(x) / sqrt(n)` – cvičenie 2.", "`qt(0.975, df = n - 1)`, `sd(x) / sqrt(n)` – exercise 2."),
  theory = c("02"),
  code = "taste <- round(rnorm(sample(6:12, 1), mean = runif(1, 5, 8), sd = runif(1, 0.5, 1.5)), 1)",
  key = function(e) with(e, {
    d <- sd(taste) / sqrt(length(taste)) * qt(0.975, length(taste) - 1)
    list(n = length(taste), mean = mean(taste), lower = mean(taste) - d, upper = mean(taste) + d)
  })
)

ULOHY$bcg <- list(
  title = tr("Mapa cena × kvalita (BCG)", "Price × quality map (BCG)"),
  text = tr(
    "Vektory `price` a `quality` obsahujú cenu a senzorickú kvalitu 6 produktov (A – F). Nakreslite modifikovanú BCG maticu s deliacimi čiarami v priemeroch a určte, ktoré produkty sú „výhodná kúpa“ (nižšia cena, vyššia kvalita) a ktoré „problémové“.",
    "The vectors `price` and `quality` hold the price and sensory quality of 6 products (A – F). Draw the modified BCG matrix with dividing lines at the means and determine which products are “good value” (lower price, higher quality) and which are “problem” products."
  ),
  hint = tr("`plot()`, `abline(v = mean(price))`, `text()` – cvičenie 2.", "`plot()`, `abline(v = mean(price))`, `text()` – exercise 2."),
  theory = c("02", "04"),
  code = "price <- setNames(round(runif(6, 4, 10), 2), LETTERS[1:6])\nquality <- setNames(round(runif(6, 3, 9), 1), LETTERS[1:6])",
  key = function(e) with(e, list(
    mean_price = mean(price), mean_quality = mean(quality),
    good_value = paste(names(price)[price < mean(price) & quality > mean(quality)], collapse = " "),
    problem = paste(names(price)[price > mean(price) & quality < mean(quality)], collapse = " ")
  ))
)

ULOHY$binom <- list(
  title = tr("Rozlišovací test", "Discrimination test"),
  text = tr(
    "Objekt `method` hovorí, či išlo o trojuholníkový test alebo duo-trio, `n` je počet hodnotiteľov a `correct` počet správnych odpovedí. Sformulujte hypotézy, vypočítajte binomický test (α = 0,05) a d′. Napíšte záver – dá sa tvrdiť, že vzorky sú rovnaké?",
    "The object `method` tells whether a triangle or a duo-trio test was run, `n` is the number of assessors and `correct` the number of correct answers. State the hypotheses, compute the binomial test (α = 0.05) and d′. Write a conclusion – can you claim the samples are the same?"
  ),
  hint = tr("`binom.test(..., alternative = \"greater\")`, `sensR::discrim()` – cvičenia 5a a 13.", "`binom.test(..., alternative = \"greater\")`, `sensR::discrim()` – exercises 5a and 13."),
  theory = c("05a", "13"),
  code = paste(
    "method <- sample(c(\"triangle\", \"duotrio\"), 1)",
    "n <- sample(30:70, 1)",
    "correct <- rbinom(1, n, if (method == \"triangle\") runif(1, 0.34, 0.56) else runif(1, 0.51, 0.72))",
    sep = "\n"
  ),
  key = function(e) with(e, {
    pg <- if (method == "triangle") 1 / 3 else 1 / 2
    p <- binom.test(correct, n, p = pg, alternative = "greater")$p.value
    d <- coef(sensR::discrim(correct, n, method = method))["d-prime", "Estimate"]
    list(method = method, n = n, correct = correct, p_value = p, significant = p < 0.05, d_prime = d)
  })
)

ULOHY$parovy <- list(
  title = tr("Porovnanie dvoch produktov (párové dáta)", "Comparing two products (paired data)"),
  text = tr(
    "Vektory `product_A` a `product_B` obsahujú hodnotenia tých istých hodnotiteľov. Overte normalitu rozdielov, vyberte vhodný test (párový t-test alebo Wilcoxon) a rozhodnite, či sa produkty líšia.",
    "The vectors `product_A` and `product_B` contain ratings by the same assessors. Check the normality of the differences, choose a suitable test (paired t-test or Wilcoxon) and decide whether the products differ."
  ),
  hint = tr("`shapiro.test(A - B)`, `t.test(..., paired = TRUE)`, `wilcox.test(..., paired = TRUE)` – cvičenie 5a.", "`shapiro.test(A - B)`, `t.test(..., paired = TRUE)`, `wilcox.test(..., paired = TRUE)` – exercise 5a."),
  theory = c("05a"),
  code = paste(
    "n_pairs <- sample(10:16, 1)",
    "base <- rnorm(n_pairs, 6, 1)",
    "product_A <- round(pmin(9, pmax(1, base + rnorm(n_pairs, 0, 0.6))), 1)",
    "product_B <- round(pmin(9, pmax(1, base + runif(1, -0.2, 1.1) + rnorm(n_pairs, 0, 0.6))), 1)",
    "rm(base)",
    sep = "\n"
  ),
  key = function(e) with(e, list(
    mean_difference = mean(product_B - product_A),
    shapiro_p = shapiro.test(product_B - product_A)$p.value,
    t_test_p = t.test(product_A, product_B, paired = TRUE)$p.value,
    wilcoxon_p = suppressWarnings(wilcox.test(product_A, product_B, paired = TRUE, exact = FALSE)$p.value)
  ))
)

ULOHY$chi <- list(
  title = tr("Preferencia príchutí (χ² test)", "Flavour preference (χ² test)"),
  text = tr(
    "Vektor `choices` obsahuje počty spotrebiteľov, ktorí si vybrali príchuť A – D. Je preferencia rovnomerná? Vypočítajte χ² test a nakreslite stĺpcový graf s očakávanou početnosťou.",
    "The vector `choices` contains the numbers of consumers who chose flavour A – D. Is the preference uniform? Compute the χ² test and draw a bar chart with the expected count."
  ),
  hint = tr("`chisq.test()`, `barplot()`, `abline(h = sum(x) / 4)` – cvičenie 5a.", "`chisq.test()`, `barplot()`, `abline(h = sum(x) / 4)` – exercise 5a."),
  theory = c("05a"),
  code = "choices <- setNames(as.vector(rmultinom(1, sample(80:150, 1), prob = runif(4, 0.6, 1.4))), c(\"A\", \"B\", \"C\", \"D\"))",
  key = function(e) with(e, {
    t <- chisq.test(choices)
    list(total = sum(choices), chi2 = unname(t$statistic), p_value = t$p.value, most_chosen = names(which.max(choices)))
  })
)

ULOHY$nezavisle <- list(
  title = tr("Viac produktov – nezávislé skupiny", "Several products – independent groups"),
  text = tr(
    "Data.frame `scores` obsahuje hodnotenia produktov od rôznych skupín hodnotiteľov (každý hodnotil len jeden produkt). Overte normalitu v skupinách, vyberte ANOVA alebo Kruskal-Wallis a pri preukaznom rozdiele urobte post-hoc porovnanie.",
    "The data.frame `scores` contains ratings of products by different groups of assessors (each rated only one product). Check normality within groups, choose ANOVA or Kruskal-Wallis and, if significant, run a post-hoc comparison."
  ),
  hint = tr("`tapply(..., shapiro.test)`, `kruskal.test(score ~ product)`, `pairwise.wilcox.test()` – cvičenie 5b.", "`tapply(..., shapiro.test)`, `kruskal.test(score ~ product)`, `pairwise.wilcox.test()` – exercise 5b."),
  theory = c("05b", "05c"),
  code = paste(
    "k <- sample(3:4, 1)",
    "n_group <- sample(8:12, 1)",
    "effect <- runif(k, -1.5, 1.5)",
    "scores <- data.frame(",
    "  product = factor(rep(LETTERS[1:k], each = n_group)),",
    "  score = round(pmin(10, pmax(0, 5 + rep(effect, each = n_group) + rexp(k * n_group, 1) - 1)), 1)",
    ")",
    "rm(effect)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    pw <- suppressWarnings(pairwise.wilcox.test(scores$score, scores$product, p.adjust.method = "bonferroni", exact = FALSE)$p.value)
    list(
      min_shapiro_p = min(tapply(scores$score, scores$product, function(x) shapiro.test(x)$p.value)),
      kruskal_p = kruskal.test(score ~ product, data = scores)$p.value,
      anova_p = summary(aov(score ~ product, data = scores))[[1]][1, 5],
      significant_pairs = sum(pw < 0.05, na.rm = TRUE),
      best_product = names(which.max(tapply(scores$score, scores$product, mean)))
    )
  })
)

ULOHY$bloky <- list(
  title = tr("Viac produktov – každý hodnotí všetko", "Several products – everyone rates everything"),
  text = tr(
    "Data.frame `liking` obsahuje hodnotenia 4 receptúr; každý hodnotiteľ ochutnal všetky. Určte dizajn, vyberte Friedmanov test alebo ANOVA s blokom hodnotiteľ, urobte post-hoc porovnanie a odporučte najlepšiu receptúru.",
    "The data.frame `liking` contains ratings of 4 recipes; every assessor tasted all of them. Determine the design, choose the Friedman test or ANOVA with an assessor block, run a post-hoc comparison and recommend the best recipe."
  ),
  hint = tr("`friedman.test(liking ~ recipe | assessor)`, `aov(liking ~ recipe + assessor)`, `TukeyHSD()` – cvičenie 5b.", "`friedman.test(liking ~ recipe | assessor)`, `aov(liking ~ recipe + assessor)`, `TukeyHSD()` – exercise 5b."),
  theory = c("05b", "05d"),
  code = paste(
    "n_assessors <- sample(8:14, 1)",
    "recipe_effect <- runif(4, -1.5, 1.5)",
    "assessor_effect <- rnorm(n_assessors, 0, 1)",
    "liking <- data.frame(",
    "  assessor = factor(rep(1:n_assessors, times = 4)),",
    "  recipe = factor(rep(LETTERS[1:4], each = n_assessors))",
    ")",
    "liking$liking <- round(pmin(9, pmax(1, 6 + rep(recipe_effect, each = n_assessors) +",
    "  rep(assessor_effect, times = 4) + rnorm(4 * n_assessors, 0, 0.8))))",
    "rm(recipe_effect, assessor_effect)",
    sep = "\n"
  ),
  key = function(e) with(e, list(
    friedman_p = friedman.test(liking ~ recipe | assessor, data = liking)$p.value,
    anova_block_p = summary(aov(liking ~ recipe + assessor, data = liking))[[1]][1, 5],
    best_recipe = names(which.max(tapply(liking$liking, liking$recipe, mean))),
    worst_recipe = names(which.min(tapply(liking$liking, liking$recipe, mean)))
  ))
)

ULOHY$korelacia <- list(
  title = tr("Korelácia a regresia", "Correlation and regression"),
  text = tr(
    "Vektory `sugar` (g/100 g), `sweetness` a `acidity` (senzorické hodnotenia) opisujú 15 vzoriek nápoja. Vypočítajte Spearmanove korelácie, zostrojte regresný model sladkosti podľa cukru a predikujte sladkosť pri 8 g cukru.",
    "The vectors `sugar` (g/100 g), `sweetness` and `acidity` (sensory ratings) describe 15 drink samples. Compute Spearman correlations, build a regression model of sweetness on sugar and predict sweetness at 8 g of sugar."
  ),
  hint = tr("`cor(..., method = \"spearman\")`, `lm(sweetness ~ sugar)`, `predict()` – cvičenie 6.", "`cor(..., method = \"spearman\")`, `lm(sweetness ~ sugar)`, `predict()` – exercise 6."),
  theory = c("06"),
  code = paste(
    "sugar <- round(runif(15, 2, 12), 1)",
    "sweetness <- round(1 + runif(1, 0.4, 0.8) * sugar + rnorm(15, 0, 1), 1)",
    "acidity <- round(8 - 0.3 * sugar + rnorm(15, 0, 1.2), 1)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    m <- lm(sweetness ~ sugar)
    list(
      spearman_sugar_sweetness = cor(sugar, sweetness, method = "spearman"),
      spearman_sugar_acidity = cor(sugar, acidity, method = "spearman"),
      intercept = coef(m)[[1]], slope = coef(m)[[2]], r_squared = summary(m)$r.squared,
      prediction_8g = predict(m, data.frame(sugar = 8))[[1]]
    )
  })
)

ULOHY$pca <- list(
  title = tr("Analýza hlavných komponentov", "Principal component analysis"),
  text = tr(
    "Data.frame `survey` obsahuje odpovede 20 spotrebiteľov (7-bodová škála) na dôležitosť chuti, ceny, značky a obalu. Urobte PCA z korelačnej matice, určte počet komponentov podľa Kaisera, nakreslite biplot a pomenujte prvý komponent.",
    "The data.frame `survey` contains answers of 20 consumers (7-point scale) on the importance of taste, price, brand and package. Run a PCA on the correlation matrix, determine the number of components by Kaiser, draw a biplot and name the first component."
  ),
  hint = tr("`princomp(survey, cor = TRUE)`, `summary(..., loadings = TRUE)`, `biplot()` – cvičenie 7.", "`princomp(survey, cor = TRUE)`, `summary(..., loadings = TRUE)`, `biplot()` – exercise 7."),
  theory = c("07"),
  code = paste(
    "f1 <- rnorm(20)",
    "f2 <- rnorm(20)",
    "survey <- data.frame(",
    "  taste = 4 + f1 + rnorm(20, 0, 0.6), price = 4 - 0.5 * f1 + f2 + rnorm(20, 0, 0.6),",
    "  brand = 4 + f2 + rnorm(20, 0, 0.6), package = 4 + 0.8 * f2 + rnorm(20, 0, 0.7)",
    ")",
    "survey[] <- lapply(survey, function(x) pmin(7, pmax(1, round(x))))",
    "rm(f1, f2)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    ev <- eigen(cor(survey))$values
    list(eigen_1 = ev[1], eigen_2 = ev[2], eigen_3 = ev[3], kaiser_components = sum(ev > 1), pc1_percent = 100 * ev[1] / sum(ev))
  })
)

ULOHY$zhluky <- list(
  title = tr("Zhluková analýza produktov", "Cluster analysis of products"),
  text = tr(
    "Matica `profiles` obsahuje senzorický profil 8 produktov (P1 – P8) v štyroch deskriptoroch. Urobte hierarchické zhlukovanie (Ward, euklidovská vzdialenosť), rozdeľte produkty do 3 skupín a nájdite produkt najpodobnejší P1.",
    "The matrix `profiles` contains the sensory profile of 8 products (P1 – P8) on four descriptors. Run hierarchical clustering (Ward, Euclidean distance), split the products into 3 groups and find the product most similar to P1."
  ),
  hint = tr("`hclust(dist(profiles), method = \"ward.D2\")`, `cutree(k = 3)` – cvičenie 8.", "`hclust(dist(profiles), method = \"ward.D2\")`, `cutree(k = 3)` – exercise 8."),
  theory = c("08"),
  code = paste(
    "centers <- matrix(runif(12, 2, 8), 3)",
    "group <- sample(rep(1:3, length.out = 8))",
    "profiles <- round(centers[group, ] + matrix(rnorm(32, 0, 0.6), 8), 2)",
    "dimnames(profiles) <- list(paste0(\"P\", 1:8), c(\"sweet\", \"sour\", \"bitter\", \"salty\"))",
    "rm(centers, group)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    g <- cutree(hclust(dist(profiles), method = "ward.D2"), k = 3)
    dd <- as.matrix(dist(profiles))["P1", -1]
    list(groups = paste(tapply(names(g), g, paste, collapse = "+"), collapse = " | "), closest_to_P1 = names(which.min(dd)), distance = min(dd))
  })
)

ULOHY$korespondencna <- list(
  title = tr("Korešpondenčná analýza", "Correspondence analysis"),
  text = tr(
    "Matica `tab` obsahuje počty spotrebiteľov zo 4 cieľových skupín (G1 – G4), ktorí si vybrali produkt A, B alebo C. Otestujte závislosť χ² testom, urobte korešpondenčnú analýzu a opíšte, ktorá skupina preferuje ktorý produkt.",
    "The matrix `tab` contains counts of consumers from 4 target groups (G1 – G4) who chose product A, B or C. Test the association with a χ² test, run a correspondence analysis and describe which group prefers which product."
  ),
  hint = tr("`chisq.test(tab)`, `MASS::corresp(tab, nf = 2)`, `biplot()` – cvičenie 9.", "`chisq.test(tab)`, `MASS::corresp(tab, nf = 2)`, `biplot()` – exercise 9."),
  theory = c("09"),
  code = "tab <- matrix(rpois(12, lambda = runif(12, 5, 30)), nrow = 4, dimnames = list(paste0(\"G\", 1:4), c(\"A\", \"B\", \"C\")))",
  key = function(e) with(e, {
    p <- tab / sum(tab)
    r <- rowSums(p)
    cc <- colSums(p)
    s <- svd((p - r %o% cc) / sqrt(r %o% cc))$d^2
    list(chi2_p = suppressWarnings(chisq.test(tab)$p.value), dim1_percent = 100 * s[1] / sum(s), total = sum(tab))
  })
)

ULOHY$turf <- list(
  title = tr("TURF – výber ingrediencií", "TURF – choosing ingredients"),
  text = tr(
    "Matica `likes` (100 respondentov × 6 ingrediencií) obsahuje 1, ak má respondent ingredienciu rád. Nájdite kombináciu 3 ingrediencií s najvyšším dosahom (reach) a vypočítajte jej frekvenciu. Líši sa od troch najobľúbenejších ingrediencií?",
    "The matrix `likes` (100 respondents × 6 ingredients) contains 1 if a respondent likes the ingredient. Find the combination of 3 ingredients with the highest reach and compute its frequency. Does it differ from the three most popular ingredients?"
  ),
  hint = tr("`combn(colnames(likes), 3)`, `mean(rowSums(likes[, k]) > 0)` – cvičenie 11a.", "`combn(colnames(likes), 3)`, `mean(rowSums(likes[, k]) > 0)` – exercise 11a."),
  theory = c("11a"),
  code = paste(
    "ingredients <- c(\"cheese\", \"ham\", \"olives\", \"corn\", \"mushrooms\", \"onion\")",
    "likes <- sapply(runif(6, 0.2, 0.7), function(p) rbinom(100, 1, p))",
    "colnames(likes) <- ingredients",
    sep = "\n"
  ),
  key = function(e) with(e, {
    cmb <- combn(colnames(likes), 3)
    reach <- apply(cmb, 2, function(k) mean(rowSums(likes[, k]) > 0))
    best <- cmb[, which.max(reach)]
    list(
      best_combination = paste(best, collapse = "+"), reach = max(reach),
      frequency = mean(rowSums(likes[, best])),
      top3_popular = paste(names(sort(colMeans(likes), decreasing = TRUE))[1:3], collapse = "+")
    )
  })
)

ULOHY$jar <- list(
  title = tr("JAR hodnotenie", "JAR evaluation"),
  text = tr(
    "Vektor `jar` obsahuje počty odpovedí na JAR škále sladkosti (--, -, JAR, +, ++). Vypočítajte podiely, nakreslite graf a odporučte, či treba sladkosť upraviť a ktorým smerom.",
    "The vector `jar` contains the counts of answers on a JAR sweetness scale (--, -, JAR, +, ++). Compute the shares, draw a chart and recommend whether and in which direction to adjust the sweetness."
  ),
  hint = tr("`prop.table(jar)`, `barplot()`; pravidlo: JAR ≥ 70 % v poriadku – cvičenie 12.", "`prop.table(jar)`, `barplot()`; rule: JAR ≥ 70% is fine – exercise 12."),
  theory = c("12"),
  code = paste(
    "jar <- as.vector(rmultinom(1, sample(60:120, 1), prob = c(runif(1, 0.02, 0.1), runif(1, 0.05, 0.25),",
    "  runif(1, 0.3, 0.65), runif(1, 0.05, 0.25), runif(1, 0.02, 0.12))))",
    "names(jar) <- c(\"--\", \"-\", \"JAR\", \"+\", \"++\")",
    sep = "\n"
  ),
  key = function(e) with(e, {
    s <- jar / sum(jar)
    low <- s[1] + s[2]
    high <- s[4] + s[5]
    rec <- if (s[3] >= 0.7) "ok" else if (high > low) "decrease" else "increase"
    list(jar_percent = 100 * s[[3]], too_little_percent = 100 * low, too_much_percent = 100 * high, recommendation = rec)
  })
)

ULOHY$trvanlivost <- list(
  title = tr("Senzorická trvanlivosť", "Sensory shelf life"),
  text = tr(
    "Data.frame `shelf` obsahuje čas (h) prvého odmietnutia produktu každým spotrebiteľom; `rejected = 0` znamená, že do konca testu (72 h) neodmietol. Zostrojte Kaplan-Meierovu krivku, určte medián trvanlivosti a čas, keď produkt odmietne 25 % spotrebiteľov.",
    "The data.frame `shelf` contains the time (h) of each consumer’s first rejection; `rejected = 0` means no rejection by the end of the test (72 h). Build the Kaplan-Meier curve and determine the median shelf life and the time at which 25% of consumers reject the product."
  ),
  hint = tr("`survival::survfit(Surv(time, rejected) ~ 1)`, `summary(fit)` – cvičenie 10.", "`survival::survfit(Surv(time, rejected) ~ 1)`, `summary(fit)` – exercise 10."),
  theory = c("10"),
  code = paste(
    "raw <- rweibull(20, shape = runif(1, 2, 4), scale = runif(1, 30, 60))",
    "shelf <- data.frame(time = pmin(72, ceiling(raw / 12) * 12), rejected = as.integer(raw <= 72))",
    "rm(raw)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    fit <- survival::survfit(survival::Surv(time, rejected) ~ 1, data = shelf)
    s <- summary(fit)
    t75 <- s$time[s$surv <= 0.75][1]
    list(n = nrow(shelf), rejections = sum(shelf$rejected), median_h = unname(summary(fit)$table["median"]), time_25pct_rejected_h = t75)
  })
)

ULOHY$panel <- list(
  title = tr("Výkonnosť hodnotiteľov", "Assessor performance"),
  text = tr(
    "Data.frame `panel` obsahuje hodnotenie intenzity jedného deskriptora (6 hodnotiteľov × 4 produkty × 2 opakovania). Vypočítajte pre každého hodnotiteľa zhodu s panelom a opakovateľnosť a nájdite najslabšieho hodnotiteľa. Ako sa zmení F produktu v ANOVA, ak ho vynecháte?",
    "The data.frame `panel` contains intensity ratings of one descriptor (6 assessors × 4 products × 2 replicates). For every assessor compute agreement with the panel and repeatability and find the weakest assessor. How does the product F in ANOVA change when you leave him/her out?"
  ),
  hint = tr("`tapply(..., list(product, assessor), mean)`, `cor()`, `aov(intensity ~ product + assessor)` – cvičenie 15.", "`tapply(..., list(product, assessor), mean)`, `cor()`, `aov(intensity ~ product + assessor)` – exercise 15."),
  theory = c("15", "16"),
  code = paste(
    "panel <- expand.grid(assessor = factor(paste0(\"H\", 1:6)), product = factor(paste0(\"P\", 1:4)), replicate = factor(1:2))",
    "truth <- runif(4, 2, 8)",
    "shift <- rnorm(6, 0, 0.8)",
    "panel$intensity <- truth[panel$product] + shift[panel$assessor] + rnorm(nrow(panel), 0, 0.5)",
    "odd <- panel$assessor == paste0(\"H\", sample(1:6, 1))",
    "panel$intensity[odd] <- mean(truth) + rnorm(sum(odd), 0, 1.8)",
    "panel$intensity <- round(pmin(10, pmax(0, panel$intensity)), 1)",
    "rm(truth, shift, odd)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    avg <- tapply(panel$intensity, list(panel$product, panel$assessor), mean)
    agreement <- apply(avg, 2, cor, y = rowMeans(avg))
    weakest <- names(which.min(agreement))
    f_all <- summary(aov(intensity ~ product + assessor, data = panel))[[1]][1, 4]
    f_red <- summary(aov(intensity ~ product + assessor, data = droplevels(panel[panel$assessor != weakest, ])))[[1]][1, 4]
    list(weakest_assessor = weakest, weakest_agreement = min(agreement), F_all = f_all, F_without_weakest = f_red)
  })
)

ULOHY$cata <- list(
  title = tr("CATA – ktoré atribúty rozlišujú produkty", "CATA – which attributes discriminate the products"),
  text = tr(
    "Data.frame `cata` obsahuje odpovede 30 spotrebiteľov na 3 produkty (1 = atribút začiarkol). Spočítajte tabuľku počtov produkty × atribúty a pre každý atribút vypočítajte Cochranov Q test. Ktoré atribúty produkty preukazne rozlišujú?",
    "The data.frame `cata` contains answers of 30 consumers for 3 products (1 = attribute ticked). Build the table of counts products × attributes and compute Cochran’s Q test for every attribute. Which attributes discriminate the products significantly?"
  ),
  hint = tr("`tapply()`, `DescTools::CochranQTest(y ~ product | consumer, data = ...)` – cvičenie 17.", "`tapply()`, `DescTools::CochranQTest(y ~ product | consumer, data = ...)` – exercise 17."),
  theory = c("17"),
  code = paste(
    "attributes <- c(\"sweet\", \"sour\", \"creamy\", \"fruity\")",
    "prob <- matrix(runif(12, 0.1, 0.8), nrow = 3, dimnames = list(c(\"A\", \"B\", \"C\"), attributes))",
    "cata <- expand.grid(consumer = factor(1:30), product = factor(c(\"A\", \"B\", \"C\")))",
    "for (a in attributes) cata[[a]] <- rbinom(nrow(cata), 1, prob[as.character(cata$product), a])",
    "rm(prob, a)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    cochran <- function(y, g, b) {
      m <- tapply(y, list(b, g), sum)
      k <- ncol(m)
      tot <- sum(m)
      q <- (k - 1) * (k * sum(colSums(m)^2) - tot^2) / (k * tot - sum(rowSums(m)^2))
      pchisq(q, k - 1, lower.tail = FALSE)
    }
    p <- sapply(attributes, function(a) cochran(cata[[a]], cata$product, cata$consumer))
    c(setNames(as.list(p), paste0("cochran_p_", attributes)), list(significant = paste(attributes[p < 0.05], collapse = " ")))
  })
)

ULOHY$tds <- list(
  title = tr("TDS krivky", "TDS curves"),
  text = tr(
    "Matica `tds` (20 hodnotiteľov × 41 časových bodov) obsahuje dominantný atribút v každej sekunde. Vypočítajte miery dominancie, nakreslite TDS krivky s hladinou náhody a hranicou významnosti a určte, v ktorom intervale je každý atribút preukazne dominantný.",
    "The matrix `tds` (20 assessors × 41 time points) contains the dominant attribute at every second. Compute dominance rates, draw TDS curves with the chance level and the significance limit and determine in which interval each attribute is significantly dominant."
  ),
  hint = tr("`colMeans(tds == a)`, P₀ + 1,645·√(P₀(1 − P₀)/n), `matplot()` – cvičenie 18.", "`colMeans(tds == a)`, P₀ + 1.645·√(P₀(1 − P₀)/n), `matplot()` – exercise 18."),
  theory = c("18"),
  code = paste(
    "time <- 0:40",
    "tds_attributes <- c(\"sweet\", \"sour\", \"bitter\", \"astringent\")",
    "peaks <- sort(runif(4, 3, 37))",
    "weight <- sapply(peaks, function(p) dnorm(time, p, runif(1, 4, 8)))",
    "tds <- t(sapply(1:20, function(h) apply(weight, 1, function(w) sample(tds_attributes, 1, prob = w + 1e-4))))",
    "rm(peaks, weight)",
    sep = "\n"
  ),
  key = function(e) with(e, {
    dom <- sapply(tds_attributes, function(a) colMeans(tds == a))
    lim <- 0.25 + 1.645 * sqrt(0.25 * 0.75 / 20)
    iv <- sapply(tds_attributes, function(a) {
      t <- time[dom[, a] > lim]
      if (length(t)) paste0(min(t), "-", max(t), " s") else "none"
    })
    c(list(significance_limit = lim), setNames(as.list(iv), paste0("dominant_", tds_attributes)))
  })
)

ULOHY$sila <- list(
  title = tr("Plánovanie veľkosti panelu", "Planning the panel size"),
  text = tr(
    "Objekty `d_target` a `target_power` udávajú očakávaný rozdiel medzi vzorkami (d′) a požadovanú silu testu. Vypočítajte potrebný počet hodnotiteľov pre triangel, tetrádu a 2-AFC a odporučte metódu.",
    "The objects `d_target` and `target_power` give the expected difference between samples (d′) and the required power. Compute the required number of assessors for the triangle, tetrad and 2-AFC and recommend a method."
  ),
  hint = tr("`library(sensR)`, `d.primeSS(d_target, target.power = target_power, method = \"triangle\")` – cvičenie 14.", "`library(sensR)`, `d.primeSS(d_target, target.power = target_power, method = \"triangle\")` – exercise 14."),
  theory = c("14", "13"),
  code = "d_target <- round(runif(1, 0.7, 1.5), 1)\ntarget_power <- sample(c(0.8, 0.9), 1)",
  key = function(e) with(e, {
    suppressPackageStartupMessages(library(sensR))
    list(
    d_target = d_target, target_power = target_power,
    n_triangle = sensR::d.primeSS(d_target, target.power = target_power, method = "triangle"),
    n_tetrad = sensR::d.primeSS(d_target, target.power = target_power, method = "tetrad"),
    n_2AFC = sensR::d.primeSS(d_target, target.power = target_power, method = "twoAFC")
    )
  })
)
