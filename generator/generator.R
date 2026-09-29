# ============================================================
# Generátor samostatného cvičenia / Exercise generator
# ============================================================
# Použitie (z koreňového priečinka repozitára):
#   source("generator/generator.R")
#   generuj_cvicenie(5)                                  # týždeň 5, slovensky
#   generuj_cvicenie(5, jazyk = "en")                    # anglicky
#   generuj_cvicenie(5, studenti = c(123456, 234567))    # + kľúč s výsledkami pre vyučujúceho
#   kluc_cvicenia(5, studenti = c(123456, 234567))       # len kľúč (data.frame)
#
# Každý študent zadá do skriptu svoje ID (napr. číslo z AIS). Z neho sa cez set.seed()
# vygenerujú jeho vlastné dáta, takže výsledky sa u študentov líšia a vyučujúci ich
# overí kľúčom vygenerovaným pre rovnaké ID.

GEN_DIR <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f)) dirname(normalizePath(f)) else normalizePath("generator")
})
source(file.path(GEN_DIR, "ulohy.R"), encoding = "UTF-8")
source(file.path(GEN_DIR, "tyzdne.R"), encoding = "UTF-8")

SAIT_URL <- "https://senzorika.github.io/SaIT"
SAIT_MAIL <- "senzorickelaboratoriumfbp@gmail.com"

seed_ulohy <- function(student_id, tyzden, uloha) (student_id %% 1000000) * 1000 + tyzden * 10 + uloha

ziskaj_tyzden <- function(tyzden) {
  plan <- TYZDNE[[as.character(tyzden)]]
  if (is.null(plan)) {
    stop("Pre týždeň ", tyzden, " sa cvičenie negeneruje (dostupné týždne: ",
         paste(names(TYZDNE), collapse = ", "), "). V 12. a 13. týždni sú prezentácie a zápočet.")
  }
  plan
}

# ---- answer key -------------------------------------------------------------------------
kluc_cvicenia <- function(tyzden, studenti) {
  plan <- ziskaj_tyzden(tyzden)
  rows <- lapply(studenti, function(id) {
    env <- new.env()
    out <- list(student_id = id)
    for (i in seq_along(plan$tasks)) {
      task <- ULOHY[[plan$tasks[i]]]
      set.seed(seed_ulohy(id, tyzden, i))
      eval(parse(text = task$code), envir = env)
      res <- task$key(env)
      for (nm in names(res)) {
        v <- res[[nm]]
        out[[paste0("U", i, "_", nm)]] <- if (is.numeric(v)) round(v, 4) else as.character(v)
      }
    }
    as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- student R script ---------------------------------------------------------------------
wrap_comment <- function(text, width = 90) {
  text <- gsub("`", "", text)
  paste0("# ", strwrap(text, width = width), collapse = "\n")
}

skript_studenta <- function(tyzden, jazyk) {
  plan <- ziskaj_tyzden(tyzden)
  L <- function(x) x[[jazyk]]
  sk <- jazyk == "sk"
  head <- c(
    "# ============================================================",
    sprintf(if (sk) "# Samostatné cvičenie – týždeň %d (%s)" else "# Independent exercise – week %d (%s)", tyzden, plan$date),
    paste0("# ", L(plan$title)),
    "# ============================================================",
    if (sk) "# 1. Do premennej student_id zadajte svoje ID (číslo z AIS) a spustite celý skript."
    else "# 1. Put your ID (student number) into student_id and run the whole script.",
    if (sk) "# 2. Z ID sa vygenerujú vaše vlastné dáta – každý študent má iné čísla."
    else "# 2. Your ID generates your own data – every student gets different numbers.",
    if (sk) "# 3. Pod každou úlohou doplňte svoj kód a slovný záver (ako komentár)."
    else "# 3. Add your code and a written conclusion (as a comment) under every task.",
    sprintf(if (sk) "# 4. Skript pošlite na %s, predmet: SaIT - tyzden %d - Meno Priezvisko (ID)"
            else "# 4. Send the script to %s, subject: SaIT - week %d - Name Surname (ID)", SAIT_MAIL, tyzden),
    "",
    "student_id <- 0",
    if (sk) "if (student_id == 0) stop(\"Najprv zadajte svoje student_id.\")"
    else "if (student_id == 0) stop(\"Enter your student_id first.\")",
    sprintf("seed <- (student_id %%%% 1000000) * 1000 + %d", tyzden * 10),
    ""
  )
  body <- unlist(lapply(seq_along(plan$tasks), function(i) {
    task <- ULOHY[[plan$tasks[i]]]
    c(
      "#---------------------------------------------------------------------------------------",
      sprintf(if (sk) "# ÚLOHA %d: %s" else "# TASK %d: %s", i, L(task$title)),
      "#---------------------------------------------------------------------------------------",
      wrap_comment(L(task$text)),
      paste0(if (sk) "# Pomôcka: " else "# Hint: ", gsub("`", "", L(task$hint))),
      "",
      if (sk) "# vaše dáta (nemeňte):" else "# your data (do not change):",
      sprintf("set.seed(seed + %d)", i),
      task$code,
      "",
      if (sk) "# váš kód:" else "# your code:",
      "",
      "",
      if (sk) "# záver:" else "# conclusion:",
      "",
      ""
    )
  }))
  paste(c(head, body), collapse = "\n")
}

# ---- HTML page ----------------------------------------------------------------------------
html_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
md_code <- function(x) gsub("`([^`]+)`", "<code>\\1</code>", html_esc(x))

theory_link <- function(slug, jazyk) {
  if (jazyk == "sk") sprintf("%s/teoria/cvicenie%s.html", SAIT_URL, slug)
  else sprintf("%s/theory_EN/exercise%s.html", SAIT_URL, slug)
}
exercise_label <- function(slug, jazyk) {
  num <- sub("^0", "", slug)
  if (jazyk == "sk") paste("Teória", num) else paste("Theory", num)
}

stranka_html <- function(tyzden, jazyk, skript) {
  plan <- ziskaj_tyzden(tyzden)
  L <- function(x) x[[jazyk]]
  sk <- jazyk == "sk"
  css_file <- file.path(dirname(GEN_DIR), "teoria", "style.css")
  css <- if (file.exists(css_file)) paste(readLines(css_file, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
  file_name <- sprintf("cvicenie_tyzden%02d_%s.R", tyzden, jazyk)
  script_uri <- paste0("data:text/plain;charset=utf-8;base64,", jsonlite_free_b64(skript))
  subject <- utils::URLencode(sprintf(if (sk) "SaIT - tyzden %d - Meno Priezvisko (ID)" else "SaIT - week %d - Name Surname (ID)", tyzden), reserved = TRUE)
  deck <- if (sk) sprintf("%s/prezentacie/sk/%s.html", SAIT_URL, plan$deck[1]) else sprintf("%s/prezentacie/en/%s.html", SAIT_URL, plan$deck[2])

  theory_links <- paste(sprintf('<a href="%s">%s</a>', sapply(plan$exercises, theory_link, jazyk = jazyk),
                                sapply(plan$exercises, exercise_label, jazyk = jazyk)), collapse = "")
  points <- paste(sprintf("<li>%s</li>", md_code(L(plan$points))), collapse = "")
  datasets <- if (length(plan$datasets)) {
    paste(sprintf('<li><a href="%s">%s</a> – %s</li>', sapply(plan$datasets, function(d) DATASETY[[d]][["url"]]),
                  sapply(plan$datasets, function(d) basename(DATASETY[[d]][["url"]])),
                  sapply(plan$datasets, function(d) DATASETY[[d]][[jazyk]])), collapse = "")
  } else ""
  tasks <- paste(vapply(seq_along(plan$tasks), function(i) {
    task <- ULOHY[[plan$tasks[i]]]
    sprintf('<section class="case"><h2><span class="level l%d">%s %d</span> %s</h2><p>%s</p><p class="note"><strong>%s</strong>%s</p><details><summary>%s</summary><pre>%s</pre></details></section>',
            min(i, 3), if (sk) "úloha" else "task", i, html_esc(L(task$title)), md_code(L(task$text)),
            if (sk) "Pomôcka" else "Hint", md_code(L(task$hint)),
            if (sk) "Ako vzniknú vaše dáta" else "How your data are generated", html_esc(task$code))
  }, character(1)), collapse = "\n")

  txt <- if (sk) list(
    badge = "SAMOSTATNÉ CVIČENIE", week = "týždeň", lecture = "Prednáška",
    how = "Ako na to", steps = c(
      sprintf('Stiahnite si <a href="%s" download="%s">R skript cvičenia</a> a otvorte ho v RStudiu.', script_uri, file_name),
      "Do riadku <code>student_id &lt;- 0</code> zadajte svoje ID (číslo z AIS) – vygenerujú sa vaše vlastné dáta, každý študent má iné.",
      "Pod každou úlohou doplňte kód a krátky slovný záver.",
      sprintf("Hotový skript pošlite na <a href=\"mailto:%s\">%s</a>.", SAIT_MAIL, SAIT_MAIL)),
    theory = "Teória v skratke", key = "Kľúčové pojmy", more = "Podrobná teória s riešenými príkladmi a prezentácia:",
    slides = "🎓 Prezentácia", data = "Datasety", data_note = "Vaše vlastné dáta vytvorí skript z vášho ID. Na precvičenie môžete použiť aj datasety kurzu:",
    tasks = "Úlohy", submit = "Odovzdanie", submit_text = "R skript s kódom a závermi na", subject = "predmet",
    send = "✉️ Odoslať riešenie", download = "⬇️ Stiahnuť R skript"
  ) else list(
    badge = "INDEPENDENT EXERCISE", week = "week", lecture = "Lecture",
    how = "How it works", steps = c(
      sprintf('Download the <a href="%s" download="%s">R script of the exercise</a> and open it in RStudio.', script_uri, file_name),
      "Put your ID (student number) into the line <code>student_id &lt;- 0</code> – your own data will be generated, different for every student.",
      "Add your code and a short written conclusion under every task.",
      sprintf("Send the finished script to <a href=\"mailto:%s\">%s</a>.", SAIT_MAIL, SAIT_MAIL)),
    theory = "Theory in brief", key = "Key concepts", more = "Detailed theory with solved examples and slides:",
    slides = "🎓 Slides", data = "Datasets", data_note = "Your own data are created by the script from your ID. For practice you can also use the course datasets:",
    tasks = "Tasks", submit = "Submission", submit_text = "R script with code and conclusions to", subject = "subject",
    send = "✉️ Send solution", download = "⬇️ Download R script"
  )

  paste0(
    '<!doctype html><html lang="', jazyk, '"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>', html_esc(sprintf("%s %d – %s", if (sk) "Týždeň" else "Week", tyzden, L(plan$title))), '</title>',
    '<link href="https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&family=JetBrains+Mono:wght@400;600&display=swap" rel="stylesheet">',
    '<style>', css, '</style></head><body><div class="wrap">',
    '<header class="top"><span class="brand">', if (sk) "Senzometria v R · SaIT" else "Sensometrics in R · SaIT", '</span><span>', plan$date, '</span></header>',
    '<section class="hero"><span class="num">', txt$badge, ' · ', txt$week, ' ', tyzden, '</span><h1>', html_esc(L(plan$title)), '</h1>',
    '<p class="lead">', txt$lecture, ': ', html_esc(L(plan$lecture)), '</p>',
    '<div class="files"><a href="', script_uri, '" download="', file_name, '">', txt$download, '</a><a class="deck" href="', deck, '">', txt$slides, '</a></div></section>',
    '<div class="goals"><h2>', txt$how, '</h2><ol>', paste(sprintf("<li>%s</li>", txt$steps), collapse = ""), '</ol></div>',
    '<h2>', txt$theory, '</h2><p>', md_code(L(plan$theory)), '</p>',
    '<div class="note"><strong>', txt$key, '</strong><ul>', points, '</ul></div>',
    '<p>', txt$more, '</p><div class="files">', theory_links, '<a class="deck" href="', deck, '">', txt$slides, '</a></div>',
    if (nzchar(datasets)) paste0('<h2>', txt$data, '</h2><p>', txt$data_note, '</p><ul>', datasets, '</ul>') else "",
    '<h2>', txt$tasks, '</h2>', tasks,
    '<div class="submit"><div><strong>', txt$submit, '</strong><br>', txt$submit_text, ' <a href="mailto:', SAIT_MAIL, '">', SAIT_MAIL, '</a>, ',
    txt$subject, ': <code>', html_esc(utils::URLdecode(subject)), '</code></div>',
    '<a class="btn" href="mailto:', SAIT_MAIL, '?subject=', subject, '">', txt$send, '</a></div>',
    '</div></body></html>'
  )
}

jsonlite_free_b64 <- function(text) {
  bytes <- as.integer(charToRaw(enc2utf8(text)))
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  pad <- (3 - length(bytes) %% 3) %% 3
  bytes <- c(bytes, rep(0L, pad))
  m <- matrix(bytes, nrow = 3)
  n <- m[1, ] * 65536 + m[2, ] * 256 + m[3, ]
  idx <- rbind(n %/% 262144, (n %/% 4096) %% 64, (n %/% 64) %% 64, n %% 64) + 1
  out <- chars[idx]
  if (pad > 0) out[(length(out) - pad + 1):length(out)] <- "="
  paste(out, collapse = "")
}

# ---- main -------------------------------------------------------------------------------
generuj_cvicenie <- function(tyzden, jazyk = c("sk", "en"), studenti = NULL,
                             vystup = file.path(GEN_DIR, "vystup")) {
  jazyk <- match.arg(jazyk)
  plan <- ziskaj_tyzden(tyzden)
  dir <- file.path(vystup, sprintf("tyzden%02d_%s", tyzden, jazyk))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  skript <- skript_studenta(tyzden, jazyk)
  r_file <- file.path(dir, sprintf("cvicenie_tyzden%02d_%s.R", tyzden, jazyk))
  html_file <- file.path(dir, sprintf("cvicenie_tyzden%02d_%s.html", tyzden, jazyk))
  writeLines(enc2utf8(skript), r_file, useBytes = TRUE)
  writeLines(enc2utf8(stranka_html(tyzden, jazyk, skript)), html_file, useBytes = TRUE)
  message("Cvičenie: ", html_file)
  message("Skript:   ", r_file)
  if (!is.null(studenti)) {
    kluc <- kluc_cvicenia(tyzden, studenti)
    key_file <- file.path(dir, sprintf("KLUC_tyzden%02d.csv", tyzden))
    utils::write.csv(kluc, key_file, row.names = FALSE, fileEncoding = "UTF-8")
    message("Kľúč:     ", key_file, "  (len pre vyučujúceho – neposielať študentom!)")
  }
  invisible(dir)
}
