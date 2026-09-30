# Runs every exercise script in a fresh R process and reports which ones fail.
# Usage (from the repository root):
#   Rscript tools/check_scripts.R                # datasets are read from GitHub, as students do
#   Rscript tools/check_scripts.R --local-data   # datasets are read from the local datasety/ folder

args <- commandArgs(trailingOnly = TRUE)
local_data <- "--local-data" %in% args
data_url <- "https://raw.githubusercontent.com/senzorika/SaIT/master/datasety/"

scripts <- c(
  list.files(".", pattern = "^cvicenie[0-9]+[a-d]?\\.R$"),
  list.files("exercises_EN", pattern = "^exercise[0-9]+[a-d]?\\.R$", full.names = TRUE)
)
stopifnot(length(scripts) > 0, dir.exists("datasety"))

# install.packages() and View() are interactive conveniences for students; here they are no-ops.
# file.choose() (exercise 11b) returns the sample review file.
preamble <- c(
  "install.packages <- function(...) invisible(NULL)",
  "View <- function(...) invisible(NULL)",
  sprintf("file.choose <- function(...) '%s'", normalizePath("datasety/recenzie_en.txt", winslash = "/")),
  "options(warn = 1)",
  "pdf(NULL)"
)

run_script <- function(path) {
  code <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (local_data) {
    local_dir <- paste0("file:///", normalizePath("datasety", winslash = "/"), "/")
    code <- gsub(data_url, local_dir, code, fixed = TRUE)
  }
  tmp <- tempfile(fileext = ".R")
  writeLines(enc2utf8(c(preamble, code)), tmp, useBytes = TRUE)
  workdir <- tempfile("run")
  dir.create(workdir)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--encoding=UTF-8", "-e", shQuote(sprintf("setwd('%s'); source('%s', encoding = 'UTF-8')", gsub("\\\\", "/", workdir), gsub("\\\\", "/", tmp)))),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  list(ok = is.null(status) || status == 0, log = out)
}

failed <- character(0)
for (s in scripts) {
  res <- run_script(s)
  cat(sprintf("%-32s %s\n", s, if (res$ok) "OK" else "FAILED"))
  if (!res$ok) {
    failed <- c(failed, s)
    cat(paste0("    ", utils::tail(res$log, 8)), sep = "\n")
  }
}

cat(sprintf("\n%d scripts, %d failed\n", length(scripts), length(failed)))
if (length(failed) > 0) quit(status = 1)
