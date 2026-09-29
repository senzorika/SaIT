# ============================================================
# Cvičenie 1: R, RStudio a spolupráca
# Teória: https://senzorika.github.io/SaIT/teoria/cvicenie01.html
# ============================================================

#--------------------------------------------------------------------------
# Instalacia potrebneho softwaru
#--------------------------------------------------------------------------

# download a instalacia statistickeho balika R
# https://cran.r-project.org/bin/windows/base/

# download a instalacia grafickeho IDE Rstudio
# https://posit.co/download/rstudio-desktop/


#--------------------------------------------------------------------------
# Groupware nastroje a kooperativna praca (na projekte resp. predmete)
#--------------------------------------------------------------------------

# Slack - vo svete popularny... skola skor vyuziva Teams
# https://join.slack.com/

# Trello (vhodna pre individualne/skupinove projekty)
# https://trello.com/

# Notion (sprava projektov)
# https://notion.so/

# github (repozitar, na riadenu dokumentaciu/ verziovanie dokumentov)
# https://github.com/

# webkonferencie a online chat
# https://meet.jit.si/sait2024

# instalacia balikov pre cvicenia 1-18 (staci raz)
install.packages(c(
  "sensR", "pwr", "SensoMineR", "FactoMineR", "lmerTest", "emmeans", "DescTools",
  "PMCMRplus", "readxl", "curl", "cluster", "factoextra", "tidyverse", "quantmod",
  "gsheet", "tm", "SnowballC", "wordcloud", "RColorBrewer", "syuzhet", "ggplot2",
  "HH", "latticeExtra", "fmsb"
))
# turfR (cvicenie 11a) je len v archive CRAN - postup instalacie je v cvicenie11a.R

# volitelne baliky (v cviceniach sa nepouzivaju)
# install.packages(c("devtools", "rmarkdown", "telegram", "telegram.bot"))
# devtools::install_github("leonawicz/rockchain")
# devtools::install_github("Ram-N/weatherData")
# devtools::install_github("m-dev-/likert")
