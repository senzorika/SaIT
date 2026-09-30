# ============================================================
# Exercise 1: R, RStudio and collaboration
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise01.html
# ============================================================

#--------------------------------------------------------------------------
# Installing the required software
#--------------------------------------------------------------------------

# download and install the R statistical environment
# https://cran.r-project.org/bin/windows/base/

# download and install the RStudio graphical IDE
# https://posit.co/download/rstudio-desktop/


#--------------------------------------------------------------------------
# Groupware tools and collaborative work (on a project or course)
#--------------------------------------------------------------------------

# Slack - popular worldwide... the university mostly uses Teams
# https://join.slack.com/

# Trello (suitable for individual/group projects)
# https://trello.com/

# Notion (project management)
# https://notion.so/

# GitHub (repository for controlled documentation / document versioning)
# https://github.com/

# web conferencing and online chat (agree on a room name within your group)
# https://meet.jit.si/

# White Noise (private encrypted messenger for team communication)
# https://www.whitenoise.chat/

# OpenCode (open-source AI coding agent for the terminal - help with writing and explaining R code)
# https://opencode.ai/

# installing packages for all exercises (once is enough)
install.packages(c(
  "sensR", "pwr", "SensoMineR", "FactoMineR", "lmerTest", "emmeans", "DescTools",
  "PMCMRplus", "readxl", "curl", "cluster", "factoextra", "tidyverse", "quantmod",
  "gsheet", "tm", "SnowballC", "wordcloud", "RColorBrewer", "syuzhet", "ggplot2",
  "HH", "latticeExtra", "fmsb"
))
# turfR (exercise 11a) is only in the CRAN archive - see exercise11a.R for installation

# optional packages (not used in the exercises)
# install.packages(c("devtools", "rmarkdown", "telegram", "telegram.bot"))
# devtools::install_github("leonawicz/rockchain")
# devtools::install_github("Ram-N/weatherData")
# devtools::install_github("m-dev-/likert")
