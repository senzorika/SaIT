# Shared setup for all presentations: colour palette and base graphics style (reset for every chunk)
pal <- c("#0f7b6c", "#d9622b", "#3a5fcd", "#b8457e", "#c49a12")
knitr::knit_hooks$set(style = function(before, options, envir) {
  if (before) {
    par(
      bg = "#f7f6f2", mar = c(4, 4, 1.5, 1), las = 1, bty = "l",
      col.axis = "#4a4f5a", col.lab = "#4a4f5a", fg = "#8a8f99", cex = 1.05
    )
  }
})
knitr::opts_chunk$set(style = TRUE)
