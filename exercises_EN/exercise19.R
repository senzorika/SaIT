# ============================================================
# Exercise 19: Assessment case studies I
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise19.html
# ============================================================
# Send your solution (R script + a short written conclusion) to: senzorickelaboratoriumfbp@gmail.com
# Subject: SaIT - exercise 19 - Name Surname

#---------------------------------------------------------------------------------------
# CASE STUDY A (very easy): A new cocoa supplier
#---------------------------------------------------------------------------------------
# A chocolate factory is considering a cheaper cocoa supplier. The sensory laboratory ran
# a triangle test: 48 assessors, 21 of them identified the odd sample correctly.
#
# Tasks:
# 1. State the null and the alternative hypothesis.
# 2. Choose and compute an appropriate test (alpha = 0.05).
# 3. Write one sentence of conclusion for management. Can you claim the chocolates are the same?

correct <- 21
assessors <- 48


#---------------------------------------------------------------------------------------
# CASE STUDY B (medium): Four yogurt recipes
#---------------------------------------------------------------------------------------
# The R&D department prepared 4 yogurt recipes (A - D). Each of 12 assessors tasted all
# 4 samples in random order and rated overall liking on a 9-point hedonic scale
# (1 = dislike extremely, 9 = like extremely).
#
# Tasks:
# 1. Show the data in a suitable plot and describe what it shows.
# 2. Determine the experimental design (dependent / independent samples) and check the assumptions.
# 3. Choose and compute a global test.
# 4. If the difference is significant, find out with a post-hoc test which recipes differ.
# 5. Recommend one recipe to management and justify it (2 - 3 sentences).

yogurts <- data.frame(
  assessor = factor(rep(paste0("H", 1:12), times = 4), levels = paste0("H", 1:12)),
  recipe = factor(rep(c("A", "B", "C", "D"), each = 12)),
  liking = c(
    6, 5, 7, 6, 5, 6, 7, 5, 6, 6, 5, 7, # A
    6, 6, 7, 5, 6, 7, 6, 6, 5, 7, 6, 6, # B
    4, 3, 5, 4, 3, 4, 5, 3, 4, 2, 4, 5, # C
    8, 7, 8, 7, 8, 9, 7, 8, 7, 8, 8, 6 # D
  )
)
head(yogurts)
