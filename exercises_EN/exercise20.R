# ============================================================
# Exercise 20: Assessment case studies II
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise20.html
# ============================================================
# Send your solution (R script + a short written conclusion for each study) to: senzorickelaboratoriumfbp@gmail.com
# Subject: SaIT - exercise 20 - Name Surname

#---------------------------------------------------------------------------------------
# CASE STUDY A (easy): Flavours and sweetness
#---------------------------------------------------------------------------------------
# A dairy plans a new range of flavoured milk. 120 consumers chose their favourite of three
# flavours. Those who chose strawberry then rated it on a JAR sweetness scale.
#
# Tasks:
# 1. Is the flavour preference uniform? Choose and compute a test.
# 2. Compute the shares of the JAR categories and draw a chart.
# 3. Recommend whether and how to adjust the sweetness of the strawberry milk.

flavours <- c(strawberry = 58, peach = 41, mango = 21)
jar_sweetness <- c("--" = 2, "-" = 5, "JAR" = 25, "+" = 17, "++" = 9)


#---------------------------------------------------------------------------------------
# CASE STUDY B (medium): Sensory shelf life of a salad
#---------------------------------------------------------------------------------------
# 20 consumers evaluated a fresh vegetable salad stored at 8 °C every 12 hours.
# The time (h) of the first rejection was recorded. Consumers who did not reject it
# by the end of the test (72 h) have rejection = 0.
#
# Tasks:
# 1. Build the Kaplan-Meier acceptance curve.
# 2. Determine the median sensory shelf life.
# 3. The producer wants at most 25% of consumers to reject the product at the time of
#    consumption. What use-by time do you recommend?

salad <- data.frame(
  time = c(36, 48, 48, 60, 24, 48, 72, 60, 36, 48, 60, 72, 24, 48, 72, 36, 48, 72, 60, 48),
  rejection = c(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
)


#---------------------------------------------------------------------------------------
# CASE STUDY C (hard): Panel audit and a new recipe
#---------------------------------------------------------------------------------------
# A descriptive panel (10 assessors) rated the bitterness of 5 dark chocolates (P1 - P5)
# in two replicates on a 10-point scale. Before deciding on a new recipe, management
# wants to know whether the panel can be trusted.
#
# Tasks:
# 1. Evaluate panel performance (product discrimination, agreement, repeatability).
# 2. Find the assessor who harms the panel the most and justify it.
# 3. Evaluate the differences between products with a mixed model (assessor = random effect)
#    with and without this assessor. Which products differ?
# 4. The new recipe P6 should be slightly less bitter than P3; the expected difference is d' = 0.9.
#    How many assessors are needed for a triangle test with 80% power? Is the tetrad better?

set.seed(2020)
products <- paste0("P", 1:5)
true_bitterness <- c(P1 = 4, P2 = 5, P3 = 6.5, P4 = 5, P5 = 3.5)
panel <- expand.grid(
  assessor = factor(paste0("H", 1:10), levels = paste0("H", 1:10)),
  product = factor(products), replicate = factor(1:2)
)
shift <- rnorm(10, 0, 1)
panel$bitterness <- true_bitterness[as.character(panel$product)] +
  shift[as.integer(panel$assessor)] + rnorm(nrow(panel), 0, 0.7)
weak <- panel$assessor == "H7"
panel$bitterness[weak] <- 5 + rnorm(sum(weak), 0, 1.8)
panel$bitterness <- pmin(10, pmax(0, round(panel$bitterness, 1)))
head(panel)
