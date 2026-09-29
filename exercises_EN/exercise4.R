# ============================================================
# Exercise 4: Charts and data visualisation
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise04.html
# ============================================================

#--------------------------------------------------------------
# Today's datasets
#--------------------------------------------------------------
# today's data: 9 assessors rated 4 products (a,b,c,d), values are overall quality (mean of attributes)
assessor <- c("as1", "as2", "as3", "as4", "as5", "as6", "as7", "as8", "as9")
a <- c(7.5, 6.9, 7, 7, 6.9, 7.1, 7.2, 7.5, 6.9)
b <- c(7.5, 8.1, 7.5, 7.4, 7.1, 7.5, 7.2, 7.2, 6.9)
c <- c(7, 6.1, 6.7, 6.1, 6.9, 7.1, 7.2, 7.2, 6.9)
d <- c(7.5, 6.9, 7, 7, 6.9, 7.1, 7.2, 7.2, 6.9)
tab <- data.frame(a, b, c, d)

# number of food research institutes in Eastern Europe (fictitious numbers)
countries <- c("Slovakia", "Poland", "Czechia", "Hungary", "Ukraine", "Bulgaria", "Slovenia", "Serbia", "Romania")
count <- c(7, 21, 15, 13, 19, 7, 9, 6, 6)
institutes <- data.frame(countries, count)


#--------------------------------------------------------------
# charts
#--------------------------------------------------------------
plot(a)
dotchart(a)
barplot(a, names.arg = assessor, horiz = TRUE, col = "lavender")
barplot(c(a, b, c, d), col = c("lightblue", "mistyrose", "lightcyan", "lavender"))
pie(a)
pie(count, labels = countries)
hist(a)
plot(density(a))

# a slightly more complex chart
# ================================
# 2 beverage filling lines (in ml, 2 minutes)
set.seed(123) # reproducible random data
line1 <- rnorm(120, mean = 1000, sd = 0.6)
line2 <- rnorm(120, mean = 1000, sd = 0.6)
plot(line1, type = "l", main = "test chart", xlab = "seconds (filling)", ylab = "lines (1 - red, 2 - blue)")
lines(line2, col = "lightblue")
abline(h = mean(line1), col = "red")
abline(h = mean(line2), col = "blue")
grid(nx = 6, ny = NA, col = "gray")


# an even more complex chart
# ================================
library(quantmod)
# stocks + indicators
getSymbols("LHA.DE", src = "yahoo", from = "2018-01-01", to = Sys.Date())
candleChart(LHA.DE, TA = c(addMACD(), addRSI(), addBBands(), addVo()), subset = "2018::2019", theme = "white")

# PRACTICAL PART
# ==================================
# Task 1
# choose any food group (3+ products) from the Open Food Facts database,
# create a chart of your choice and send the result to: senzorickelaboratoriumfbp@gmail.com

# Task 2
# find out the preferences of individual cola drinks using online forms
# install the package for reading Google Sheets
install.packages("gsheet")
# load the package
library(gsheet)
# load the data from the online survey
preferences <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1DqIcZD1bDUV8cP_UJvVNeQL8Pq97UgIf-nt_fsWC5gY/edit?usp=sharing")

# vectors from the online database
# the content of the survey may change - we use all numeric columns
numeric_cols <- preferences[sapply(preferences, is.numeric)]
results <- colMeans(numeric_cols, na.rm = TRUE)
results
boxplot(numeric_cols, main = "Online survey results")

# Task 3: which cola-drink stocks are currently leading the stock market? :)
