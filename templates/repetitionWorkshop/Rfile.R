# ==============================================================================
# R-Code - repetition of workshop content
# date of creation: May 2025
# authors: Julius
# ==============================================================================
rm(list=ls()) # clean environment
# dev.off() # clean plots

# !!! within a project not needed
# sets the directory of location of this script as the current directory
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # not needed if you use an R project!
getwd() # get your current working directory



############################################################################
# load packages, data
############################################################################

################
# load packages
################
# if packages are not already installed, the function will install and activate them
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
## Error handling Install packages in R base
# options(repos="https://CRAN.R-project.org")

usePackage("tidyverse") # readr is loaded
usePackage("psych")

rm(usePackage)


################
# load data
################



# library(readr) # loaded as a dependency of tidyverse
setwd("data")
# if you have no idea how data is structured
head(readLines("tramo1998etal_twins.csv"))


dat_twins <- read_csv("tramo1998etal_twins.csv")
View(dat_twins)
setwd("..")

## without setwd:
# dat_twins <- read_csv("data/tramo1998etal_twins.csv")



############################################################################
# prepare data
############################################################################


################
# change basic data structures
################
str(dat_twins)


### WHY?! using factor:
boxplot(dat_twins$IQ ~ dat_twins$GES)

# GES: Sex (1 = male, 2 = female)
dat_twins$GES
factor(x = dat_twins$GES, levels = c(1, 2), labels = c("male", "female"))

dat_twins$GES <- factor(x = dat_twins$GES, levels = c(1, 2), labels = c("male", "female"))


boxplot(dat_twins$IQ ~ dat_twins$GES)


# GR: Birth order
dat_twins$GR
factor(dat_twins$GR, ordered = TRUE)
## alternative base command:
# ordered(x = dat_twins$GR)



################
# compute new variables
################
dat_twins$IQ.cat <- cut(x = dat_twins$IQ, breaks = 3, labels = c("below average", "medium", "above average"))
dat_twins$IQ.cat
dat_twins$IQ.cat <- factor(dat_twins$IQ.cat, ordered = TRUE)
dat_twins$IQ.cat

### WHY?!
# Assign colors to factor levels
colors <- rainbow(length(levels(dat_twins$IQ.cat)))
# Plot using the colors
plot(dat_twins$IQ ~ dat_twins$CC, col = colors[dat_twins$IQ.cat], pch = 16)
# Add legend
legend("topright", legend = levels(dat_twins$IQ.cat), col = colors, pch = 16, title = "IQ Category")

boxplot(dat_twins$CC ~ dat_twins$IQ.cat)



############################################################################
# analyses
############################################################################


################
# check out Polymorphism by computing linear regression models
################
### wait for it....
# Scatter plot of IQ vs CC
plot(dat_twins$IQ ~ dat_twins$CC,
     main = "Linear Regression: IQ ~ CC",
     xlab = "CC",
     ylab = "IQ",
     pch = 16)

# Fit linear model
lm1 <- lm(IQ ~ CC, data = dat_twins)

# Add regression line in red
abline(lm1, col = "red", lwd = 2)


# Fit quadratic model: IQ ~ CC + CC^2
lm2 <- lm(IQ ~ CC + I(CC^2), data = dat_twins)

# Plot original data
plot(dat_twins$IQ ~ dat_twins$CC,
     main = "Quadratic Regression: IQ ~ CC + CC^2",
     xlab = "CC",
     ylab = "IQ",
     pch = 16)

# Add regression line in red
abline(lm1, col = "red", lwd = 2)


# Add quadratic regression line
cc_vals <- seq(min(dat_twins$CC), max(dat_twins$CC), length.out = 100)
pred_vals <- predict(lm2, newdata = data.frame(CC = cc_vals))
lines(cc_vals, pred_vals, col = "blue", lwd = 2)

summary(lm1)
summary(lm2)


### now Polymorphism :-)....
summary(dat_twins)
plot(lm1)



################
# the famous correlation
################
round(x = cor(x = dat_twins[, c("VG", "CC", "KU", "KG", "IQ")],
              method = "pearson"), digits = 2)

round(x = cor(x = dat_twins[, c("VG", "CC", "KU", "KG", "IQ")],
              method = "spearman"), digits = 2)


psych::corPlot(r = cor(x = dat_twins[, c("VG", "CC", "KU", "KG", "IQ")],
                        method = "pearson"))

cor.test(dat_twins$CC, dat_twins$VG)


cor.test(dat_twins$CC, dat_twins$IQ)









################
# descriptive
################
head(dat_twins)
table(dat_twins$IQ.cat)

summary(dat_twins)

summary(dat_twins$IQ)
boxplot(dat_twins$IQ)

psych::describe(x = as.data.frame(dat_twins)[, c("IQ")])
psych::describe(x = dat_twins$IQ)


dat_twins %>%
  group_by(IQ.cat) %>%
  summarise(N = n(), mean = mean(x = IQ), sd = sd(x = IQ))


################
# inferential
################
aov_results_baseR <- aov(CC ~ IQ.cat, data = dat_twins)
summary(aov_results_baseR)

library(report)
report::report(aov_results_baseR)

library(emmeans)
# Pairwise comparisons with Tukey adjustment
emmeans_result <- emmeans(aov_results_baseR, pairwise ~ IQ.cat, adjust = "tukey")
# View results
emmeans_result$contrasts



# library(afex)
# # Run ANOVA
# aov_result_afex <- aov_car(CC ~ IQ.cat + Error(ID), data = dat_twins)
# # View results
# summary(aov_result_afex)




