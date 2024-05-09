# ==============================================================================
# R-Code - my first R Script
# date of creation: XXX
# authors: XXX
# ==============================================================================
rm(list=ls()) # clean environment
# dev.off() # clean plots


getwd() # points to your current working directory
# sets the directory of location of this script as the current directory
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # not needed if you use an R project!


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

usePackage("tidyverse")
usePackage("psych")

rm(usePackage)

################
# load packages
################
set.seed(123)
size <- 100
dat <- data.frame(score = rnorm(n = size, mean = 5, sd = 1),
                  group = factor(x = rbinom(n = size, size = 1, prob = .3)),
                  letters = sample(x = letters, size = size, replace = TRUE))
levels(dat$group) <- c("young", "old")

rm(size)

############################################################################
# analyses
############################################################################

################
# check out data structures
################
typeof(dat$score)
class(dat$score)

typeof(dat)
class(dat)


################
# descriptive
################
table(dat$group)

summary(dat$score)
boxplot(dat$score)
psych::describe(x = dat[, c("score")])




################
# inferential
################
boxplot(dat$score ~ dat$group)
t.test(dat$score ~ dat$group)



