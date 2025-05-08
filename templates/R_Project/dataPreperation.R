# ==============================================================================
# R-Code - my first R Script for data preparation
# date of creation: XXX
# authors: XXX
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

usePackage("tidyverse")
usePackage("haven") # ... to load data

rm(usePackage)


################
# load data
################
setwd("data")
dir()
dat <- read.csv(file = "tramo1998etal_twins.csv")
setwd("..")

############################################################################
# prepare data
############################################################################
head(dat)

dat$GES <- factor(dat$GES, levels = c(2, 1), labels = c("female", "male"))

head(dat)



################
# save data
################
setwd("outputs")
dir()
saveRDS(object = dat, file = "tramo1998etal_twins_clean.rds")
dir()
setwd("..")
