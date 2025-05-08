# ==============================================================================
# R-Code - my first R Script for data analyses
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
# load packages, data, functions
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
usePackage("stargazer")
usePackage("report")
usePackage("Cairo")

rm(usePackage)


################
# load data
################
setwd("outputs")
dir()
dat <- readRDS(file = "tramo1998etal_twins_clean.rds")
setwd("..")


################
# functions
################
# save graphic object as png file
save_graphic <- function(filename){
  tmp <- paste(filename, ".png", sep = "")
  Cairo::Cairo(file=tmp,
               type="png",
               units="px",
               width=2500,
               height=1700,
               pointsize=44, # text size is artifically shrunked when saved
               dpi= "auto",
               bg = "white")
}




############################################################################
# analyze data
###################################################a#########################
### ! remark: I am analyzing here a different data set
setwd("outputs")
dev.off()
save_graphic(filename = "myFirstPlot")
plot(cars$speed, cars$dist)
# hist(x = rnorm(n = 1000, mean = 10, 1))
dev.off()


plot(cars$dist, cars$speed)
lm1 <- lm(formula = speed ~ dist, data = cars)
abline(lm1)
summary(lm1)


stargazer::stargazer(lm1, type = "html", out = "myLM.html")


?report
report::report(lm1)
setwd("..")
