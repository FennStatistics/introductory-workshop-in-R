# ==============================================================================
# R-Code - my first R Script
# date of creation: XXX
# authors: XXX
# ==============================================================================

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

usePackage("haven") # load SPSS, ... data
usePackage("tidyverse") # data cleaning and summarizing
usePackage("psych") # psychometric analysis (EFA)

## psychometric analysis
usePackage("moments") # skewness, kurtosis

## outputs
usePackage("stargazer") # create tables
usePackage("report") # get reports of statistical tests in APA7

usePackage("lavaan") # for CFA

usePackage("Cairo") # environment to save graphics

usePackage("epade") # easy plots like bar3d

usePackage("fontawesome")

usePackage("DT")

usePackage("afex")
usePackage("BayesFactor")
usePackage("ggstatsplot")

usePackage("rstatix")

usePackage("janitor")

usePackage("emo") # Emojis in rmarkdown
