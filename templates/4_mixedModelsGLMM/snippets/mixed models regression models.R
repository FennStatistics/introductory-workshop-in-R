# ==============================================================================
# R-Code - simulation of data
# date of creation: March 2020
# authors: Julius Fenn
# ==============================================================================
rm(list=ls())
# dev.off()

############################################################################
# load packages
############################################################################
# wenn Pakete nicht bereits installiert sind, wird die Funktion diese installieren und aktivieren
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
## Fehlerbehandlung Pakete in R base installieren
# options(repos="https://CRAN.R-project.org")

usePackage("lmerTest")
usePackage("lme4")


# usePackage("performance") # assessment of Regression Models Performance -> ICC
# usePackage("stargazer") # Tabellen erstellen
# usePackage("ggplot2")
# usePackage("lattice")
# usePackage("dplyr") # pipe operators
# rm(usePackage)


############################################################################
# package: lmerTest
# https://www.rdocumentation.org/packages/lmerTest/versions/2.0-36/topics/step
############################################################################

##################
# lmerTest package
##################
?ham
m <- lmer(Informed.liking ~ Product*Information*Gender+
            (1|Consumer) + (1|Product:Consumer), data=ham)
summary(m)

#elimination of non-significant effects
s <- step(m)

#plot of post-hoc analysis of the final model
plot(s)

m <- lmer(Coloursaturation ~ TVset*Picture+
            (1|Assessor)+(1|Assessor:TVset), data=TVbo)
summary(m)
step(m, keep.effs = "Assessor")
