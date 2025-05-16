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

usePackage("MASS")
usePackage("Rcmdr")


# usePackage("performance") # assessment of Regression Models Performance -> ICC
# usePackage("stargazer") # Tabellen erstellen
# usePackage("ggplot2")
# usePackage("lattice")
# usePackage("dplyr") # pipe operators
# rm(usePackage)


############################################################################
# package: article sthda, different packages
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
############################################################################

##################
# by MASS package
##################
### linear regression model
# Fit the full model
full.model <- lm(Fertility ~., data = swiss)
summary(full.model)

# Stepwise regression model
help(stepwise)
step.model <- stepAIC(object = full.model, direction = "both",
                      trace = TRUE)
summary(step.model)


### GLM
example(birthwt)
birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
birthwt.step <- stepAIC(object = birthwt.glm, direction = "both",
                        trace = TRUE)
birthwt.step$anova
summary(birthwt.step)

##################
# leaps package
##################
usePackage("leaps")
models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "seqrep")
summary(models)


##################
# caret package
##################
usePackage("caret")

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 4)
