# ==============================================================================
# R-Code - Vortrag HLM 29.08.19: Pakete laden, Datenaufbereitung
# Erstellungsdatum: August 2019
# Autoren: Julius Fenn
# ==============================================================================
############################################################################
# Pakete, Daten laden
############################################################################
#####
# Pakete
#####
# if R-packages are not installed, function will install and load them
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

### solutions for possible errors:
## downloading error:
# options(repos="https://CRAN.R-project.org")
## no right to install packages in default library path folder:
# .libPaths()
usePackage("knitr") # create dynamic document
usePackage("foreign") # load spss data
usePackage("multilevel") # generate multilevel data with different ICCs
usePackage("performance") # assessment of Regression Models Performance -> ICC
usePackage("lme4") # mixed models / GLMMs
usePackage("stargazer") # tables
usePackage("lattice") # graphics
usePackage("Rcpp")
usePackage("tidyverse") # for data wrangling

rm(usePackage)

# usePackage("lattice") # graphics
# usePackage("ggplot2") # graphics
# usePackage("graphics") # add text, segments to plot // possibly not used within workshop

# usePackage("DHARMa") # check residuals normal distribution, over-/ underdispersion // possibly not used within workshop
# usePackage("influence.ME") # outlier analysis // possibly not used within workshop


# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#####
# Daten
#####
datenLV <- foreign::read.spss("IQB-LV-2011_SchuelerInnen-Eltern_CF.sav", use.value.labels = TRUE, to.data.frame = TRUE)
### warnings() -> variables with no specified value labels (=numeric)
datenLV$Version_v1_21.01.2019 <- NULL
datenLV[,c(7,10,16,17,32:54)] <- sapply(datenLV[c(7,10,16,17,32:54)],as.character)
datenLV[,c(7,10,16,17,32:54)] <- sapply(datenLV[c(7,10,16,17,32:54)],as.numeric)

### keep every third school
schoolids <- unique(datenLV$idsch_FDZ)
schoolids <- schoolids[seq(1, length(schoolids), 3)]
datenLV <- datenLV[datenLV$idsch_FDZ %in% schoolids, ]
datenLV$idsch_FDZ <- factor(datenLV$idsch_FDZ)
levels(datenLV$idsch_FDZ) <- 1:67
rm(schoolids) # remove object

### subset of variables
datenLV <- datenLV[,c(1:6, 8, 10,14, 16, 24:27, 33, 35, 39, 50:54)]

### change labels, recode certain variables
# labels
levels(datenLV$Emigr) <- c("Mig", "keinMig")
levels(datenLV$SBuecher) <- c("10 Buecher", "25 Buecher","100 Buecher","200 Buecher","mehr 200 Buecher")

# recode
datenLV$SSkMa_b <- 5 - as.numeric(datenLV$SSkMa_b)
datenLV$SSkMa_b <- factor(datenLV$SSkMa_b)
levels(datenLV$SSkMa_b) <- c("stimme voellig zu", "stimme eher zu", "stimme eher nicht zu", "stimme nicht zu")

### create school level variables
# economic disadvantage
quantile(datenLV$EHisei, probs = c(.33,.66), na.rm = TRUE)
tmp <- aggregate(datenLV$EHisei,by=list(datenLV$idsch_FDZ),FUN=function(x) mean(x, na.rm=TRUE))
tmp$schoolEconDis <- ifelse(test = tmp$x <=  41, yes = "<33% oekonomisch benachteiligt",
       no = ifelse(test = tmp$x >  41 & tmp$x <= 55, yes = "33-66% oekonomisch mittel",
       no = ifelse(test = tmp$x > 55, yes = ">66% oekonomisch bevorzugt", no = "LOGICAL ERROR")))
tmp$x <- NULL
colnames(tmp) <- c("idsch_FDZ", "schoolEconDis")
datenLV <- merge(datenLV, tmp, by= "idsch_FDZ")
datenLV$schoolEconDis <- factor(datenLV$schoolEconDis, levels =
                                  c("<33% oekonomisch benachteiligt",
                                    "33-66% oekonomisch mittel",
                                    ">66% oekonomisch bevorzugt"))

# proportions of migrants (under / over average)
tmp <- aggregate(as.numeric(datenLV$Emigr) - 1,by=list(datenLV$idsch_FDZ),FUN=function(x) mean(x, na.rm=TRUE))
tmp$x <- 1 - tmp$x
tmp$schoolMiganteil <- ifelse(test = tmp$x <=  .2, yes = "< 20% Miganteil",
                            no = ifelse(test = tmp$x >  .2, yes = "> 20% Miganteil", no = "LOGICAL ERROR"))
tmp$x <- NULL
colnames(tmp) <- c("idsch_FDZ", "schoolMiganteil")
datenLV <- merge(datenLV, tmp, by= "idsch_FDZ")
datenLV$schoolMiganteil <- factor(datenLV$schoolMiganteil, levels =
                                  c("< 20% Miganteil",
                                    "> 20% Miganteil"))
rm(tmp) # remove object


#####
# Gewichtung
#####
# ?lmer #remark weights ->
datenLV$wgHOUSEWHT <- nrow(datenLV) * datenLV$wgtSTUD / sum(datenLV$wgtSTUD)
# nrow(datenLV)
# sum(datenLV$wgHOUSEWHT)
# aggregate(datenLV$wgHOUSEWHT,by=list(datenLV$idsch_FDZ),FUN=function(x) mean(x, na.rm=TRUE))
