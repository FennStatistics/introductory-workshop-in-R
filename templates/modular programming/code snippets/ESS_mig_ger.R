setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

rm(list=ls(all=TRUE))
graphics.off()

library(tidyverse)
library(haven)
library(psych)

dir()

dat <- haven::read_spss(file = "ESS1-9e01_1multi.sav")
table(dat$impcntr)
dat$imsmetn <- 5 - dat$imsmetn
dat$imdfetn <- 5 - dat$imdfetn
dat$impcntr <- 5 - dat$impcntr


colnames(dat)

sel_var <- c("imbgeco", "imueclt", "imwbcnt")
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_imEnrich <- rowMeans(x = dat[, sel_var], na.rm = TRUE)


sel_var <- c("imsmetn", "imdfetn", "impcntr")
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_imAllow <- rowMeans(x = dat[, sel_var], na.rm = TRUE)


sel_var <- str_detect(string = colnames(dat), pattern = "^stf")
sum(sel_var)
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_stf <- rowMeans(x = dat[, sel_var], na.rm = TRUE)



summary(dat$mean_imEnrich)
hist(dat$mean_imEnrich)


tmp <- dat %>%
  group_by(cntry, essround) %>%
  summarise(meanEnrich = mean(mean_imEnrich, na.rm = TRUE),
            sdEnrich = sd(mean_imEnrich, na.rm = TRUE),
            meanAllow = mean(mean_imAllow, na.rm = TRUE),
            sdAllow = sd(mean_imAllow, na.rm = TRUE),
            meanStf = mean(mean_stf, na.rm = TRUE),
            sdStf = sd(mean_stf, na.rm = TRUE))
tmp




cor(dat$mean_imAllow, dat$mean_imEnrich, use = "com")# Default line plot
p <- ggplot(tmp, aes(x=essround, y=meanEnrich, group=cntry, color=cntry)) +
  geom_line() +
  geom_point()
  # geom_errorbar(aes(ymin=meanEnrich-sdEnrich, ymax=meanEnrich+sdEnrich), width=.01,
  #               position=position_dodge(0.5))
print(p)





p <- ggplot(tmp, aes(x=essround, y=meanAllow, group=cntry, color=cntry)) +
  geom_line() +
  geom_point()
  # geom_errorbar(aes(ymin=meanAllow-sdAllow, ymax=meanAllow+sdAllow), width=.1,
  #               position=position_dodge(0.05))
print(p)




dat %>%
  ggplot( aes(x=essround, y=mean_im, group=cntry, color=cntry)) +
  geom_line()


colnames(dat)
dat$cntry <- factor(dat$cntry)
dat <- within(dat, cntry <- relevel(cntry, ref = 3))


lm1 <- lm(formula = mean_imEnrich ~ happy + cntry + essround, data = dat)
summary(lm1)


dat_sel <- dat %>%
  filter(cntry == "DE")
lm2 <- lm(formula = mean_imEnrich ~ happy + essround + lrscale + mean_stf, data = dat_sel)
summary(lm2)
# plot(lm2)

lm3 <- lm(formula = mean_imAllow ~ happy + essround + lrscale + mean_stf, data = dat_sel)
summary(lm3)
plot(lm3)


hist(dat_sel$mean_imAllow)

cor(dat_sel$mean_imAllow, dat_sel$lrscale, use ="pairwise.complete.obs", method = "pearson")
plot(dat_sel$mean_imAllow ~ dat_sel$mean_stf)
abline(lm(dat_sel$mean_imAllow ~ dat_sel$mean_stf))

dat_sel <- dat %>%
  filter(cntry == "GB", essround == 7 | essround == 8)
table(dat_sel$essround)

t.test(dat_sel$mean_imEnrich ~ dat_sel$essround)






cars
rbind(cars, c("a","b"))













############################################################################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

rm(list=ls(all=TRUE))
graphics.off()

library(tidyverse)
library(haven)
library(psych)



# change working directory
setwd("data")
dir()
## load data
dat <- haven::read_spss(file = "ESS1-9e01_1.sav")
dat
glimpse(dat) # str
colnames(dat)
summary(dat)

dat$cntry <- NULL


dat$imsmetn <- 5 - dat$imsmetn
dat$imdfetn <- 5 - dat$imdfetn
dat$impcntr <- 5 - dat$impcntr



sel_var <- c("imbgeco", "imueclt", "imwbcnt")
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_imEnrich <- rowMeans(x = dat[, sel_var], na.rm = TRUE)


sel_var <- c("imsmetn", "imdfetn", "impcntr")
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_imAllow <- rowMeans(x = dat[, sel_var], na.rm = TRUE)

sel_var <- str_detect(string = colnames(dat), pattern = "^trst")
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_trst <- rowMeans(x = dat[, sel_var], na.rm = TRUE)

sel_var <- str_detect(string = colnames(dat), pattern = "^stf")
psych::corPlot(dat[, sel_var],numbers=TRUE,stars=TRUE)
dat$mean_stf <- rowMeans(x = dat[, sel_var], na.rm = TRUE)
dat$sd_stf <- dat %>%
  select(matches(match = "^stf")) %>%
  apply(.,1, sd, na.rm = TRUE)

a <- dat %>%
  select(matches(match = "^stf"))
sd(a[111,]) == dat$sd_stf[111]

# insufficient item responding
plot(dat$mean_stf, dat$sd_stf)


#############################
plot(dat$mean_imEnrich, dat$mean_trst)
plot(dat$mean_imEnrich, dat$mean_stf)
plot(dat$mean_imEnrich, dat$agea)

cor(dat$mean_imEnrich, dat$mean_trst, use = "com")
cor(dat$mean_imEnrich, dat$mean_stf, use = "com")
cor(dat$mean_imEnrich, dat$agea, use = "com")


table(dat$gndr)
boxplot(dat$mean_imEnrich ~ dat$gndr)


##########################################
hist(dat$lrscale)
dat$lrscale_scaled <- scale(x = dat$lrscale, center = TRUE, scale = TRUE)
hist(dat$lrscale_scaled)
dat$lrscale_scaled[dat$lrscale_scaled > 1]
mean_upper <- mean(dat$lrscale_scaled[dat$lrscale_scaled > 1], na.rm = TRUE)
mean_lower <- mean(dat$lrscale_scaled[dat$lrscale_scaled < -1], na.rm = TRUE)
abline(v=mean_upper, col = "red")
abline(v=mean_lower, col = "red")



dat_upper <-dat %>%
  filter(dat$lrscale_scaled > mean_upper)
dat_lower <-dat %>%
  filter(dat$lrscale_scaled < mean_lower)


#############################################
dat$lrscale_dummy <- ifelse(test = dat$lrscale_scaled > mean_upper, yes = 2, no =
         ifelse(test = dat$lrscale_scaled < mean_lower, yes = 1, no = 0))
table(dat$lrscale_dummy)


tmp <- dat %>%
  group_by(lrscale_dummy) %>%
  summarise(N = n(),
            meanEnrich = mean(mean_imEnrich, na.rm = TRUE),
            sdEnrich = sd(mean_imEnrich, na.rm = TRUE),
            meanAllow = mean(mean_imAllow, na.rm = TRUE),
            sdAllow = sd(mean_imAllow, na.rm = TRUE),
            meanStf = mean(mean_stf, na.rm = TRUE),
            sdStf = sd(mean_stf, na.rm = TRUE),
            meanTrst = mean(mean_trst, na.rm = TRUE),
            sdTrst = sd(mean_trst, na.rm = TRUE))
tmp



#######################################
lm_upper <- lm(formula = mean_imEnrich ~ mean_trst + mean_stf + happy + agea + gndr, data = dat_upper)
lm_lower <- lm(formula = mean_imEnrich ~ mean_trst + mean_stf + happy + agea + gndr, data = dat_lower)
summary(lm_upper)
summary(lm_lower)


lm_overall <- lm(formula = mean_imEnrich ~ mean_trst + mean_stf + happy + lrscale + agea + gndr, data = dat)
summary(lm_overall)

##########################################


efa1 <- fa.parallel(x = dat[,str_subset(string = colnames(dat), pattern = "^stf")], fa = "fa",n.iter=50)
efa1

## not accounting for the non-normal / skewed data
efa1 = fa(r = dat[,str_subset(string = colnames(dat), pattern = "^stf")], nfactors = 2, rotate = "oblimin")
## Loading required namespace: GPArotation
fa.diagram(efa1)
efa1



### correlation measures:
# spearman
cor_mat <- cor(dat[,str_subset(string = colnames(dat), pattern = "^stf")],
               use = "pairwise.complete.obs",
               method = "spearman")

### reliability measures:
# Cronbachs
rel_cronbach <- psych::alpha(cor_mat)
rel_cronbach
### EFA (PAF):
fit_efa <- fa(r = cor_mat, nfactors = 1,
              rotate = "Promax", fm = "pa", max.iter = 500)
fit_efa


tmpKMO <- psych::KMO(cor_mat)
if(any(tmpKMO$MSAi < .6)){
  cat("KMO criteria is to low (< .6) for:", "\n",
      names(tmpKMO$MSAi[tmpKMO$MSAi < .6]), "\n",
      "mean KMO:", round(x = tmpKMO$MSA, digits = 2), "\n")
}
tmpKMO

omega1 <- omega(dat[,str_subset(string = colnames(dat), pattern = "^stf")], nfactors = 2)
omega1


#####################
efa1 <- fa.parallel(x = dat[,str_subset(string = colnames(dat), pattern = "^stf|^trst")], fa = "fa",n.iter=50)
efa1

colnames(dat[,str_subset(string = colnames(dat), pattern = "^stf|^trst")])
psych::corPlot(dat[,str_subset(string = colnames(dat), pattern = "^stf|^trst")],numbers=TRUE,stars=TRUE)


############################################################
############################################################

### Korrelation, Reliabilität (Cronbachs), EFA
corr_rel_EFA <- function(constructlist = NULL, constnum = NULL,
                         data = NULL,
                         nfacs = 1){
  ### correlation measures:
  # spearman
  cor_mat <- cor(data[,constructlist[[constnum]]],
                 use = "pairwise.complete.obs",
                 method = "spearman")

  ### reliability measures:
  # Cronbachs
  rel_cronbach <- psych::alpha(cor_mat)

  ### EFA (PAF):
  fit_efa <- fa(r = data[,constructlist[[constnum]]], nfactors = nfacs,
                rotate = "Promax", fm = "pa", max.iter = 500)


  ### return objects as list
  return_list <- list(round(x = cor_mat, digits = 2),
                      rel_cronbach,
                      fit_efa
  )
  names(return_list) <- c("Cor: Spearman",
                          "Reliability: Cronbach",
                          "fit EFA (PAF)")

  # > print
  cat("mean inter-item-correlation (Spearman):",
      round(x = mean(colMeans(x = cor_mat)), digits = 2), "\n\n")

  cat("Cronbachs Alpha:",
      round(x = rel_cronbach$total[[1]], digits = 2), "\n\n")

  cat("EFA (PAF) variance accounted first factor:",
      round(x = fit_efa$Vaccounted[2], digits = 2), "for", nfacs, "factors", "\n")
  tmpKMO <- psych::KMO(cor_mat)
  if(any(tmpKMO$MSAi < .6)){
    cat("KMO criteria is to low (< .6) for:", "\n",
        names(tmpKMO$MSAi[tmpKMO$MSAi < .6]), "\n",
        "mean KMO:", round(x = tmpKMO$MSA, digits = 2), "\n")
  }
  #

  return(return_list)
}



constructs_list <- list()
(vars_tmp <-  c("imbgeco", "imueclt", "imwbcnt"))
constructs_list[[1]] <- vars_tmp
(vars_tmp <-  str_subset(string = colnames(dat), pattern = "^trst"))
constructs_list[[2]] <- vars_tmp

tmp <- corr_rel_EFA(constructlist = constructs_list, constnum = 1, data = dat, nfacs = 1)
tmp
tmp <- corr_rel_EFA(constructlist = constructs_list, constnum = 2, data = dat, nfacs = 2)
tmp
psych::corPlot(dat[, constructs_list[[2]]],numbers=TRUE,stars=TRUE)
psych::corPlot(dat[, c("trstprl", "trstplt", "trstprt", "trstep",  "trstun")],numbers=TRUE,stars=TRUE)



efa_parallel <- fa.parallel(x = dat[, constructs_list[[2]]], fa = "fa",n.iter=50)
efa_parallel
