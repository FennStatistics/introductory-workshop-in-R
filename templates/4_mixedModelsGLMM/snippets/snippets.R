library(mice)
library(lme4)


############################################################################
# run fully unpolled regressions
############################################################################
?sleepstudy
str(sleepstudy); dim(sleepstudy)


################
# single LMs
################

(lm1 <- lm(Reaction ~ Days, sleepstudy)) # overall


fit <- list()
for(s in unique(sleepstudy$Subject)){
  tmp_dat <- sleepstudy[sleepstudy$Subject %in% s, ]
  fit[[s]] <- lm(Reaction ~ Days, tmp_dat)
  # lm(Reaction ~ poly(x = Days, degree = 1), tmp_dat)
}


## pool estimates to get variation coefficients
(pooledEstimates <- pool(object = fit))


## set up matrix e.g.
mat <- matrix(data = NA, nrow = length(fit), ncol = 2)

for(i in 1:length(fit)){
  mat[i,] <- c(summary(fit[[i]])$coefficients[2,4], # linear term
               summary(fit[[i]])$r.squared)  # r squared
}

mat


################
# ggplot2
################
### adjust theme
ggplot_theme <- theme(axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12),
                      axis.text.x = element_text(size=10,hjust=0.5,vjust=0.5,face="plain", colour = "black"),
                      axis.text.y = element_text(size=12,face="plain", colour = "black"),
                      panel.border = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      legend.position="none")  # no legend


ggplot(data = sleepstudy, aes(x = Days, y = Reaction)) + ggplot_theme +
  geom_line(size=.5,aes(col = Subject, alpha = .1))


################
# xyplot - R package lattice
################

xyplot(
  Reaction  ~ Days | Subject,
  data = sleepstudy,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(lm(y~x), col='#0080ff')
  },
  grid = TRUE
)


################
################
# EMAtools
################
################

# center a L1-predictor variable at the person mean (cwc/centering within cluster)
# extraverted behavior:
# AA$extra.cwc <- EMAtools::pcenter(AA$ID, AA$extra)

# create person means for a L1 variable (pm/person mean)
# mean extraverted behavior across all occasions:
# AA$extra.pm <- EMAtools::pmean(AA$ID, AA$extra)

# center a L2-predictor variable at the grand mean (cgm)
# "by hand" (function not [yet] included in EMAtools):
# AA$extra.pm.cgm <- AA$extra.pm - mean(tapply(AA$extra.pm, AA$ID, mean, na.rm=TRUE), na.rm=TRUE)
