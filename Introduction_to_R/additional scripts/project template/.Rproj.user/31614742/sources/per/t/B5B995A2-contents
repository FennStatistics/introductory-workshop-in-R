############################################################################
##### getDescriptives()
# get multiple summary statistics to R console AND / OR to HTML
############################################################################
### functionals:
# dataset = dat
# variables = sel_var
# nameAPAtable = "immigrationEnrich"
getDescriptives <- function(dataset = NULL,
                            variables = NULL,
                            nameAPAtable = NULL){
  x <- dataset[, variables]
  ## table
  tmp_descriptives <- sapply(x, function(x) c(
    "Mean"= mean(x, na.rm=TRUE),
    "SD" = sd(x, na.rm=TRUE),
    "Median" = median(x, na.rm=TRUE),
    "CoeffofVariation" = sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE),
    "Minimum" = min(x, na.rm=TRUE),
    "Maximun" = max(x, na.rm=TRUE),
    "Lower Quantile" = as.numeric(quantile(x,0, na.rm=TRUE)),
    "Upper Quantile" = as.numeric(quantile(x,1, na.rm=TRUE)),
    "Skewness" = moments::skewness(x = x, na.rm=TRUE),
    "Kurtosis(-3)" = moments::kurtosis(x = x, na.rm=TRUE) -3,
    "KS-Test" = ks.test(x = x, y = "pnorm", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))$p.value
  )
  )
  tmp_descriptives <- round(x = tmp_descriptives, digits = 2)
  ## as APA 7 table in HTML using stargazer
  ## round digits
  out_tmp <- round(x = t(tmp_descriptives), digits = 2)
  if(!is.null(nameAPAtable)){
    notNeeded <- capture.output(stargazer(out_tmp, type = "html", summary = FALSE,
                                          out = paste0(nameAPAtable, ".html")))
  }
  return(tmp_descriptives)
}
