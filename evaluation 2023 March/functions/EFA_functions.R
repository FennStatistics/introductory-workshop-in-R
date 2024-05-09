########################################
# test number of factors
########################################
dimensionalityTest <- function(label, regEx, dataset){
  tmp_dat <- dataset[, str_detect(string = colnames(dataset),
                                  pattern = regEx)]

  if(label == "Overall"){
    fa_parallel_out <- fa.parallel(x = tmp_dat, fm = "minres", fa="both", main = label, cor = "cor")

  }else if(label == "Trust"){
    fa_parallel_out <- fa.parallel(x = tmp_dat, fm = "minres", fa="both", main = label, cor = "cor")
  }else{
    fa_parallel_out <- fa.parallel(x = tmp_dat, fm = "minres", fa="both", main = label, cor = "poly")
  }

  cat(label, "\n")
  cat("Number of components: ", fa_parallel_out$ncomp, "\n")
  cat("\n")

  return(fa_parallel_out)
}


########################################
# explorative factor analysis
########################################
## args
# i = 2
# label = searchLogic$label[i]
# regEx = searchLogic$regEx[i]
# dataset = questionnaire
# nfac = 1
explorativeFactorAnalysis <- function(label, regEx, dataset, nfac = 1, showCronbach = FALSE){
  tmp_dat <- dataset[, str_detect(string = colnames(dataset),
                                  pattern = regEx)]

  tmp_dat <- tmp_dat[, str_detect(string = colnames(tmp_dat),
                                  pattern = "^mean_", negate = TRUE)]
  ## EFA
  if(label == "Overall"){
    tmp_cor <- cor(tmp_dat)

    tmp_fa_out <- fa(r = tmp_dat, nfactors = nfac, rotate = "promax", cor ="cor")
  }else{
    tmp_cor <- polychoric(tmp_dat)
    tmp_cor <- tmp_cor$rho

    tmp_fa_out <- fa(r = tmp_dat, nfactors = nfac, rotate = "promax", cor ="poly")
  }


  ## get various goodness of fit statistics
  tmp_fa_out_fs <- factor.stats(r = tmp_dat,
                                f = tmp_fa_out)


  ## get factor scores
  if(nfac == 1){
    tmp_fs <- factor.scores(tmp_dat, tmp_fa_out, method="Thurstone")
  }else{
    tmp_fs <- factor.scores(tmp_dat, tmp_fa_out, method="tenBerge")
  }

  ## print some standard tests
  # > Cronbachs Alpha
  if(showCronbach){
    rel_cronbach <- psych::alpha(tmp_cor)
    cat("Cronbachs Alpha:", round(x = rel_cronbach$total$raw_alpha, digits = 2), "\n")
  }

  # > Kaiser, Meyer, Olkin Measure
  tmpKMO <- psych::KMO(tmp_cor)
  if(any(tmpKMO$MSAi < .6)){
    cat("KMO criteria is to low (< .6) for:", "\n",
        names(tmpKMO$MSAi[tmpKMO$MSAi < .6]), "\n",
        "mean KMO:", round(x = tmpKMO$MSA, digits = 2), "\n")
  }

  return(list(tmp_fa_out, tmp_fa_out_fs, tmp_fs))
}
