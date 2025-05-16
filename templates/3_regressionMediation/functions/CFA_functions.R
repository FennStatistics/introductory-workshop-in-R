########################################
# get CFA fit statistics
########################################
## args
# dataset = questionnaire
# regularExp = "^LiWang.*anthropomorphism$"
# labelLatent = "LiWangAnthropomorphism"
# showPlots = TRUE
# computeEFA = TRUE
# computeCFA = TRUE
# computeCFAMplus = FALSE

CFAstats <- function(dataset, regularExp, labelLatent = "latentVar",
                     showPlots = FALSE,
                     computeEFA=FALSE,
                     computeCFA = FALSE,
                     computeCFAMplus = FALSE){


  out_list <- list()


  tmp_correlation <- cor(dataset[, str_detect(string = colnames(dataset),
                                              pattern = regularExp)]
                         , use = "pairwise.complete.obs")

  ### show descriptive statistics
  if(showPlots){
    ## plot correlation matrix
    psych::cor.plot(r = tmp_correlation,
                    upper = FALSE, xlas = 2, main = labelLatent)

    ## get descriptive statistics
    tmp <- getDescriptivesSurvey(dataset = dataset, regEx = regularExp, sorted = TRUE)
    cat("\n\ndescriptive statistics:", "\n")
    print(tmp)

    out_list$desTable <- tmp
  }


  # > remove "-" from variables
  tmp_vars <- str_subset(string = colnames(dataset), pattern = regularExp)

  names(dataset)[names(dataset) %in% tmp_vars] <- str_remove_all(string = tmp_vars, pattern = "-")
   # and regularExp
  regularExp <- str_remove_all(string = regularExp , pattern = "-")

  tmp_vars <- str_subset(string = colnames(dataset), pattern = regularExp)


  cat("\n\nvariables under investigation: ", tmp_vars, "\n\n")

  ### Cronbachs Alpha
  rel_cronbach <- psych::alpha(tmp_correlation)
  cat("Cronbachs Alpha:", round(x = rel_cronbach$total$raw_alpha, digits = 2), "\n")

  out_list$cronbachAlpha <- rel_cronbach


  ### EFA in psych
  if(computeEFA){
    tmp_dim <- dimensionalityTest(label = labelLatent,
                                     regEx = regularExp, dataset = dataset)
    tmp_EFA <- explorativeFactorAnalysis(label = labelLatent,
                                         regEx = regularExp,
                                         dataset = dataset, nfac = 1)
    cat("\n\nEFA factor loadings (1 factor solution):", "\n")
    print(tmp_EFA[[1]]$loadings)

    out_list$dimTest <- tmp_dim
    out_list$EFA <- tmp_EFA
  }



  ### CFA in lavaan
  if(computeCFA){
    mod_lavaan <- model_lavaan(vars = tmp_vars, labelLatentVar = labelLatent)
    ### MLM estimator
    fit <-cfa(mod_lavaan, data = dataset, estimator = "MLR")
    ### WLSMV estimator
    # fit <-cfa(mod_lavaan, data = dataset, estimator = "WLSMV")
    # summary
    cat("CFA summary and fit statistics:", "\n")
    print(summary(fit, standardized = TRUE, fit.measures = TRUE))
    # semplot
    semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)
    cat("\n\nCFA first 6 Modification Indices:", "\n")
    print(head(modificationindices(fit, sort=T)))

    out_list$CFA <- fit
  }


  ### CFA in Mplus
  if(computeCFAMplus){
    tmp_dat_Mplus <- dataset[,tmp_vars]

    # if any var name >= 8
    if(any(nchar(colnames(tmp_dat_Mplus)) >= 8)){
      tmp_names <- str_extract(string = colnames(tmp_dat_Mplus), pattern = "[:alpha:]*")
      # and all var name equal
      if(sum(duplicated(x = tmp_names)) == ncol(tmp_dat_Mplus) - 1){
        tmp_names <- str_sub(string = tmp_names, start = 1, end = 5)
        tmp_names <- paste0(tmp_names, 1:length(tmp_names))

        colnames(tmp_dat_Mplus) <- tmp_names
      }
    }


    ### generate Syntax:
    ## var names
    tmp_vars <- paste0(paste0(colnames(tmp_dat_Mplus), collapse = "\n"), ";")

    ## CFA
    vec_variables <- colnames(tmp_dat_Mplus)
    vec_CFA <- c()
    for(i in 1:ncol(tmp_dat_Mplus)){
      if(i == 1){
        vec_CFA[i] <- paste0(labelLatent, " BY ", vec_variables[i], "*(p", i, ")")
      }else{
        vec_CFA[i] <- paste0(vec_variables[i], "(p", i, ")")

      }
    }

    tmp_CFA <- paste0(paste0(vec_CFA, collapse = "\n"), ";")

    ## errors
    tmp_errors <- paste0(paste0(colnames(tmp_dat_Mplus), "(e", 1:ncol(tmp_dat_Mplus), ")",
                                collapse = "\n"), ";")

    tmp_p <- paste0("p", 1:ncol(tmp_dat_Mplus), collapse = " + \n")
    tmp_e <- paste0("e", 1:ncol(tmp_dat_Mplus), collapse = " + \n")


    ### run CFA
    tmp_CFA_Mplus  <- mplusObject(

      TITLE = paste0("CFA for scale ", labelLatent),

      DATA = "LISTWISE=ON;",

      VARIABLE = paste0("usevariables =", tmp_vars),

      ANALYSIS =
        "
    type=general;
    estimator = MLR;
  ",
  MODEL =paste0(tmp_CFA, "\n\n",
                labelLatent, "@1", "\n\n",
                tmp_errors),

  MODELCONSTRAINT  =paste0("NEW(CM_True); ! True-Score variance", "\n",
                           "CM_True = ((", tmp_p, ")**2)*1.0;", "\n\n",

                           "NEW(CM_Error); ! error variance", "\n",
                           "CM_Error = (", tmp_e,");", "\n\n",

                           "NEW(CM_OMEGA); ! compute OMEGA (true-Score variance / total variance)", "\n",
                           "CM_OMEGA = (CM_True)/(CM_True + CM_Error);"),
  PLOT =
    "
    type = plot3;
  ",

  OUTPUT = "sampstat standardized stdyx residual modindices;",
  usevariables = colnames(tmp_dat_Mplus),
  rdata = tmp_dat_Mplus)




    res <- mplusModeler(tmp_CFA_Mplus,
                        modelout = paste0("CFA_", labelLatent, ".inp"),
                        run = 1L)

    cat("\n\nCFA Mplus summary and fit statistics:", "\n")
    print(res)

    ### get Mc Donalds Omega
    tmp_omega <- res$results$parameters$unstandardized[res$results$parameters$unstandardized$param == "CM_OMEGA",]
    L = log(tmp_omega$est/(1-tmp_omega$est))
    SEL = tmp_omega$se/(tmp_omega$est*(1-tmp_omega$est))
    CI_L_LO = L - 1.96*SEL
    CI_L_UP = L + 1.96*SEL
    CI_R_LO = 1/(1+exp(-CI_L_LO))
    CI_R_UP = 1/(1+exp(-CI_L_UP))

    cat("\nMC Donalds Omega: ", round(x = tmp_omega$est, digits = 2), "[",
        round(x = CI_R_LO, digits = 2), ",",
        round(x = CI_R_UP, digits = 2), "]"
    )


    out_list$CFAmplus <- res


  }

  return(out_list)
}


########################################
# get lavaan syntax for CFA (1 dimension)
########################################
model_lavaan <- function(vars = NULL, labelLatentVar, verbose=FALSE){
  vec_mod <- c()
  for(i in 1:length(vars)){
    vec_mod[i] <- paste0(c(vars[i], " + "), collapse = "")
  }
  vec_mod[length(vec_mod)] <- gsub(pattern = "+", fixed = TRUE, replacement = "", x = vec_mod[length(vec_mod)])
  vec_mod <- paste(vec_mod,collapse="")
  mod <- paste0(labelLatentVar, " =~ ",vec_mod)
  mod <- str_trim(string = mod)

  if(verbose){
    print(mod)
  }

  return(mod)
}



########################################
# get lavaan syntax for correlated residuals
########################################
getCorrelatedResidualsSyntax <- function(vec_variables, labelLatentLabel, verbose=FALSE){
  tmp_CFA <- model_lavaan(vars = vec_variables, labelLatentVar = labelLatentLabel)

  mat <- matrix(data = NA, nrow = length(vec_variables), ncol = length(vec_variables))
  mat <- lower.tri(x = mat, diag = FALSE)

  h = 1
  vec_CR <- c()
  for(c in 1:length(vec_variables)){
    for(r in 1:length(vec_variables)){
      if(mat[r,c]){
        # cat(paste0(vec_variables[c], " ~~ v",h, "*", vec_variables[r]), "\n")
        vec_CR[h] <- paste0(vec_variables[c], " ~~ v",h, "*", vec_variables[r])
        h=h+1
      }
    }

  }

  out <- paste0(tmp_CFA, "\n\n#correlated residuls:\n", paste0(vec_CR, collapse = "\n"))
  if(verbose){
    cat(out)
  }
  return(out)
}


########################################
# get syntax for cross validation
########################################
getCrossValidationSyntax <- function(fittedModel, verbose=FALSE){
  E.coefs <- parameterEstimates(fittedModel)
  E.coefs
  C.syntax <- ""
  check_latent = 1
  for (i in 1:nrow(E.coefs)) {
    if (E.coefs$op[i] == "~1") {
      C.syntax <- c(C.syntax, paste0(E.coefs$lhs[i], " ~ ", E.coefs$est[i], "*1"))
    }else if(E.coefs$op[i] == "=~"){

      if(check_latent == 1){
        C.syntax <- c(C.syntax, paste0(E.coefs$lhs[i], E.coefs$op[i],
                                       E.coefs$est[i], "*", E.coefs$rhs[i]))
        check_latent = check_latent + 1
      }else{
        C.syntax <- c(C.syntax, paste0("+",
                                       E.coefs$est[i], "*", E.coefs$rhs[i]))
      }
    }else{
      C.syntax <- c(C.syntax, paste0(E.coefs$lhs[i], E.coefs$op[i],
                                     E.coefs$est[i], "*", E.coefs$rhs[i]))
      check_latent = 1
    }
  }

  C.syntax <- paste(C.syntax[1:length(C.syntax)-1], collapse = "\n")
  # C.syntax <- paste(C.syntax, collapse = "\n")
if(verbose){
  cat(C.syntax)
}
  return(C.syntax)
}



########################################
# run cross validation
########################################
crossValidationCFA <- function(dataset = questionnaire_reduced, nRep = 100, lavaanSyntax = mod_lavaan){
  for(i in 1:nRep){
    ## generate train and test data sets
    dt = sort(sample(nrow(questionnaire_reduced), nrow(questionnaire_reduced)*.60))
    train<-questionnaire_reduced[dt,]
    test<-questionnaire_reduced[-dt,]

    ## fit train data set
    ## MLR estimator
    fit_train <-cfa(mod_lavaan, data = train, estimator = "MLR")
    # summary(fit_train, standardized = TRUE, fit.measure=TRUE)

    ## fit test data set
    # > fix parameter estimates
    out_SyntaxCrossValidation <- getCrossValidationSyntax(fittedModel = fit_train)

    fit_test <-cfa(out_SyntaxCrossValidation, data = test, estimator = "MLR")
    # summary(fit_test, standardized = TRUE, fit.measure=TRUE)

    tmp_fit <- round(fitmeasures(fit_test,
                                 fit.measures =c("aic", "bic", "logl", "pvalue",
                                                 "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                                                 "srmr", "cfi", "tli")), digits = 3)

    if(i == 1){
      out_fit <- tmp_fit
    }else{
      out_fit <- rbind(out_fit, tmp_fit)
    }
  }
  return(as.data.frame(out_fit))
}
