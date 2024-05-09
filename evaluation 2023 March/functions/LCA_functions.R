########################################
# get fit statistics for LCA
########################################
getLCAfitstatistics <- function(listMplusOutput){
  ### extract model params
  mat <- matrix(data = NA, nrow = length(listMplusOutput), ncol = 9)

  for(i in 1:length(listMplusOutput)){
    # print(i)

    res <- listMplusOutput[[i]]

    tmp_CAIC <- -2*res$results$summaries$LL +
      res$results$summaries$Parameters * (log(res$results$summaries$Observations)+1)


    mat[i,] <- c(i+1,

                 res$results$summaries$LL,

                 res$results$summaries$AIC,
                 res$results$summaries$BIC,
                 res$results$summaries$aBIC,
                 tmp_CAIC,
                 
                 res$results$summaries$BLRT_PValue,
                 res$results$summaries$T11_VLMR_PValue,

                 res$results$summaries$Entropy)
  }


  mat <- as.data.frame(mat)
  colnames(mat) <- c("Classes", "LL", "AIC", "BIC", "SABIC", "CAIC", "BLRTp", "VLMRLRTp", "Entropy")

  ## plot model results
  mat_tmp <- mat
  mat_tmp$LL <- NULL
  mat_tmp <- mat_tmp[,1:5]

  mat_long <- gather(mat_tmp, fitstatistic, value, AIC:CAIC, factor_key=TRUE)
  p <- ggplot(mat_long, aes(x = Classes, y = value, colour = fitstatistic, group = fitstatistic)) +
    geom_line()
  print(p)



  return(mat)
}