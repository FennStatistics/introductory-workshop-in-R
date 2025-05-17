########################################
# get descriptive statistics
########################################
getDescriptivesSurvey <- function(dataset = questionnaire, regEx,
                            sorted = TRUE,
                            nameAPAtable = NULL){
  x <- dataset[, str_detect(string = colnames(dataset),
                            pattern = regEx)]


  ## table
  tmp_descriptives <- sapply(x, function(x) c(
    "Mean"= mean(x,na.rm=TRUE),
    "SD" = sd(x),
    "Median" = median(x),
    "CoeffofVariation" = sd(x)/mean(x,na.rm=TRUE),
    "Minimum" = min(x),
    "Maximun" = max(x),
    "Lower Quantile" = as.numeric(quantile(x,0)),
    "Upper Quantile" = as.numeric(quantile(x,1)),
    "Skewness" = moments::skewness(x = x),
    "Kurtosis(-3)" = moments::kurtosis(x = x) -3,
    "KS-Test" = ks.test(x = x, y = "pnorm", mean(x), sd(x))$p.value
  )
  )
  tmp_descriptives <- round(x = tmp_descriptives, digits = 2)

  # print(t(tmp_descriptives))


  ## ggplot
  tmp_long <- x %>%
    gather(key="variables", value="value") %>%
    mutate(variables = gsub("\\.", " ",variables)) %>%
    mutate(value = round(as.numeric(value),0))

  if(sorted){
    p <- tmp_long %>%
      mutate(variables = fct_reorder(variables, value)) %>%
      ggplot( aes(x=value, color=variables, fill=variables)) +
      geom_histogram(alpha=0.8, bins = 30) +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("") +
      ylab("Frequency") +
      facet_wrap(~variables)
  }else{
    p <- tmp_long %>%
      ggplot( aes(x=value, color=variables, fill=variables)) +
      geom_histogram(alpha=0.8, bins = 30) +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("") +
      ylab("Frequency") +
      facet_wrap(~variables)
  }

  print(p)


  ## round digits
  out_tmp <- round(x = t(tmp_descriptives), digits = 2)

  ## stargazer
  if(!is.null(nameAPAtable)){
    notNeeded <- capture.output(stargazer(out_tmp, type = "html", summary = FALSE,
                                          out = paste0(nameAPAtable, ".html")))
  }

  return(out_tmp)
}
