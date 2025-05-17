getLCAfitstatistics <- function(listLCAoutput, start_class = 2) {
  results <- data.frame(
    Classes = integer(),
    LL = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    SABIC = numeric(),
    CAIC = numeric(),
    Entropy = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(listLCAoutput)) {
    model <- listLCAoutput[[i]]
    if (is.null(model)) next

    k <- start_class + i - 1  # Class count (e.g., 2, 3, 4...)

    # Check for required components
    if (any(sapply(c("llik", "aic", "bic", "N", "npar", "posterior"), function(x) is.null(model[[x]])))) {
      warning("Model ", k, " is missing required fields. Skipping.")
      next
    }

    LL <- model$llik
    AIC <- model$aic
    BIC <- model$bic
    N <- model$N
    npar <- model$npar
    CAIC <- -2 * LL + npar * (log(N) + 1)
    SABIC <- -2 * LL + npar * log((N + 2) / 24)

    post_probs <- model$posterior
    entropy <- -sum(post_probs * log(post_probs + 1e-10)) / N
    norm_entropy <- 1 - entropy / log(k)

    results <- rbind(results, data.frame(
      Classes = k,
      LL = LL,
      AIC = AIC,
      BIC = BIC,
      SABIC = SABIC,
      CAIC = CAIC,
      Entropy = norm_entropy
    ))
  }

  if (nrow(results) == 0) {
    warning("No valid models found.")
    return(NULL)
  }

  # Plot
  results_long <- results %>%
    pivot_longer(cols = c(AIC, BIC, SABIC, CAIC), names_to = "FitStatistic", values_to = "Value")

  p <- ggplot(results_long, aes(x = Classes, y = Value, color = FitStatistic)) +
    geom_line() +
    geom_point() +
    labs(title = "Fit Statistics across LCA Models", x = "Number of Classes", y = "Fit Value") +
    theme_minimal()

  print(p)

  return(results)
}





plot_poLCA_profile <- function(lca_model, class_labels = NULL, title = NULL) {
  # Extract probabilities
  probs <- lca_model$probs

  # Convert to tidy format
  tidy_probs <- map2_df(probs, names(probs), function(df, var_name) {
    df %>%
      as.data.frame() %>%
      mutate(class = row_number()) %>%
      pivot_longer(-class, names_to = "response", values_to = "prob") %>%
      mutate(variable = var_name)
  })

  # Convert class to factor
  if (!is.null(class_labels)) {
    tidy_probs$class <- factor(tidy_probs$class, levels = seq_along(class_labels), labels = class_labels)
  } else {
    tidy_probs$class <- factor(tidy_probs$class)
  }

  # Plot
  ggplot(tidy_probs, aes(x = response, y = prob, fill = class)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(
      x = "Response Category",
      y = "Probability",
      fill = "Latent Class",
      title = title %||% "Latent Class Profiles"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
