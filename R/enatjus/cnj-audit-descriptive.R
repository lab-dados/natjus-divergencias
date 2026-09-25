# Copiado de lab-dados/enatjus, R/cnj-audit-descriptive.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

cnj_audit_default_shrinkage_fit <- function(data, tmb_threads) {
  fit_data <- dplyr::mutate(
    data,
    pair_id = factor(.data$pair_id),
    pair_origin = interaction(.data$pair_id, .data$origin, drop = TRUE)
  )
  glmmTMB::glmmTMB(
    cbind(favourable, total - favourable) ~
      1 + (1 | pair_id) + (1 | pair_origin),
    family = stats::binomial(),
    data = fit_data,
    control = glmmTMB::glmmTMBControl(parallel = as.integer(tmb_threads))
  )
}

cnj_audit_default_shrinkage_predict <- function(model, data) {
  fit_data <- dplyr::mutate(
    data,
    pair_id = factor(.data$pair_id),
    pair_origin = interaction(.data$pair_id, .data$origin, drop = TRUE)
  )
  as.numeric(stats::predict(model, newdata = fit_data, type = "response"))
}

#' Fit hierarchical binomial shrinkage for descriptive cells
#'
#' @param cells Pair-by-origin binomial counts.
#' @param tmb_threads TMB threads used by the single fit.
#' @param fit_function Injectable fitter used for tests and blind replication.
#' @param predict_function Injectable response-scale predictor.
#' @param convergence_function Injectable convergence check.
#' @return Cells with shrunken rates and a convergence status.
#' @keywords internal
cnj_audit_shrunken_rates <- function(
    cells,
    tmb_threads = 1L,
    fit_function = cnj_audit_default_shrinkage_fit,
    predict_function = cnj_audit_default_shrinkage_predict,
    convergence_function = has_converged) {
  cnj_audit_require_columns(
    cells,
    c("pair_id", "origin", "favourable", "total"),
    "descriptive cells"
  )
  tmb_threads <- cnj_audit_scalar_integer(tmb_threads, "tmb_threads")
  if (nrow(cells) == 0L) {
    cells$shrunken_rate <- numeric()
    return(list(
      cells = cells,
      model = NULL,
      convergence_status = "not_applicable"
    ))
  }

  fit <- tryCatch(
    fit_function(cells, tmb_threads),
    error = function(condition) NULL
  )
  if (is.null(fit)) {
    cells$shrunken_rate <- NA_real_
    return(list(
      cells = cells,
      model = NULL,
      convergence_status = "optimization_failure"
    ))
  }
  if (!isTRUE(convergence_function(fit))) {
    cells$shrunken_rate <- NA_real_
    return(list(
      cells = cells,
      model = fit,
      convergence_status = "non_positive_definite_hessian"
    ))
  }

  predictions <- predict_function(fit, cells)
  if (!is.numeric(predictions) || length(predictions) != nrow(cells)) {
    stop("Shrinkage must return one prediction per cell.", call. = FALSE)
  }
  if (any(!is.finite(predictions)) || any(predictions < 0 | predictions > 1)) {
    stop("Shrinkage predictions must be finite probabilities.", call. = FALSE)
  }
  cells$shrunken_rate <- as.numeric(predictions)
  list(cells = cells, model = fit, convergence_status = "converged")
}
