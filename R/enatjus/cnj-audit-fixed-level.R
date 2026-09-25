# Copiado de lab-dados/enatjus, R/cnj-audit-fixed-level.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Origin contrasts on the linear predictor
#'
#' Returns one contrast per origin, on the logit scale, with the reference
#' origin pinned at zero. For `fixest` these are the `i()` coefficients; for
#' `glmmTMB` they are the origin random effects, which are already centred.
#'
#' @param model A fitted model.
#' @param origin_column Name of the origin variable inside the fitted terms.
#' @return A named numeric vector of contrasts.
#' @keywords internal
cnj_audit_origin_contrasts <- function(model, origin_column = "origem_tratada") {
  if (inherits(model, "fixest")) {
    estimates <- stats::coef(model)
    prefix <- paste0(origin_column, "::")
    selected <- estimates[startsWith(names(estimates), prefix)]
    if (length(selected) == 0L) {
      stop("No origin indicator found in the fitted model.", call. = FALSE)
    }
    names(selected) <- sub(prefix, "", names(selected), fixed = TRUE)
    reference <- setdiff(
      levels(model$model_info$data[[origin_column]]), names(selected)
    )
    # The reference level is pinned at zero rather than dropped: dispersion
    # across origins is only defined over the complete roster, and omitting the
    # baseline would silently measure a panel one origin short.
    contrasts <- c(selected, stats::setNames(0, "__reference__"))
    names(contrasts)[length(contrasts)] <- if (length(reference) == 1L) {
      reference
    } else {
      "Nacional"
    }
    return(contrasts)
  }
  if (inherits(model, "glmmTMB")) {
    effects <- glmmTMB::ranef(model)$cond[[origin_column]]
    if (is.null(effects)) {
      stop("The mixed model carries no origin random effect.", call. = FALSE)
    }
    return(stats::setNames(effects[[1]], rownames(effects)))
  }
  stop("Unsupported model class for origin contrasts.", call. = FALSE)
}

#' Recentre origin contrasts on a single declared intercept
#'
#' @param contrasts Origin contrasts on the logit scale.
#' @param reference_intercept The declared reference intercept, identical
#'   across every window so that only contrasts move.
#' @return Predicted probabilities per origin at the fixed level.
#' @keywords internal
cnj_audit_fixed_level_probabilities <- function(contrasts, reference_intercept) {
  if (!is.numeric(reference_intercept) || length(reference_intercept) != 1L ||
      !is.finite(reference_intercept)) {
    stop("The reference intercept must be one finite number.", call. = FALSE)
  }
  stats::plogis(reference_intercept + contrasts)
}

#' Finite-sample corrected between-origin variance
#'
#' The observed variance between estimated origin probabilities has
#' expectation equal to the true variance plus the mean sampling variance of
#' the estimates, so it rises on its own as the per-origin sample shrinks.
#' The correction subtracts the mean sampling variance estimated by
#' resampling. It can be negative, and that is the statistic working: under
#' null heterogeneity half the samples land below the expected sampling
#' variance. The uncorrected value is kept alongside, never replaced.
#'
#' @param probabilities Observed probabilities per origin.
#' @param replicate_matrix Replicates by origin, one row per draw.
#' @return A one-row tibble carrying corrected and truncated values.
#' @keywords internal
cnj_audit_finite_sample_correction <- function(probabilities,
                                               replicate_matrix = NULL) {
  observed_var <- stats::var(probabilities)
  sampling_var <- if (is.null(replicate_matrix)) {
    NA_real_
  } else {
    mean(apply(replicate_matrix, 2L, stats::var))
  }
  corrected_var <- observed_var - sampling_var
  tibble::tibble(
    sd_uncorrected = sqrt(observed_var),
    var_observed = observed_var,
    mean_sampling_var = sampling_var,
    # Signed, so a negative corrected variance stays visible as a negative
    # number instead of collapsing onto zero. Both columns are published.
    var_corrected = corrected_var,
    sd_corrected_signed = sign(corrected_var) * sqrt(abs(corrected_var)),
    sd_corrected_truncated = sqrt(pmax(corrected_var, 0))
  )
}

#' Standardize over the rows the model can actually predict
#'
#' `fixest` drops fixed-effect levels seen with only one outcome and returns NA
#' for any row carrying a dropped level. Those rows cannot be standardized
#' over, so they are removed once and the surviving share is reported, rather
#' than absorbed by an `na.rm` that would hide how much of the declared
#' distribution the cell actually covers.
#'
#' @param model A fitted model.
#' @param data The declared standardization sample.
#' @param origins Origin roster.
#' @param origin_column Origin variable name.
#' @return A list with `estimates` and `coverage`.
#' @keywords internal
cnj_audit_standardize_estimable <- function(model, data, origins,
                                            origin_column = "origem_tratada") {
  probe <- suppressWarnings(cnj_audit_predict_response(
    model, cnj_audit_counterfactual_value(data, origin_column, origins[[1]])
  ))
  estimable <- is.finite(probe)
  if (!any(estimable)) {
    stop("No row of the standardization sample is estimable.", call. = FALSE)
  }
  standardized <- cnj_audit_standardize_origins(
    model, data[estimable, , drop = FALSE], origins = origins,
    origin = origin_column
  )
  list(estimates = standardized$by_origin$estimate, coverage = mean(estimable))
}
