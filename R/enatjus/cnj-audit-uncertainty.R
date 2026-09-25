# Copiado de lab-dados/enatjus, R/cnj-audit-uncertainty.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Declare uncertainty paths supported by each audit family
#'
#' The descriptive family uses its own binomial-cell adapter because it does
#' not compare two fitted note-level specifications.
#'
#' @return A safe aggregate capability registry.
#' @keywords internal
cnj_audit_uncertainty_capabilities <- function() {
  tibble::tribble(
    ~family, ~engine, ~profile_interval, ~paired_bootstrap, ~decision_matrix, ~temporal_bootstrap, ~decision_metrics, ~simulation, ~limitation,
    "descriptive", "binomial_cells", FALSE, TRUE, TRUE, TRUE, "raw_equal_pair_sd|shrunken_equal_pair_sd", "paired_binomial_cells", NA_character_,
    "model_1_1", "fixest", FALSE, TRUE, TRUE, TRUE, "origin_probability_sd", "bernoulli_fitted_probability", "no latent glmmTMB parameter to profile",
    "model_1_2", "glmmTMB", TRUE, TRUE, TRUE, TRUE, "origin_probability_sd", "glmmTMB_parametric", NA_character_,
    "model_2", "glmmTMB", TRUE, TRUE, TRUE, TRUE, "conitec_marginal_effect_sd", "glmmTMB_parametric", NA_character_
  )
}

cnj_audit_uncertainty_family_specifications <- function(family) {
  switch(family,
    model_1_1 = "model_1_1",
    model_1_2 = "model_1_2",
    model_2 = c("model_2_slope", "model_2_intercept"),
    character()
  )
}

cnj_audit_standardization_digest <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("The standardization frame must be a non-empty data frame.",
         call. = FALSE)
  }
  digest::digest(data, algo = "sha256", serialize = TRUE)
}

#' Bind a fitted estimator to its immutable bootstrap sample
#'
#' The returned object is private. It retains row-level data only in memory so
#' every simulated response can be assigned to both specifications over the
#' same ordered rows.
#'
#' @param fitted Result returned by [cnj_audit_fit_estimator()].
#' @param specification Estimator specification used by `fitted`.
#' @param data Exact ordered model sample.
#' @param family Audit family.
#' @param sample_key Unique private row key.
#' @return A private uncertainty fit bundle.
#' @keywords internal
cnj_audit_uncertainty_fit <- function(
  fitted,
  specification,
  data,
  family = c("model_1_1", "model_1_2", "model_2"),
  sample_key = "internal_order",
  standardization_data = data
) {
  family <- match.arg(family)
  if (!is.list(fitted) ||
    !all(c("model", "diagnostics") %in% names(fitted))) {
    stop("`fitted` must be a canonical audit estimator result.", call. = FALSE)
  }
  required_specification <- c(
    "name", "engine", "response", "origin", "formula_digest"
  )
  if (!is.list(specification) ||
    !all(required_specification %in% names(specification))) {
    stop("Invalid audit model specification.", call. = FALSE)
  }
  allowed_specifications <- cnj_audit_uncertainty_family_specifications(family)
  if (!specification$name %in% allowed_specifications) {
    stop("The specification does not belong to the requested family.",
      call. = FALSE
    )
  }
  expected_engine <- cnj_audit_uncertainty_capabilities() |>
    dplyr::filter(.data$family == .env$family) |>
    dplyr::pull(.data$engine)
  if (!identical(specification$engine, expected_engine)) {
    stop("The specification engine does not match its uncertainty family.",
      call. = FALSE
    )
  }
  diagnostics <- fitted$diagnostics
  if (!is.list(diagnostics) ||
    !all(c("convergence_status", "identifiability") %in% names(diagnostics))) {
    stop("The fitted estimator is missing audit diagnostics.", call. = FALSE)
  }
  required_columns <- c(sample_key, specification$response, specification$origin)
  cnj_audit_require_columns(data, required_columns, "uncertainty data")
  sample_values <- as.character(data[[sample_key]])
  if (nrow(data) == 0L || anyNA(sample_values) ||
    any(!nzchar(sample_values)) || anyDuplicated(sample_values)) {
    stop("The uncertainty sample key must be complete and unique.",
      call. = FALSE
    )
  }
  response <- data[[specification$response]]
  if (!is.numeric(response) && !is.logical(response)) {
    stop("The uncertainty response must be binary.", call. = FALSE)
  }
  if (anyNA(response) || any(!response %in% c(0, 1))) {
    stop("The uncertainty response must contain only zero and one.",
      call. = FALSE
    )
  }
  origins <- if (is.factor(data[[specification$origin]])) {
    levels(droplevels(data[[specification$origin]]))
  } else {
    sort(unique(as.character(data[[specification$origin]])))
  }
  if (length(origins) < 2L || anyNA(origins) || any(!nzchar(origins))) {
    stop("Uncertainty estimation needs at least two origins.", call. = FALSE)
  }
  cnj_audit_require_columns(
    standardization_data,
    c(sample_key, specification$origin),
    "uncertainty standardization frame"
  )
  standardization_keys <- as.character(standardization_data[[sample_key]])
  standardization_origins <- sort(unique(as.character(
    standardization_data[[specification$origin]]
  )), method = "radix")
  if (nrow(standardization_data) == 0L || anyNA(standardization_keys) ||
      any(!nzchar(standardization_keys)) ||
      anyDuplicated(standardization_keys) ||
      anyNA(standardization_origins) ||
      !setequal(standardization_origins, origins)) {
    stop(
      "The standardization frame must use the complete fitted-origin support.",
      call. = FALSE
    )
  }
  covariate_columns <- setdiff(names(data), specification$response)

  structure(
    list(
      family = family,
      engine = specification$engine,
      specification = specification,
      model = fitted$model,
      diagnostics = diagnostics,
      data = data,
      standardization_data = standardization_data,
      standardization_digest = cnj_audit_standardization_digest(
        standardization_data
      ),
      response = specification$response,
      origin = specification$origin,
      origins = origins,
      sample_key = sample_key,
      sample_digest = cnj_audit_sample_digest(data),
      covariate_digest = digest::digest(
        data[covariate_columns],
        algo = "sha256", serialize = TRUE
      ),
      response_digest = cnj_audit_hash_values(as.character(response))
    ),
    class = "cnj_audit_uncertainty_fit"
  )
}

cnj_audit_profile_result <- function(
  fit,
  parameter,
  level,
  profile_status,
  estimate = NA_real_,
  conf_low = NA_real_,
  conf_high = NA_real_,
  failure_reason = NA_character_
) {
  result <- tibble::tibble(
    family = fit$family,
    specification = fit$specification$name,
    parameter = parameter,
    parameter_scale = "glmmTMB_internal",
    method = "profile",
    level = as.double(level),
    profile_status = profile_status,
    identifiability = fit$diagnostics$identifiability,
    estimate = as.double(estimate),
    conf_low = as.double(conf_low),
    conf_high = as.double(conf_high),
    failure_reason = failure_reason
  )
  cnj_audit_validate_public_output(result)
  result
}

cnj_audit_default_glmmtmb_profiler <- function(
  model, parameter, level, ncpus
) {
  parallel_mode <- if (ncpus > 1L && identical(.Platform$OS.type, "unix")) {
    "multicore"
  } else {
    "no"
  }
  profiled <- stats::profile(
    model,
    parm = parameter,
    parallel = parallel_mode,
    ncpus = ncpus
  )
  interval <- stats::confint(profiled, level = level)
  interval <- as.matrix(interval)
  if (nrow(interval) != 1L || ncol(interval) < 2L ||
    !all(c(".focal", "value") %in% names(profiled))) {
    stop("The profiler did not return one complete interval.", call. = FALSE)
  }
  finite_profile <- is.finite(profiled$.focal) & is.finite(profiled$value)
  if (!any(finite_profile)) {
    stop("The profile has no finite likelihood values.", call. = FALSE)
  }
  point_index <- which(finite_profile)[[
    which.min(profiled$value[finite_profile])
  ]]
  c(
    conf_low = unname(interval[1L, 1L]),
    conf_high = unname(interval[1L, 2L]),
    estimate = unname(profiled$.focal[[point_index]])
  )
}

#' Obtain a typed profile interval for an identifiable glmmTMB parameter
#'
#' @param fit Private uncertainty fit bundle.
#' @param parameter One glmmTMB parameter name.
#' @param level Confidence level.
#' @param ncpus Profile processes. On Unix, values above one use multicore mode.
#' @param profiler Injectable expensive profile boundary.
#' @return A one-row safe aggregate interval or typed failure.
#' @keywords internal
cnj_audit_glmmtmb_profile_interval <- function(
  fit,
  parameter,
  level = 0.95,
  ncpus = 1L,
  profiler = cnj_audit_default_glmmtmb_profiler
) {
  if (!inherits(fit, "cnj_audit_uncertainty_fit")) {
    stop("`fit` must be an uncertainty fit bundle.", call. = FALSE)
  }
  if (length(parameter) != 1L || !is.character(parameter) ||
    is.na(parameter) || !nzchar(parameter)) {
    stop("`parameter` must be one non-empty model parameter.", call. = FALSE)
  }
  if (length(level) != 1L || !is.numeric(level) || !is.finite(level) ||
    level <= 0 || level >= 1) {
    stop("`level` must lie strictly between zero and one.", call. = FALSE)
  }
  if (length(ncpus) != 1L || !is.numeric(ncpus) || is.na(ncpus) ||
    ncpus != as.integer(ncpus) || ncpus < 1L) {
    stop("`ncpus` must be one positive integer.", call. = FALSE)
  }
  if (!is.function(profiler)) {
    stop("`profiler` must be a function.", call. = FALSE)
  }
  ncpus <- as.integer(ncpus)
  if (!identical(fit$engine, "glmmTMB")) {
    return(cnj_audit_profile_result(
      fit, parameter, level, "unsupported",
      failure_reason = "unsupported_engine"
    ))
  }
  if (!identical(fit$diagnostics$convergence_status, "converged") ||
    !identical(fit$diagnostics$identifiability, "identified") ||
    is.null(fit$model)) {
    return(cnj_audit_profile_result(
      fit, parameter, level, "failed",
      failure_reason = "not_identified"
    ))
  }

  interval <- tryCatch(
    profiler(fit$model, parameter, level, ncpus),
    error = function(condition) NULL
  )
  required <- c("conf_low", "conf_high", "estimate")
  if (is.null(interval)) {
    return(cnj_audit_profile_result(
      fit, parameter, level, "failed",
      failure_reason = "profile_failure"
    ))
  }
  if (!is.numeric(interval) || !all(required %in% names(interval))) {
    return(cnj_audit_profile_result(
      fit, parameter, level, "failed",
      failure_reason = "invalid_profile_interval"
    ))
  }
  values <- unname(interval[required])
  if (anyNA(values) || any(!is.finite(values)) || values[[1L]] > values[[2L]]) {
    return(cnj_audit_profile_result(
      fit, parameter, level, "failed",
      failure_reason = "invalid_profile_interval"
    ))
  }

  cnj_audit_profile_result(
    fit,
    parameter,
    level,
    "ok",
    estimate = values[[3L]],
    conf_low = values[[1L]],
    conf_high = values[[2L]]
  )
}
