# Copiado de lab-dados/enatjus, R/cnj-audit-estimators.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

cnj_audit_model_name <- function(value, argument) {
  if (
    length(value) != 1L || is.na(value) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", value)
  ) {
    stop("`", argument, "` must be one syntactic column name.", call. = FALSE)
  }
  value
}

#' Record the model inputs needed to enforce estimator comparability
#'
#' @param data The exact ordered model sample.
#' @param specification One specification returned by
#'   [cnj_audit_estimator_specifications()].
#' @param sample_key Unique internal row-key column.
#' @return A private comparability record. It must not be exported as output.
#' @keywords internal
cnj_audit_comparability_record <- function(
    data, specification, sample_key = "id_nota") {
  sample_key <- cnj_audit_model_name(sample_key, "sample_key")
  if (!is.data.frame(data) || !sample_key %in% names(data)) {
    stop("Comparability data are missing the sample key.", call. = FALSE)
  }
  sample_values <- as.character(data[[sample_key]])
  if (anyNA(sample_values) || any(!nzchar(sample_values)) ||
      anyDuplicated(sample_values)) {
    stop("The model sample key must be complete and unique.", call. = FALSE)
  }
  required_fields <- c(
    "name", "formula", "formula_digest", "fixed_signature"
  )
  if (!is.list(specification) ||
      !all(required_fields %in% names(specification))) {
    stop("Invalid audit model specification.", call. = FALSE)
  }
  observed_formula_digest <- digest::digest(
    paste(deparse(specification$formula), collapse = "\n"),
    algo = "sha256",
    serialize = FALSE
  )
  if (!identical(observed_formula_digest, specification$formula_digest)) {
    stop("The model formula changed after specification construction.",
         call. = FALSE)
  }

  structure(
    list(
      specification = specification$name,
      sample_key = sample_values,
      fixed_signature = specification$fixed_signature,
      formula_digest = specification$formula_digest
    ),
    class = "cnj_audit_comparability_record"
  )
}

#' Assert that two estimators answer the same question on the same rows
#'
#' @param reference First comparability record.
#' @param comparison Second comparability record.
#' @return `TRUE`; otherwise throws a contract error.
#' @keywords internal
cnj_audit_assert_comparable <- function(reference, comparison) {
  if (
    !inherits(reference, "cnj_audit_comparability_record") ||
      !inherits(comparison, "cnj_audit_comparability_record")
  ) {
    stop("Both inputs must be comparability records.", call. = FALSE)
  }
  if (!identical(reference$sample_key, comparison$sample_key)) {
    stop("Comparable estimators do not use the same ordered model sample.",
         call. = FALSE)
  }
  if (!identical(reference$fixed_signature, comparison$fixed_signature)) {
    stop("Comparable estimators do not use the same fixed part.",
         call. = FALSE)
  }
  TRUE
}

#' Classify estimator diagnostics without changing the specification
#'
#' @param engine `fixest` or `glmmTMB`.
#' @param convergence_code Optimizer exit code; zero means success.
#' @param pd_hessian Whether the mixed-model Hessian is positive definite.
#' @param random_sd Estimated random-effect standard deviations.
#' @param random_correlation Estimated random-effect correlations.
#' @param warning_messages Warning text retained only in memory for
#'   classification; it is not returned.
#' @param max_abs_gradient Largest absolute fixed-parameter gradient reported
#'   by the optimizer. It is retained as scale-dependent evidence and does not
#'   override the optimizer convergence code when finite.
#' @param tolerance Boundary tolerance for variance and correlation checks.
#' @return A sanitized diagnostic list.
#' @keywords internal
cnj_audit_classify_estimator_diagnostics <- function(
    engine = c("fixest", "glmmTMB"), convergence_code = 0L,
    pd_hessian = TRUE, random_sd = numeric(),
    random_correlation = numeric(), warning_messages = character(),
    max_abs_gradient = NA_real_, tolerance = 1e-6) {
  engine <- match.arg(engine)
  if (length(convergence_code) != 1L || is.na(convergence_code)) {
    convergence_code <- 1L
  }
  random_sd <- random_sd[is.finite(random_sd)]
  random_correlation <- random_correlation[is.finite(random_correlation)]
  warning_messages <- as.character(warning_messages)
  separation_detected <- any(grepl(
    paste(
      "complete separation", "perfect separation", "perfect prediction",
      "fitted probabilities numerically 0 or 1", "non-finite coefficient",
      "separation signal",
      sep = "|"
    ),
    warning_messages,
    ignore.case = TRUE
  ))
  singular <- length(random_sd) > 0L && any(random_sd <= tolerance)
  boundary_correlation <- length(random_correlation) > 0L &&
    any(abs(random_correlation) >= 1 - tolerance)
  invalid_gradient <- length(max_abs_gradient) != 1L ||
    (!is.na(max_abs_gradient) && !is.finite(max_abs_gradient))

  convergence_status <- if (convergence_code != 0L || invalid_gradient) {
    "optimization_failure"
  } else if (separation_detected) {
    "separation"
  } else if (engine == "glmmTMB" && !isTRUE(pd_hessian)) {
    "non_positive_definite_hessian"
  } else if (singular) {
    "singular"
  } else if (boundary_correlation) {
    "boundary_correlation"
  } else {
    "converged"
  }
  identifiability <- if (
    convergence_status %in% c(
      "optimization_failure", "non_positive_definite_hessian", "singular"
    ) || separation_detected
  ) {
    "not_identified"
  } else if (boundary_correlation) {
    "partially_identified"
  } else {
    "identified"
  }

  list(
    convergence_status = convergence_status,
    identifiability = identifiability,
    separation_detected = separation_detected,
    singular = singular,
    boundary_correlation = boundary_correlation,
    max_abs_gradient = as.double(max_abs_gradient),
    convergence_code = as.integer(convergence_code),
    pd_hessian = if (engine == "glmmTMB") isTRUE(pd_hessian) else NA
  )
}

cnj_audit_random_structure <- function(model) {
  variance_correlation <- tryCatch(
    glmmTMB::VarCorr(model)$cond,
    error = function(condition) NULL
  )
  if (is.null(variance_correlation)) {
    return(list(sd = numeric(), correlation = numeric()))
  }

  standard_deviations <- unlist(lapply(variance_correlation, function(block) {
    values <- attr(block, "stddev")
    if (is.null(values)) {
      values <- sqrt(pmax(diag(as.matrix(block)), 0))
    }
    unname(values)
  }), use.names = FALSE)
  correlations <- unlist(lapply(variance_correlation, function(block) {
    correlation <- attr(block, "correlation")
    if (is.null(correlation) && nrow(as.matrix(block)) > 1L) {
      correlation <- stats::cov2cor(as.matrix(block))
    }
    if (is.null(correlation) || nrow(as.matrix(correlation)) < 2L) {
      return(numeric())
    }
    unname(correlation[upper.tri(correlation)])
  }), use.names = FALSE)

  list(sd = standard_deviations, correlation = correlations)
}

cnj_audit_estimator_diagnostics <- function(model, engine, warning_messages) {
  if (engine == "glmmTMB") {
    random_structure <- cnj_audit_random_structure(model)
    convergence_code <- model$fit$convergence
    pd_hessian <- model$sdr$pdHess
    gradient <- model$sdr$gradient.fixed
    max_abs_gradient <- if (is.null(gradient) || length(gradient) == 0L) {
      NA_real_
    } else {
      max(abs(as.double(gradient)))
    }
  } else {
    random_structure <- list(sd = numeric(), correlation = numeric())
    convergence_code <- if (isFALSE(model$convStatus)) 1L else 0L
    pd_hessian <- TRUE
    max_abs_gradient <- NA_real_
  }
  coefficients <- tryCatch(
    if (engine == "glmmTMB") {
      unlist(glmmTMB::fixef(model), use.names = FALSE)
    } else {
      unlist(stats::coef(model), use.names = FALSE)
    },
    error = function(condition) numeric()
  )
  if (length(coefficients) > 0L && any(!is.finite(coefficients))) {
    warning_messages <- c(warning_messages, "non-finite coefficient")
  }
  finite_coefficients <- coefficients[is.finite(coefficients)]
  if (length(finite_coefficients) > 0L &&
      any(abs(finite_coefficients) >= 20)) {
    warning_messages <- c(warning_messages, "separation signal: large coefficient")
  }
  fitted_probabilities <- tryCatch(
    as.double(stats::fitted(model)),
    error = function(condition) numeric()
  )
  fitted_probabilities <- fitted_probabilities[is.finite(fitted_probabilities)]
  if (
    length(fitted_probabilities) > 0L && convergence_code != 0L &&
      min(fitted_probabilities, na.rm = TRUE) <= 1e-8 &&
      max(fitted_probabilities, na.rm = TRUE) >= 1 - 1e-8
  ) {
    warning_messages <- c(
      warning_messages, "separation signal: boundary fitted probabilities"
    )
  }
  cnj_audit_classify_estimator_diagnostics(
    engine = engine,
    convergence_code = convergence_code,
    pd_hessian = pd_hessian,
    random_sd = random_structure$sd,
    random_correlation = random_structure$correlation,
    warning_messages = warning_messages,
    max_abs_gradient = max_abs_gradient
  )
}

#' Fit one pre-specified estimator and return sanitized diagnostics
#'
#' @param specification One specification returned by
#'   [cnj_audit_estimator_specifications()].
#' @param data Exact model sample.
#' @param fitter Optional injected fitter for tests or alternate execution
#'   adapters. It receives `formula`, `data`, and `specification` once.
#' @param tmb_threads OpenMP threads for a `glmmTMB` fit.
#' @return A model and diagnostics, or a sanitized fit failure.
#' @keywords internal
cnj_audit_fit_estimator <- function(
    specification, data, fitter = NULL, tmb_threads = 1L) {
  required_fields <- c(
    "engine", "formula", "formula_digest", "vcov", "fixed_signature"
  )
  if (!is.list(specification) ||
      !all(required_fields %in% names(specification))) {
    stop("Invalid audit model specification.", call. = FALSE)
  }
  formula_digest <- digest::digest(
    paste(deparse(specification$formula), collapse = "\n"),
    algo = "sha256",
    serialize = FALSE
  )
  if (!identical(formula_digest, specification$formula_digest)) {
    stop("The model formula changed after specification construction.",
         call. = FALSE)
  }
  if (
    length(tmb_threads) != 1L || is.na(tmb_threads) ||
      tmb_threads != as.integer(tmb_threads) || tmb_threads < 1L
  ) {
    stop("`tmb_threads` must be one positive integer.", call. = FALSE)
  }
  tmb_threads <- as.integer(tmb_threads)

  if (is.null(fitter)) {
    fitter <- function(formula, data, specification) {
      if (specification$engine == "fixest") {
        return(fixest::feglm(
          fml = formula,
          data = data,
          family = "binomial",
          vcov = specification$vcov
        ))
      }
      glmmTMB::glmmTMB(
        formula = formula,
        data = data,
        family = stats::binomial(),
        control = glmmTMB::glmmTMBControl(parallel = tmb_threads)
      )
    }
  }
  if (!is.function(fitter)) {
    stop("`fitter` must be a function.", call. = FALSE)
  }

  warning_messages <- character()
  warning_classes <- character()
  fit_error <- NULL
  model <- tryCatch(
    withCallingHandlers(
      fitter(specification$formula, data, specification),
      warning = function(condition) {
        warning_messages <<- c(warning_messages, conditionMessage(condition))
        warning_classes <<- c(
          warning_classes, paste(class(condition), collapse = "/")
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) {
      fit_error <<- condition
      NULL
    }
  )

  if (is.null(model)) {
    diagnostics <- cnj_audit_classify_estimator_diagnostics(
      engine = specification$engine,
      convergence_code = 1L,
      warning_messages = warning_messages
    )
    return(list(
      model = NULL,
      formula_digest = formula_digest,
      diagnostics = diagnostics,
      warning_classes = unique(warning_classes),
      error_class = if (is.null(fit_error)) "invalid_fit" else
        paste(class(fit_error), collapse = "/"),
      error_message = "fit failed; inspect the private execution trace"
    ))
  }

  list(
    model = model,
    formula_digest = formula_digest,
    diagnostics = cnj_audit_estimator_diagnostics(
      model, specification$engine, warning_messages
    ),
    warning_classes = unique(warning_classes),
    error_class = NA_character_,
    error_message = NA_character_
  )
}

cnj_audit_predict_response <- function(model, newdata) {
  if (inherits(model, "glmmTMB")) {
    return(stats::predict(
      model,
      newdata = newdata,
      type = "response",
      re.form = NULL,
      allow.new.levels = FALSE
    ))
  }
  stats::predict(model, newdata = newdata, type = "response")
}

cnj_audit_counterfactual_value <- function(data, column, value) {
  original <- data[[column]]
  if (is.factor(original)) {
    if (!value %in% levels(original)) {
      stop("Counterfactual level is absent from the model data.", call. = FALSE)
    }
    data[[column]] <- factor(value, levels = levels(original))
  } else {
    data[[column]] <- rep(value, nrow(data))
  }
  data
}

cnj_audit_validate_predictions <- function(predictions, expected_length) {
  if (
    !is.numeric(predictions) || length(predictions) != expected_length ||
      anyNA(predictions) || any(!is.finite(predictions)) ||
      any(predictions < 0 | predictions > 1)
  ) {
    stop(
      "The response predictor must return one finite probability per row.",
      call. = FALSE
    )
  }
  as.double(predictions)
}

cnj_audit_probability_dispersion <- function(estimates, origins) {
  prediction_matrix <- matrix(
    estimates,
    nrow = 1L,
    dimnames = list(NULL, origins)
  )
  dispersion <- cnj_audit_standardized_dispersion(prediction_matrix)
  tibble::tibble(
    sd = dispersion$sd,
    range = dispersion$range,
    iqr = dispersion$iqr,
    n_origins = length(origins)
  )
}

#' Standardize each origin over one common empirical distribution
#'
#' @param model A fitted fixed- or mixed-effects model.
#' @param data The common empirical standardization sample.
#' @param origins Origin levels to contrast with equal final weight.
#' @param origin Origin column name.
#' @param predictor Function accepting `model` and `newdata` and returning
#'   response-scale probabilities.
#' @return Origin-specific probabilities and equal-origin dispersion metrics.
#' @keywords internal
cnj_audit_standardize_origins <- function(
    model, data, origins = NULL, origin = "origin",
    predictor = cnj_audit_predict_response) {
  origin <- cnj_audit_model_name(origin, "origin")
  if (!is.data.frame(data) || nrow(data) == 0L || !origin %in% names(data)) {
    stop("Standardization needs non-empty data with an origin column.",
         call. = FALSE)
  }
  if (is.null(origins)) {
    origins <- sort(unique(data[[origin]]))
  }
  origins <- as.character(origins)
  if (length(origins) < 2L || anyNA(origins) || any(!nzchar(origins)) ||
      anyDuplicated(origins)) {
    stop("Standardization needs at least two unique origins.", call. = FALSE)
  }
  if (length(setdiff(origins, as.character(unique(data[[origin]])))) > 0L) {
    stop("Every standardized origin must occur in the model data.",
         call. = FALSE)
  }

  estimates <- vapply(origins, function(origin_value) {
    newdata <- cnj_audit_counterfactual_value(data, origin, origin_value)
    predictions <- cnj_audit_validate_predictions(
      predictor(model, newdata), nrow(newdata)
    )
    mean(predictions)
  }, numeric(1))

  list(
    by_origin = tibble::tibble(origin = origins, estimate = unname(estimates)),
    metrics = cnj_audit_probability_dispersion(estimates, origins)
  )
}
