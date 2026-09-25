# Copiado de lab-dados/enatjus, R/cnj-audit-models.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Wilson interval for binomial rates
#'
#' @param favourable Number of favourable notes.
#' @param total Number of notes.
#' @param level Confidence level.
#' @return A tibble with estimates and interval limits.
#' @keywords internal
cnj_audit_wilson <- function(favourable, total, level = 0.95) {
  if (any(total <= 0L) || any(favourable < 0L) || any(favourable > total)) {
    stop("Binomial counts must satisfy 0 <= favourable <= total.", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  estimate <- as.double(favourable) / as.double(total)
  denominator <- 1 + z^2 / total
  centre <- (estimate + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt(
    estimate * (1 - estimate) / total + z^2 / (4 * total^2)
  ) / denominator

  tibble::tibble(
    estimate = estimate,
    conf_low = pmax(0, centre - half_width),
    conf_high = pmin(1, centre + half_width)
  )
}

#' Summarise standardized predictions across origins
#'
#' @param predictions Matrix with empirical notes in rows and origins in columns.
#' @return Origin probabilities and equal-origin dispersion metrics.
#' @keywords internal
cnj_audit_standardized_dispersion <- function(predictions) {
  if (!is.matrix(predictions) || nrow(predictions) == 0L || ncol(predictions) < 2L) {
    stop("Predictions need notes in rows and at least two origins in columns.",
         call. = FALSE)
  }
  origin_probability <- colMeans(predictions)
  list(
    origin_probability = origin_probability,
    sd = stats::sd(origin_probability),
    range = diff(range(origin_probability)),
    iqr = stats::IQR(origin_probability)
  )
}
