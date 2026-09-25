# Copiado de lab-dados/enatjus, R/convergence.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Has a `glmmTMB` fit converged?
#'
#' Two conditions, and the second is the one that matters here. `glmmTMB`
#' signals trouble through warnings and a non-positive-definite Hessian rather
#' than through an error, so a fit that failed flows straight into `summary()`
#' and into a figure looking like an estimate. The exit code alone misses that
#' case: the optimiser can report success on a likelihood surface where the
#' variance components are not identified.
#'
#' `isTRUE()` on both halves, and not a bare comparison: `model$sdr` is absent
#' when the fit never reached the standard-error step, and `NULL == 0` is
#' `logical(0)`, which `&&` rejects with an error under R 4.3 and silently
#' passed as missing before it. The failure mode being guarded against must
#' not be able to crash the guard.
#'
#' @param model A `glmmTMB` fit.
#' @return `TRUE` or `FALSE`, never `NA`.
#' @export
has_converged <- function(model) {
  isTRUE(model$fit$convergence == 0) && isTRUE(model$sdr$pdHess)
}
