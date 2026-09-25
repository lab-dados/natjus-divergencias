# Copiado de lab-dados/enatjus, R/attrition.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

the <- new.env(parent = emptyenv())

#' Begin an attrition log
#'
#' @param data The unfiltered data.
#' @param label Name for the starting row of the log.
#' @return `data`, unchanged.
#' @export
start_attrition <- function(data, label = "loaded") {
  the$attrition <- tibble::tibble(
    step = label,
    removed_false = 0L,
    removed_na = 0L,
    remaining = nrow(data)
  )
  data
}

#' Filter rows, recording what was removed
#'
#' @param data A data frame.
#' @param condition A filtering expression evaluated within `data`.
#' @param reason Short description of what the filter keeps, used as the label
#'   in the attrition table.
#' @return `data` restricted to the rows where `condition` is `TRUE`.
#' @export
keep <- function(data, condition, reason) {
  # `data` must be forced before the guard below. Under the native pipe,
  # `x |> start_attrition() |> keep(...)` compiles to
  # `keep(start_attrition(x), ...)`, and R evaluates arguments lazily: without
  # this, the guard runs while `start_attrition()` is still an unevaluated
  # promise and the first `keep()` of every pipeline fails.
  force(data)

  if (is.null(the$attrition)) {
    stop("Call `start_attrition()` before `keep()`.", call. = FALSE)
  }

  # Standard R non-standard evaluation, the same mechanism `dplyr::filter()`
  # uses: `condition` is an unevaluated expression written in this package's
  # own scripts, evaluated against the columns of `data` and then the caller's
  # frame. No external or user-supplied string is ever parsed here.
  kept <- eval(substitute(condition), data, parent.frame())
  if (!is.logical(kept)) {
    stop("`condition` in `keep(\"", reason, "\")` is not logical.", call. = FALSE)
  }
  # A condition of the wrong length recycles under `[`, so the row would be
  # written to the attrition table before anything complained -- and a scalar
  # condition never complains at all.
  if (length(kept) != nrow(data)) {
    stop(
      "`condition` in `keep(\"", reason, "\")` has length ", length(kept),
      " for ", nrow(data), " rows.",
      call. = FALSE
    )
  }

  removed_na <- sum(is.na(kept))
  removed_false <- sum(!kept, na.rm = TRUE)
  kept[is.na(kept)] <- FALSE

  the$attrition <- dplyr::bind_rows(
    the$attrition,
    tibble::tibble(
      step = reason,
      removed_false = as.integer(removed_false),
      removed_na = as.integer(removed_na),
      remaining = as.integer(sum(kept))
    )
  )

  if (!any(kept)) {
    stop(
      "Filter \"", reason, "\" removed every row.\n",
      "This is the failure mode a vocabulary mismatch produces: the ",
      "expression is valid, matches nothing, and would otherwise reach the ",
      "models as an empty sample.",
      call. = FALSE
    )
  }

  data[kept, , drop = FALSE]
}

#' The attrition log for the current session
#'
#' @return A tibble with one row per filter: how many notes it removed because
#'   the condition was false, how many because it could not be evaluated, and
#'   how many remained.
#' @export
attrition_table <- function() {
  if (is.null(the$attrition)) {
    stop("No attrition recorded yet.", call. = FALSE)
  }
  the$attrition
}
