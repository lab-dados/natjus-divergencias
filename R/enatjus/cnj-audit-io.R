# Copiado de lab-dados/enatjus, R/cnj-audit-io.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Hash an ordered vector with length-prefixed values
#'
#' @param values Character values in their canonical order.
#' @return A SHA-256 hex digest.
#' @keywords internal
cnj_audit_hash_values <- function(values) {
  if (anyNA(values)) {
    stop("Values must be non-missing before hashing.", call. = FALSE)
  }
  values <- enc2utf8(as.character(values))
  stream <- paste0(
    vapply(values, nchar, integer(1), type = "bytes"),
    ":", values, "\n",
    collapse = ""
  )
  digest::digest(stream, algo = "sha256", serialize = FALSE)
}

#' Hash the exact sample without exposing technical-note identifiers
#'
#' @param data Private audit rows carrying the salted `internal_order` key.
#' @return A SHA-256 digest of the row-order-independent sample set.
#' @keywords internal
cnj_audit_sample_digest <- function(data) {
  cnj_audit_require_columns(data, "internal_order", "audit sample")
  keys <- as.character(data$internal_order)
  if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    stop(
      "An audit sample requires complete unique internal_order keys.",
      call. = FALSE
    )
  }
  cnj_audit_hash_values(sort(keys, method = "radix"))
}

#' Validate that an aggregate output contains no restricted data
#'
#' @param data A data frame intended for an audit artefact.
#' @return `data`, invisibly.
#' @keywords internal
cnj_audit_validate_public_output <- function(data) {
  allowed_audit_ids <- c(
    "cell_id", "fit_id", "result_id", "reference_result_id",
    "specification_id", "reference_specification_id", "decision_id",
    "reference_decision_id", "decision_result_id", "window_id",
    "mechanism_id", "comparison_id", "critique_id"
  )
  prohibited <- grepl(
    paste0(
      "(^id_|_id$|idNota|process_key|process_number|numero_processo|",
      "data_nascimento|^txt|nome|person|free.?text)"
    ),
    names(data),
    ignore.case = TRUE
  ) & !names(data) %in% allowed_audit_ids
  if (any(prohibited)) {
    stop("Audit output contains a prohibited column: ",
         paste(names(data)[prohibited], collapse = ", "), call. = FALSE)
  }

  character_columns <- data[vapply(data, is.character, logical(1))]
  if (length(character_columns) > 0L) {
    process_pattern <- "\\b[0-9]{7}-?[0-9]{2}\\.?[0-9]{4}\\.?[0-9]\\.?[0-9]{2}\\.?[0-9]{4}\\b"
    carries_process <- any(vapply(
      character_columns,
      function(column) any(grepl(process_pattern, column, perl = TRUE), na.rm = TRUE),
      logical(1)
    ))
    if (carries_process) {
      stop("Audit output contains a CNJ process value.", call. = FALSE)
    }
  }

  invisible(data)
}
