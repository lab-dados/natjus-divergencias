# Copiado de lab-dados/enatjus, R/cnj-audit-contract.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

cnj_audit_scalar_integer <- function(value, name, minimum = 1L) {
  if (
    length(value) != 1L || !is.numeric(value) || is.na(value) ||
      !is.finite(value) || value != floor(value) || value < minimum ||
      value > .Machine$integer.max
  ) {
    stop("`", name, "` must be one integer >= ", minimum, ".", call. = FALSE)
  }
  as.integer(value)
}

cnj_audit_require_columns <- function(data, required, object_name) {
  if (!is.data.frame(data)) {
    stop("`", object_name, "` must be a data frame.", call. = FALSE)
  }
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "`", object_name, "` is missing required columns: ",
      paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }
}
