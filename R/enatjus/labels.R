# Copiado de lab-dados/enatjus, R/labels.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Normalise a form label for comparison
#'
#' Upper case, trimmed, accents stripped. Both sides of every comparison in
#' `1-tidy.R` go through this, and the point is that they go through the *same*
#' function rather than through the same intention.
#'
#' ICU through `stringi`, and deliberately not `iconv(x, to =
#' "ASCII//TRANSLIT")`, which is what this used to be. `//TRANSLIT` delegates
#' to glibc, whose behaviour is a function of the locale, and it fails by
#' returning `NA` rather than by raising: under `LC_ALL=C` every accented
#' value of `txtDcb` came back `NA`, `NA %in% labels` is `FALSE`, and the
#' negation kept exactly the rows the filter exists to remove. The mirror half
#' dropped the notes it should have kept. `Rscript` under cron, a systemd unit
#' or a container with no locale generated all land in `LC_CTYPE=C`.
#'
#' Making both sides of the comparison pass through this one function is not
#' enough on its own, and that was the first attempt: the sentinels come from
#' package constants, which carry an encoding mark, and the data comes from a
#' parquet, which under `LC_ALL=C` did not -- so the two sides flattened
#' differently even through identical code. ICU carries its own transliteration
#' tables and consults no environment variable, so the output is the same
#' everywhere.
#'
#' Transliterate first, then upper-case: `toupper()` is itself locale-aware and
#' under `C` reaches only ASCII, so an accented character surviving to that
#' point would pass through unchanged.
#'
#' Fixing the locale in `run-all.R` would also have worked and is deliberately
#' not what this does: `LC_COLLATE` decides how `dplyr::arrange()` orders
#' strings, so pinning it to stabilise one comparison would silently reorder
#' the rows of every table sorted by text.
#'
#' What this does not protect against is two distinct labels transliterating to
#' the same string. That has to be checked against the vocabulary actually
#' present; see [flatten_label_collisions()].
#'
#' @param x Character vector of labels.
#' @return `x`, normalised.
#' @export
flatten_label <- function(x) {
  toupper(trimws(stringi::stri_trans_general(x, "Latin-ASCII")))
}
