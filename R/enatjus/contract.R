# Copiado de lab-dados/enatjus, R/contract.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' Closed-vocabulary levels of the analysis base
#'
#' Every value the modelling code knows how to interpret, per variable. These
#' fields come from `<select>` elements of the e-NatJus form, so their value
#' set is closed by construction: a value outside it means the form or the
#' pipeline changed, and that is a decision to take deliberately rather than
#' absorb into a catch-all recode.
#'
#' `origem_tratada` is deliberately absent. New NatJus are legitimate and must
#' not abort the run; see [ORIGIN_SENTINELS].
#'
#' @keywords internal
EXPECTED_LEVELS <- list(
  selConclusao = c("Favoravel", "Nao favoravel", "NÃO_PREENCHIDO"),
  selTipoTecnologia = c("Medicamento", "Procedimento", "Produto", "NÃO_PREENCHIDO"),
  selEsfera = c("Estadual", "Federal", "NÃO_PREENCHIDO"),
  selStaGenero = c("Feminino", "Masculino", "NÃO_PREENCHIDO"),
  selDefensoriaPublica = c("Defensoria Publica", "Ministerio Publico", "NÃO_PREENCHIDO"),
  selRecomendacaoConitec = c("Favoravel", "Desfavoravel", "Nao avaliada", "NÃO_PREENCHIDO"),
  selIndicacaoConformidade = c(
    "Sim", "Nao", "Nao sabe", "Nao informado", "NÃO_APLICÁVEL", "NÃO_PREENCHIDO"
  ),
  selPrevistoProtocolo = c(
    "Sim", "Nao", "Nao sabe", "Nao informado", "NÃO_APLICÁVEL", "NÃO_PREENCHIDO"
  ),
  selRegistroAnvisa = c("Sim", "Nao", "NÃO_APLICÁVEL", "NÃO_PREENCHIDO"),
  selDisponivelSus = c(
    "Sim", "Nao", "Nao sabe", "Nao informado", "NÃO_PREENCHIDO"
  ),
  selOncologico = c(
    "Sim", "Nao", "Nao sabe", "Nao informado", "NÃO_APLICÁVEL", "NÃO_PREENCHIDO"
  ),
  selExisteBiossimilar = c("Sim", "Nao", "NÃO_APLICÁVEL", "NÃO_PREENCHIDO"),
  # Urgency alleged in the request. Unlike its siblings it describes the case,
  # not the technology, and it is the covariate Model 3 leans on hardest -- so
  # a renamed level here would quietly change that model rather than stop it.
  # The three levels were read off the treated base on 2026-08-23.
  selAlegacaoUrgencia = c("Sim", "Nao", "NÃO_PREENCHIDO")
)

#' Values of `origem_tratada` that identify no NatJus
#'
#' The pipeline writes `NA` when the responsible institution is absent from
#' `dicionario_natjus.csv`, and a sentinel label when the note itself carries
#' no origin. That label changed gender between dictionary versions --
#' "Não informada" through 2026-01, "Não informado" afterwards -- so both are
#' declared here: the analysis must behave the same whichever build produced
#' the file it is handed.
#'
#' @keywords internal
ORIGIN_SENTINELS <- c("Não informado", "Não informada")

#' Free-text and identifier columns the modelling pipeline reads
#' @keywords internal
REQUIRED_COLUMNS <- c(
  "idNotaTecnica",
  "txtDcb",
  "txtCid",
  "origem_tratada",
  "data_emissao",
  # Added when the deck's numbers were reproduced from the parquet. Neither
  # belongs in EXPECTED_LEVELS: the contract must require them to be PRESENT,
  # not pin down their levels.
  #
  # `selSituacaoAnvisa` is read to build the rule that drops Anvisa status
  # labels leaked into `txtDcb` -- reading the vocabulary from the data is the
  # point, so declaring the expected levels here would put back the hardcoded
  # list the rule exists to avoid.
  #
  # `data_emissao_fonte` records where each emission date came from. The
  # `#tabela-associada` fallback writes a *finalisation* date into
  # `data_emissao`, and the slides say emission, so the sample keeps only
  # `"aviso"`. Its levels are a property of the scraper, not of the form.
  "selSituacaoAnvisa",
  "data_emissao_fonte",
  names(EXPECTED_LEVELS)
)

#' Expected shape of `data_emissao`
#'
#' Emission dates arrive as text and are parsed downstream with
#' `lubridate::dmy_hms()`, which returns `NA` and a warning -- not an error --
#' on anything else. A single silent `NA` here propagates: `1-tidy.R` derives
#' the time variable from this column, so unparsed dates leave the sample
#' without ever being counted.
#' @keywords internal
EMISSION_DATE_PATTERN <- "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$"

#' Placeholder written by the pipeline for an empty form field
#' @keywords internal
NOT_FILLED <- "NÃO_PREENCHIDO"

#' Placeholder for a field that does not apply to this kind of technology
#'
#' Distinct from [NOT_FILLED]: the form was not left blank, the question does
#' not apply. It dominates the medicine columns of notes about procedures and
#' products, which is why it is almost absent from a medicines-only sample.
#' @keywords internal
NOT_APPLICABLE <- "NÃO_APLICÁVEL"

#' The label the pipeline writes where the form gave no substantive answer
#'
#' Every value in [SENTINEL_LEVELS] folds to this, in `1-tidy.R`, and from
#' there it is a level like any other: it reaches the models, the tables and
#' the figures under this name.
#'
#' Here rather than written out at each site because it was written out at
#' fifteen, across four scripts, three of them `filter(conitec != "N/I")` on
#' samples that have to stay identical. Renaming it by editing those fifteen
#' would leave whichever one was missed quietly selecting different rows, with
#' no error: the comparison stays valid, it just stops excluding anything.
#'
#' Short and displayed as-is, so it is chosen to read in a figure axis, not to
#' be self-explanatory. Whatever names it in prose has to say so nearby.
#' @keywords internal
MISSING_LABEL <- "N/I"

#' Every string the form writes to mean "no substantive answer"
#'
#' Declared in one place because the columns used to disagree about which of
#' them to fold: one folded all four, three folded only the two "don't know"
#' labels, and one folded none. That divergence had no reason behind it and
#' left the same substantive answer under different names depending on which
#' column was read.
#' @keywords internal
SENTINEL_LEVELS <- c("Nao sabe", "Nao informado", NOT_FILLED, NOT_APPLICABLE)

#' The `data_emissao_fonte` level the sample keeps
#'
#' The scraper records where each emission date came from. The
#' `#tabela-associada` fallback writes a *finalisation* date into
#' `data_emissao`, and the slides say emission, so `1-tidy.R` keeps only the
#' notes whose date came from the notice itself.
#'
#' Named here rather than written as a literal in the filter, and validated by
#' [validate_contract()], for a reason that cuts against the note on
#' [REQUIRED_COLUMNS]: the levels of that column are a property of the scraper
#' and the contract deliberately does not pin them down, but the sample
#' depends on this one value existing. A rename upstream would leave the
#' filter matching nothing, and an empty sample is the failure this whole file
#' exists to make loud. Declaring the column present is not enough when a
#' string inside it carries the filter.
#'
#' Measured on the internal base on 2026-08-23: 490,768 notes of 541,884 carry
#' `"aviso"`, 48,968 `"ausente"` and 2,148 `"tabela"`.
#' @keywords internal
EMISSION_SOURCE_KEPT <- "aviso"

#' The clinical covariates of Model 3
#'
#' The six fields describing the technology under request. They enter Model 3
#' as controls, the robustness ladders as an alternative specification, and
#' `tab_preenchimento_origem.csv` as the subject of the non-exogeneity caveat.
#'
#' Here rather than in a script because three of `data-raw/` need it and the
#' scripts hand objects to each other through the global environment: two
#' copies of this vector are two vectors, the later assignment silently wins,
#' and nothing compares them. `selAlegacaoUrgencia` is deliberately not in it:
#' the six describe the technology, it describes the case, and every use draws
#' that line.
#' @keywords internal
CLINICAL_COVARIATES <- c(
  "selIndicacaoConformidade",
  "selPrevistoProtocolo",
  "selRegistroAnvisa",
  "selDisponivelSus",
  "selOncologico",
  "selExisteBiossimilar"
)

#' Validate the analysis base against the vocabulary contract
#'
#' Fails on the first structural deviation instead of letting it turn into a
#' quietly smaller sample. Checks, in order: that every column the modelling
#' code reads is present; that each closed-vocabulary field carries only
#' declared levels; and that emission dates match the format the parser
#' downstream assumes.
#'
#' @param d A data frame read from the analysis parquet.
#' @return `d`, invisibly.
#' @export
validate_contract <- function(d) {
  missing_columns <- setdiff(REQUIRED_COLUMNS, names(d))
  if (length(missing_columns) > 0) {
    hint <- if ("origem_tratada" %in% missing_columns) {
      paste0(
        "\n`origem_tratada` is built by the pipeline's treatment step, so this ",
        "is most likely one of the untreated parquets, where the treated base ",
        "was expected. ENATJUS_BASE_DIR may be pointing at the wrong folder."
      )
    } else {
      ""
    }
    stop(
      "Analysis base is missing required columns: ",
      paste(missing_columns, collapse = ", "), hint,
      call. = FALSE
    )
  }

  # `NA` used to be dropped here before the comparison, so a column that
  # arrived half missing passed the contract and reached the models as a level
  # the modelling code silently excludes. That is the failure this function is
  # written to prevent, in the one form it was blind to: the form field is a
  # `<select>` with a placeholder, so it is never empty at source, and `NA`
  # means the pipeline lost it. Zero of these columns carry any `NA` on the
  # internal base as of 2026-08-23, which is why failing is affordable.
  missing_values <- vapply(
    names(EXPECTED_LEVELS),
    function(column) sum(is.na(d[[column]])),
    integer(1)
  )
  if (any(missing_values > 0)) {
    affected <- missing_values[missing_values > 0]
    stop(
      "`NA` in closed-vocabulary column(s): ",
      paste0(names(affected), " (n = ",
             formatC(affected, format = "d", big.mark = ","), ")",
             collapse = ", "),
      "\nThese fields come from `<select>` elements and are never empty at ",
      "source, so an `NA` here was lost in the pipeline, not left blank by ",
      "the person filling the form.",
      call. = FALSE
    )
  }

  for (column in names(EXPECTED_LEVELS)) {
    observed <- unique(d[[column]])
    unknown <- setdiff(observed[!is.na(observed)], EXPECTED_LEVELS[[column]])
    if (length(unknown) > 0) {
      counts <- vapply(
        unknown,
        function(value) sum(d[[column]] == value, na.rm = TRUE),
        integer(1)
      )
      stop(
        "Undeclared level(s) in `", column, "`: ",
        paste0(unknown, " (n = ", formatC(counts, format = "d", big.mark = ","), ")",
               collapse = ", "),
        "\nDecide how the modelling code should treat them, then add them to ",
        "`EXPECTED_LEVELS` in R/contract.R.",
        call. = FALSE
      )
    }
  }

  if (!EMISSION_SOURCE_KEPT %in% d[["data_emissao_fonte"]]) {
    stop(
      "No row has `data_emissao_fonte == \"", EMISSION_SOURCE_KEPT, "\"`, ",
      "which is the only source `1-tidy.R` keeps, so the sample would come ",
      "out empty.\nObserved level(s): ",
      paste0("\"", utils::head(sort(unique(d[["data_emissao_fonte"]])), 5), "\"",
             collapse = ", "),
      "\nIf the scraper renamed it, change `EMISSION_SOURCE_KEPT` in ",
      "R/contract.R rather than the filter.",
      call. = FALSE
    )
  }

  dates <- d[["data_emissao"]]
  malformed <- !is.na(dates) & dates != NOT_FILLED &
    !grepl(EMISSION_DATE_PATTERN, dates)
  if (any(malformed)) {
    stop(
      "`data_emissao` does not match dd/mm/yyyy hh:mm:ss in ",
      formatC(sum(malformed), format = "d", big.mark = ","), " row(s), e.g. ",
      paste0("\"", utils::head(unique(dates[malformed]), 3), "\"",
             collapse = ", "),
      "\nParsing these with `lubridate::dmy_hms()` would drop them silently.",
      call. = FALSE
    )
  }

  invisible(d)
}
