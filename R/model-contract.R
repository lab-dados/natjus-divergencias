# Preparar o contrato fechado de covariáveis de baixa dimensão ----------------
#
# Rodar a partir da raiz do repositório depois que `R/04-origins.R` tiver
# publicado a população final. Dar `source()` neste arquivo define as funções e
# não escreve nada.

source("R/04-origins.R")

factor_spec <- function(column, levels, reference, substantive_levels, contrasts) {
  list(
    column = column,
    levels = levels,
    reference = reference,
    substantive_levels = substantive_levels,
    contrasts = contrasts,
    coding = "treatment",
    missing_level = MISSING_VALUE,
    missing_role = "auxiliary"
  )
}

factor_contrast_matrix <- function(spec) {
  stats::contr.treatment(
    spec$levels,
    base = match(spec$reference, spec$levels)
  )
}

model_contract <- function() {
  binary_levels <- c("Nao", "Sim", MISSING_VALUE)
  binary_contrast <- list(sim_vs_nao = c(left = "Sim", right = "Nao"))
  clinical <- setNames(
    lapply(CLINICAL_FIELDS, function(field) {
      factor_spec(
        paste0("cov_", field),
        binary_levels,
        "Nao",
        c("Nao", "Sim"),
        binary_contrast
      )
    }),
    CLINICAL_FIELDS
  )

  contract <- list(
    factors = c(
      list(
        conitec = factor_spec(
          "cov_selRecomendacaoConitec",
          c("Nao avaliada", "Favoravel", "Desfavoravel", MISSING_VALUE),
          "Nao avaliada",
          c("Nao avaliada", "Favoravel", "Desfavoravel"),
          list(
            favourable_vs_not_evaluated = c(left = "Favoravel", right = "Nao avaliada"),
            unfavourable_vs_not_evaluated = c(left = "Desfavoravel", right = "Nao avaliada"),
            favourable_vs_unfavourable = c(left = "Favoravel", right = "Desfavoravel")
          )
        ),
        sex = factor_spec(
          "cov_selStaGenero",
          c("Feminino", "Masculino", MISSING_VALUE),
          "Feminino",
          c("Feminino", "Masculino"),
          list(masculine_vs_feminine = c(left = "Masculino", right = "Feminino"))
        ),
        legal_representation = factor_spec(
          "cov_selDefensoriaPublica",
          c("Defensoria Publica", "Ministerio Publico", MISSING_VALUE),
          "Defensoria Publica",
          c("Defensoria Publica", "Ministerio Publico"),
          list(
            prosecution_vs_public_defence = c(
              left = "Ministerio Publico",
              right = "Defensoria Publica"
            )
          )
        )
      ),
      clinical
    ),
    time = list(
      source_column = "month_of_emission",
      output_column = "ano_mes",
      anchor = SERIES_START,
      unit = "calendar_month",
      annual_multiplier = 12L
    ),
    support = list(
      status = "not_defined",
      criterion = NULL,
      classify = FALSE
    )
  )
  validate_model_contract_definition(contract)
  contract
}

validate_model_contract_definition <- function(contract) {
  required_sections <- c("factors", "time", "support")
  if (!identical(names(contract), required_sections)) {
    stop("Model contract sections differ from the required schema.", call. = FALSE)
  }
  if (!length(contract$factors) || anyDuplicated(names(contract$factors))) {
    stop("Model factor specifications must be named and unique.", call. = FALSE)
  }

  factor_columns <- vapply(contract$factors, `[[`, character(1), "column")
  if (anyDuplicated(factor_columns)) {
    stop("Model factor columns must be unique.", call. = FALSE)
  }
  for (name in names(contract$factors)) {
    spec <- contract$factors[[name]]
    required <- c(
      "column",
      "levels",
      "reference",
      "substantive_levels",
      "contrasts",
      "coding",
      "missing_level",
      "missing_role"
    )
    if (!identical(names(spec), required)) {
      stop("Factor specification has an invalid schema: ", name, call. = FALSE)
    }
    if (anyNA(spec$levels) || any(!nzchar(spec$levels)) || anyDuplicated(spec$levels)) {
      stop("Factor levels must be present, non-empty and unique: ", name, call. = FALSE)
    }
    if (!identical(spec$reference, spec$levels[[1L]])) {
      stop("The reference must be the first factor level: ", name, call. = FALSE)
    }
    if (!identical(spec$coding, "treatment")) {
      stop("Factor coding must be explicit treatment coding: ", name, call. = FALSE)
    }
    if (!identical(spec$missing_level, MISSING_VALUE) ||
        !identical(spec$missing_role, "auxiliary") ||
        !spec$missing_level %in% spec$levels ||
        spec$missing_level %in% spec$substantive_levels) {
      stop("The auxiliary missing level is inconsistent: ", name, call. = FALSE)
    }
    if (!all(spec$substantive_levels %in% spec$levels)) {
      stop("A substantive level is absent from the factor vocabulary: ", name, call. = FALSE)
    }
    for (contrast_name in names(spec$contrasts)) {
      contrast <- spec$contrasts[[contrast_name]]
      if (!identical(names(contrast), c("left", "right")) ||
          identical(unname(contrast[["left"]]), unname(contrast[["right"]])) ||
          !all(unname(contrast) %in% spec$substantive_levels)) {
        stop("Factor contrast is inconsistent: ", name, "/", contrast_name, call. = FALSE)
      }
    }
  }

  if (!inherits(contract$time$anchor, "Date") ||
      !identical(contract$time$unit, "calendar_month") ||
      !identical(contract$time$annual_multiplier, 12L)) {
    stop("Time contract differs from calendar months and a twelve-month reading.", call. = FALSE)
  }
  if (!identical(contract$support, list(
    status = "not_defined",
    criterion = NULL,
    classify = FALSE
  ))) {
    stop("Contrast support must remain undefined and non-classifying.", call. = FALSE)
  }
  invisible(contract)
}

calendar_month_index <- function(value, anchor) {
  if (!inherits(value, "Date") || anyNA(value)) {
    stop("Time values must be complete Date objects.", call. = FALSE)
  }
  value_parts <- as.POSIXlt(value)
  anchor_parts <- as.POSIXlt(anchor)
  as.integer(
    (value_parts$year - anchor_parts$year) * 12L +
      (value_parts$mon - anchor_parts$mon)
  )
}

prepare_model_contract <- function(data, contract = model_contract()) {
  validate_model_contract_definition(contract)
  if (!is.data.frame(data)) {
    stop("Model input must be a data frame.", call. = FALSE)
  }
  factor_columns <- vapply(contract$factors, `[[`, character(1), "column")
  required <- c("idNotaTecnica", factor_columns, contract$time$source_column)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop(
      "Model input lacks required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (contract$time$output_column %in% names(data)) {
    stop("Model input already contains the derived time column.", call. = FALSE)
  }
  if (anyNA(data$idNotaTecnica) || anyDuplicated(data$idNotaTecnica)) {
    stop("Technical-note IDs must be present and unique.", call. = FALSE)
  }

  prepared <- data
  for (name in names(contract$factors)) {
    spec <- contract$factors[[name]]
    values <- as.character(prepared[[spec$column]])
    if (anyNA(values)) {
      stop("Factor contains an unnormalised missing value: ", spec$column, call. = FALSE)
    }
    unknown <- setdiff(unique(values), spec$levels)
    if (length(unknown)) {
      stop(
        "Factor contains unsupported values in ", spec$column, ": ",
        paste(sort(unknown), collapse = ", "),
        call. = FALSE
      )
    }
    prepared[[spec$column]] <- factor(values, levels = spec$levels)
    contrasts(prepared[[spec$column]]) <- factor_contrast_matrix(spec)
  }

  month_index <- calendar_month_index(
    prepared[[contract$time$source_column]],
    contract$time$anchor
  )
  if (any(month_index < 0L)) {
    stop("Model input contains a month before the fixed time anchor.", call. = FALSE)
  }
  prepared[[contract$time$output_column]] <- month_index

  assert_model_contract(data, prepared, contract)
  list(data = prepared, metadata = contract)
}

assert_model_contract <- function(input, prepared, metadata) {
  validate_model_contract_definition(metadata)
  if (nrow(input) != nrow(prepared) ||
      !identical(input$idNotaTecnica, prepared$idNotaTecnica)) {
    stop("Model preparation changed row count, IDs or row order.", call. = FALSE)
  }

  factor_columns <- vapply(metadata$factors, `[[`, character(1), "column")
  inherited <- setdiff(names(input), factor_columns)
  unchanged <- vapply(inherited, function(name) {
    identical(input[[name]], prepared[[name]])
  }, logical(1))
  if (!all(unchanged)) {
    stop(
      "Model preparation changed inherited columns: ",
      paste(names(unchanged)[!unchanged], collapse = ", "),
      call. = FALSE
    )
  }
  expected_names <- c(names(input), metadata$time$output_column)
  if (!identical(names(prepared), expected_names)) {
    stop("Model preparation added or removed an unexpected column.", call. = FALSE)
  }

  for (name in names(metadata$factors)) {
    spec <- metadata$factors[[name]]
    value <- prepared[[spec$column]]
    if (!is.factor(value) || !identical(levels(value), spec$levels)) {
      stop("Prepared factor levels differ from the contract: ", spec$column, call. = FALSE)
    }
    if (!identical(contrasts(value), factor_contrast_matrix(spec))) {
      stop("Prepared factor contrasts differ from treatment coding: ", spec$column, call. = FALSE)
    }
    if (!identical(as.character(value), as.character(input[[spec$column]]))) {
      stop("Factor preparation changed categorical values: ", spec$column, call. = FALSE)
    }
  }

  expected_month <- calendar_month_index(
    input[[metadata$time$source_column]],
    metadata$time$anchor
  )
  actual_month <- prepared[[metadata$time$output_column]]
  if (!is.integer(actual_month) || !identical(actual_month, expected_month)) {
    stop("Prepared month index differs from the fixed calendar scale.", call. = FALSE)
  }
  invisible(prepared)
}
