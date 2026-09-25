source("R/model-contract.R")

expect_error <- function(expression, pattern = NULL) {
  message <- tryCatch(
    {
      force(expression)
      NA_character_
    },
    error = function(error) conditionMessage(error)
  )
  if (is.na(message)) stop("Expected an error, but the expression succeeded.")
  if (!is.null(pattern) && !grepl(pattern, message, fixed = TRUE)) {
    stop("Error did not contain the expected text: ", pattern)
  }
  invisible(message)
}

make_fixture <- function() {
  result <- tibble::tibble(
    idNotaTecnica = sprintf("synthetic-%02d", 1:4),
    month_of_emission = as.Date(c("2019-05-01", "2019-06-01", "2020-04-01", "2020-05-01")),
    cov_selRecomendacaoConitec = c("Nao avaliada", "Favoravel", "Desfavoravel", MISSING_VALUE),
    cov_selStaGenero = c("Feminino", "Masculino", MISSING_VALUE, "Feminino"),
    cov_selDefensoriaPublica = c("Defensoria Publica", "Ministerio Publico", MISSING_VALUE, "Defensoria Publica"),
    payload = c(4L, 3L, 2L, 1L)
  )
  for (field in CLINICAL_FIELDS) {
    result[[paste0("cov_", field)]] <- c("Nao", "Sim", MISSING_VALUE, "Nao")
  }
  result
}

check_synthetic <- function() {
  original_contrasts <- getOption("contrasts")
  on.exit(options(contrasts = original_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  contract <- model_contract()
  expected_conitec <- list(
    favourable_vs_not_evaluated = c(left = "Favoravel", right = "Nao avaliada"),
    unfavourable_vs_not_evaluated = c(left = "Desfavoravel", right = "Nao avaliada"),
    favourable_vs_unfavourable = c(left = "Favoravel", right = "Desfavoravel")
  )
  stopifnot(
    identical(contract$factors$conitec$levels, c("Nao avaliada", "Favoravel", "Desfavoravel", MISSING_VALUE)),
    identical(contract$factors$conitec$reference, "Nao avaliada"),
    identical(contract$factors$conitec$contrasts, expected_conitec),
    identical(contract$factors$sex$reference, "Feminino"),
    identical(contract$factors$legal_representation$reference, "Defensoria Publica"),
    all(vapply(contract$factors, `[[`, character(1), "coding") == "treatment"),
    all(vapply(contract$factors[CLINICAL_FIELDS], `[[`, character(1), "reference") == "Nao"),
    identical(contract$time$anchor, as.Date("2019-05-01")),
    identical(contract$time$annual_multiplier, 12L),
    identical(contract$support$status, "not_defined"),
    is.null(contract$support$criterion),
    identical(contract$support$classify, FALSE)
  )

  fixture <- make_fixture()
  result <- prepare_model_contract(fixture, contract)
  prepared <- result$data
  stopifnot(
    identical(result$metadata, contract),
    nrow(prepared) == nrow(fixture),
    identical(prepared$idNotaTecnica, fixture$idNotaTecnica),
    identical(prepared$payload, fixture$payload),
    identical(prepared$ano_mes, c(0L, 1L, 11L, 12L)),
    identical(0.125 * result$metadata$time$annual_multiplier, 1.5)
  )
  for (name in names(contract$factors)) {
    spec <- contract$factors[[name]]
    stopifnot(
      is.factor(prepared[[spec$column]]),
      identical(levels(prepared[[spec$column]]), spec$levels),
      identical(contrasts(prepared[[spec$column]]), factor_contrast_matrix(spec)),
      identical(as.character(prepared[[spec$column]]), as.character(fixture[[spec$column]]))
    )
  }

  without_missing <- fixture
  for (name in names(contract$factors)) {
    spec <- contract$factors[[name]]
    values <- without_missing[[spec$column]]
    values[values == MISSING_VALUE] <- spec$reference
    without_missing[[spec$column]] <- values
  }
  prepared_without_missing <- prepare_model_contract(without_missing, contract)$data
  stopifnot(all(vapply(names(contract$factors), function(name) {
    spec <- contract$factors[[name]]
    MISSING_VALUE %in% levels(prepared_without_missing[[spec$column]])
  }, logical(1))))

  unknown <- fixture
  unknown$cov_selRecomendacaoConitec[[1L]] <- "Unsupported"
  expect_error(prepare_model_contract(unknown, contract), "Unsupported")

  null_value <- fixture
  null_value$cov_selStaGenero[[1L]] <- NA_character_
  expect_error(prepare_model_contract(null_value, contract), "unnormalised missing value")

  expect_error(
    prepare_model_contract(fixture[, setdiff(names(fixture), "cov_selStaGenero")], contract),
    "cov_selStaGenero"
  )
  expect_error(
    prepare_model_contract(transform(fixture, idNotaTecnica = replace(idNotaTecnica, 2L, idNotaTecnica[[1L]])), contract),
    "unique"
  )
  expect_error(
    prepare_model_contract(transform(fixture, ano_mes = 0:3), contract),
    "already contains"
  )
  expect_error(
    prepare_model_contract(transform(fixture, month_of_emission = as.Date("2019-04-01")), contract),
    "before the fixed time anchor"
  )

  cat("Synthetic model-contract checks passed: factors, contrasts, missing values, time and preservation.\n")
}

check_real <- function() {
  contract <- model_contract()
  input <- read_analysis_covariates()
  result <- prepare_model_contract(input, contract)
  prepared <- result$data
  factor_columns <- vapply(contract$factors, `[[`, character(1), "column")


  stopifnot(
    nrow(input) == 273734L,
    nrow(prepared) == 273734L,
    dplyr::n_distinct(prepared$analysis_origin) == 25L,
    identical(input$idNotaTecnica, prepared$idNotaTecnica),
    identical(range(prepared$ano_mes), c(0L, 87L)),
    !any(as.character(prepared$cov_selRecomendacaoConitec) == MISSING_VALUE),
    identical(result$metadata$support$status, "not_defined"),
    is.null(result$metadata$support$criterion),
    !result$metadata$support$classify
  )
  for (name in names(contract$factors)) {
    spec <- contract$factors[[name]]
    before <- sort(table(as.character(input[[spec$column]])))
    after <- sort(table(as.character(prepared[[spec$column]])))
    stopifnot(
      identical(before, after),
      identical(levels(prepared[[spec$column]]), spec$levels),
      identical(contrasts(prepared[[spec$column]]), factor_contrast_matrix(spec))
    )
  }
  stopifnot(all(vapply(setdiff(names(input), factor_columns), function(name) {
    identical(input[[name]], prepared[[name]])
  }, logical(1))))

  cat(
    "Real model-contract checks passed: ", nrow(prepared),
    " notes, ", dplyr::n_distinct(prepared$analysis_origin),
    " analytical origins and months 0-87.\n",
    sep = ""
  )
}


check_synthetic()
if (!"--synthetic-only" %in% commandArgs(trailingOnly = TRUE)) check_real()
