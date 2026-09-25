source("R/04-origins.R")

expect_error <- function(expression) {
  inherits(try(force(expression), silent = TRUE), "try-error")
}

make_notes <- function(counts) {
  labels <- rep(names(counts), as.integer(counts))
  tibble::tibble(
    idNotaTecnica = sprintf("synthetic-%03d", seq_along(labels)),
    origem_tratada = labels,
    payload = seq_along(labels)
  )
}

check_synthetic <- function() {
  map <- origin_decision_map()
  stopifnot(
    nrow(map) == 13L,
    !anyDuplicated(map$origin_received),
    setequal(unique(map$origin_identification), setdiff(ORIGIN_IDENTIFICATION_VALUES, "not_adjudicated")),
    expect_error(validate_origin_map(map[-1L, ])),
    expect_error(validate_origin_map(bind_rows(map, map[1L, ]))),
    expect_error(validate_origin_map(mutate(map, origin_identification = replace(origin_identification, 1L, "unknown"))))
  )

  counts <- c(
    "PR" = 2L,
    "PR/CAMS" = 2L,
    "PR/CHR" = 1L,
    "PR/CHC-UFPR" = 1L,
    "PR/UEL" = 1L,
    "SP" = 3L,
    "SP/HC" = 2L,
    "AM" = 1L,
    "AM/SES" = 5L,
    "AM/SEMSA" = 2L,
    "RS" = 1L,
    "RS/DMJ" = 5L,
    "TelessaúdeRS-UFRGS" = 5L,
    "BA" = 5L
  )
  notes <- make_notes(counts)
  classified <- classify_origins(notes)

  stopifnot(
    identical(notes$idNotaTecnica, classified$idNotaTecnica),
    identical(notes$origem_tratada, classified$origin_received),
    identical(notes$payload, classified$payload),
    identical(classified$origem_tratada, classified$analysis_origin),
    all(classified$analysis_origin[classified$origin_received %in% names(counts)[1:5]] == "PR"),
    all(classified$analysis_origin[classified$origin_received %in% c("SP", "SP/HC")] == "SP"),
    classified$analysis_origin[classified$origin_received == "AM"] == "AM",
    classified$analysis_origin[classified$origin_received == "RS"] == "RS",
    classified$analysis_origin[classified$origin_received == "AM/SES"] == "AM/SES",
    classified$analysis_origin[classified$origin_received == "RS/DMJ"] == "RS/DMJ",
    classified$origin_identification[classified$origin_received == "BA"] == "not_adjudicated",
    classified$origin_identification[classified$origin_received == "AM"] == "state_unidentified",
    classified$origin_identification[classified$origin_received == "PR/CAMS"] == "state_aggregate",
    classified$origin_identification[classified$origin_received == "AM/SES"] == "specific_retained",
    expect_error(classify_origins(classified)),
    expect_error(classify_origins(notes[c(1L, 1L), ])),
    expect_error(classify_origins(transform(notes, idNotaTecnica = replace(idNotaTecnica, 1L, NA_character_))))
  )

  tracked <- start_attrition(classified, "synthetic population")
  sample <- apply_analysis_origin_floor(tracked, floor = 5L)
  included <- sort(unique(sample$analysis_origin))
  stopifnot(
    identical(included, sort(c("AM/SES", "BA", "PR", "RS/DMJ", "SP", "TelessaúdeRS-UFRGS"))),
    sum(sample$analysis_origin == "PR") == sum(counts[1:5]),
    sum(sample$analysis_origin == "SP") == sum(counts[c("SP", "SP/HC")]),
    !any(sample$analysis_origin %in% c("AM", "AM/SEMSA", "RS")),
    sum(grepl("analytical origins with at least", attrition_table()$step)) == 1L
  )

  entered_id <- sample$idNotaTecnica[sample$analysis_origin == "PR"][1L]
  exited_id <- classified$idNotaTecnica[classified$analysis_origin == "AM"][1L]
  previous_ids <- c(setdiff(sample$idNotaTecnica, entered_id), exited_id)
  previous_sample <- tibble::tibble(idNotaTecnica = previous_ids)
  ledger <- build_origin_ledger(classified, previous_sample, sample)
  missing_historical <- bind_rows(
    previous_sample,
    tibble::tibble(idNotaTecnica = "absent-from-pre-floor")
  )
  stopifnot(
    setequal(unique(ledger$transition), c("retained", "entered", "exited", "excluded_both")),
    ledger$transition[ledger$idNotaTecnica == entered_id] == "entered",
    ledger$transition[ledger$idNotaTecnica == exited_id] == "exited",
    sum(ledger$previous_in_sample) == nrow(previous_sample),
    expect_error(build_origin_ledger(classified, missing_historical, sample)),
    expect_error(build_origin_ledger(classified, previous_sample[c(1L, 1L), ], sample))
  )

  cat("Synthetic origin checks passed: map, classification, aggregation, floor and reconciliation.\n")
}

check_real <- function() {
  sample <- read_analysis_sample()
  covariates <- read_analysis_covariates()
  ledger <- tibble::as_tibble(arrow::read_parquet(ANALYSIS_ORIGIN_LEDGER_PATH))
  verification <- read_diagnostic_table(ANALYSIS_VERIFICATION_PATH)
  production <- read_diagnostic_table(ANALYSIS_PRODUCTION_PATH)
  map <- read_diagnostic_table(file.path(ANALYSIS_TABLE_DIR, "origin-map.csv"))
  reconciliation <- read_diagnostic_table(file.path(ANALYSIS_TABLE_DIR, "sample-reconciliation.csv"))

  stopifnot(
    nrow(sample) == 273734L,
    n_distinct(sample$analysis_origin) == 25L,
    identical(sample$idNotaTecnica, covariates$idNotaTecnica),
    nrow(ledger) == 273960L,
    sum(ledger$transition == "entered") == 58L,
    sum(ledger$transition == "exited") == 0L,
    sum(ledger$transition == "excluded_both") == 226L,
    all(ledger$origin_received[ledger$transition == "entered"] == "PR/CHC-UFPR"),
    sum(sample$origin_received != sample$analysis_origin) == 1337L,
    sum(sample$analysis_origin == "PR") == 7519L,
    sum(sample$analysis_origin == "SP") == 17433L,
    nrow(production) == 32L,
    sum(production$inclusion == "in sample") == 25L,
    sum(production$notes_sample) == 273734L,
    all(verification$passed),
    all(vapply(names(map), function(name) identical(map[[name]], origin_decision_map()[[name]]), logical(1))),
    sum(reconciliation$notes[reconciliation$transition == "retained"]) == 273676L,
    sum(reconciliation$notes[reconciliation$transition == "entered"]) == 58L,
    sum(reconciliation$notes[reconciliation$transition == "exited"]) == 0L,
    sum(reconciliation$notes[reconciliation$transition == "excluded_both"]) == 226L
  )

  expected_diagnostics <- paste0(
    c(
      "biosimilar-reclassification",
      "clinical-indicators",
      "clinical-summary",
      "covariate-by-origin-month",
      "covariate-levels",
      "covariate-summary",
      "icd-codes",
      "icd-summary",
      "medicine-candidates",
      "medicine-lexical-screen",
      "medicine-spelling",
      "rarity-by-origin",
      "rarity-level-maps",
      "rarity-summary",
      "sample-flags"
    ),
    ".csv"
  )
  stopifnot(all(file.exists(file.path(ANALYSIS_TABLE_DIR, expected_diagnostics))))

  cat("Real origin checks passed: 273,734 notes, 25 analytical origins and complete diagnostics.\n")
}

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L || (length(arguments) == 1L && arguments != "--synthetic-only")) {
  stop("Usage: Rscript tools/check-origins.R [--synthetic-only]", call. = FALSE)
}

check_synthetic()
if (!identical(arguments, "--synthetic-only")) check_real()
