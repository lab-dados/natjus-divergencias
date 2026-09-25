# Montar a população final de análise depois de classificar os rótulos de origem ---------
#
# O montador da amostra expõe de propósito a população imediatamente antes do
# último filtro de tamanho por origem. Este script começa ali, preserva o
# rótulo recebido do coletor, deriva a origem analítica decidida para o
# artigo, e só então aplica o piso comum de 100 notas. Ele escreve produtos
# novos em `data/analysis-*` e `tables/analysis/`; a amostra histórica, as
# tabelas e os diagnósticos ficam intocados.
#
# Rodar a partir da raiz do repositório:
#
#     Rscript R/04-origins.R
#
# Dar `source()` neste arquivo define as funções e não escreve nada.

source("R/01-sample.R")
source("R/03-covariates.R")
source("R/assert-sample.R")

ANALYSIS_SAMPLE_PATH <- "data/analysis-sample.parquet"
ANALYSIS_COVARIATES_PATH <- "data/analysis-covariates.parquet"
ANALYSIS_ORIGIN_LEDGER_PATH <- "data/analysis-origin-ledger.parquet"
ANALYSIS_TABLE_DIR <- "tables/analysis"
ANALYSIS_ATTRITION_PATH <- file.path(ANALYSIS_TABLE_DIR, "attrition.csv")
ANALYSIS_PRODUCTION_PATH <- file.path(ANALYSIS_TABLE_DIR, "production-by-analysis-origin.csv")
ANALYSIS_VERIFICATION_PATH <- file.path(ANALYSIS_TABLE_DIR, "verification.csv")

# As contagens da população aprovada (verification_table()) valem para esta
# base, a publicada no Hugging Face (R/enatjus/read-base.R).
CURRENT_BASE_SHA256 <- BASE_SHA256
ORIGIN_IDENTIFICATION_VALUES <- c(
  "specific_retained",
  "state_unidentified",
  "state_aggregate",
  "not_adjudicated"
)

# A tabela é a única declaração de como os 13 rótulos recebidos viram
# unidades analíticas. O status descreve a classificação analítica, não prova
# documental de que uma instituição específica escreveu uma nota.
origin_decision_map <- function() {
  tibble::tribble(
    ~origin_received,       ~origin_family, ~analysis_origin,       ~origin_identification,
    "AM",                  "AM",           "AM",                  "state_unidentified",
    "AM/SES",              "AM",           "AM/SES",              "specific_retained",
    "AM/SEMSA",            "AM",           "AM/SEMSA",            "specific_retained",
    "PR",                  "PR",           "PR",                  "state_aggregate",
    "PR/CAMS",             "PR",           "PR",                  "state_aggregate",
    "PR/CHR",              "PR",           "PR",                  "state_aggregate",
    "PR/CHC-UFPR",         "PR",           "PR",                  "state_aggregate",
    "PR/UEL",              "PR",           "PR",                  "state_aggregate",
    "RS",                  "RS",           "RS",                  "state_unidentified",
    "RS/DMJ",              "RS",           "RS/DMJ",              "specific_retained",
    "TelessaúdeRS-UFRGS",  "RS",           "TelessaúdeRS-UFRGS",  "specific_retained",
    "SP",                  "SP",           "SP",                  "state_aggregate",
    "SP/HC",               "SP",           "SP",                  "state_aggregate"
  )
}

validate_origin_map <- function(map) {
  required <- c(
    "origin_received",
    "origin_family",
    "analysis_origin",
    "origin_identification"
  )
  if (!identical(names(map), required)) {
    stop("Origin map columns differ from the required schema.", call. = FALSE)
  }
  if (nrow(map) != 13L || anyDuplicated(map$origin_received)) {
    stop("Origin map must contain 13 unique received labels.", call. = FALSE)
  }
  if (anyNA(map) || any(!vapply(map, function(x) all(nzchar(x)), logical(1)))) {
    stop("Origin map contains a missing or empty value.", call. = FALSE)
  }
  if (!all(map$origin_identification %in% ORIGIN_IDENTIFICATION_VALUES)) {
    stop("Origin map contains an unsupported identification value.", call. = FALSE)
  }

  canonical <- origin_decision_map() |>
    arrange(origin_received)
  supplied <- map |>
    arrange(origin_received)
  if (!identical(supplied, canonical)) {
    stop("Origin map differs from the closed 13-label decision.", call. = FALSE)
  }
  invisible(map)
}

# A classificação muda só a origem usada analiticamente. Toda outra coluna de
# entrada continua equivalente byte a byte no R, enquanto `origin_received`
# mantém o rótulo do coletor disponível para auditoria e para análises
# alternativas posteriores.
classify_origins <- function(notes, map = origin_decision_map()) {
  validate_origin_map(map)
  required <- c("idNotaTecnica", "origem_tratada")
  if (!all(required %in% names(notes))) {
    stop("Input lacks the technical-note ID or treated origin.", call. = FALSE)
  }
  derived <- c(
    "origin_received",
    "origin_family",
    "analysis_origin",
    "origin_identification"
  )
  if (any(derived %in% names(notes))) {
    stop("Input already contains analytical-origin columns.", call. = FALSE)
  }
  if (anyNA(notes$idNotaTecnica) || anyDuplicated(notes$idNotaTecnica)) {
    stop("Technical-note IDs must be present and unique.", call. = FALSE)
  }

  original_origin <- notes$origem_tratada
  result <- notes |>
    mutate(origin_received = as.character(origem_tratada)) |>
    left_join(map, by = "origin_received", relationship = "many-to-one") |>
    mutate(
      origin_family = if_else(
        is.na(origin_family) & !is.na(origin_received),
        origin_received,
        origin_family
      ),
      analysis_origin = if_else(
        is.na(analysis_origin) & !is.na(origin_received),
        origin_received,
        analysis_origin
      ),
      origin_identification = if_else(
        is.na(origin_identification) & !is.na(origin_received),
        "not_adjudicated",
        origin_identification
      ),
      origem_tratada = analysis_origin
    )

  if (!identical(notes$idNotaTecnica, result$idNotaTecnica) || nrow(notes) != nrow(result)) {
    stop("Origin classification changed IDs, row order or row count.", call. = FALSE)
  }
  if (!identical(as.character(original_origin), result$origin_received)) {
    stop("Received origin labels were not preserved.", call. = FALSE)
  }
  inherited <- setdiff(names(notes), "origem_tratada")
  unchanged <- vapply(inherited, function(name) {
    identical(notes[[name]], result[[name]])
  }, logical(1))
  if (!all(unchanged)) {
    stop("Origin classification changed inherited columns.", call. = FALSE)
  }
  if (!identical(result$origem_tratada, result$analysis_origin)) {
    stop("The canonical origin does not match the analytical origin.", call. = FALSE)
  }
  invisible(result)
}

apply_analysis_origin_floor <- function(notes, floor = ORIGIN_FLOOR) {
  if (length(floor) != 1L || is.na(floor) || floor < 1L) {
    stop("Origin floor must be one positive integer.", call. = FALSE)
  }
  eligible <- notes |>
    count(analysis_origin) |>
    filter(!is.na(analysis_origin), n >= floor) |>
    pull(analysis_origin)
  keep(
    notes,
    analysis_origin %in% eligible,
    paste0("analytical origins with at least ", floor, " notes")
  )
}

build_origin_ledger <- function(before_floor, previous_sample, analysis_sample) {
  if (anyNA(previous_sample$idNotaTecnica) || anyDuplicated(previous_sample$idNotaTecnica)) {
    stop("Historical sample IDs must be present and unique.", call. = FALSE)
  }
  missing_historical_ids <- setdiff(
    previous_sample$idNotaTecnica,
    before_floor$idNotaTecnica
  )
  if (length(missing_historical_ids)) {
    stop(
      "The reconstructed pre-floor population is missing ",
      length(missing_historical_ids),
      " historical sample IDs.",
      call. = FALSE
    )
  }

  origin_sizes <- before_floor |>
    count(analysis_origin, name = "analysis_origin_notes_before_floor")
  result <- before_floor |>
    left_join(origin_sizes, by = "analysis_origin", relationship = "many-to-one") |>
    transmute(
      idNotaTecnica,
      origin_received,
      origin_family,
      analysis_origin,
      origin_identification,
      previous_in_sample = idNotaTecnica %in% previous_sample$idNotaTecnica,
      analysis_in_sample = idNotaTecnica %in% analysis_sample$idNotaTecnica,
      transition = case_when(
        previous_in_sample & analysis_in_sample ~ "retained",
        !previous_in_sample & analysis_in_sample ~ "entered",
        previous_in_sample & !analysis_in_sample ~ "exited",
        .default = "excluded_both"
      ),
      analysis_origin_notes_before_floor,
      analysis_origin_above_floor = analysis_origin_notes_before_floor >= ORIGIN_FLOOR,
      reason = if_else(
        analysis_origin_above_floor,
        "analysis origin meets floor",
        "analysis origin below floor"
      )
    )
  if (sum(result$transition %in% c("retained", "exited")) != nrow(previous_sample)) {
    stop("Origin ledger does not account for every historical sample ID.", call. = FALSE)
  }
  result
}

origin_flow_table <- function(before_floor, previous_sample, analysis_sample) {
  before_floor |>
    mutate(
      previous_in_sample = idNotaTecnica %in% previous_sample$idNotaTecnica,
      analysis_in_sample = idNotaTecnica %in% analysis_sample$idNotaTecnica
    ) |>
    group_by(
      origin_received,
      origin_family,
      analysis_origin,
      origin_identification
    ) |>
    summarise(
      notes_before_floor = n(),
      notes_previous_sample = sum(previous_in_sample),
      notes_analysis_sample = sum(analysis_in_sample),
      received_differs_from_analysis = first(origin_received != analysis_origin),
      .groups = "drop"
    ) |>
    arrange(origin_family, analysis_origin, origin_received)
}

sample_reconciliation_table <- function(origin_ledger) {
  origin_ledger |>
    count(transition, name = "notes") |>
    tidyr::complete(
      transition = c("retained", "entered", "exited", "excluded_both"),
      fill = list(notes = 0L)
    )
}

build_analysis_bundle <- function(base, previous_sample) {
  before_floor <- build_sample(base, apply_origin_floor = FALSE) |>
    classify_origins()
  if (anyNA(before_floor[c(
    "origin_received",
    "origin_family",
    "analysis_origin",
    "origin_identification"
  )])) {
    stop("The pre-floor population contains an unclassified origin.", call. = FALSE)
  }

  analysis_sample <- apply_analysis_origin_floor(before_floor)
  attrition <- attrition_table()
  classified_base <- classify_origins(base)
  production <- production_table(classified_base, analysis_sample)
  origin_ledger <- build_origin_ledger(before_floor, previous_sample, analysis_sample)
  covariates <- build_covariates(analysis_sample)
  covariate_tables <- covariate_diagnostics(analysis_sample, covariates)

  list(
    before_floor = before_floor,
    previous_sample_rows = nrow(previous_sample),
    sample = analysis_sample,
    attrition = attrition,
    production = production,
    origin_ledger = origin_ledger,
    origin_map = origin_decision_map(),
    origin_flow = origin_flow_table(before_floor, previous_sample, analysis_sample),
    sample_reconciliation = sample_reconciliation_table(origin_ledger),
    covariates = covariates,
    covariate_tables = covariate_tables
  )
}

verification_table <- function(bundle, base_sha256) {
  structural <- tibble::tribble(
    ~check, ~expected, ~observed,
    "map rows", 13L, nrow(bundle$origin_map),
    "pre-floor IDs are unique", nrow(bundle$before_floor), n_distinct(bundle$before_floor$idNotaTecnica),
    "sample IDs are unique", nrow(bundle$sample), n_distinct(bundle$sample$idNotaTecnica),
    "covariate IDs match sample", nrow(bundle$sample), sum(bundle$covariates$idNotaTecnica == bundle$sample$idNotaTecnica),
    "canonical origin matches analytical origin", nrow(bundle$sample), sum(bundle$sample$origem_tratada == bundle$sample$analysis_origin),
    "attrition ends at sample rows", nrow(bundle$sample), bundle$attrition$remaining[nrow(bundle$attrition)],
    "production notes sum to sample rows", nrow(bundle$sample), sum(bundle$production$notes_sample),
    "production origins match sample", n_distinct(bundle$sample$analysis_origin), sum(bundle$production$inclusion == "in sample"),
    "historical sample IDs accounted for", bundle$previous_sample_rows, sum(bundle$origin_ledger$previous_in_sample),
    "no IDs exit the historical sample", 0L, sum(bundle$origin_ledger$transition == "exited")
  )

  if (identical(base_sha256, CURRENT_BASE_SHA256)) {
    current_build <- tibble::tribble(
      ~check, ~expected, ~observed,
      "pre-floor rows", 273960L, nrow(bundle$before_floor),
      "analysis rows", 273734L, nrow(bundle$sample),
      "analysis origins", 25L, n_distinct(bundle$sample$analysis_origin),
      "removed by analytical floor", 226L, nrow(bundle$before_floor) - nrow(bundle$sample),
      "entered IDs", 58L, sum(bundle$origin_ledger$transition == "entered"),
      "entered PR/CHC-UFPR IDs", 58L, sum(bundle$origin_ledger$transition == "entered" & bundle$origin_ledger$origin_received == "PR/CHC-UFPR"),
      "received label differs from analytical origin", 1337L, sum(bundle$sample$origin_received != bundle$sample$analysis_origin),
      "PR rows", 7519L, sum(bundle$sample$analysis_origin == "PR"),
      "SP rows", 17433L, sum(bundle$sample$analysis_origin == "SP"),
      "production rows", 32L, nrow(bundle$production)
    )
    structural <- bind_rows(structural, current_build)
  }

  structural |>
    mutate(
      expected = as.character(expected),
      observed = as.character(observed),
      passed = expected == observed
    )
}

validate_analysis_bundle <- function(bundle, base_sha256) {
  verification <- verification_table(bundle, base_sha256)
  if (any(!verification$passed)) {
    failed <- verification |>
      filter(!passed) |>
      transmute(message = paste0(check, ": expected ", expected, ", observed ", observed)) |>
      pull(message)
    stop("Analysis-origin verification failed: ", paste(failed, collapse = "; "), call. = FALSE)
  }
  if (!identical(bundle$sample$idNotaTecnica, bundle$covariates$idNotaTecnica)) {
    stop("Sample and covariate IDs differ in value or order.", call. = FALSE)
  }
  invisible(verification)
}

historical_product_paths <- function() {
  paths <- c(
    "data/sample.parquet",
    "data/covariates.parquet",
    list.files("tables", pattern = "[.]csv$", full.names = TRUE, recursive = FALSE)
  )
  paths <- sort(paths)
  if (any(!file.exists(paths))) {
    stop("A historical product is missing before the analysis-origin run.", call. = FALSE)
  }
  paths
}

hash_products <- function(paths) {
  stats::setNames(
    vapply(paths, digest::digest, character(1), algo = "sha256", file = TRUE),
    paths
  )
}

read_provenance_value <- function(path, key) {
  prefix <- paste0("# ", key, ": ")
  values <- sub(prefix, "", grep(paste0("^", prefix), readLines(path), value = TRUE))
  if (length(values) != 1L) {
    stop("Provenance key not found exactly once: ", key, call. = FALSE)
  }
  values
}

required_analysis_checks <- function() {
  c(
    "map rows",
    "pre-floor IDs are unique",
    "sample IDs are unique",
    "covariate IDs match sample",
    "canonical origin matches analytical origin",
    "attrition ends at sample rows",
    "production notes sum to sample rows",
    "production origins match sample",
    "historical sample IDs accounted for",
    "no IDs exit the historical sample",
    "pre-floor rows",
    "analysis rows",
    "analysis origins",
    "removed by analytical floor",
    "entered IDs",
    "entered PR/CHC-UFPR IDs",
    "received label differs from analytical origin",
    "PR rows",
    "SP rows",
    "production rows",
    "historical products unchanged",
    "analysis sample sha256",
    "analysis covariates sha256",
    "analysis origin ledger sha256"
  )
}

read_analysis_verification <- function(path = ANALYSIS_VERIFICATION_PATH) {
  verification <- read_diagnostic_table(path)
  if (!identical(names(verification), c("check", "expected", "observed", "passed")) ||
      anyDuplicated(verification$check)) {
    stop("Analysis verification table has an invalid schema.", call. = FALSE)
  }
  missing_checks <- setdiff(required_analysis_checks(), verification$check)
  if (length(missing_checks)) {
    stop("Analysis verification table is incomplete.", call. = FALSE)
  }
  matches <- as.character(verification$expected) == as.character(verification$observed)
  if (any(!(verification$passed %in% TRUE)) || !identical(verification$passed, matches)) {
    stop("Analysis verification table contains a failed or inconsistent check.", call. = FALSE)
  }
  verification
}

assert_verified_hash <- function(path, verification, check) {
  expected <- verification$expected[verification$check == check]
  if (length(expected) != 1L || !identical(
    expected,
    digest::digest(path, algo = "sha256", file = TRUE)
  )) {
    stop("Published file differs from its verified SHA-256: ", path, call. = FALSE)
  }
  invisible(path)
}

read_analysis_sample <- function(
    path = ANALYSIS_SAMPLE_PATH,
    attrition_path = ANALYSIS_ATTRITION_PATH,
    production_path = ANALYSIS_PRODUCTION_PATH,
    verification_path = ANALYSIS_VERIFICATION_PATH) {
  required <- c(path, attrition_path, production_path, verification_path)
  if (any(!file.exists(required))) {
    stop("Final analysis-origin products are incomplete. Run `Rscript R/04-origins.R`.", call. = FALSE)
  }
  verification <- read_analysis_verification(verification_path)
  assert_verified_hash(path, verification, "analysis sample sha256")
  sample <- tibble::as_tibble(arrow::read_parquet(path))
  required_sample_columns <- c(
    "idNotaTecnica",
    "origem_tratada",
    "origin_received",
    "origin_family",
    "analysis_origin",
    "origin_identification"
  )
  if (!all(required_sample_columns %in% names(sample))) {
    stop("Final analysis sample lacks a required column.", call. = FALSE)
  }
  if (anyNA(sample$idNotaTecnica) || anyDuplicated(sample$idNotaTecnica)) {
    stop("Final analysis sample IDs must be present and unique.", call. = FALSE)
  }
  assert_sample(sample, attrition_path, production_path)
  if (anyNA(sample[c(
    "origin_received",
    "origin_family",
    "analysis_origin",
    "origin_identification"
  )])) {
    stop("Final analysis sample contains an unclassified origin.", call. = FALSE)
  }
  if (!identical(sample$origem_tratada, sample$analysis_origin)) {
    stop("Canonical and analytical origins differ in the final sample.", call. = FALSE)
  }

  production <- readr::read_csv(production_path, show_col_types = FALSE)
  required_production <- c("origin", "notes_sample", "inclusion")
  if (!all(required_production %in% names(production))) {
    stop("Analysis production table has an invalid schema.", call. = FALSE)
  }
  sample_counts <- sample |>
    count(origin = origem_tratada, name = "notes_sample") |>
    arrange(origin)
  production_counts <- production |>
    filter(inclusion == "in sample") |>
    select(origin, notes_sample) |>
    arrange(origin)
  if (!identical(sample_counts$origin, production_counts$origin) ||
      !identical(as.integer(sample_counts$notes_sample), as.integer(production_counts$notes_sample))) {
    stop("Per-origin sample counts differ from the production table.", call. = FALSE)
  }
  sample
}

read_analysis_covariates <- function(
    path = ANALYSIS_COVARIATES_PATH,
    sample_path = ANALYSIS_SAMPLE_PATH,
    verification_path = ANALYSIS_VERIFICATION_PATH) {
  if (!file.exists(path)) {
    stop("Final analysis covariates are missing. Run `Rscript R/04-origins.R`.", call. = FALSE)
  }
  verification <- read_analysis_verification(verification_path)
  assert_verified_hash(path, verification, "analysis covariates sha256")
  sample <- read_analysis_sample(path = sample_path, verification_path = verification_path)
  covariates <- tibble::as_tibble(arrow::read_parquet(path))
  assert_covariates(sample, covariates)
}

write_stage <- function(bundle, stamp, verification, stage_dir) {
  data_dir <- file.path(stage_dir, "data")
  table_dir <- file.path(stage_dir, ANALYSIS_TABLE_DIR)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

  sample_path <- file.path(data_dir, basename(ANALYSIS_SAMPLE_PATH))
  covariates_path <- file.path(data_dir, basename(ANALYSIS_COVARIATES_PATH))
  ledger_path <- file.path(data_dir, basename(ANALYSIS_ORIGIN_LEDGER_PATH))
  arrow::write_parquet(bundle$sample, sample_path)
  arrow::write_parquet(bundle$covariates, covariates_path)
  arrow::write_parquet(bundle$origin_ledger, ledger_path)

  tables <- c(
    list(
      attrition = bundle$attrition,
      origin_map = bundle$origin_map,
      origin_flow = bundle$origin_flow,
      sample_reconciliation = bundle$sample_reconciliation
    ),
    bundle$covariate_tables
  )
  write_diagnostic_tables(tables, table_dir, stamp, "final_analysis_sample", nrow(bundle$sample))
  # `assert_sample()` lê a produção como CSV comum, diferente do livro-razão
  # de atrição e dos diagnósticos, cujos leitores pulam explicitamente os
  # comentários de proveniência.
  readr::write_csv(
    bundle$production,
    file.path(table_dir, "production-by-analysis-origin.csv"),
    na = ""
  )

  staged_sample <- tibble::as_tibble(arrow::read_parquet(sample_path))
  staged_covariates <- tibble::as_tibble(arrow::read_parquet(covariates_path))
  staged_ledger <- tibble::as_tibble(arrow::read_parquet(ledger_path))
  assert_sample(
    staged_sample,
    file.path(table_dir, "attrition.csv"),
    file.path(table_dir, "production-by-analysis-origin.csv")
  )
  assert_covariates(staged_sample, staged_covariates)
  if (nrow(staged_ledger) != nrow(bundle$before_floor)) {
    stop("Staged origin ledger does not cover the pre-floor population.", call. = FALSE)
  }

  parquet_hashes <- tibble::tribble(
    ~check, ~expected, ~observed, ~passed,
    "analysis sample sha256", digest::digest(sample_path, algo = "sha256", file = TRUE), digest::digest(sample_path, algo = "sha256", file = TRUE), TRUE,
    "analysis covariates sha256", digest::digest(covariates_path, algo = "sha256", file = TRUE), digest::digest(covariates_path, algo = "sha256", file = TRUE), TRUE,
    "analysis origin ledger sha256", digest::digest(ledger_path, algo = "sha256", file = TRUE), digest::digest(ledger_path, algo = "sha256", file = TRUE), TRUE
  )
  write_diagnostic_tables(
    list(verification = bind_rows(verification, parquet_hashes)),
    table_dir,
    stamp,
    "final_analysis_sample",
    nrow(bundle$sample)
  )
  invisible(stage_dir)
}

publish_file <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  dir.create("data", showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(destination), "-"), tmpdir = "data")
  if (!file.copy(source, temporary, overwrite = TRUE)) {
    stop("Could not stage final file: ", destination, call. = FALSE)
  }
  if (file.exists(destination) && unlink(destination) != 0L) {
    stop("Could not replace existing file: ", destination, call. = FALSE)
  }
  if (!file.rename(temporary, destination)) {
    stop("Could not publish final file: ", destination, call. = FALSE)
  }
  invisible(destination)
}

publish_stage <- function(stage_dir) {
  staged_data <- file.path(stage_dir, "data")
  staged_tables <- file.path(stage_dir, ANALYSIS_TABLE_DIR)
  data_files <- list.files(staged_data, full.names = TRUE)
  table_files <- list.files(staged_tables, full.names = TRUE)

  # O arquivo de verificação é a marca de conclusão. Remover o anterior
  # primeiro impede que leitores aceitem um conjunto misto de antigo e novo
  # depois de uma publicação interrompida; a marca nova é sempre publicada
  # por último.
  if (file.exists(ANALYSIS_VERIFICATION_PATH)) unlink(ANALYSIS_VERIFICATION_PATH)
  for (path in data_files) {
    publish_file(path, file.path("data", basename(path)))
  }
  for (path in table_files[basename(table_files) != "verification.csv"]) {
    publish_file(path, file.path(ANALYSIS_TABLE_DIR, basename(path)))
  }
  publish_file(
    file.path(staged_tables, "verification.csv"),
    ANALYSIS_VERIFICATION_PATH
  )
  invisible(TRUE)
}

run_origins <- function() {
  historical_paths <- historical_product_paths()
  historical_hashes <- hash_products(historical_paths)

  base_path <- analysis_base_path()
  base <- read_analysis_base(base_path)
  stamp <- base_stamp(base_path, nrow(base))
  historical_base_sha256 <- read_provenance_value("tables/attrition.csv", "base_sha256")
  if (!identical(unname(stamp["base_sha256"]), historical_base_sha256)) {
    stop("The current base differs from the historical sample provenance.", call. = FALSE)
  }

  previous_sample <- read_sample()
  bundle <- build_analysis_bundle(base, previous_sample)
  verification <- validate_analysis_bundle(bundle, unname(stamp["base_sha256"]))
  if (!identical(historical_hashes, hash_products(historical_paths))) {
    stop("A historical product changed while the analysis bundle was built.", call. = FALSE)
  }

  names(stamp)[names(stamp) == "sample_built_at"] <- "analysis_built_at"
  stamp <- c(
    stamp,
    historical_sample_sha256 = digest::digest("data/sample.parquet", algo = "sha256", file = TRUE),
    origin_map_sha256 = digest::digest(
      readr::format_csv(bundle$origin_map),
      algo = "sha256",
      serialize = FALSE
    )
  )
  verification <- bind_rows(
    verification,
    tibble(
      check = "historical products unchanged",
      expected = "1",
      observed = "1",
      passed = TRUE
    )
  )

  stage_dir <- tempfile(".analysis-stage-", tmpdir = "data")
  dir.create(stage_dir)
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)
  write_stage(bundle, stamp, verification, stage_dir)
  if (!identical(historical_hashes, hash_products(historical_paths))) {
    stop("A historical product changed while new files were staged.", call. = FALSE)
  }
  publish_stage(stage_dir)
  if (!identical(historical_hashes, hash_products(historical_paths))) {
    stop("A historical product changed while new files were published.", call. = FALSE)
  }

  read_analysis_sample()
  read_analysis_covariates()
  print(verification, n = Inf)
  message(
    "Final analysis sample: ", format(nrow(bundle$sample), big.mark = ","),
    " notes in ", n_distinct(bundle$sample$analysis_origin), " analytical origins."
  )
  invisible(bundle)
}

if (sys.nframe() == 0L) run_origins()
