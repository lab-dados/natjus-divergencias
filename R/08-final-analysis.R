# Análise descritiva e de modelos definitiva ----------------------------------
#
# Este é o único driver dos resultados descritivos finais, dos dois modelos de
# origem aprovados, das métricas candidatas de divergência e do intervalo de
# perfil do desvio-padrão de NatJus. Dar source no arquivo define funções e não
# escreve nada. Uma execução direta faz duas coisas, nesta ordem.
# `run_final_stages()` ajusta as etapas (descritiva, modelos principais,
# candidatos de razão de verossimilhança e perfil) e grava cada uma como
# checkpoint; um checkpoint compatível com as entradas e a especificação atuais
# é reaproveitado em vez de reajustado. `run_final_analysis()` lê esses
# checkpoints, valida cada insumo e publica um pacote de saída carimbado e
# internamente reconciliado, sem ajustar modelo nenhum.

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(glmmTMB)
})

source("R/enatjus.R")
source("R/model-contract.R")

# Commit do pacote enatjus de que vêm as funções de R/enatjus/; vai para os
# carimbos dos CSVs e para a impressão digital dos checkpoints.
EXPECTED_PACKAGE_SHA <- ENATJUS_COMMIT
# Impressão digital da implementação de cada etapa, que entra na chave do
# checkpoint. É um valor fixo, e não o hash deste arquivo, para que mudar uma
# tabela ou um comentário não refaça ajustes de meia hora; em troca, quem muda
# o que uma etapa calcula tem de trocar o valor dela, senão o checkpoint antigo
# é reaproveitado em silêncio. Os três mudaram em 24/09/2026, quando os ajustes
# do glmmTMB passaram a terminar com passos de Newton
# (nlminb_with_newton_polish), o que muda o ajuste, o perfil que parte dele e
# as tabelas lidas dele. Cada valor é o SHA-256 do rótulo ao lado.
REUSABLE_STAGE_IMPLEMENTATION_SHA <- "28294fbe16e0bfafb222a90859ba1326d012271625d501a9814ae5ee1bb52eba"  # stages: glmmTMB with Newton polish, 2026-09-24
REUSABLE_PROFILE_IMPLEMENTATION_SHA <- "b449855d6092088343156735d4d6e88b3e27b29700eabe159a06466bf1242a48"  # profile: from the Newton-polished fit, 2026-09-24
REUSABLE_LIGHT_REPORTING_IMPLEMENTATION_SHA <- "410e87a5f0b35c650c36a6fd2d687fe52b74faacc83fa9ec5755637362e7f567"  # reporting: from the Newton-polished fits, 2026-09-24
REFERENCE_ORIGIN <- "Nacional"
PAIR_CELL_FLOORS <- c(5L, 10L, 20L, 50L)
PAIR_RANGE_THRESHOLDS <- seq(0, 1, by = 0.1)
# Nível de todos os intervalos: Wilson, Wald e perfil de verossimilhança. O
# valor entra na impressão digital do checkpoint do perfil, então mudá-lo força
# o perfil a ser recalculado.
INTERVAL_LEVEL <- 0.95
TMB_THREADS <- 1L
PROFILE_WORKERS <- 4L

MODEL_FIT_DIR <- "data/model-fits"
FINAL_TABLE_DIR <- "tables/analysis"
FINAL_LOG_DIR <- "logs"
OUTPUT_PREFIX <- "final-analysis-"

sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

formula_digest <- function(formula) {
  digest::digest(
    paste(deparse(formula), collapse = "\n"),
    algo = "sha256",
    serialize = FALSE
  )
}

checkpoint_fingerprint <- function(metadata) {
  digest::digest(metadata, algo = "sha256", serialize = TRUE)
}

move_to_dated_backup <- function(path, category = "checkpoints") {
  if (!file.exists(path)) return(NA_character_)
  destination <- file.path(
    "backups",
    paste0(category, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S")),
    path
  )
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(path, destination)) {
    stop("Could not move existing file to dated backup: ", path, call. = FALSE)
  }
  destination
}

checkpoint_compute <- function(namespace, key, metadata, compute) {
  if (!is.function(compute)) stop("Checkpoint computation must be a function.", call. = FALSE)
  directory <- file.path(MODEL_FIT_DIR, "checkpoints", namespace)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, paste0(key, ".rds"))
  sidecar <- paste0(path, ".meta.rds")
  fingerprint <- checkpoint_fingerprint(metadata)
  if (file.exists(path)) {
    if (!file.exists(sidecar)) {
      move_to_dated_backup(path, "legacy-checkpoint-without-sidecar")
    } else {
      saved_header <- tryCatch(readRDS(sidecar), error = function(error) NULL)
      compatible <- is.list(saved_header) &&
        identical(saved_header$fingerprint, fingerprint)
      if (compatible) {
        saved <- readRDS(path)
        if (!is.list(saved) || !all(c("header", "payload") %in% names(saved)) ||
            !identical(saved$header$fingerprint, fingerprint)) {
          stop("Checkpoint payload does not match its compatible sidecar: ", path, call. = FALSE)
        }
        log_line("reusing checkpoint ", path)
        return(saved$payload)
      }
      move_to_dated_backup(path, "incompatible-checkpoint")
      move_to_dated_backup(sidecar, "incompatible-checkpoint")
    }
  } else if (file.exists(sidecar)) {
    move_to_dated_backup(sidecar, "orphan-checkpoint-sidecar")
  }
  payload <- compute()
  header <- list(
    fingerprint = fingerprint,
    metadata = metadata,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  staged <- paste0(path, ".stage-", Sys.getpid(), "-", format(Sys.time(), "%H%M%OS6"))
  staged_sidecar <- paste0(sidecar, ".stage-", Sys.getpid(), "-", format(Sys.time(), "%H%M%OS6"))
  saveRDS(list(header = header, payload = payload), staged)
  saveRDS(header, staged_sidecar)
  if (!file.rename(staged, path)) {
    stop("Could not publish checkpoint payload: ", path, call. = FALSE)
  }
  if (!file.rename(staged_sidecar, sidecar)) {
    stop("Could not publish checkpoint sidecar: ", sidecar, call. = FALSE)
  }
  payload
}

checkpoint_read <- function(namespace, key, metadata) {
  path <- file.path(MODEL_FIT_DIR, "checkpoints", namespace, paste0(key, ".rds"))
  sidecar <- paste0(path, ".meta.rds")
  if (!file.exists(path) || !file.exists(sidecar)) {
    stop("Required checkpoint or sidecar is missing: ", path, call. = FALSE)
  }
  expected_fingerprint <- checkpoint_fingerprint(metadata)
  saved_header <- readRDS(sidecar)
  if (!is.list(saved_header) ||
      !identical(saved_header$fingerprint, expected_fingerprint)) {
    stop("Required checkpoint is incompatible and was left in place: ", path, call. = FALSE)
  }
  saved <- readRDS(path)
  if (!is.list(saved) || !all(c("header", "payload") %in% names(saved)) ||
      !identical(saved$header$fingerprint, expected_fingerprint)) {
    stop("Checkpoint payload does not match its compatible sidecar: ", path, call. = FALSE)
  }
  log_line("reusing checkpoint ", path)
  saved$payload
}

log_line <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", ..., "\n", sep = "")
}

require_final_packages <- function() {
  packages <- c(
    "arrow", "digest", "dplyr", "fixest", "ggplot2", "glmmTMB",
    "jsonlite", "lubridate", "readr", "tibble", "tidyr"
  )
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(packages)
}

input_provenance <- function() {
  verification <- read_analysis_verification()
  expected_rows <- verification$expected[verification$check == "analysis rows"]
  expected_origins <- verification$expected[verification$check == "analysis origins"]
  if (!identical(expected_rows, "273734") || !identical(expected_origins, "25")) {
    stop("The verified final population is not the approved 273,734-note, 25-origin input.", call. = FALSE)
  }
  tibble::tibble(
    input = c(
      ANALYSIS_SAMPLE_PATH,
      ANALYSIS_COVARIATES_PATH,
      ANALYSIS_ATTRITION_PATH,
      ANALYSIS_PRODUCTION_PATH,
      ANALYSIS_VERIFICATION_PATH
    ),
    sha256 = vapply(
      c(
        ANALYSIS_SAMPLE_PATH,
        ANALYSIS_COVARIATES_PATH,
        ANALYSIS_ATTRITION_PATH,
        ANALYSIS_PRODUCTION_PATH,
        ANALYSIS_VERIFICATION_PATH
      ),
      sha256_file,
      character(1)
    )
  )
}

read_final_inputs <- function() {
  require_final_packages()
  sample <- read_analysis_sample()
  covariates <- read_analysis_covariates()
  if (!identical(sample$idNotaTecnica, covariates$idNotaTecnica)) {
    stop("Final sample and covariate IDs differ in value or order.", call. = FALSE)
  }
  if (nrow(covariates) != 273734L || dplyr::n_distinct(covariates$analysis_origin) != 25L) {
    stop("Final inputs differ from the approved population dimensions.", call. = FALSE)
  }
  production <- readr::read_csv(ANALYSIS_PRODUCTION_PATH, show_col_types = FALSE)
  attrition <- read_diagnostic_table(ANALYSIS_ATTRITION_PATH)
  verification <- read_analysis_verification()
  rm(sample)
  invisible(gc())
  list(
    covariates = covariates,
    production = production,
    attrition = attrition,
    verification = verification,
    provenance = input_provenance()
  )
}

clinical_columns <- function(contract = model_contract()) {
  vapply(contract$factors[CLINICAL_FIELDS], `[[`, character(1), "column")
}

low_dimension_columns <- function(contract = model_contract()) {
  vapply(contract$factors, `[[`, character(1), "column")
}

prepare_final_frame <- function(covariates, contract = model_contract()) {
  prepared <- prepare_model_contract(covariates, contract)$data
  origins <- sort(unique(as.character(prepared$analysis_origin)), method = "radix")
  if (!REFERENCE_ORIGIN %in% origins) {
    stop("The approved reference origin is absent from the final population.", call. = FALSE)
  }
  origin_levels <- c(REFERENCE_ORIGIN, setdiff(origins, REFERENCE_ORIGIN))
  prepared$origin <- factor(as.character(prepared$analysis_origin), levels = origin_levels)
  prepared$medicine <- factor(as.character(prepared$medicine))
  prepared$cid <- factor(as.character(prepared$icd_category))
  prepared$id_nota <- as.character(prepared$idNotaTecnica)
  prepared$internal_order <- sprintf("%09d", seq_len(nrow(prepared)))
  if (anyNA(prepared[c("origin", "medicine", "cid", "id_nota", "internal_order")])) {
    stop("The final model frame contains a missing model key.", call. = FALSE)
  }
  if (anyDuplicated(prepared$id_nota) || anyDuplicated(prepared$internal_order)) {
    stop("The final model frame does not have unique note IDs and order keys.", call. = FALSE)
  }
  if (!identical(prepared$idNotaTecnica, covariates$idNotaTecnica)) {
    stop("Model preparation changed note order.", call. = FALSE)
  }
  required <- unique(c(
    "idNotaTecnica", "id_nota", "internal_order", "y",
    "analysis_origin", "origin_family", "origin",
    "medicine", "cid", "icd_category", "medicine_icd",
    "month_of_emission", contract$time$output_column,
    low_dimension_columns(contract)
  ))
  missing <- setdiff(required, names(prepared))
  if (length(missing)) {
    stop("Compact model frame lacks required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  prepared[, required, drop = FALSE]
}

make_specification <- function(name, engine, formula, fixed_signature, vcov = NULL) {
  list(
    name = name,
    engine = engine,
    formula = formula,
    formula_digest = formula_digest(formula),
    vcov = vcov,
    fixed_signature = fixed_signature,
    temporal_form = "linear_calendar_month",
    process_dependence = FALSE,
    response = "y",
    origin = "origin",
    conitec = model_contract()$factors$conitec$column
  )
}

final_estimator_specifications <- function(contract = model_contract()) {
  fixed_terms <- c(low_dimension_columns(contract), contract$time$output_column)
  fixed_signature <- paste(sort(fixed_terms), collapse = " + ")
  fixed_text <- paste(fixed_terms, collapse = " + ")
  formula_11 <- stats::as.formula(paste0(
    "y ~ i(origin, ref = \"", REFERENCE_ORIGIN, "\") + ",
    fixed_text,
    " | medicine + cid"
  ))
  formula_12 <- stats::as.formula(paste0(
    "y ~ ", fixed_text,
    " + (1 | origin) + (1 | medicine) + (1 | cid)"
  ))
  list(
    model_1_1 = make_specification(
      "model_1_1", "fixest", formula_11, fixed_signature,
      stats::as.formula("~ medicine + cid")
    ),
    model_1_2 = make_specification(
      "model_1_2", "glmmTMB", formula_12, fixed_signature
    )
  )
}

validate_final_specifications <- function(specifications = final_estimator_specifications()) {
  if (!identical(names(specifications), c("model_1_1", "model_1_2"))) {
    stop("Final specifications must contain only the two approved origin models.", call. = FALSE)
  }
  if (!identical(
    specifications$model_1_1$fixed_signature,
    specifications$model_1_2$fixed_signature
  )) {
    stop("The two approved models do not share the same fixed part.", call. = FALSE)
  }
  required_terms <- c(low_dimension_columns(), model_contract()$time$output_column)
  formula_text <- vapply(
    specifications,
    function(x) paste(deparse(x$formula), collapse = " "),
    character(1)
  )
  if (any(!vapply(required_terms, function(term) all(grepl(term, formula_text, fixed = TRUE)), logical(1)))) {
    stop("A required low-dimensional or time term is absent from a final model.", call. = FALSE)
  }
  if (!grepl("ref = \"Nacional\"", formula_text[["model_1_1"]], fixed = TRUE)) {
    stop("The fixed-effects model does not use Nacional as its explicit origin reference.", call. = FALSE)
  }
  if (!all(grepl("medicine", formula_text, fixed = TRUE)) ||
      !all(grepl("cid", formula_text, fixed = TRUE))) {
    stop("Medicine and CID are absent from an approved group structure.", call. = FALSE)
  }
  invisible(specifications)
}

# O nlminb, com o controle padrão do glmmTMB, para longe do ótimo nestes
# modelos: às vezes pelo limite de avaliações, às vezes por convergência
# relativa com gradiente entre 0,03 e 0,6. Nesse ponto, a diferença de
# arredondamento que o TMB produz de um processo para outro, na 15.ª casa da
# função objetivo, vira até 1e-4 nas estimativas, o bastante para mudar o
# último dígito exibido de um intervalo. Passos de Newton a partir do ponto do
# nlminb levam o gradiente abaixo de NEWTON_GRADIENT_TOLERANCE, e duas rodadas
# em processos separados passam a concordar em cerca de 1e-12. A Hessiana vem
# de diferenças finitas do gradiente, porque a do TMB não existe para o
# objetivo com os efeitos aleatórios integrados.
NEWTON_GRADIENT_TOLERANCE <- 1e-8
NEWTON_MAX_STEPS <- 10L
# Limite da guarda que interrompe a rodada (assert_polished_fit). É mais folgado
# que o do polimento porque o polimento para assim que o gradiente do
# otimizador cai abaixo de NEWTON_GRADIENT_TOLERANCE, então o gradiente final
# pode ficar em qualquer ponto abaixo dele, e o do sdreport, medido no ponto
# que o TMB reporta, pode passar um pouco. Um polimento que não chega a
# NEWTON_GRADIENT_TOLERANCE já é barrado pelo status (o otimizador devolve
# falha); este limite pega o ponto reportado longe do polido. Nos modelos
# reais, um ajuste sem polimento para com gradiente entre 0,03 e 0,6.
POLISH_GUARD_TOLERANCE <- 1e-6

nlminb_with_newton_polish <- function(start, objective, gradient, control = list(), ...) {
  fit <- stats::nlminb(start, objective, gradient, control = control, ...)
  par <- fit$par
  steps <- 0L
  while (max(abs(gradient(par))) >= NEWTON_GRADIENT_TOLERANCE && steps < NEWTON_MAX_STEPS) {
    par <- par - solve(stats::optimHess(par, objective, gradient), gradient(par))
    steps <- steps + 1L
  }
  polished <- max(abs(gradient(par))) < NEWTON_GRADIENT_TOLERANCE
  fit$par <- par
  fit$objective <- objective(par)
  # has_converged() lê o código do nlminb. Ele passa a dizer se o ponto final
  # atingiu a tolerância do gradiente, e a mensagem guarda o que o nlminb disse.
  fit$convergence <- if (polished) 0L else 1L
  fit$message <- sprintf(
    "%s; %d Newton step(s), gradient %s tolerance %g",
    fit$message, steps, if (polished) "below" else "still above", NEWTON_GRADIENT_TOLERANCE
  )
  fit
}

# O gradiente conferido é o do sdreport, calculado no ponto que o glmmTMB
# reporta (last.par.best do TMB), e não o do otimizador: o TMB só troca esse
# ponto quando o objetivo cai, e os últimos passos de Newton mexem no objetivo
# abaixo do ruído de ponto flutuante. Um ajuste que termine fora da tolerância
# interrompe a rodada, porque as estimativas voltariam a variar entre
# processos. Vale também para os modelos restritos da razão de verossimilhança,
# que passam por fit_one_estimator e não vão para a tabela de diagnósticos; o
# log guarda a linha de cada um.
assert_polished_fit <- function(name, diagnostics) {
  gradient <- diagnostics$max_abs_gradient
  log_line("fit ", name, ": ", diagnostics$convergence_status,
           ", max |gradient| ", format(gradient, digits = 3))
  if (diagnostics$convergence_status %in% c("optimization_failure", "non_positive_definite_hessian") ||
      !isTRUE(gradient < POLISH_GUARD_TOLERANCE)) {
    stop(
      "Fit of ", name, " ended with status ", diagnostics$convergence_status,
      " and max |gradient| ", format(gradient, digits = 3),
      "; the guard tolerance is ", POLISH_GUARD_TOLERANCE, call. = FALSE
    )
  }
  invisible(TRUE)
}

fit_one_estimator <- function(specification, data, tmb_threads = TMB_THREADS) {
  fitter <- function(formula, data, specification) {
    if (specification$engine == "fixest") {
      return(suppressMessages(fixest::feglm(
        fml = formula,
        data = data,
        family = "binomial",
        vcov = specification$vcov,
        notes = FALSE
      )))
    }
    glmmTMB::glmmTMB(
      formula = formula,
      data = data,
      family = stats::binomial(),
      control = glmmTMB::glmmTMBControl(
        parallel = as.integer(tmb_threads),
        optimizer = nlminb_with_newton_polish
      )
    )
  }
  start <- Sys.time()
  fitted <- withCallingHandlers(
    cnj_audit_fit_estimator(
      specification,
      data,
      fitter = fitter,
      tmb_threads = tmb_threads
    ),
    warning = function(condition) {
      if (grepl("NaNs", conditionMessage(condition), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (is.null(fitted$model)) {
    stop(
      "Fit failed for ", specification$name, ": ", fitted$error_message,
      call. = FALSE
    )
  }
  if (specification$engine == "glmmTMB") {
    assert_polished_fit(specification$name, fitted$diagnostics)
  }
  list(
    specification = specification,
    result = fitted,
    model = fitted$model,
    elapsed_seconds = elapsed
  )
}

estimated_row_indices <- function(model, input_rows) {
  if (inherits(model, "fixest")) {
    index <- as.integer(fixest::obs(model))
  } else {
    index <- seq_len(input_rows)
  }
  if (!length(index) || anyNA(index) || any(index < 1L | index > input_rows) || anyDuplicated(index)) {
    stop("The fitted model returned an invalid estimated-row index.", call. = FALSE)
  }
  index
}

flatten_diagnostics <- function(fit_bundle) {
  diagnostics <- fit_bundle$result$diagnostics
  tibble::tibble(
    model = fit_bundle$specification$name,
    engine = fit_bundle$specification$engine,
    convergence_status = diagnostics$convergence_status,
    identifiability = diagnostics$identifiability,
    separation_detected = diagnostics$separation_detected,
    singular = diagnostics$singular,
    boundary_correlation = diagnostics$boundary_correlation,
    max_abs_gradient = diagnostics$max_abs_gradient,
    convergence_code = diagnostics$convergence_code,
    pd_hessian = diagnostics$pd_hessian,
    warning_classes = paste(fit_bundle$result$warning_classes, collapse = ";"),
    elapsed_seconds = fit_bundle$elapsed_seconds
  )
}

fit_population_records <- function(fit_bundle, frame) {
  index <- estimated_row_indices(fit_bundle$model, nrow(frame))
  estimated <- rep(FALSE, nrow(frame))
  estimated[index] <- TRUE
  by_origin <- tibble::tibble(
    origin = as.character(frame$origin),
    estimated = estimated
  ) |>
    group_by(origin) |>
    summarise(
      input_notes = n(),
      estimated_notes = sum(estimated),
      discarded_notes = sum(!estimated),
      coverage = mean(estimated),
      .groups = "drop"
    ) |>
    mutate(model = fit_bundle$specification$name, .before = 1L)
  list(
    summary = tibble::tibble(
      model = fit_bundle$specification$name,
      input_notes = nrow(frame),
      estimated_notes = length(index),
      discarded_notes = nrow(frame) - length(index),
      coverage = length(index) / nrow(frame),
      input_origins = n_distinct(frame$origin),
      estimated_origins = n_distinct(frame$origin[index]),
      input_medicines = n_distinct(frame$medicine),
      estimated_medicines = n_distinct(frame$medicine[index]),
      input_cids = n_distinct(frame$cid),
      estimated_cids = n_distinct(frame$cid[index])
    ),
    by_origin = by_origin,
    input_ids = frame$id_nota,
    estimated_ids = frame$id_nota[index],
    discarded_ids = frame$id_nota[!estimated],
    estimated_index = index
  )
}

fixest_exclusion_records <- function(fit_bundle, frame) {
  if (!inherits(fit_bundle$model, "fixest")) {
    return(list(
      levels = tibble::tibble(
        model = character(), variable = character(), level = character(), notes = integer()
      ),
      coefficients = tibble::tibble(
        model = character(), coefficient = character(), reason = character()
      )
    ))
  }
  removed <- fit_bundle$model$fixef_removed
  level_rows <- list()
  if (length(removed)) {
    for (variable in intersect(names(removed), c("medicine", "cid"))) {
      values <- as.character(frame[[variable]])
      for (level in as.character(removed[[variable]])) {
        level_rows[[length(level_rows) + 1L]] <- tibble::tibble(
          model = fit_bundle$specification$name,
          variable = variable,
          level = level,
          notes = sum(values == level)
        )
      }
    }
  }
  collinear <- fit_bundle$model$collin.var
  list(
    levels = if (length(level_rows)) bind_rows(level_rows) else tibble::tibble(
      model = character(), variable = character(), level = character(), notes = integer()
    ),
    coefficients = tibble::tibble(
      model = fit_bundle$specification$name,
      coefficient = as.character(collinear),
      reason = "removed_by_estimator_collinearity"
    )
  )
}

model_coefficient_table <- function(fit_bundle) {
  model <- fit_bundle$model
  estimates <- if (inherits(model, "glmmTMB")) {
    glmmTMB::fixef(model)$cond
  } else {
    stats::coef(model)
  }
  covariance <- if (inherits(model, "glmmTMB")) {
    as.matrix(stats::vcov(model)$cond)
  } else {
    as.matrix(stats::vcov(model))
  }
  standard_error <- sqrt(pmax(diag(covariance)[names(estimates)], 0))
  z <- stats::qnorm(1 - (1 - INTERVAL_LEVEL) / 2)
  tibble::tibble(
    model = fit_bundle$specification$name,
    term = names(estimates),
    estimate_logit = as.double(estimates),
    standard_error = as.double(standard_error),
    conf_low_logit = estimate_logit - z * standard_error,
    conf_high_logit = estimate_logit + z * standard_error,
    odds_ratio = exp(estimate_logit),
    odds_ratio_low = exp(conf_low_logit),
    odds_ratio_high = exp(conf_high_logit),
    interval_method = "wald",
    candidate = TRUE,
    default = FALSE
  )
}

random_effect_table <- function(fit_bundle) {
  if (!inherits(fit_bundle$model, "glmmTMB")) {
    return(tibble::tibble(
      model = character(), group = character(), standard_deviation = numeric(), variance = numeric()
    ))
  }
  blocks <- glmmTMB::VarCorr(fit_bundle$model)$cond
  bind_rows(lapply(names(blocks), function(group) {
    block <- blocks[[group]]
    sd <- attr(block, "stddev")
    if (is.null(sd)) sd <- sqrt(pmax(diag(as.matrix(block)), 0))
    tibble::tibble(
      model = fit_bundle$specification$name,
      group = group,
      standard_deviation = as.double(sd[[1L]]),
      variance = as.double(sd[[1L]]^2)
    )
  }))
}

fit_scalar_summary <- function(fit_bundle) {
  fitted_model <- fit_bundle$model
  ll <- stats::logLik(fitted_model)
  tibble::tibble(
    model = fit_bundle$specification$name,
    engine = fit_bundle$specification$engine,
    nobs = stats::nobs(fitted_model),
    log_likelihood = as.double(ll),
    parameters = as.integer(attr(ll, "df")),
    aic = stats::AIC(fitted_model),
    bic = stats::BIC(fitted_model),
    family = "binomial",
    link = "logit",
    event = "favourable technical-note conclusion",
    formula = paste(deparse(fit_bundle$specification$formula), collapse = " "),
    formula_sha256 = fit_bundle$specification$formula_digest,
    fixed_signature = fit_bundle$specification$fixed_signature
  )
}

wilson_from_counts <- function(favourable, total, level = INTERVAL_LEVEL) {
  result <- cnj_audit_wilson(favourable, total, level = level)
  names(result) <- c("rate", "wilson_low", "wilson_high")
  result
}

production_descriptives <- function(frame) {
  counts <- frame |>
    group_by(origin = as.character(origin)) |>
    summarise(
      notes = n(),
      favourable = sum(y),
      first_month = min(month_of_emission),
      last_month = max(month_of_emission),
      .groups = "drop"
    )
  bind_cols(counts, wilson_from_counts(counts$favourable, counts$notes)) |>
    arrange(desc(notes))
}

conitec_descriptives <- function(frame, contract = model_contract()) {
  spec <- contract$factors$conitec
  counts <- frame |>
    mutate(conitec_level = as.character(.data[[spec$column]])) |>
    count(origin = as.character(origin), conitec_level, y, name = "notes") |>
    tidyr::complete(
      origin,
      conitec_level = spec$levels,
      y = 0:1,
      fill = list(notes = 0L)
    ) |>
    group_by(origin, conitec_level) |>
    summarise(
      total = sum(notes),
      favourable = sum(notes[y == 1L]),
      .groups = "drop"
    )
  intervals <- counts |>
    filter(total > 0L)
  intervals <- bind_cols(
    intervals,
    wilson_from_counts(intervals$favourable, intervals$total)
  )
  counts |>
    left_join(intervals, by = c("origin", "conitec_level", "total", "favourable")) |>
    mutate(
      substantive = conitec_level %in% spec$substantive_levels,
      auxiliary_missing = conitec_level == spec$missing_level
    )
}

pair_counts <- function(frame) {
  frame |>
    transmute(
      pair_id = as.character(medicine_icd),
      medicine = as.character(medicine),
      cid = as.character(icd_category),
      origin = as.character(origin),
      y = as.integer(y)
    ) |>
    group_by(pair_id, medicine, cid, origin) |>
    summarise(favourable = sum(y), total = n(), .groups = "drop") |>
    arrange(pair_id, origin)
}

pair_support_at_floor <- function(counts, floor, minimum_origins = 2L) {
  supported <- counts |>
    filter(total >= floor)
  retained_pairs <- supported |>
    count(pair_id, name = "n_origins") |>
    filter(n_origins >= minimum_origins) |>
    pull(pair_id)
  supported |>
    filter(pair_id %in% retained_pairs)
}

pair_cell_intervals <- function(cells, floor) {
  intervals <- wilson_from_counts(cells$favourable, cells$total)
  bind_cols(cells, intervals) |>
    mutate(
      floor = as.integer(floor),
      raw_rate = favourable / total,
      arcsine_root_rate = asin(sqrt(raw_rate)),
      .before = 1L
    )
}

pair_dispersion <- function(cells, floor, estimate_column, estimate_type) {
  cells <- cells |>
    filter(total > 0L, is.finite(.data[[estimate_column]]))
  if (!nrow(cells)) {
    return(tibble::tibble(
      pair_id = character(), medicine = character(), cid = character(),
      floor = integer(), estimate_type = character(), n_origins = integer(),
      notes = integer(), minimum_n = integer(), maximum_n = integer(),
      rate_min = numeric(), rate_max = numeric(), range = numeric(),
      standard_deviation = numeric(), iqr = numeric(), arcsine_range = numeric(),
      arcsine_standard_deviation = numeric(), arcsine_iqr = numeric()
    ))
  }
  cells |>
    group_by(pair_id, medicine, cid) |>
    summarise(
      floor = as.integer(.env$floor),
      estimate_type = .env$estimate_type,
      n_origins = n(),
      notes = sum(total),
      minimum_n = min(total),
      maximum_n = max(total),
      rate_min = min(.data[[estimate_column]]),
      rate_max = max(.data[[estimate_column]]),
      range = rate_max - rate_min,
      standard_deviation = stats::sd(.data[[estimate_column]]),
      iqr = stats::IQR(.data[[estimate_column]]),
      arcsine_range = diff(range(asin(sqrt(.data[[estimate_column]])))),
      arcsine_standard_deviation = stats::sd(asin(sqrt(.data[[estimate_column]]))),
      arcsine_iqr = stats::IQR(asin(sqrt(.data[[estimate_column]]))),
      .groups = "drop"
    )
}

pair_range_thresholds <- function(raw_dispersion) {
  tidyr::crossing(
    raw_dispersion,
    threshold = PAIR_RANGE_THRESHOLDS
  ) |>
    group_by(floor, threshold) |>
    summarise(
      eligible_pairs = n(),
      pairs_at_or_above_threshold = sum(range >= threshold),
      fraction_at_or_above_threshold = mean(range >= threshold),
      .groups = "drop"
    )
}

pair_extremes <- function(cells) {
  qualifying <- cells |>
    group_by(floor, pair_id, medicine, cid) |>
    filter(min(raw_rate) == 0, max(raw_rate) == 1) |>
    ungroup()
  qualifying |>
    group_by(floor, pair_id, medicine, cid) |>
    mutate(
      pair_origins = n(),
      selection_rule = "at_least_one_zero_and_one_one_raw_cell"
    ) |>
    ungroup() |>
    filter(raw_rate %in% c(0, 1)) |>
    arrange(floor, pair_id, raw_rate, origin)
}

local_national_extremes <- function(cells) {
  national <- cells |>
    filter(origin == REFERENCE_ORIGIN) |>
    select(
      floor, pair_id, medicine, cid,
      national_total = total,
      national_favourable = favourable,
      national_rate = raw_rate,
      national_wilson_low = wilson_low,
      national_wilson_high = wilson_high
    )
  local <- cells |>
    filter(origin != REFERENCE_ORIGIN) |>
    rename(
      local_origin = origin,
      local_total = total,
      local_favourable = favourable,
      local_rate = raw_rate,
      local_wilson_low = wilson_low,
      local_wilson_high = wilson_high
    )
  comparisons <- inner_join(
    local,
    national,
    by = c("floor", "pair_id", "medicine", "cid")
  ) |>
    mutate(local_minus_national = local_rate - national_rate)
  positive <- comparisons |>
    group_by(floor, local_origin) |>
    filter(local_minus_national == max(local_minus_national)) |>
    mutate(direction = "largest_local_advantage") |>
    ungroup()
  negative <- comparisons |>
    group_by(floor, local_origin) |>
    filter(local_minus_national == min(local_minus_national)) |>
    mutate(direction = "largest_national_advantage") |>
    ungroup()
  bind_rows(positive, negative) |>
    arrange(floor, local_origin, direction, pair_id)
}

build_descriptive_bundle <- function(frame, tmb_threads = TMB_THREADS) {
  counts <- pair_counts(frame)
  support <- list()
  cell_tables <- list()
  dispersion <- list()
  shrinkage_models <- list()
  for (floor in PAIR_CELL_FLOORS) {
    supported <- pair_support_at_floor(counts, floor)
    raw_cells <- pair_cell_intervals(supported, floor)
    shrinkage <- cnj_audit_shrunken_rates(
      supported |>
        select(pair_id, origin, favourable, total),
      tmb_threads = tmb_threads
    )
    shrinkage_models[[as.character(floor)]] <- shrinkage$model
    shrunken <- raw_cells |>
      left_join(
        shrinkage$cells |>
          select(pair_id, origin, shrunken_rate),
        by = c("pair_id", "origin")
      ) |>
      mutate(shrinkage_status = shrinkage$convergence_status)
    support[[length(support) + 1L]] <- tibble::tibble(
      floor = floor,
      input_cells = nrow(counts),
      retained_cells = nrow(supported),
      retained_pairs = n_distinct(supported$pair_id),
      retained_origins = n_distinct(supported$origin),
      retained_notes = sum(supported$total),
      shrinkage_status = shrinkage$convergence_status
    )
    cell_tables[[length(cell_tables) + 1L]] <- shrunken
    raw_dispersion <- pair_dispersion(shrunken, floor, "raw_rate", "raw")
    shrunken_dispersion <- if (
      identical(shrinkage$convergence_status, "converged") &&
      nrow(shrunken) > 0L && all(is.finite(shrunken$shrunken_rate))
    ) {
      pair_dispersion(shrunken, floor, "shrunken_rate", "shrunken")
    } else {
      raw_dispersion[0, ] |>
        mutate(estimate_type = "shrunken")
    }
    dispersion[[length(dispersion) + 1L]] <- bind_rows(
      raw_dispersion,
      shrunken_dispersion
    )
  }
  cells <- bind_rows(cell_tables)
  dispersion <- bind_rows(dispersion)
  list(
    production = production_descriptives(frame),
    conitec_rates = conitec_descriptives(frame),
    pair_support = bind_rows(support),
    pair_cells = cells,
    pair_dispersion = dispersion,
    pair_range_thresholds = pair_range_thresholds(filter(dispersion, estimate_type == "raw")),
    pair_extremes = pair_extremes(cells),
    local_national_extremes = local_national_extremes(cells),
    shrinkage_models = shrinkage_models
  )
}

fixed_covariance <- function(model, clustered = TRUE) {
  if (inherits(model, "glmmTMB")) {
    return(as.matrix(stats::vcov(model)$cond))
  }
  if (inherits(model, "fixest")) {
    if (clustered) return(as.matrix(stats::vcov(model)))
    return(as.matrix(stats::vcov(model, vcov = "iid")))
  }
  stop("Unsupported model class for fixed-effect covariance.", call. = FALSE)
}

origin_contrasts_quietly <- function(model, origin_column = "origin") {
  suppressWarnings(
    cnj_audit_origin_contrasts(
      model,
      origin_column = origin_column
    )
  )
}

standardize_estimable_quietly <- function(model, data, origins, origin_column = "origin") {
  if (inherits(model, "fixest")) {
    contrasts <- origin_contrasts_quietly(model, origin_column = origin_column)
    contrasts <- contrasts[origins]
    current_effect <- unname(contrasts[as.character(data[[origin_column]])])
    observed_link <- suppressWarnings(
      stats::predict(model, newdata = data, type = "link")
    )
    estimable <- is.finite(observed_link) & is.finite(current_effect)
    if (!any(estimable) || any(!is.finite(contrasts))) {
      stop("No complete fixed-origin standardization sample is estimable.", call. = FALSE)
    }
    reference_link <- observed_link[estimable] - current_effect[estimable]
    estimates <- vapply(origins, function(origin) {
      mean(stats::plogis(reference_link + contrasts[[origin]]))
    }, numeric(1))
    if (length(unique(contrasts)) > 1L && length(unique(estimates)) == 1L) {
      stop("Fixed-origin counterfactual predictions did not vary with the origin effect.", call. = FALSE)
    }
    return(list(estimates = estimates, coverage = mean(estimable)))
  }
  withCallingHandlers(
    cnj_audit_standardize_estimable(
      model,
      data,
      origins,
      origin_column = origin_column
    ),
    warning = function(condition) {
      if (startsWith(conditionMessage(condition), "contrasts dropped from factor")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

origin_random_sd <- function(model, group = "origin") {
  block <- glmmTMB::VarCorr(model)$cond[[group]]
  if (is.null(block)) return(NA_real_)
  sd <- attr(block, "stddev")
  if (is.null(sd)) sd <- sqrt(pmax(diag(as.matrix(block)), 0))
  as.double(sd[[1L]])
}

origin_metric_values <- function(fit_bundle, standardization_data) {
  model <- fit_bundle$model
  origins <- levels(droplevels(standardization_data$origin))
  contrasts <- origin_contrasts_quietly(model, origin_column = "origin")
  contrasts <- contrasts[origins]
  standardized <- standardize_estimable_quietly(
    model,
    standardization_data,
    origins,
    origin_column = "origin"
  )
  sigma <- if (inherits(model, "glmmTMB")) origin_random_sd(model) else NA_real_
  c(
    mor = if (is.finite(sigma)) exp(sqrt(2) * sigma * stats::qnorm(0.75)) else NA_real_,
    latent_origin_sd = sigma,
    empirical_origin_sd_logit = stats::sd(contrasts),
    latent_icc = if (is.finite(sigma)) sigma^2 / (sigma^2 + pi^2 / 3) else NA_real_,
    standardized_sd_probability = stats::sd(standardized$estimates),
    standardized_mean_probability = mean(standardized$estimates),
    standardized_range_probability = diff(range(standardized$estimates)),
    standardized_iqr_probability = stats::IQR(standardized$estimates),
    standardization_coverage = standardized$coverage
  )
}

fixed_level_anchor_candidates <- function(frame, fit_11) {
  origins <- levels(droplevels(frame$origin))
  standardized_11 <- standardize_estimable_quietly(
    fit_11$model,
    frame,
    origins,
    origin_column = "origin"
  )
  tibble::tibble(
    anchor = c("pooled_outcome_logit", "model_1_1_standardized_mean_logit"),
    reference_intercept = c(
      stats::qlogis(mean(frame$y)),
      stats::qlogis(mean(standardized_11$estimates))
    ),
    candidate = TRUE,
    default = FALSE
  )
}

metric_candidate_table <- function(fit_bundles, frame, anchors, replicate_matrices = list()) {
  rows <- list()
  for (fit_name in names(fit_bundles)) {
    fit <- fit_bundles[[fit_name]]
    base <- origin_metric_values(fit, frame)
    rows[[length(rows) + 1L]] <- tibble::tibble(
      model = fit_name,
      anchor = NA_character_,
      metric = names(base),
      estimate = as.double(base),
      scale = c(
        "odds_ratio", "logit_sd", "logit_sd", "latent_variance_fraction",
        "probability_sd", "probability", "probability_range", "probability_iqr", "fraction"
      ),
      finite_sample_correction = FALSE,
      candidate = TRUE,
      default = FALSE
    )
    contrasts <- origin_contrasts_quietly(fit$model, origin_column = "origin")
    for (i in seq_len(nrow(anchors))) {
      anchor <- anchors[i, ]
      probabilities <- cnj_audit_fixed_level_probabilities(
        contrasts,
        anchor$reference_intercept
      )
      replicate_matrix <- replicate_matrices[[fit_name]]
      correction <- cnj_audit_finite_sample_correction(
        probabilities,
        replicate_matrix
      )
      values <- c(
        fixed_level_sd_uncorrected = correction$sd_uncorrected,
        fixed_level_variance_observed = correction$var_observed,
        fixed_level_mean_sampling_variance = correction$mean_sampling_var,
        fixed_level_variance_corrected = correction$var_corrected,
        fixed_level_sd_corrected_signed = correction$sd_corrected_signed,
        fixed_level_sd_corrected_truncated = correction$sd_corrected_truncated
      )
      rows[[length(rows) + 1L]] <- tibble::tibble(
        model = fit_name,
        anchor = anchor$anchor,
        metric = names(values),
        estimate = as.double(values),
        scale = c("probability_sd", "probability_variance", "probability_variance", "probability_variance", "probability_sd", "probability_sd"),
        finite_sample_correction = metric != "fixed_level_sd_uncorrected" & metric != "fixed_level_variance_observed",
        candidate = TRUE,
        default = FALSE
      )
    }
  }
  bind_rows(rows)
}

origin_effect_candidates <- function(fit_bundles, frame, anchors) {
  rows <- list()
  raw <- frame |>
    group_by(origin = as.character(origin)) |>
    summarise(notes = n(), favourable = sum(y), raw_rate = mean(y), .groups = "drop")
  for (fit_name in names(fit_bundles)) {
    fit <- fit_bundles[[fit_name]]
    origins <- levels(droplevels(frame$origin))
    contrasts <- origin_contrasts_quietly(fit$model, origin_column = "origin")
    contrasts <- contrasts[origins]
    standardized <- standardize_estimable_quietly(
      fit$model,
      frame,
      origins,
      origin_column = "origin"
    )
    reference_value <- contrasts[[REFERENCE_ORIGIN]]
    centered <- contrasts - reference_value
    for (i in seq_len(nrow(anchors))) {
      anchor <- anchors[i, ]
      fixed_probability <- cnj_audit_fixed_level_probabilities(
        centered,
        anchor$reference_intercept
      )
      rows[[length(rows) + 1L]] <- tibble::tibble(
        model = fit_name,
        origin = origins,
        anchor = anchor$anchor,
        origin_effect_logit = as.double(contrasts),
        contrast_vs_nacional_logit = as.double(centered),
        odds_ratio_vs_nacional = exp(centered),
        fixed_level_probability = as.double(fixed_probability),
        standardized_probability = as.double(standardized$estimates),
        standardization_coverage = standardized$coverage,
        candidate = TRUE,
        default = FALSE
      ) |>
        left_join(raw, by = "origin")
    }
  }
  bind_rows(rows)
}

origin_effect_wald_intervals <- function(fit_bundle, level = INTERVAL_LEVEL) {
  model <- fit_bundle$model
  z <- stats::qnorm(1 - (1 - level) / 2)
  if (inherits(model, "fixest")) {
    origin_contrasts <- origin_contrasts_quietly(
      model,
      origin_column = "origin"
    )
    origins <- names(origin_contrasts)
    beta <- stats::coef(model)
    covariance <- fixed_covariance(model, clustered = TRUE)
    estimates <- stats::setNames(rep(0, length(origins)), origins)
    variances <- stats::setNames(rep(0, length(origins)), origins)
    for (origin in setdiff(origins, REFERENCE_ORIGIN)) {
      name <- names(beta)[names(beta) == paste0("origin::", origin)]
      if (length(name) == 1L) {
        estimates[[origin]] <- beta[[name]]
        variances[[origin]] <- covariance[name, name]
      } else {
        estimates[[origin]] <- variances[[origin]] <- NA_real_
      }
    }
    standard_error <- sqrt(pmax(variances, 0))
    return(tibble::tibble(
      model = fit_bundle$specification$name,
      origin = origins,
      effect = estimates,
      contrast_vs_nacional = estimates,
      standard_error = standard_error,
      conf_low = estimates - z * standard_error,
      conf_high = estimates + z * standard_error,
      scale = "logit",
      interval_method = "wald_two_way_medicine_cid",
      conditional_covariance_between_origins = "fixed_effect_covariance",
      candidate = TRUE,
      default = FALSE
    ))
  }
  effects <- suppressWarnings(glmmTMB::ranef(model, condVar = TRUE))$cond$origin
  values <- as.double(effects[[1L]])
  names(values) <- rownames(effects)
  conditional_variance <- attr(effects, "condVar")
  if (is.null(conditional_variance)) {
    variances <- stats::setNames(rep(NA_real_, length(values)), names(values))
  } else {
    variances <- stats::setNames(
      as.double(conditional_variance[1L, 1L, ]),
      dimnames(conditional_variance)[[3L]] %||% names(values)
    )
    variances <- variances[names(values)]
  }
  reference <- values[[REFERENCE_ORIGIN]]
  reference_variance <- variances[[REFERENCE_ORIGIN]]
  contrast <- values - reference
  contrast_variance <- variances + reference_variance
  contrast_variance[[REFERENCE_ORIGIN]] <- 0
  standard_error <- sqrt(pmax(contrast_variance, 0))
  tibble::tibble(
    model = fit_bundle$specification$name,
    origin = names(values),
    effect = values,
    contrast_vs_nacional = contrast,
    standard_error = standard_error,
    conf_low = contrast - z * standard_error,
    conf_high = contrast + z * standard_error,
    scale = "logit",
    interval_method = "conditional_mode_wald",
    conditional_covariance_between_origins = "unavailable_assumed_zero_for_contrast_variance",
    candidate = TRUE,
    default = FALSE
  )
}

origin_theta_parameter <- function(model) {
  parameters <- cnj_audit_glmmtmb_latent_parameters(model)
  matches <- parameters[grepl("[|]origin[.]", parameters)]
  if (length(matches) != 1L) {
    stop("Could not identify exactly one origin variance parameter.", call. = FALSE)
  }
  matches
}

build_uncertainty_fit <- function(fit_bundle, frame) {
  required <- c("internal_order", "id_nota", "y", "origin")
  if (!all(required %in% names(frame)) ||
      anyNA(frame$internal_order) || anyDuplicated(frame$internal_order) ||
      !identical(frame$id_nota, as.character(frame$idNotaTecnica))) {
    stop("Uncertainty frame does not preserve the unique model order and note IDs.", call. = FALSE)
  }
  cnj_audit_uncertainty_fit(
    fit_bundle$result,
    fit_bundle$specification,
    frame,
    family = "model_1_2",
    sample_key = "internal_order",
    standardization_data = frame
  )
}

profile_origin_sd_interval <- function(fit_bundle, frame, level = INTERVAL_LEVEL,
                                       ncpus = 1L, profiler = cnj_audit_default_glmmtmb_profiler) {
  if (!identical(fit_bundle$specification$name, "model_1_2")) {
    return(tibble::tibble(
      model = fit_bundle$specification$name,
      parameter = "origin_standard_deviation",
      method = "likelihood_profile",
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      status = "unsupported_specification", failure_reason = "profile_wrapper_accepts_only_base_model",
      candidate = TRUE, default = FALSE
    ))
  }
  uncertainty_fit <- build_uncertainty_fit(fit_bundle, frame)
  parameter <- origin_theta_parameter(fit_bundle$model)
  profiled <- cnj_audit_glmmtmb_profile_interval(
    uncertainty_fit,
    parameter,
    level = level,
    ncpus = ncpus,
    profiler = profiler
  )
  tibble::tibble(
    model = fit_bundle$specification$name,
    parameter = "origin_standard_deviation",
    method = "likelihood_profile_log_standard_deviation",
    estimate = ifelse(profiled$profile_status == "ok", exp(profiled$estimate), NA_real_),
    conf_low = ifelse(profiled$profile_status == "ok", exp(profiled$conf_low), NA_real_),
    conf_high = ifelse(profiled$profile_status == "ok", exp(profiled$conf_high), NA_real_),
    status = profiled$profile_status,
    failure_reason = profiled$failure_reason,
    candidate = TRUE,
    default = FALSE
  )
}

restricted_specification <- function(specification, name, remove_fixed = character(), remove_origin = FALSE) {
  fixed_terms <- setdiff(c(low_dimension_columns(), model_contract()$time$output_column), remove_fixed)
  if (specification$engine == "glmmTMB") {
    random_terms <- c(if (!remove_origin) "(1 | origin)", "(1 | medicine)", "(1 | cid)")
    formula <- stats::as.formula(paste0(
      "y ~ ", paste(c(fixed_terms, random_terms), collapse = " + ")
    ))
    return(make_specification(name, "glmmTMB", formula, paste(sort(fixed_terms), collapse = " + ")))
  }
  origin_term <- if (remove_origin) character() else paste0("i(origin, ref = \"", REFERENCE_ORIGIN, "\")")
  formula <- stats::as.formula(paste0(
    "y ~ ", paste(c(origin_term, fixed_terms), collapse = " + "),
    " | medicine + cid"
  ))
  make_specification(
    name,
    "fixest",
    formula,
    paste(sort(fixed_terms), collapse = " + "),
    stats::as.formula("~ medicine + cid")
  )
}

likelihood_ratio_record <- function(full, restricted, comparison, boundary = FALSE) {
  full_ll <- stats::logLik(full$model)
  restricted_ll <- stats::logLik(restricted$model)
  statistic <- 2 * (as.double(full_ll) - as.double(restricted_ll))
  df <- as.integer(attr(full_ll, "df") - attr(restricted_ll, "df"))
  same_n <- identical(stats::nobs(full$model), stats::nobs(restricted$model))
  p_regular <- if (same_n && df > 0L && statistic >= 0) {
    stats::pchisq(statistic, df = df, lower.tail = FALSE)
  } else NA_real_
  p_boundary <- if (boundary && same_n && df == 1L && statistic >= 0) {
    0.5 * stats::pchisq(statistic, df = 1L, lower.tail = FALSE)
  } else NA_real_
  tibble::tibble(
    comparison = comparison,
    full_model = full$specification$name,
    restricted_model = restricted$specification$name,
    n_full = stats::nobs(full$model),
    n_restricted = stats::nobs(restricted$model),
    same_estimation_n = same_n,
    statistic = statistic,
    degrees_of_freedom = df,
    null_on_boundary = boundary,
    p_value_regular = p_regular,
    p_value_boundary_mixture = p_boundary,
    technically_comparable = same_n && is.finite(statistic) && df > 0L,
    candidate = TRUE,
    default = FALSE
  )
}

fit_reporting_candidates <- function(main_fits, frame) {
  clinical <- clinical_columns()
  spec_11 <- final_estimator_specifications()$model_1_1
  spec_12 <- final_estimator_specifications()$model_1_2
  tests <- list()
  summaries <- list()

  records_11 <- fit_population_records(main_fits$model_1_1, frame)
  common_11 <- droplevels(frame[records_11$estimated_index, , drop = FALSE])
  common_full <- fit_one_estimator(
    restricted_specification(spec_11, "model_1_1_common_full"),
    common_11
  )
  summaries[[length(summaries) + 1L]] <- fit_scalar_summary(common_full)

  without_origin_11 <- fit_one_estimator(
    restricted_specification(spec_11, "model_1_1_without_origin", remove_origin = TRUE),
    common_11
  )
  tests[[length(tests) + 1L]] <- likelihood_ratio_record(
    common_full,
    without_origin_11,
    "fixed_origin_block",
    boundary = FALSE
  )
  summaries[[length(summaries) + 1L]] <- fit_scalar_summary(without_origin_11)
  rm(without_origin_11)
  invisible(gc())

  without_clinical_11 <- fit_one_estimator(
    restricted_specification(spec_11, "model_1_1_without_clinical", remove_fixed = clinical),
    common_11
  )
  tests[[length(tests) + 1L]] <- likelihood_ratio_record(
    common_full,
    without_clinical_11,
    "fixed_clinical_block",
    boundary = FALSE
  )
  summaries[[length(summaries) + 1L]] <- fit_scalar_summary(without_clinical_11)
  rm(without_clinical_11, common_full, common_11, records_11)
  invisible(gc())

  without_origin_12 <- fit_one_estimator(
    restricted_specification(spec_12, "model_1_2_without_origin", remove_origin = TRUE),
    frame
  )
  tests[[length(tests) + 1L]] <- likelihood_ratio_record(
    main_fits$model_1_2,
    without_origin_12,
    "random_origin_variance",
    boundary = TRUE
  )
  summaries[[length(summaries) + 1L]] <- fit_scalar_summary(without_origin_12)
  rm(without_origin_12)
  invisible(gc())

  without_clinical_12 <- fit_one_estimator(
    restricted_specification(spec_12, "model_1_2_without_clinical", remove_fixed = clinical),
    frame
  )
  tests[[length(tests) + 1L]] <- likelihood_ratio_record(
    main_fits$model_1_2,
    without_clinical_12,
    "mixed_clinical_block",
    boundary = FALSE
  )
  summaries[[length(summaries) + 1L]] <- fit_scalar_summary(without_clinical_12)
  rm(without_clinical_12)
  invisible(gc())

  result <- list(
    tests = bind_rows(tests),
    summaries = bind_rows(summaries) |>
      mutate(
        sample_label = case_when(
          startsWith(model, "model_1_1_") ~ "model_1_1_common_estimated_sample",
          startsWith(model, "model_1_2_") ~ "main_input",
          TRUE ~ NA_character_
        )
      ),
    checkpoint_schema = "lightweight_reporting_candidates_v1"
  )
  invisible(gc())
  result
}

fit_main_models <- function(frame) {
  specifications <- validate_final_specifications()
  comparability <- lapply(
    specifications,
    cnj_audit_comparability_record,
    data = frame,
    sample_key = "id_nota"
  )
  cnj_audit_assert_comparable(
    comparability$model_1_1,
    comparability$model_1_2
  )
  fit_11 <- fit_one_estimator(specifications$model_1_1, frame)
  fit_12 <- fit_one_estimator(specifications$model_1_2, frame)
  list(
    specifications = specifications,
    comparability = comparability,
    fits = list(model_1_1 = fit_11, model_1_2 = fit_12)
  )
}

write_stamped_csv <- function(data, path, provenance) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  stamp <- c(
    paste0("# generated_at: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("# enatjus_sha: ", EXPECTED_PACKAGE_SHA),
    paste0("# ", provenance$input, "_sha256: ", provenance$sha256)
  )
  writeLines(stamp, path)
  readr::write_csv(data, path, append = TRUE, col_names = TRUE, na = "")
  invisible(path)
}

commit_staged_files <- function(staged_paths, canonical_paths) {
  if (length(staged_paths) != length(canonical_paths) ||
      anyDuplicated(staged_paths) || anyDuplicated(canonical_paths) ||
      any(!file.exists(staged_paths))) {
    stop("Staged publication inventory is incomplete or non-unique.", call. = FALSE)
  }
  backup_root <- file.path(
    "backups",
    paste0("final-analysis-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), "-", Sys.getpid())
  )
  moved_old <- character()
  for (canonical in canonical_paths[file.exists(canonical_paths)]) {
    backup <- file.path(backup_root, canonical)
    dir.create(dirname(backup), recursive = TRUE, showWarnings = FALSE)
    if (!file.rename(canonical, backup)) {
      for (old in rev(moved_old)) {
        restored <- file.path(backup_root, old)
        dir.create(dirname(old), recursive = TRUE, showWarnings = FALSE)
        if (file.exists(restored)) file.rename(restored, old)
      }
      stop("Could not move prior bundle to backup: ", canonical, call. = FALSE)
    }
    moved_old <- c(moved_old, canonical)
  }

  published <- character()
  for (i in seq_along(staged_paths)) {
    staged <- staged_paths[[i]]
    canonical <- canonical_paths[[i]]
    dir.create(dirname(canonical), recursive = TRUE, showWarnings = FALSE)
    if (!file.rename(staged, canonical)) {
      rollback_root <- file.path(dirname(staged_paths[[1L]]), "rollback-new")
      for (new_path in rev(published)) {
        rollback <- file.path(rollback_root, new_path)
        dir.create(dirname(rollback), recursive = TRUE, showWarnings = FALSE)
        if (file.exists(new_path)) file.rename(new_path, rollback)
      }
      restore_failed <- character()
      for (old in moved_old) {
        backup <- file.path(backup_root, old)
        dir.create(dirname(old), recursive = TRUE, showWarnings = FALSE)
        if (file.exists(backup) && !file.rename(backup, old)) {
          restore_failed <- c(restore_failed, old)
        }
      }
      if (length(restore_failed)) {
        stop(
          "Publication failed and rollback could not restore: ",
          paste(restore_failed, collapse = ", "),
          call. = FALSE
        )
      }
      stop("Publication failed; the prior bundle was restored: ", canonical, call. = FALSE)
    }
    published <- c(published, canonical)
  }
  invisible(list(published = published, backed_up = moved_old, backup_root = backup_root))
}

output_table_paths <- function() {
  names <- c(
    "input-provenance", "descriptive-production", "descriptive-conitec-rates",
    "descriptive-pair-support", "descriptive-pair-cells", "descriptive-pair-dispersion",
    "descriptive-pair-range-thresholds", "descriptive-pair-extremes",
    "descriptive-local-national-extremes", "fit-summary", "fit-populations",
    "fit-populations-by-origin", "fit-diagnostics", "fit-coefficients",
    "fit-random-effects", "fixest-removed-levels", "metric-candidates",
    "origin-effect-candidates", "origin-effect-wald-intervals",
    "profile-interval-candidates", "likelihood-ratio-candidates", "manifest"
  )
  stats::setNames(
    file.path(FINAL_TABLE_DIR, paste0(OUTPUT_PREFIX, names, ".csv")),
    names
  )
}

model_output_paths <- function() {
  names <- c("model-1-1", "model-1-2", "descriptive-shrinkage", "reconstruction-bundle")
  stats::setNames(
    file.path(MODEL_FIT_DIR, paste0(OUTPUT_PREFIX, names, ".rds")),
    names
  )
}

publish_final_bundle <- function(inputs, frame, descriptive, main, reporting, profile) {
  canonical_table_paths <- output_table_paths()
  canonical_model_paths <- model_output_paths()
  canonical_session_path <- file.path(FINAL_LOG_DIR, paste0(OUTPUT_PREFIX, "session-info.txt"))
  stage_root <- file.path(
    MODEL_FIT_DIR,
    paste0(".publish-stage-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), "-", Sys.getpid())
  )
  table_paths <- stats::setNames(
    file.path(stage_root, unname(canonical_table_paths)),
    names(canonical_table_paths)
  )
  model_paths <- stats::setNames(
    file.path(stage_root, unname(canonical_model_paths)),
    names(canonical_model_paths)
  )
  session_path <- file.path(stage_root, canonical_session_path)
  dir.create(stage_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(model_paths[[1L]]), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(session_path), recursive = TRUE, showWarnings = FALSE)

  populations <- lapply(main$fits, fit_population_records, frame = frame)
  exclusions <- fixest_exclusion_records(main$fits$model_1_1, frame)
  anchors <- fixed_level_anchor_candidates(frame, main$fits$model_1_1)
  # Sem réplicas de bootstrap, as métricas com correção de amostra finita saem
  # como não disponíveis (NA); a lista vazia diz isso à função.
  replicate_matrices <- stats::setNames(
    vector("list", length(main$fits)),
    names(main$fits)
  )
  metrics <- metric_candidate_table(main$fits, frame, anchors, replicate_matrices)
  effects <- origin_effect_candidates(main$fits, frame, anchors)

  tables <- list(
    `input-provenance` = inputs$provenance,
    `descriptive-production` = descriptive$production,
    `descriptive-conitec-rates` = descriptive$conitec_rates,
    `descriptive-pair-support` = descriptive$pair_support,
    `descriptive-pair-cells` = descriptive$pair_cells,
    `descriptive-pair-dispersion` = descriptive$pair_dispersion,
    `descriptive-pair-range-thresholds` = descriptive$pair_range_thresholds,
    `descriptive-pair-extremes` = descriptive$pair_extremes,
    `descriptive-local-national-extremes` = descriptive$local_national_extremes,
    `fit-summary` = bind_rows(lapply(main$fits, fit_scalar_summary)),
    `fit-populations` = bind_rows(lapply(populations, `[[`, "summary")),
    `fit-populations-by-origin` = bind_rows(lapply(populations, `[[`, "by_origin")),
    `fit-diagnostics` = bind_rows(lapply(main$fits, flatten_diagnostics)),
    `fit-coefficients` = bind_rows(lapply(main$fits, model_coefficient_table)),
    `fit-random-effects` = bind_rows(lapply(main$fits, random_effect_table)),
    `fixest-removed-levels` = exclusions$levels,
    `metric-candidates` = metrics,
    `origin-effect-candidates` = effects,
    `origin-effect-wald-intervals` = bind_rows(lapply(main$fits, origin_effect_wald_intervals)),
    `profile-interval-candidates` = profile,
    `likelihood-ratio-candidates` = reporting$tests
  )
  for (name in names(tables)) {
    write_stamped_csv(tables[[name]], table_paths[[name]], inputs$provenance)
  }

  saveRDS(
    list(fit = main$fits$model_1_1, population = populations$model_1_1),
    model_paths[["model-1-1"]]
  )
  saveRDS(
    list(fit = main$fits$model_1_2, population = populations$model_1_2),
    model_paths[["model-1-2"]]
  )
  saveRDS(descriptive$shrinkage_models, model_paths[["descriptive-shrinkage"]])
  saveRDS(
    list(
      input_hashes = inputs$provenance,
      contract = model_contract(),
      specifications = main$specifications,
      generated_at = Sys.time()
    ),
    model_paths[["reconstruction-bundle"]]
  )

  writeLines(
    c(
      paste0("generated_at: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
      paste0("enatjus_sha: ", EXPECTED_PACKAGE_SHA),
      "",
      capture.output(utils::sessionInfo())
    ),
    session_path
  )

  staged_manifest_files <- c(
    unname(table_paths[names(table_paths) != "manifest"]),
    unname(model_paths),
    session_path
  )
  canonical_manifest_files <- c(
    unname(canonical_table_paths[names(canonical_table_paths) != "manifest"]),
    unname(canonical_model_paths),
    canonical_session_path
  )
  manifest <- tibble::tibble(
    path = canonical_manifest_files,
    bytes = as.double(file.size(staged_manifest_files)),
    sha256 = vapply(staged_manifest_files, sha256_file, character(1)),
    kind = case_when(
      grepl("[.]rds$", canonical_manifest_files) ~ "local_rds",
      grepl("[.]csv$", canonical_manifest_files) ~ "aggregate_csv",
      TRUE ~ "log"
    )
  )
  write_stamped_csv(manifest, table_paths[["manifest"]], inputs$provenance)

  staged_all <- c(staged_manifest_files, unname(table_paths[["manifest"]]))
  canonical_all <- c(canonical_manifest_files, unname(canonical_table_paths[["manifest"]]))
  if (any(!file.exists(staged_all)) || any(file.size(staged_all) <= 0L)) {
    stop("The staged final bundle is incomplete.", call. = FALSE)
  }
  staged_production <- readr::read_csv(
    table_paths[["descriptive-production"]],
    comment = "#",
    show_col_types = FALSE
  )
  staged_populations <- readr::read_csv(
    table_paths[["fit-populations"]],
    comment = "#",
    show_col_types = FALSE
  )
  staged_metrics <- readr::read_csv(
    table_paths[["metric-candidates"]],
    comment = "#",
    show_col_types = FALSE
  )
  corrected_metrics <- staged_metrics$finite_sample_correction %in% TRUE
  uncorrected_fixed_metrics <- staged_metrics$metric %in% c(
    "fixed_level_sd_uncorrected", "fixed_level_variance_observed"
  )
  if (sum(staged_production$notes) != nrow(frame) ||
      any(staged_populations$input_notes != nrow(frame)) ||
      any(staged_metrics$default %in% TRUE) ||
      any(!is.na(staged_metrics$estimate[corrected_metrics])) ||
      any(!is.finite(staged_metrics$estimate[uncorrected_fixed_metrics]))) {
    stop("The staged final bundle failed its population or candidate reconciliation.", call. = FALSE)
  }
  observed_hashes <- vapply(staged_manifest_files, sha256_file, character(1))
  if (!identical(unname(observed_hashes), unname(manifest$sha256))) {
    stop("The staged manifest does not match staged file hashes.", call. = FALSE)
  }

  commit <- commit_staged_files(staged_all, canonical_all)
  invisible(list(
    tables = tables,
    manifest = manifest,
    paths = canonical_all,
    commit = commit,
    stage_root = stage_root
  ))
}

run_synthetic_checks <- function() {
  require_final_packages()
  set.seed(42)
  contract <- model_contract()
  origins <- c(
    "Nacional", "AC", "AL", "AM/SES", "BA", "CE", "DFT", "ES", "GO",
    "MA", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RN", "RR",
    "RS/DMJ", "SC", "SE", "SP", "TO", "TelessaúdeRS-UFRGS"
  )
  origin_family_map <- c(
    Nacional = "Nacional", AC = "AC", AL = "AL", `AM/SES` = "AM", BA = "BA",
    CE = "CE", DFT = "DFT", ES = "ES", GO = "GO", MA = "MA", MS = "MS",
    MT = "MT", PA = "PA", PB = "PB", PE = "PE", PI = "PI", PR = "PR",
    RN = "RN", RR = "RR", `RS/DMJ` = "RS", SC = "SC", SE = "SE",
    SP = "SP", TO = "TO", `TelessaúdeRS-UFRGS` = "RS"
  )
  medicines <- paste0("M", sprintf("%02d", 1:6))
  cids <- paste0("C", sprintf("%02d", 1:4))
  n <- 2400L
  fixture <- tibble::tibble(
    idNotaTecnica = sprintf("synthetic-%05d", seq_len(n)),
    analysis_origin = sample(origins, n, replace = TRUE),
    origem_tratada = analysis_origin,
    origin_family = unname(origin_family_map[analysis_origin]),
    medicine = sample(medicines, n, replace = TRUE),
    icd_category = sample(cids, n, replace = TRUE),
    medicine_icd = paste0(nchar(medicine), ":", medicine, "|", icd_category),
    month_of_emission = as.Date("2019-05-01") + 30L * sample(0:24, n, replace = TRUE),
    cov_selRecomendacaoConitec = sample(
      contract$factors$conitec$substantive_levels,
      n,
      replace = TRUE
    ),
    cov_selStaGenero = sample(contract$factors$sex$levels, n, replace = TRUE),
    cov_selDefensoriaPublica = sample(
      contract$factors$legal_representation$levels,
      n,
      replace = TRUE
    )
  )
  for (field in CLINICAL_FIELDS) {
    fixture[[paste0("cov_", field)]] <- sample(c("Nao", "Sim", MISSING_VALUE), n, replace = TRUE)
  }
  origin_effect <- stats::setNames(seq(-0.8, 0.8, length.out = length(origins)), origins)
  conitec_effect <- c("Nao avaliada" = 0, "Favoravel" = 0.7, "Desfavoravel" = -0.5)
  linear <- -0.7 + origin_effect[fixture$analysis_origin] +
    conitec_effect[fixture$cov_selRecomendacaoConitec] +
    0.015 * calendar_month_index(fixture$month_of_emission, SERIES_START)
  fixture$y <- stats::rbinom(n, 1L, stats::plogis(linear))
  frame <- prepare_final_frame(fixture, contract)
  specs <- validate_final_specifications()
  stopifnot(
    identical(specs$model_1_1$fixed_signature, specs$model_1_2$fixed_signature),
    all(clinical_columns() %in% all.vars(specs$model_1_1$formula)),
    all(clinical_columns() %in% all.vars(specs$model_1_2$formula)),
    identical(frame$id_nota, fixture$idNotaTecnica),
    levels(frame$origin)[[1L]] == REFERENCE_ORIGIN
  )

  production <- production_descriptives(frame)
  conitec_rates <- conitec_descriptives(frame)
  stopifnot(
    sum(production$notes) == nrow(frame),
    all(conitec_rates$rate[conitec_rates$total > 0] >= 0 & conitec_rates$rate[conitec_rates$total > 0] <= 1)
  )

  counts <- pair_counts(frame)
  cells <- pair_cell_intervals(pair_support_at_floor(counts, 5L), 5L)
  stopifnot(
    all(cells$total >= 5L),
    all(cells$wilson_low <= cells$raw_rate + 1e-12),
    all(cells$wilson_high + 1e-12 >= cells$raw_rate)
  )
  descriptive_bundle <- build_descriptive_bundle(frame)
  stopifnot(
    identical(descriptive_bundle$pair_support$floor, PAIR_CELL_FLOORS),
    sum(descriptive_bundle$production$notes) == nrow(frame)
  )

  # Cada motivo de parada da guarda do polimento, e o caso que passa.
  guard_stops <- function(status, gradient) {
    inherits(tryCatch(
      assert_polished_fit("synthetic", list(convergence_status = status, max_abs_gradient = gradient)),
      error = function(error) error
    ), "error")
  }
  stopifnot(
    guard_stops("optimization_failure", 1e-10),
    guard_stops("non_positive_definite_hessian", 1e-10),
    guard_stops("converged", 10 * POLISH_GUARD_TOLERANCE),
    guard_stops("converged", NA_real_),
    !guard_stops("converged", POLISH_GUARD_TOLERANCE / 10),
    !guard_stops("singular", POLISH_GUARD_TOLERANCE / 10)
  )
  # E o ajuste passa pela guarda: sem passos de Newton e com tolerância
  # inalcançável, o otimizador devolve falha, e fit_one_estimator tem de parar.
  unpolished_fit_stops <- (function() {
    saved <- list(NEWTON_GRADIENT_TOLERANCE = NEWTON_GRADIENT_TOLERANCE, NEWTON_MAX_STEPS = NEWTON_MAX_STEPS)
    on.exit(for (name in names(saved)) assign(name, saved[[name]], envir = globalenv()))
    assign("NEWTON_GRADIENT_TOLERANCE", 0, envir = globalenv())
    assign("NEWTON_MAX_STEPS", 0L, envir = globalenv())
    outcome <- tryCatch(fit_one_estimator(specs$model_1_2, frame), error = function(error) error)
    inherits(outcome, "error") && grepl("guard tolerance", conditionMessage(outcome), fixed = TRUE)
  })()
  stopifnot(unpolished_fit_stops)

  fits <- list(
    model_1_1 = fit_one_estimator(specs$model_1_1, frame),
    model_1_2 = fit_one_estimator(specs$model_1_2, frame)
  )
  uncertainty_fit <- build_uncertainty_fit(fits$model_1_2, frame)
  stopifnot(
    all(vapply(fits, function(x) !is.null(x$model), logical(1))),
    identical(fits$model_1_1$specification$fixed_signature, fits$model_1_2$specification$fixed_signature),
    inherits(uncertainty_fit, "cnj_audit_uncertainty_fit"),
    identical(uncertainty_fit$sample_key, "internal_order"),
    identical(uncertainty_fit$data$internal_order, frame$internal_order)
  )
  origin_intervals <- bind_rows(lapply(fits, origin_effect_wald_intervals))
  stopifnot(
    nrow(origin_intervals) == 2L * length(origins),
    all(origin_intervals$candidate),
    !any(origin_intervals$default)
  )

  anchors <- fixed_level_anchor_candidates(frame, fits$model_1_1)
  metrics <- metric_candidate_table(fits, frame, anchors)
  stopifnot(
    all(c("mor", "latent_origin_sd", "latent_icc", "standardized_sd_probability") %in% metrics$metric),
    all(metrics$candidate),
    !any(metrics$default),
    all(is.na(metrics$estimate[metrics$finite_sample_correction])),
    metrics$estimate[
      metrics$model == "model_1_1" &
        metrics$metric == "standardized_sd_probability"
    ] > 0,
    all(is.finite(metrics$estimate[metrics$metric %in% c(
      "fixed_level_sd_uncorrected", "fixed_level_variance_observed"
    )]))
  )

  reporting <- fit_reporting_candidates(fits, frame)
  stopifnot(
    nrow(reporting$tests) == 4L,
    !"fits" %in% names(reporting),
    identical(reporting$checkpoint_schema, "lightweight_reporting_candidates_v1"),
    as.double(object.size(reporting)) < 1e6,
    all(reporting$tests$candidate),
    !any(reporting$tests$default)
  )
  # A correção finita tem de preservar uma variância corrigida negativa em vez
  # de substituí-la em silêncio. Esta matriz tem, por construção, mais variância
  # amostral do que a variância observada entre origens.
  correction <- cnj_audit_finite_sample_correction(
    c(0.49, 0.50, 0.51),
    matrix(c(0.1, 0.5, 0.9, 0.2, 0.5, 0.8), nrow = 2L, byrow = TRUE)
  )
  stopifnot(correction$var_corrected < 0, correction$sd_corrected_truncated == 0)

  # Dispersão constante na escala arco-seno da raiz varia na escala bruta de
  # probabilidade quando a linha de base muda.
  delta <- 0.2
  low <- sin(c(0.3 - delta, 0.3 + delta))^2
  high <- sin(c(0.8 - delta, 0.8 + delta))^2
  stopifnot(
    isTRUE(all.equal(stats::sd(asin(sqrt(low))), stats::sd(asin(sqrt(high))), tolerance = 1e-12)),
    !isTRUE(all.equal(stats::sd(low), stats::sd(high), tolerance = 1e-6))
  )

  original_directory <- getwd()
  publication_fixture <- tempfile("final-analysis-publication-")
  dir.create(publication_fixture)
  setwd(publication_fixture)
  on.exit(setwd(original_directory), add = TRUE)
  dir.create("stage", recursive = TRUE)
  dir.create("canonical", recursive = TRUE)
  writeLines("old", "canonical/result.txt")
  writeLines("new", "stage/result.txt")
  commit <- commit_staged_files("stage/result.txt", "canonical/result.txt")
  stopifnot(
    identical(readLines("canonical/result.txt"), "new"),
    identical(readLines(file.path(commit$backup_root, "canonical/result.txt")), "old")
  )
  checkpoint_counter <- new.env(parent = emptyenv())
  checkpoint_counter$n <- 0L
  checkpoint_work <- function() {
    checkpoint_counter$n <- checkpoint_counter$n + 1L
    checkpoint_counter$n
  }
  first_checkpoint <- checkpoint_compute("synthetic", "one", list(seed = 1L), checkpoint_work)
  second_checkpoint <- checkpoint_compute("synthetic", "one", list(seed = 1L), checkpoint_work)
  incompatible_checkpoint <- checkpoint_compute("synthetic", "one", list(seed = 2L), checkpoint_work)
  stopifnot(
    identical(first_checkpoint, 1L),
    identical(second_checkpoint, 1L),
    identical(incompatible_checkpoint, 2L),
    checkpoint_counter$n == 2L
  )
  setwd(original_directory)

  cat("Synthetic final-analysis checks passed: contract, descriptions, fits, checkpoints, staged publication, point metrics and finite correction.\n")
  invisible(TRUE)
}

`%||%` <- function(left, right) {
  if (is.null(left)) right else left
}

prepare_real_preflight <- function() {
  inputs <- read_final_inputs()
  frame <- prepare_final_frame(inputs$covariates)
  specifications <- validate_final_specifications()
  stopifnot(
    nrow(frame) == 273734L,
    n_distinct(frame$origin) == 25L,
    identical(frame$id_nota, as.character(inputs$covariates$idNotaTecnica)),
    identical(frame$internal_order, sprintf("%09d", seq_len(nrow(frame)))),
    !anyDuplicated(frame$internal_order),
    identical(range(frame$ano_mes), c(0L, 87L)),
    identical(specifications$model_1_1$fixed_signature, specifications$model_1_2$fixed_signature)
  )
  invisible(list(
    inputs = inputs,
    frame = frame
  ))
}

check_real_preflight <- function() {
  result <- prepare_real_preflight()
  cat(
    "Real final-analysis preflight passed: 273,734 notes, 25 origins, verified hashes, closed model contract and internal_order preserved.\n"
  )
  invisible(result)
}

# Metadados de compatibilidade de cada checkpoint. As duas funções abaixo, a
# que ajusta e a que publica, leem daqui, então um checkpoint gravado por uma é
# sempre reconhecido pela outra. As impressões digitais de implementação são
# fixas (ver o topo do arquivo); as entradas entram pelo hash de cada arquivo.
final_stage_metadata <- function(provenance) {
  input_hashes <- stats::setNames(provenance$sha256, provenance$input)
  package_versions <- c(
    R = as.character(getRversion()),
    fixest = as.character(utils::packageVersion("fixest")),
    glmmTMB = as.character(utils::packageVersion("glmmTMB"))
  )
  stage_inputs <- list(
    input_hashes = input_hashes,
    implementation_sha256 = REUSABLE_STAGE_IMPLEMENTATION_SHA,
    enatjus_sha = EXPECTED_PACKAGE_SHA,
    package_versions = package_versions
  )
  reporting_inputs <- stage_inputs
  reporting_inputs$implementation_sha256 <- REUSABLE_LIGHT_REPORTING_IMPLEMENTATION_SHA
  profile_inputs <- stage_inputs
  profile_inputs$implementation_sha256 <- REUSABLE_PROFILE_IMPLEMENTATION_SHA
  specifications <- final_estimator_specifications()
  formula_sha256 <- vapply(specifications, `[[`, character(1), "formula_digest")
  main <- c(stage_inputs, list(
    formula_sha256 = formula_sha256,
    tmb_threads = TMB_THREADS
  ))
  # A razão de verossimilhança e o perfil partem dos ajustes de `main`, então
  # a chave deles leva a de `main`: trocar só a impressão digital dos ajustes
  # já invalida os dois, em vez de reaproveitar números calculados sobre o
  # ajuste antigo.
  main_fingerprint <- checkpoint_fingerprint(main)
  list(
    descriptive = c(stage_inputs, list(
      pair_cell_floors = PAIR_CELL_FLOORS,
      pair_range_thresholds = PAIR_RANGE_THRESHOLDS,
      tmb_threads = TMB_THREADS
    )),
    main = main,
    reporting = c(reporting_inputs, list(
      main_fingerprint = main_fingerprint,
      main_formula_sha256 = formula_sha256,
      clinical_columns = clinical_columns(),
      checkpoint_schema = "lightweight_reporting_candidates_v1",
      tmb_threads = TMB_THREADS
    )),
    profile = list(
      inputs = profile_inputs,
      main_fingerprint = main_fingerprint,
      formula_sha256 = specifications$model_1_2$formula_digest,
      level = INTERVAL_LEVEL,
      ncpus = PROFILE_WORKERS,
      enatjus_sha = EXPECTED_PACKAGE_SHA
    )
  )
}

# Ajusta as etapas que a publicação lê e grava cada uma como checkpoint. É a
# única função deste script que ajusta modelo. Com checkpoint compatível (mesmas
# entradas, mesma especificação), a etapa é reaproveitada; checkpoint
# incompatível vai para uma pasta datada em backups/ e a etapa é ajustada de
# novo. A rodada completa, sem checkpoint nenhum, leva horas.
run_final_stages <- function() {
  preflight <- prepare_real_preflight()
  frame <- preflight$frame
  metadata <- final_stage_metadata(preflight$inputs$provenance)
  rm(preflight)
  invisible(gc())

  log_line("stage: descriptive")
  checkpoint_compute("stages", "descriptive", metadata$descriptive,
                     function() build_descriptive_bundle(frame))
  log_line("stage: main models")
  main <- checkpoint_compute("stages", "main-models", metadata$main,
                             function() fit_main_models(frame))
  log_line("stage: likelihood-ratio candidates")
  checkpoint_compute("stages", "reporting-candidates", metadata$reporting,
                     function() fit_reporting_candidates(main$fits, frame))
  log_line("stage: profile interval of the NatJus standard deviation")
  checkpoint_compute("profile", "model-1-2-origin-sd", metadata$profile,
                     function() profile_origin_sd_interval(
                       main$fits$model_1_2, frame,
                       level = INTERVAL_LEVEL, ncpus = PROFILE_WORKERS
                     ))
  log_line("stages done")
  invisible(TRUE)
}

run_final_analysis <- function() {
  preflight <- prepare_real_preflight()
  frame <- preflight$frame
  inputs <- list(
    production = preflight$inputs$production,
    provenance = preflight$inputs$provenance
  )
  rm(preflight)
  invisible(gc())
  metadata <- final_stage_metadata(inputs$provenance)

  log_line("loading completed checkpoints for publication without fitting")
  descriptive <- checkpoint_read("stages", "descriptive", metadata = metadata$descriptive)
  main <- checkpoint_read("stages", "main-models", metadata = metadata$main)
  reporting <- checkpoint_read("stages", "reporting-candidates", metadata = metadata$reporting)
  profile <- checkpoint_read("profile", "model-1-2-origin-sd", metadata = metadata$profile)

  log_line("publishing stamped final-analysis outputs")
  result <- publish_final_bundle(inputs, frame, descriptive, main, reporting, profile)
  log_line("done")
  invisible(result)
}

if (sys.nframe() == 0L) {
  run_final_stages()
  run_final_analysis()
}
