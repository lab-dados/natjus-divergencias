suppressPackageStartupMessages(library(dplyr))
source("R/enatjus.R")

MISSING_VALUE <- MISSING_LABEL
CLINICAL_FIELDS <- CLINICAL_COVARIATES
CATEGORY_FIELDS <- c(names(EXPECTED_LEVELS), "selSituacaoAnvisa")
ICD_PATTERN <- "\\b[A-Z][0-9]{2}(?:\\.[0-9]{1,2}|[0-9]{1,2})?\\b"

normalize_spelling <- function(x) {
  stringr::str_squish(flatten_label(as.character(x)))
}

missing_text <- function(x) {
  normalized <- normalize_spelling(x)
  sentinels <- SENTINEL_LEVELS
  vocabulary <- normalize_spelling(c(sentinels, gsub("_", " ", sentinels), MISSING_VALUE))
  is.na(x) | normalized == "" | normalized %in% vocabulary
}

normalize_category <- function(x) {
  ifelse(missing_text(x), MISSING_VALUE, as.character(x))
}

icd_chapter <- function(code) {
  category <- substr(code, 1L, 3L)
  number <- match(substr(category, 1L, 1L), LETTERS) * 100L +
    suppressWarnings(as.integer(substr(category, 2L, 3L)))
  bounds <- tibble::tribble(
    ~first, ~last, ~chapter,
    "A00", "B99", "I", "C00", "D48", "II", "D50", "D89", "III",
    "E00", "E90", "IV", "F00", "F99", "V", "G00", "G99", "VI",
    "H00", "H59", "VII", "H60", "H95", "VIII", "I00", "I99", "IX",
    "J00", "J99", "X", "K00", "K93", "XI", "L00", "L99", "XII",
    "M00", "M99", "XIII", "N00", "N99", "XIV", "O00", "O99", "XV",
    "P00", "P96", "XVI", "Q00", "Q99", "XVII", "R00", "R99", "XVIII",
    "S00", "T98", "XIX", "V01", "Y98", "XX", "Z00", "Z99", "XXI",
    "U00", "U99", "XXII"
  )
  encode <- function(x) match(substr(x, 1L, 1L), LETTERS) * 100L +
    as.integer(substr(x, 2L, 3L))
  result <- rep(MISSING_VALUE, length(code))
  for (i in seq_len(nrow(bounds))) {
    inside <- !is.na(number) & number >= encode(bounds$first[i]) &
      number <= encode(bounds$last[i]) & code != MISSING_VALUE
    result[inside] <- bounds$chapter[i]
  }
  result
}

assert_covariates <- function(notes, result) {
  stopifnot(!anyNA(notes$idNotaTecnica), !anyDuplicated(notes$idNotaTecnica),
            identical(notes$idNotaTecnica, result$idNotaTecnica),
            nrow(notes) == nrow(result), all(names(notes) %in% names(result)))
  stopifnot(all(vapply(names(notes), function(name) {
    identical(notes[[name]], result[[name]])
  }, logical(1))))
  invisible(result)
}

build_covariates <- function(notes) {
  result <- notes
  new_names <- c(paste0("cov_", CATEGORY_FIELDS), "medicine", "icd_full",
                 "icd_category", "icd_chapter", "icd_codes_detected",
                 "icd_additional_codes", "medicine_icd")
  if (any(new_names %in% names(notes))) stop("Input already contains derived covariates.")
  for (field in CATEGORY_FIELDS) result[[paste0("cov_", field)]] <- normalize_category(notes[[field]])
  result$cov_selStaGenero[!notes$sex_recorded | is.na(notes$sex_recorded)] <- MISSING_VALUE
  medicine <- normalize_spelling(notes$txtDcb)
  medicine[missing_text(notes$txtDcb) | notes$dcb_is_anvisa_label %in% TRUE] <- MISSING_VALUE
  result$medicine <- medicine
  codes <- stringr::str_extract_all(toupper(notes$txtCid), ICD_PATTERN)
  codes <- lapply(codes, function(x) x[!is.na(x)])
  result$icd_codes_detected <- lengths(codes)
  result$icd_additional_codes <- pmax(lengths(codes) - 1L, 0L)
  first <- vapply(codes, function(x) if (length(x)) x[1L] else MISSING_VALUE, character(1))
  first[!notes$icd_recorded | is.na(notes$icd_recorded) | missing_text(notes$txtCid)] <- MISSING_VALUE
  result$icd_full <- sub("^([A-Z][0-9]{2})([0-9]{1,2})$", "\\1.\\2", first)
  result$icd_category <- ifelse(first == MISSING_VALUE, MISSING_VALUE, substr(first, 1L, 3L))
  result$icd_chapter <- icd_chapter(first)
  # O prefixo de comprimento torna o par inequívoco mesmo quando o nome do medicamento tem pontuação.
  result$medicine_icd <- paste0(nchar(medicine), ":", medicine, "|", result$icd_category)
  assert_covariates(notes, result)
  result
}

clinical_diagnostics <- function(covariates) {
  summaries <- indicators <- list()
  for (scope in c("all_notes", "known_medicine")) {
    data <- if (scope == "all_notes") covariates else
      filter(covariates, medicine != MISSING_VALUE)
    for (field in CLINICAL_FIELDS) {
      cells <- data |>
        count(medicine, level = .data[[paste0("cov_", field)]], name = "notes")
      groups <- cells |>
        group_by(medicine) |>
        summarise(notes = sum(notes), levels = n(),
                  only_missing = all(level == MISSING_VALUE), .groups = "drop")
      n <- nrow(data)
      unanimous <- sum(groups$notes[groups$levels == 1L])
      missing_unanimous <- sum(groups$notes[groups$only_missing])
      summaries[[length(summaries) + 1L]] <- tibble(
        scope, variable = field, denominator = n, medicines = nrow(groups),
        missing_notes = sum(cells$notes[cells$level == MISSING_VALUE]),
        unanimous_notes = unanimous, unanimous_fraction = if (n) unanimous / n else NA_real_,
        unanimous_missing_notes = missing_unanimous,
        substantive_unanimous_notes = unanimous - missing_unanimous,
        unknown_medicine_notes_excluded = nrow(covariates) - n
      )
      for (level in sort(unique(cells$level))) {
        by_group <- groups |>
          select(medicine, total = notes) |>
          left_join(filter(cells, .data$level == .env$level) |>
                      select(medicine, successes = notes), by = "medicine") |>
          mutate(successes = coalesce(successes, 0L))
        successes <- sum(by_group$successes)
        total_ss <- if (n) successes * (1 - successes / n) else 0
        # O valor ajustado de uma dummy de grupo é a média dele, então nenhuma matriz de desenho densa é necessária.
        residual_ss <- sum(by_group$successes * (1 - by_group$successes / by_group$total))
        indicators[[length(indicators) + 1L]] <- tibble(
          scope, variable = field, level, denominator = n, notes_level = successes,
          is_missing_level = level == MISSING_VALUE,
          total_ss, residual_ss,
          r_squared = if (total_ss > 0) 1 - residual_ss / total_ss else NA_real_
        )
      }
    }
  }
  list(clinical_summary = bind_rows(summaries), clinical_indicators = bind_rows(indicators))
}

rare_level_map <- function(x, treatment = c("none", "other"), floor = 100L,
                           protect_missing = TRUE, other_label = "Other (pooled rare levels)") {
  treatment <- match.arg(treatment)
  if (length(floor) != 1L || is.na(floor) || floor < 1L) stop("floor must be positive.")
  if (treatment == "other" && other_label %in% x) stop("Pooled label already exists in input.")
  tibble(level = x) |>
    count(level, name = "notes") |>
    mutate(rare = notes < floor,
           protected = protect_missing & level == MISSING_VALUE,
           mapped_level = ifelse(treatment == "other" & rare & !protected, other_label, level))
}

apply_rare_levels <- function(x, map) {
  if (anyDuplicated(map$level)) stop("Level map is not unique.")
  index <- match(x, map$level)
  if (anyNA(index)) stop("Level map does not cover input.")
  map$mapped_level[index]
}

rarity_diagnostics <- function(covariates) {
  summaries <- origins <- mappings <- list()
  for (variable in c("medicine", "icd_category")) {
    x <- covariates[[variable]]
    for (floor in c(10L, 50L, 100L)) {
      for (treatment in c("none", "other")) {
        map <- rare_level_map(x, treatment, floor)
        changed <- apply_rare_levels(x, map) != x
        summaries[[length(summaries) + 1L]] <- tibble(
          variable, treatment, floor, protect_missing = TRUE,
          denominator = length(x), levels_before = nrow(map),
          levels_after = n_distinct(map$mapped_level), rare_levels = sum(map$rare),
          rare_notes = sum(map$notes[map$rare]),
          protected_rare_notes = sum(map$notes[map$rare & map$protected]),
          affected_levels = sum(map$level != map$mapped_level), affected_notes = sum(changed)
        )
        origins[[length(origins) + 1L]] <- tibble(origin = covariates$origem_tratada, x, changed) |>
          group_by(origin) |>
          summarise(denominator = n(), levels_before = n_distinct(x),
                    affected_levels = n_distinct(x[changed]), affected_notes = sum(changed),
                    affected_fraction = mean(changed), .groups = "drop") |>
          mutate(variable, treatment, floor, protect_missing = TRUE, .before = 1L)
        mappings[[length(mappings) + 1L]] <- mutate(map, variable, treatment, floor, .before = 1L)
      }
    }
  }
  list(rarity_summary = bind_rows(summaries), rarity_by_origin = bind_rows(origins),
       rarity_level_maps = bind_rows(mappings))
}

covariate_diagnostics <- function(notes, covariates) {
  assert_covariates(notes, covariates)
  raw_variables <- c(medicine = "txtDcb", icd_full = "txtCid", icd_category = "txtCid",
                     icd_chapter = "txtCid",
                     stats::setNames(CATEGORY_FIELDS, paste0("cov_", CATEGORY_FIELDS)))
  changes <- list()
  levels <- list()
  profiles <- list()
  for (variable in names(raw_variables)) {
    raw <- as.character(notes[[raw_variables[[variable]]]])
    derived <- covariates[[variable]]
    old <- raw
    if (startsWith(variable, "icd_")) old <- stringr::str_extract(raw, "^[A-Z0-9.]+")
    if (variable == "cov_selExisteBiossimilar") {
      old <- ifelse(raw %in% SENTINEL_LEVELS, "Nao", raw)
    }
    changed <- (is.na(old) != is.na(derived)) |
      (!is.na(old) & !is.na(derived) & old != derived)
    counts <- tibble(level = derived) |> count(level, name = "notes")
    changes[[length(changes) + 1L]] <- tibble(
      variable, source_field = raw_variables[[variable]], denominator = nrow(notes),
      raw_levels = n_distinct(raw), baseline_levels = n_distinct(old),
      derived_levels = nrow(counts), levels_under_10 = sum(counts$notes < 10L),
      notes_in_levels_under_10 = sum(counts$notes[counts$notes < 10L]),
      missing_notes = sum(derived == MISSING_VALUE), changed_from_baseline = sum(changed)
    )
    levels[[length(levels) + 1L]] <- mutate(counts, variable, .before = 1L)
    profiles[[length(profiles) + 1L]] <- tibble(
      origin = notes$origem_tratada, month = notes$month_of_emission,
      old, derived, changed
    ) |>
      group_by(origin, month) |>
      summarise(denominator = n(), changed_notes = sum(changed),
                missing_notes = sum(derived == MISSING_VALUE), .groups = "drop") |>
      mutate(variable, .before = 1L)
  }
  flags <- bind_rows(lapply(c("dcb_is_anvisa_label", "icd_recorded", "sex_recorded", "emission_date_strict"), function(flag) {
    affected <- if (flag == "dcb_is_anvisa_label") notes[[flag]] %in% TRUE else !(notes[[flag]] %in% TRUE)
    tibble(origin = notes$origem_tratada, month = notes$month_of_emission, affected) |>
      group_by(origin, month) |>
      summarise(denominator = n(), flagged_notes = sum(affected), .groups = "drop") |>
      mutate(flag, .before = 1L)
  }))
  biosimilar <- tibble(raw = notes$selExisteBiossimilar,
                      legacy = ifelse(notes$selExisteBiossimilar %in% SENTINEL_LEVELS,
                                      "Nao", notes$selExisteBiossimilar),
                      derived = covariates$cov_selExisteBiossimilar) |>
    count(raw, legacy, derived, name = "notes")
  icd <- tibble(
    denominator = nrow(notes),
    multiple_code_notes = sum(covariates$icd_codes_detected > 1L),
    additional_code_occurrences = sum(covariates$icd_additional_codes),
    recorded_but_no_code_notes = sum((notes$icd_recorded %in% TRUE) & covariates$icd_full == MISSING_VALUE),
    full_code_without_chapter_notes = sum(covariates$icd_full != MISSING_VALUE &
                                          covariates$icd_chapter == MISSING_VALUE),
    first_not_at_start_notes = sum(covariates$icd_codes_detected > 0L &
                                    !stringr::str_detect(toupper(notes$txtCid), paste0("^", ICD_PATTERN)))
  )
  icd_details <- tibble(raw = notes$txtCid, full = covariates$icd_full,
                        category = covariates$icd_category, chapter = covariates$icd_chapter,
                        codes_detected = covariates$icd_codes_detected,
                        additional_codes = covariates$icd_additional_codes) |>
    count(raw, full, category, chapter, codes_detected, additional_codes, name = "notes")
  spelling <- tibble(raw = notes$txtDcb, medicine = covariates$medicine) |>
    count(raw, medicine, name = "notes") |>
    group_by(medicine) |> mutate(raw_spellings = n()) |> ungroup()
  c(list(covariate_summary = bind_rows(changes), covariate_levels = bind_rows(levels),
         covariate_by_origin_month = bind_rows(profiles), sample_flags = flags,
         biosimilar_reclassification = biosimilar, icd_summary = icd, icd_codes = icd_details,
         medicine_spelling = spelling), clinical_diagnostics(covariates),
    rarity_diagnostics(covariates), medicine_candidates(covariates))
}

medicine_candidates <- function(covariates) {
  medicines <- covariates |> count(medicine, name = "notes") |>
    filter(medicine != MISSING_VALUE)
  # Triagem só lexical: estes afixos não estabelecem equivalência farmacêutica.
  salts <- "CLORIDRATO|BROMIDRATO|DICLORIDRATO|DIBROMIDRATO|SULFATO|BISSULFATO|FOSFATO|DIFOSFATO|ACETATO|CITRATO|MALEATO|FUMARATO|SUCCINATO|TARTARATO|HEMITARTARATO|MESILATO|ESILATO|BESILATO|PAMOATO|PALMITATO|NITRATO|GLUCONATO|LACTATO|SODICO|SODICA|POTASSICO|POTASSICA|CALCICO|CALCICA"
  remove_salt <- function(x) {
    x <- stringr::str_remove(x, paste0("^(?:", salts, ")\\s+(?:DE\\s+)?"))
    stringr::str_squish(stringr::str_remove(x, paste0("\\s+(?:", salts, ")$")))
  }
  pieces <- stringr::str_split(medicines$medicine, "\\s*[+;/]\\s*")
  signature <- function(parts, remove = FALSE) {
    parts <- stringr::str_squish(parts)
    if (remove) parts <- remove_salt(parts)
    paste(sort(parts), collapse = " + ")
  }
  medicines$association_marker <- lengths(pieces) > 1L
  medicines$salt_signature <- vapply(pieces, signature, character(1), remove = TRUE)
  medicines$association_signature <- vapply(pieces, signature, character(1))
  medicines$salt_affix_detected <- vapply(pieces, function(x) {
    any(stringr::str_squish(x) != remove_salt(stringr::str_squish(x)))
  }, logical(1))
  candidates <- bind_rows(lapply(c("salt_signature", "association_signature"), function(kind) {
    medicines |>
      mutate(signature = .data[[kind]]) |>
      group_by(signature) |>
      filter(n() > 1L, if (kind == "salt_signature") any(salt_affix_detected) else any(association_marker)) |>
      mutate(candidate_levels = n(), candidate_notes = sum(notes)) |>
      ungroup() |>
      transmute(kind, signature, medicine, notes, candidate_levels, candidate_notes)
  }))
  list(medicine_lexical_screen = medicines, medicine_candidates = candidates)
}

origin_diagnostics <- function(before_floor, sample, institutions) {
  stopifnot(!anyNA(institutions$idNotaTecnica), !anyDuplicated(institutions$idNotaTecnica),
            !anyDuplicated(before_floor$idNotaTecnica))
  missing_ids <- setdiff(before_floor$idNotaTecnica, institutions$idNotaTecnica)
  if (length(missing_ids)) stop("Institution supplement does not cover the pre-floor population.")
  joined <- before_floor |>
    left_join(institutions, by = "idNotaTecnica", relationship = "one-to-one") |>
    mutate(in_sample = idNotaTecnica %in% sample$idNotaTecnica,
           institution_missing = missing_text(txtInstituicaoResponsavel),
           institution = normalize_category(txtInstituicaoResponsavel))
  stopifnot(identical(joined$idNotaTecnica, before_floor$idNotaTecnica))
  families <- list(RS = c("RS", "RS/DMJ", "TelessaúdeRS-UFRGS"),
                   PR = c("PR", "PR/CAMS", "PR/CHR", "PR/CHC-UFPR", "PR/UEL"),
                   AM = c("AM", "AM/SES", "AM/SEMSA"), SP = c("SP", "SP/HC"))
  labels <- tibble(family = rep(names(families), lengths(families)),
                   origin = unlist(families, use.names = FALSE))
  data <- joined |>
    inner_join(labels, by = c("origem_tratada" = "origin"), relationship = "many-to-one")
  monthly <- data |>
    count(family, origin = origem_tratada, month = month_of_emission,
          in_sample, name = "notes")
  months <- seq(min(before_floor$month_of_emission), max(before_floor$month_of_emission), by = "month")
  grid <- tidyr::crossing(labels, month = months) |>
    mutate(in_sample = origin %in% sample$origem_tratada)
  monthly <- grid |> left_join(monthly, by = c("family", "origin", "month", "in_sample")) |>
    mutate(notes = coalesce(notes, 0L))
  institution_counts <- data |>
    count(family, origin = origem_tratada, month = month_of_emission, in_sample,
          institution, institution_missing, name = "notes")
  summary <- data |>
    group_by(family, origin = origem_tratada) |>
    summarise(notes_before_floor = n(), notes_in_sample = sum(in_sample),
              missing_institution_notes = sum(institution_missing),
              recorded_institutions = n_distinct(institution[!institution_missing]),
              first_month = min(month_of_emission), last_month = max(month_of_emission),
              .groups = "drop")
  coverage <- tibble(denominator = nrow(before_floor), joined_rows = nrow(joined),
                    matched_ids = sum(before_floor$idNotaTecnica %in% institutions$idNotaTecnica),
                    missing_institution_notes = sum(joined$institution_missing),
                    selected_family_notes = nrow(data))
  list(origins_before_floor_monthly = monthly,
       origins_before_floor_institutions = institution_counts,
       origins_before_floor_summary = summary, origins_institution_join = coverage)
}

read_diagnostic_table <- function(path) {
  # "NA" é um rótulo literal de medicamento na fonte, diferente de célula vazia no CSV.
  readr::read_csv(path, comment = "#", na = "", trim_ws = FALSE, show_col_types = FALSE)
}

write_diagnostic_tables <- function(tables, output_dir, stamp, population, population_rows) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  provenance <- c(stamp, population = population, population_rows = as.character(population_rows))
  for (name in names(tables)) {
    path <- file.path(output_dir, paste0(gsub("_", "-", name), ".csv"))
    writeLines(c(paste0("# ", names(provenance), ": ", provenance),
                 sub("\n$", "", readr::format_csv(tables[[name]], na = ""))), path)
  }
}

run_covariates <- function() {
  sample_functions <- new.env(parent = globalenv())
  sys.source("R/01-sample.R", envir = sample_functions)
  source("R/assert-sample.R")
  canonical_paths <- c("data/sample.parquet", "tables/attrition.csv", "tables/production-by-natjus.csv")
  canonical_hashes <- tools::md5sum(canonical_paths)
  notes <- read_sample()
  base_path <- analysis_base_path()
  base <- read_analysis_base(base_path)
  stamp <- sample_functions$base_stamp(base_path, nrow(base))
  expected_hash <- sub("^# base_sha256: ", "", grep("^# base_sha256: ",
                                                         readLines("tables/attrition.csv"), value = TRUE))
  if (!identical(unname(stamp["base_sha256"]), expected_hash)) stop("Base differs from sample provenance.")
  names(stamp)[names(stamp) == "sample_built_at"] <- "diagnostics_built_at"
  stamp <- c(stamp, input_sample_sha256 = digest::digest(canonical_paths[1L], algo = "sha256", file = TRUE))
  covariates <- build_covariates(notes)
  tables <- covariate_diagnostics(notes, covariates)
  before_floor <- sample_functions$build_sample(base, apply_origin_floor = FALSE)
  eligible <- before_floor |> count(origem_tratada) |>
    filter(n >= sample_functions$ORIGIN_FLOOR) |> pull(origem_tratada)
  reconstructed <- filter(before_floor, origem_tratada %in% eligible)
  stopifnot(identical(reconstructed$idNotaTecnica, notes$idNotaTecnica))
  institutions <- arrow::read_parquet(base_path, col_select = dplyr::all_of(c("idNotaTecnica", "txtInstituicaoResponsavel")))
  origins <- origin_diagnostics(before_floor, notes, institutions)
  arrow::write_parquet(covariates, "data/covariates.parquet")
  assert_covariates(notes, arrow::read_parquet("data/covariates.parquet"))
  write_diagnostic_tables(tables, "tables", stamp, "sample_before_origin_decision", nrow(notes))
  write_diagnostic_tables(origins, "tables", stamp, "before_origin_floor", nrow(before_floor))
  stopifnot(identical(canonical_hashes, tools::md5sum(canonical_paths)))
  verification <- tibble(check = c("sample_rows", "sample_unique_ids", "sample_origins", "before_floor_rows",
                                   "institution_join_ids", "canonical_files_unchanged", "inherited_columns_unchanged"),
                         value = c(nrow(notes), n_distinct(notes$idNotaTecnica), n_distinct(notes$origem_tratada),
                                   nrow(before_floor), origins$origins_institution_join$matched_ids, 1L, 1L))
  write_diagnostic_tables(list(covariate_verification = verification), "tables", stamp,
                          "sample_and_pre_floor", nrow(notes))
  print(tables$covariate_summary, n = Inf)
  print(tables$clinical_summary, n = Inf)
  print(origins$origins_before_floor_summary, n = Inf)
  invisible(list(covariates = covariates, tables = tables, origins = origins))
}

if (sys.nframe() == 0L) run_covariates()
