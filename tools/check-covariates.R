source("R/03-covariates.R")
source("R/assert-sample.R")
notes <- read_sample()
fixture <- notes[seq_len(8L), ]
fixture$txtDcb <- c("  Ácido   Teste ", "ACIDO TESTE", "B", "B", "C + D", "C + D", "VALIDO", "NÃO_PREENCHIDO")
fixture$txtCid <- c("E11.9 - diagnosis; I10 - diagnosis", "E11 - diagnosis", "C50.9", "C50", "H60.1", "H60.1", "NÃO_PREENCHIDO", "malformed")
fixture$dcb_is_anvisa_label <- c(rep(FALSE, 6L), TRUE, FALSE)
fixture$icd_recorded <- c(rep(TRUE, 6L), FALSE, TRUE)
fixture$sex_recorded <- c(rep(TRUE, 7L), FALSE)
for (field in CLINICAL_FIELDS) fixture[[field]] <- c("Sim", "Sim", "Sim", "Nao", "Nao", "Nao", "NÃO_PREENCHIDO", "NÃO_PREENCHIDO")
fixture$selRegistroAnvisa <- rep("Sim", 8L)
result <- build_covariates(fixture)
stopifnot(identical(result$medicine, c("ACIDO TESTE", "ACIDO TESTE", "B", "B", "C + D", "C + D", MISSING_VALUE, MISSING_VALUE)),
          identical(result$icd_full, c("E11.9", "E11", "C50.9", "C50", "H60.1", "H60.1", MISSING_VALUE, MISSING_VALUE)),
          identical(result$icd_category, c("E11", "E11", "C50", "C50", "H60", "H60", MISSING_VALUE, MISSING_VALUE)),
          identical(result$icd_additional_codes, c(1L, rep(0L, 7L))),
          result$cov_selStaGenero[8L] == MISSING_VALUE,
          identical(result$emission_date_strict, fixture$emission_date_strict))
stopifnot(identical(icd_chapter(c("D48", "D50", "H59", "H60", "U07.1", "D49", MISSING_VALUE)),
                    c("II", "III", "VII", "VIII", "XXII", MISSING_VALUE, MISSING_VALUE)))
stopifnot(all(normalize_category(c(SENTINEL_LEVELS, "", NA, "NÃO PREENCHIDO")) == MISSING_VALUE),
          identical(normalize_category(c("Sim", "Nao")), c("Sim", "Nao")))
assert_covariates(fixture, result)
assert_covariates(fixture[c(5L, 2L, 8L), ], build_covariates(fixture[c(5L, 2L, 8L), ]))
stopifnot(inherits(try(build_covariates(fixture[c(1L, 1L), ]), silent = TRUE), "try-error"),
          inherits(try(build_covariates(result), silent = TRUE), "try-error"))
clinical <- clinical_diagnostics(result)
s <- filter(clinical$clinical_summary, scope == "all_notes", variable == CLINICAL_FIELDS[1L])
stopifnot(s$unanimous_notes == 6L, s$unanimous_missing_notes == 2L,
          s$substantive_unanimous_notes == 4L)
constant <- filter(clinical$clinical_indicators, variable == "selRegistroAnvisa")
stopifnot(all(is.na(constant$r_squared)))
for (i in seq_len(nrow(clinical$clinical_indicators))) {
  row <- clinical$clinical_indicators[i, ]
  data <- if (row$scope == "all_notes") result else filter(result, medicine != MISSING_VALUE)
  y <- as.integer(data[[paste0("cov_", row$variable)]] == row$level)
  if (length(unique(y)) == 1L) next
  fit <- stats::lm(y ~ factor(data$medicine))
  actual <- 1 - sum(residuals(fit)^2) / sum((y - mean(y))^2)
  stopifnot(isTRUE(all.equal(actual, row$r_squared, tolerance = 1e-12)))
}
x <- c(rep("A", 10L), "B", "C", MISSING_VALUE)
map <- rare_level_map(x, "other", 10L)
stopifnot(sum(apply_rare_levels(x, map) != x) == 2L,
          identical(apply_rare_levels(x, rare_level_map(x, "none", 10L)), x),
          apply_rare_levels(x, map)[13L] == MISSING_VALUE,
          sum(apply_rare_levels(x, rare_level_map(x, "other", 10L, protect_missing = FALSE)) != x) == 3L,
          inherits(try(apply_rare_levels("unseen", map), silent = TRUE), "try-error"))
lexical <- result
lexical$medicine <- c("CLORIDRATO DE TESTE", "TESTE", "A + B", "B + A", "A", "B", "C", "D")
candidates <- medicine_candidates(lexical)$medicine_candidates
stopifnot(sum(candidates$kind == "salt_signature") == 2L,
          sum(candidates$kind == "association_signature") == 2L,
          !any(candidates$medicine %in% c("A", "B")))
compact <- fixture
compact$txtCid <- c("C509; F840", "C50.9; F84.0", "C50", "C50 - diagnosis",
                    "E1190", "E11.90", "NÃO_PREENCHIDO", "malformed")
compact_result <- build_covariates(compact)
stopifnot(identical(compact_result$icd_full,
                    c("C50.9", "C50.9", "C50", "C50", "E11.90", "E11.90", MISSING_VALUE, MISSING_VALUE)),
          identical(compact_result$icd_category,
                    c("C50", "C50", "C50", "C50", "E11", "E11", MISSING_VALUE, MISSING_VALUE)),
          identical(compact_result$icd_additional_codes, c(1L, 1L, rep(0L, 6L))))
assert_covariates(compact, compact_result)
nullable <- fixture
nullable$txtCid[1L] <- NA_character_
nullable$icd_recorded[1L] <- NA
nullable$emission_date_strict[1L] <- NA
nullable_result <- build_covariates(nullable)
nullable_tables <- covariate_diagnostics(nullable, nullable_result)
stopifnot(!anyNA(nullable_tables$sample_flags$flagged_notes),
          nullable_tables$icd_summary$recorded_but_no_code_notes == 1L,
          sum(filter(nullable_tables$sample_flags, flag == "icd_recorded")$flagged_notes) == 2L)
literal_na <- read_diagnostic_table(I("level,notes\nNA,114\nN/I,787\n"))
stopifnot(identical(literal_na$level, c("NA", "N/I")))
cat("Covariate checks passed: row preservation, sentinels, ICD, clinical R-squared, rarity and lexical screens.\n")
