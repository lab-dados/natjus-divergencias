# Probabilidade padronizada por origem dentro de cada manifestação da Conitec ---
#
# O corpo do manuscrito reporta, para cada NatJus, a taxa de favoráveis que ele
# teria sobre o mix de casos da amostra inteira (a probabilidade padronizada de
# R/08). Este script repete essa padronização dentro de cada manifestação
# substantiva da Conitec: as notas de um estrato são preditas sob cada origem
# com o ajuste salvo do Modelo 1.2, mantidas todas as outras covariáveis e os
# interceptos de medicamento e de CID, e depois promediadas. Nenhum modelo é
# reajustado; o ajuste salvo da rodada definitiva é lido de data/model-fits/.
#
# Como o Modelo 1.2 não tem interação origem × Conitec, os três pontos
# padronizados de uma origem se movem em paralelo na escala logito: a figura
# que isto alimenta mostra que a dispersão entre origens sobrevive aos
# controles dentro de cada estrato, não que as origens reajam de forma
# diferente à Conitec. As taxas brutas na mesma tabela carregam essa segunda
# leitura, e os modelos de inclinação por origem de R/12 a estimam.
#
# Saída: tables/analysis/kit-conitec-stratified-standardization.csv, carimbada
# com os hashes das entradas definitivas e o commit do pacote, sob o prefixo
# `kit-` para ficar fora do manifesto que R/08 reconcilia.
#
# Rodar a partir da raiz do repositório, depois de R/08 ter publicado:
#
#     Rscript R/11-conitec-stratified-standardization.R

source("R/08-final-analysis.R")

STRATIFIED_PATH <- file.path("tables", "analysis", "kit-conitec-stratified-standardization.csv")
MODEL_1_2_PATH <- file.path(MODEL_FIT_DIR, "final-analysis-model-1-2.rds")

stratified_standardization <- function(model, frame, conitec_column) {
  origins <- levels(frame$origin)
  # O nível auxiliar N/I não tem nota na população final; os estratos são as
  # manifestações substantivas que ocorrem.
  strata <- model_contract()$factors$conitec$substantive_levels
  strata <- strata[strata %in% unique(as.character(frame[[conitec_column]]))]
  dplyr::bind_rows(lapply(strata, function(level) {
    subset <- frame[as.character(frame[[conitec_column]]) == level, , drop = FALSE]
    standardized <- standardize_estimable_quietly(model, subset, origins, "origin")
    raw <- subset |>
      dplyr::group_by(origin = as.character(origin)) |>
      dplyr::summarise(notes = dplyr::n(), favourable = sum(y), .groups = "drop")
    tibble::tibble(
      model = "model_1_2",
      conitec_level = level,
      origin = origins,
      # As estimativas voltam sem nome, na ordem de `origins`.
      standardized_probability = as.double(standardized$estimates),
      standardization_coverage = standardized$coverage
    ) |>
      dplyr::left_join(raw, by = "origin") |>
      dplyr::mutate(
        notes = dplyr::coalesce(notes, 0L),
        favourable = dplyr::coalesce(favourable, 0L),
        raw_rate = ifelse(notes > 0, favourable / notes, NA_real_)
      ) |>
      dplyr::select(model, conitec_level, origin, notes, favourable, raw_rate,
                    standardized_probability, standardization_coverage)
  }))
}

# A probabilidade padronizada da amostra inteira é a média dos valores dos
# estratos ponderada por notas, porque os estratos particionam a amostra e a
# predição de uma nota não depende de em qual estrato ela é promediada.
# Comparar com a tabela publicada confere que o ajuste salvo e o quadro
# remontado são os da rodada definitiva.
check_against_published <- function(stratified, frame) {
  published <- readr::read_csv(
    file.path("tables", "analysis", "final-analysis-origin-effect-candidates.csv"),
    comment = "#", show_col_types = FALSE
  ) |>
    dplyr::filter(model == "model_1_2", anchor == "pooled_outcome_logit") |>
    dplyr::select(origin, published = standardized_probability)
  # A probabilidade padronizada de um estrato promedia sobre cada nota daquele
  # estrato, de todas as origens, então o valor publicado da amostra inteira é
  # a média dos três ponderada pelos tamanhos dos estratos no quadro inteiro,
  # não pelas notas da própria origem em cada estrato.
  stratum_weights <- stratified |>
    dplyr::group_by(conitec_level) |>
    dplyr::summarise(stratum_notes = sum(notes), .groups = "drop")
  recomposed <- stratified |>
    dplyr::inner_join(stratum_weights, by = "conitec_level") |>
    dplyr::group_by(origin) |>
    dplyr::summarise(recomposed = sum(stratum_notes * standardized_probability) / sum(stratum_notes), .groups = "drop")
  # Cada nota fica em exatamente um estrato, então as contagens dos estratos somam o quadro.
  if (sum(stratified$notes) != nrow(frame)) {
    stop("The Conitec strata do not partition the frame.", call. = FALSE)
  }
  comparison <- dplyr::inner_join(published, recomposed, by = "origin")
  gap <- max(abs(comparison$published - comparison$recomposed))
  if (nrow(comparison) != nrow(published) || gap > 1e-6) {
    stop("Stratified standardization does not recompose the published values (max gap ", gap, ").", call. = FALSE)
  }
  gap
}

run_stratified_standardization <- function() {
  inputs <- read_final_inputs()
  frame <- prepare_final_frame(inputs$covariates)
  log_line("frame rebuilt: ", nrow(frame), " notes, ", nlevels(frame$origin), " origins")
  model <- readRDS(MODEL_1_2_PATH)$fit$model
  conitec_column <- model_contract()$factors$conitec$column
  log_line("standardizing within each Conitec manifestation")
  stratified <- stratified_standardization(model, frame, conitec_column)
  # As predições levam minutos; guardá-las antes da conferência para que uma
  # conferência que falhe possa ser inspecionada sem recalcular.
  saveRDS(stratified, file.path(MODEL_FIT_DIR, "kit-conitec-stratified-standardization.rds"))
  gap <- check_against_published(stratified, frame)
  log_line("recomposition check passed, max gap ", format(gap, digits = 3))
  write_stamped_csv(stratified, STRATIFIED_PATH, inputs$provenance)
  log_line("wrote ", STRATIFIED_PATH)
  invisible(stratified)
}

if (sys.nframe() == 0L) {
  run_stratified_standardization()
}
