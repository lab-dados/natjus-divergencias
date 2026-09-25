# Números do manuscrito como variáveis do Quarto --------------------------------
#
# Todo número que a prosa de paper.qmd enuncia é lido aqui de um CSV carimbado
# e escrito em `_variables.yml`, que o Quarto substitui onde o texto diz
# `{{< var key >}}`. Atualizar o manuscrito depois de uma nova rodada de
# análise é, portanto: rodar de novo os scripts de análise, rodar este script,
# renderizar. Nenhum número é digitado na prosa à mão.
#
# Cada variável carrega um comentário `# source:` que nomeia o arquivo, o
# filtro e a coluna de onde ela vem, para que quem lê `_variables.yml` possa
# conferir qualquer valor contra o CSV sem abrir este script. O cabeçalho
# registra o sha256 de cada entrada lida, que é o contrato de
# reprodutibilidade: mesmas entradas, mesmo arquivo (fora o carimbo de tempo).
#
# A série anual por origem precisa da própria amostra analítica, que não é
# versionada. Quando `data/analysis-sample.parquet` está presente, a série é
# recalculada e publicada como
# `tables/analysis/kit-production-by-origin-year.csv` (carimbada, prefixo
# `kit-`, fora do manifesto da rodada definitiva); do contrário, o CSV
# publicado é lido como está.
#
# Rode a partir da raiz do repositório:
#
#     Rscript R/10-manuscript-numbers.R
#
# A apresentação é em português (valores formatados, listas de nomes); as
# chaves, os nomes de coluna e os comentários gravados no arquivo de saída
# continuam em inglês.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

DIR_ANALISE <- "tables/analysis"
CAMINHO_AMOSTRA <- "data/analysis-sample.parquet"
CAMINHO_SERIE_ANUAL <- file.path(DIR_ANALISE, "kit-production-by-origin-year.csv")
# O dicionário de grafias de origem do coletor vive no repositório do próprio
# coletor, não aqui. Aponte ENATJUS_DICTIONARY para o dicionario_natjus.csv
# dele para atualizar as contagens de grafias publicadas; do contrário, o CSV
# carimbado é lido.
CAMINHO_DICIONARIO <- Sys.getenv("ENATJUS_DICTIONARY", unset = "")
CAMINHO_GRAFIAS <- file.path(DIR_ANALISE, "kit-origin-spellings.csv")
CAMINHO_SAIDA <- "_variables.yml"
# Piso de exibição por célula medicamento × CID, o mesmo que o kit de escrita usa.
PISO_EXIBICAO <- 10
# Ano de referência das comparações anuais: o último ano completo da série,
# que termina em agosto de 2026.
ANO_REFERENCIA <- 2025L
# Uma origem conta como tendo parado de alimentar a plataforma quando a última
# nota datada dela é anterior aos doze meses que fecham a série, e ela tem
# notas suficientes para "parou" significar alguma coisa.
CORTE_ABANDONO <- as.Date("2025-09-01")
MIN_NOTAS_ABANDONO <- 10L
# Número mínimo de pares extremos para uma origem ser nomeada na prosa, a
# regra que a dissertação usou para a mesma frase.
MIN_MENCAO_EXTREMOS <- 10L

caminho_final <- function(nome) file.path(DIR_ANALISE, paste0("final-analysis-", nome, ".csv"))

entradas <- character(0)
ler_csv_carimbado <- function(caminho) {
  entradas <<- union(entradas, caminho)
  readr::read_csv(caminho, comment = "#", show_col_types = FALSE)
}

fmt_int <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
fmt_pct <- function(x, digitos = 1) {
  paste0(formatC(100 * x, format = "f", digits = digitos, decimal.mark = ","), "%")
}
fmt_razao <- function(x, digitos = 1) {
  paste0(formatC(x, format = "f", digits = digitos, decimal.mark = ","), " vezes")
}
fmt_crescimento <- function(x) {
  # Variação relativa em porcentagem; a prosa diz "cresce" ou "cai", então só
  # a queda carrega sinal, o menos tipográfico.
  v <- round(100 * x)
  paste0(if (v < 0) "\u2212" else "", abs(v), "%")
}
MESES_PT <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho",
               "agosto", "setembro", "outubro", "novembro", "dezembro")
fmt_mes_ano <- function(d) {
  d <- as.Date(d)
  paste(MESES_PT[as.integer(format(d, "%m"))], "de", format(d, "%Y"))
}
fmt_dia_mes_ano <- function(d) {
  d <- as.Date(d)
  paste(as.integer(format(d, "%d")), "de", MESES_PT[as.integer(format(d, "%m"))], "de", format(d, "%Y"))
}
juntar_pt <- function(x) {
  # "a, b e c"
  n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  paste0(paste(x[-n], collapse = ", "), " e ", x[n])
}
# Como a prosa nomeia cada origem analítica.
rotulo_origem <- function(origem) {
  case_when(
    origem == "Nacional" ~ "NatJus Nacional",
    origem == "RS/DMJ" ~ "DMJ/TJRS",
    origem == "TelessaúdeRS-UFRGS" ~ "TelessaúdeRS-UFRGS",
    TRUE ~ paste("NatJus", origem)
  )
}
rotulo_origem_com_artigo <- function(origem) paste("o", rotulo_origem(origem))

variaveis <- list()
anotar <- function(chave, valor, fonte) {
  if (chave %in% names(variaveis)) stop("variável duplicada: ", chave, call. = FALSE)
  variaveis[[chave]] <<- list(valor = as.character(valor), fonte = fonte)
  invisible(valor)
}

# Coleta e amostra --------------------------------------------------------------

# O funil definitivo da rodada de análise, não o histórico de
# tables/attrition.csv; a tabela do manuscrito vem do mesmo arquivo.
caminho_funil <- file.path(DIR_ANALISE, "attrition.csv")
funil <- ler_csv_carimbado(caminho_funil)
etapa <- function(nome) {
  linha <- funil |> filter(step == nome)
  if (nrow(linha) != 1) stop("etapa do funil não encontrada: ", nome, call. = FALSE)
  linha
}
restantes <- function(nome) etapa(nome)$remaining
removidas <- function(nome) etapa(nome)$removed_false + etapa(nome)$removed_na

# A base do artigo são as notas emitidas até o fim da série: o segundo passo
# do registro, o corte de data, e não as linhas do arquivo lido.
stopifnot(startsWith(funil$step[2], "emitted by "))
anotar("base_rows", fmt_int(funil$remaining[2]),
    "tables/analysis/attrition.csv, step emitted by the series end, remaining")
anotar("attrition_rj", fmt_int(removidas("NatJus RJ excluded")),
    "tables/analysis/attrition.csv, step NatJus RJ excluded, removed_false")
anotar("attrition_no_conclusion", fmt_int(removidas("conclusion recorded")),
    "tables/analysis/attrition.csv, step conclusion recorded, removed_false")
anotar("attrition_with_conclusion", fmt_int(restantes("conclusion recorded")),
    "tables/analysis/attrition.csv, step conclusion recorded, remaining")
anotar("attrition_not_medicine", fmt_int(removidas("medicines only")),
    "tables/analysis/attrition.csv, step medicines only, removed_false")
anotar("attrition_medicines", fmt_int(restantes("medicines only")),
    "tables/analysis/attrition.csv, step medicines only, remaining")
anotar("attrition_origin_unmapped", fmt_int(removidas("origin mapped by the NatJus dictionary")),
    "tables/analysis/attrition.csv, step origin mapped by the NatJus dictionary, removed_false")
anotar("attrition_no_origin", fmt_int(removidas("note carries an origin")),
    "tables/analysis/attrition.csv, step note carries an origin, removed_false")
anotar("attrition_no_date", fmt_int(removidas("emission date recorded")),
    "tables/analysis/attrition.csv, step emission date recorded, removed_false")
anotar("attrition_before_series", fmt_int(removidas("series starts 2019-05")),
    "tables/analysis/attrition.csv, step series starts 2019-05, removed_false")
anotar("attrition_below_floor", fmt_int(removidas("analytical origins with at least 100 notes")),
    "tables/analysis/attrition.csv, step analytical origins with at least 100 notes, removed_false")
# Uma linha exibida na tabela do manuscrito soma etapas adjacentes do
# registro: as notas sem NatJus identificado (campo bruto não preenchido, ou o
# marcador de "não informado").
anotar("attrition_no_origin_total",
    fmt_int(removidas("origin mapped by the NatJus dictionary") + removidas("note carries an origin")),
    "tables/analysis/attrition.csv, steps origin mapped by the NatJus dictionary and note carries an origin, removed_false summed")
total_amostra <- restantes("analytical origins with at least 100 notes")
anotar("sample_total", fmt_int(total_amostra),
    "tables/analysis/attrition.csv, last step, remaining")
# O abstract em inglês cita o mesmo total, com a vírgula de milhar do inglês.
anotar("sample_total_en", format(total_amostra, big.mark = ",", trim = TRUE),
    "tables/analysis/attrition.csv, last step, remaining, English thousands separator")
anotar("sample_total_rounded", paste0(floor(total_amostra / 10000) * 10, " mil"),
    "tables/analysis/attrition.csv, last step, remaining, rounded down to ten thousands")

producao <- ler_csv_carimbado(file.path(DIR_ANALISE, "production-by-analysis-origin.csv"))
na_amostra <- producao |> filter(inclusion == "in sample")
anotar("n_origins", nrow(na_amostra),
    "tables/analysis/production-by-analysis-origin.csv, rows with inclusion in sample")
anotar("n_local_origins", sum(na_amostra$origin != "Nacional"),
    "tables/analysis/production-by-analysis-origin.csv, rows in sample except Nacional")
origens_rs <- c("RS/DMJ", "TelessaúdeRS-UFRGS")
anotar("n_state_origins_outside_rs", sum(!na_amostra$origin %in% c("Nacional", origens_rs)),
    "tables/analysis/production-by-analysis-origin.csv, rows in sample except Nacional, RS/DMJ and TelessaúdeRS-UFRGS")
anotar("n_origins_below_floor", sum(producao$inclusion == "below floor of 100"),
    "tables/analysis/production-by-analysis-origin.csv, rows with inclusion below floor of 100")
anotar("origins_below_floor", juntar_pt(producao$origin[producao$inclusion == "below floor of 100"]),
    "tables/analysis/production-by-analysis-origin.csv, origin of rows below floor of 100")
inicio_serie <- min(as.Date(na_amostra$first_month))
fim_serie <- max(as.Date(na_amostra$last_month))
anotar("series_start", fmt_mes_ano(inicio_serie),
    "tables/analysis/production-by-analysis-origin.csv, min first_month in sample")
anotar("series_end", fmt_mes_ano(fim_serie),
    "tables/analysis/production-by-analysis-origin.csv, max last_month in sample")
dia_fim_serie <- seq(fim_serie, by = "1 month", length.out = 2)[2] - 1
anotar("series_end_day", fmt_dia_mes_ano(dia_fim_serie),
    "tables/analysis/production-by-analysis-origin.csv, last day of max last_month in sample")
indice_mes_max <- (as.integer(format(fim_serie, "%Y")) - as.integer(format(inicio_serie, "%Y"))) * 12 +
  as.integer(format(fim_serie, "%m")) - as.integer(format(inicio_serie, "%m"))
anotar("month_index_max", indice_mes_max,
    "tables/analysis/production-by-analysis-origin.csv, months from min first_month to max last_month")

# O número de níveis de medicamento e de categoria CID, citado na tabela de
# variáveis do apêndice (tables/manuscript/kit-variables.md, escrita à mão).
niveis <- ler_csv_carimbado(file.path(DIR_ANALISE, "covariate-levels.csv"))
anotar("medicine_levels", fmt_int(sum(niveis$variable == "medicine")),
    "tables/analysis/covariate-levels.csv, rows with variable medicine")
anotar("icd_categories", fmt_int(sum(niveis$variable == "icd_category")),
    "tables/analysis/covariate-levels.csv, rows with variable icd_category")

# Identificação institucional -----------------------------------------------------

fluxo <- ler_csv_carimbado(file.path(DIR_ANALISE, "origin-flow.csv"))
recebidas <- function(nome) fmt_int(fluxo$notes_before_floor[fluxo$origin_received == nome])
anotar("received_pr", recebidas("PR"), "tables/analysis/origin-flow.csv, origin_received PR, notes_before_floor")
anotar("received_pr_cams", recebidas("PR/CAMS"), "tables/analysis/origin-flow.csv, origin_received PR/CAMS, notes_before_floor")
anotar("received_pr_chr", recebidas("PR/CHR"), "tables/analysis/origin-flow.csv, origin_received PR/CHR, notes_before_floor")
anotar("received_pr_uel", recebidas("PR/UEL"), "tables/analysis/origin-flow.csv, origin_received PR/UEL, notes_before_floor")
anotar("received_pr_chc_ufpr", recebidas("PR/CHC-UFPR"), "tables/analysis/origin-flow.csv, origin_received PR/CHC-UFPR, notes_before_floor")
anotar("received_am_semsa", recebidas("AM/SEMSA"), "tables/analysis/origin-flow.csv, origin_received AM/SEMSA, notes_before_floor")
anotar("received_am_ses", recebidas("AM/SES"), "tables/analysis/origin-flow.csv, origin_received AM/SES, notes_before_floor")
anotar("received_am", recebidas("AM"), "tables/analysis/origin-flow.csv, origin_received AM, notes_before_floor")
anotar("received_sp_hc", recebidas("SP/HC"), "tables/analysis/origin-flow.csv, origin_received SP/HC, notes_before_floor")
anotar("received_rs", recebidas("RS"), "tables/analysis/origin-flow.csv, origin_received RS, notes_before_floor")

# Série anual por origem, ano de referência 2025 --------------------------------------

if (file.exists(CAMINHO_AMOSTRA)) {
  amostra <- arrow::read_parquet(CAMINHO_AMOSTRA, col_select = c("analysis_origin", "month_of_emission"))
  anual <- amostra |>
    mutate(year = as.integer(substr(as.character(month_of_emission), 1, 4))) |>
    count(year, origin = analysis_origin, name = "notes") |>
    arrange(year, desc(notes), origin)
  sha_amostra <- digest::digest(CAMINHO_AMOSTRA, algo = "sha256", file = TRUE)
  cabecalho <- c(
    paste0("# generated_at: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("# source: ", CAMINHO_AMOSTRA),
    paste0("# source_sha256: ", sha_amostra),
    paste0("# source_rows: ", nrow(amostra)),
    "# year: calendar year of month_of_emission; the last year of the series is partial"
  )
  writeLines(c(cabecalho, readr::format_csv(anual)), CAMINHO_SERIE_ANUAL)
  cat("gravou ", CAMINHO_SERIE_ANUAL, "\n", sep = "")
}
anual <- ler_csv_carimbado(CAMINHO_SERIE_ANUAL)
stopifnot(sum(anual$notes) == total_amostra)

# Grafias do campo de instituição mapeadas para o Nacional ------------------------
#
# A chave do dicionário é "campo NatJus - campo instituição" como recebida. A
# prosa conta quantas grafias distintas do campo de instituição designam o
# Hospital Israelita Albert Einstein entre as chaves mapeadas para Nacional:
# variantes de caixa, abreviações (HIAE) e erros de digitação de Einstein ou
# Israelita. A chave "Albert Sabin" é outro hospital e fica de fora pelo
# padrão de busca.

if (nzchar(CAMINHO_DICIONARIO) && file.exists(CAMINHO_DICIONARIO)) {
  dicionario <- readr::read_csv(CAMINHO_DICIONARIO, show_col_types = FALSE)
  grafias <- dicionario |>
    filter(natjus == "Nacional") |>
    mutate(
      # Divide no primeiro " - ": a parte do NatJus pode conter hífen
      # ("Nat-Jus Nacional"), então uma divisão gulosa em "-" guardaria a
      # chave inteira.
      institution_spelling = trimws(sub("^.*? - ", "", origem, perl = TRUE)),
      notes = as.integer(count)
    ) |>
    filter(grepl("einst|eistein|eintein|eisntein|hiae|israelit|israelirt", institution_spelling, ignore.case = TRUE)) |>
    group_by(natjus, institution_spelling) |>
    summarise(notes = sum(notes), .groups = "drop") |>
    arrange(desc(notes), institution_spelling)
  sha_dicionario <- digest::digest(CAMINHO_DICIONARIO, algo = "sha256", file = TRUE)
  cabecalho <- c(
    paste0("# generated_at: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    "# source: the collector's dicionario_natjus.csv (not versioned here)",
    paste0("# source_sha256: ", sha_dicionario),
    paste0("# source_rows: ", nrow(dicionario)),
    "# rows: distinct spellings of the institution field, among keys mapped to Nacional, that designate the Einstein"
  )
  writeLines(c(cabecalho, readr::format_csv(grafias)), CAMINHO_GRAFIAS)
  cat("gravou ", CAMINHO_GRAFIAS, "\n", sep = "")
}
grafias <- ler_csv_carimbado(CAMINHO_GRAFIAS)
anotar("einstein_spellings", fmt_int(nrow(grafias)),
    "tables/analysis/kit-origin-spellings.csv, number of rows")
anotar("einstein_notes", fmt_int(sum(grafias$notes)),
    "tables/analysis/kit-origin-spellings.csv, sum of notes")

por_ano <- anual |>
  group_by(year) |>
  summarise(
    national = sum(notes[origin == "Nacional"]),
    local = sum(notes[origin != "Nacional"]),
    .groups = "drop"
  ) |>
  arrange(year) |>
  mutate(
    ratio = national / local,
    national_growth = national / lag(national) - 1,
    local_growth = local / lag(local) - 1
  )
linha_ano <- function(y) por_ano |> filter(year == y)
anos <- por_ano$year
for (y in anos) {
  r <- linha_ano(y)
  anotar(paste0("local_notes_", y), fmt_int(r$local),
      sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, sum of notes of origins other than Nacional", y))
  anotar(paste0("national_notes_", y), fmt_int(r$national),
      sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, notes of Nacional", y))
  anotar(paste0("national_local_ratio_", y), fmt_razao(r$ratio),
      sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, Nacional divided by the other origins", y))
  if (!is.na(r$national_growth)) {
    anotar(paste0("national_growth_", y), fmt_crescimento(r$national_growth),
        sprintf("tables/analysis/kit-production-by-origin-year.csv, Nacional in %d over %d, minus one", y, y - 1))
  }
}
anotar("reference_year", ANO_REFERENCIA, "constant of R/10-manuscript-numbers.R: last complete year of the series")
anotar("local_notes_reference_year", fmt_int(linha_ano(ANO_REFERENCIA)$local),
    sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, sum of notes of origins other than Nacional", ANO_REFERENCIA))
anotar("national_local_ratio_reference_year", fmt_razao(linha_ano(ANO_REFERENCIA)$ratio),
    sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, Nacional divided by the other origins", ANO_REFERENCIA))
anotar("national_growth_reference_year", fmt_crescimento(linha_ano(ANO_REFERENCIA)$national_growth),
    sprintf("tables/analysis/kit-production-by-origin-year.csv, Nacional in %d over %d, minus one", ANO_REFERENCIA, ANO_REFERENCIA - 1))
anos_nacional_lidera <- anos[por_ano$ratio > 1]
anotar("years_national_above_locals", juntar_pt(as.character(anos_nacional_lidera)),
    "tables/analysis/kit-production-by-origin-year.csv, years in which Nacional exceeds the sum of the other origins")
anotar("years_locals_above_national", juntar_pt(as.character(anos[por_ano$ratio < 1])),
    "tables/analysis/kit-production-by-origin-year.csv, years in which the other origins exceed Nacional")
texto_serie_local <- por_ano |>
  filter(year >= 2022, year <= ANO_REFERENCIA) |>
  mutate(text = paste0(fmt_int(local), " em ", year)) |>
  pull(text) |>
  juntar_pt()
anotar("local_notes_2022_to_reference_year", texto_serie_local,
    sprintf("tables/analysis/kit-production-by-origin-year.csv, sum of notes of origins other than Nacional, 2022 to %d", ANO_REFERENCIA))

ranqueado <- anual |>
  filter(origin != "Nacional") |>
  group_by(year) |>
  arrange(desc(notes), origin, .by_group = TRUE) |>
  mutate(rank_local = row_number()) |>
  ungroup()
topo_referencia <- ranqueado |> filter(year == ANO_REFERENCIA, rank_local <= 3)
anotar("top_local_origins_reference_year", juntar_pt(rotulo_origem_com_artigo(topo_referencia$origin)),
    sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, three largest origins other than Nacional", ANO_REFERENCIA))
anotar("top_local_origins_reference_year_notes", juntar_pt(fmt_int(topo_referencia$notes)),
    sprintf("tables/analysis/kit-production-by-origin-year.csv, year %d, notes of the three largest origins other than Nacional", ANO_REFERENCIA))
ordinal_pt <- function(n) paste0(n, "ª")
origem_ano <- function(origem, ano) {
  r <- ranqueado |> filter(.data$origin == .env$origem, .data$year == .env$ano)
  if (nrow(r) == 0) list(notas = 0L, posto = NA_integer_) else list(notas = r$notes, posto = r$rank_local)
}
chave_de <- function(origem) tolower(gsub("[^A-Za-z0-9]+", "_", iconv(origem, to = "ASCII//TRANSLIT")))
# Origens que a prosa de produção nomeia ano a ano: as que caíram (BA, GO, MS,
# TelessaúdeRS-UFRGS), as que compensaram (SP, PR, MT, ES) e duas pequenas. O
# último ano da série, 2026, é parcial (até agosto) e a prosa diz isso onde
# quer que o cite.
for (origem in c("BA", "GO", "MS", "TelessaúdeRS-UFRGS", "SC", "PI", "SP", "PR", "MT", "ES")) {
  for (y in 2021:2026) {
    oa <- origem_ano(origem, y)
    anotar(sprintf("notes_%s_%d", chave_de(origem), y), fmt_int(oa$notas),
        sprintf("tables/analysis/kit-production-by-origin-year.csv, origin %s, year %d, notes (0 when absent)", origem, y))
    anotar(sprintf("rank_local_%s_%d", chave_de(origem), y),
        if (is.na(oa$posto)) "sem nota" else ordinal_pt(oa$posto),
        sprintf("tables/analysis/kit-production-by-origin-year.csv, origin %s, year %d, rank among origins other than Nacional", origem, y))
  }
}
anos_ba_maior_local <- ranqueado |> filter(origin == "BA", rank_local == 1) |> pull(year)
anotar("years_ba_first_local", juntar_pt(as.character(anos_ba_maior_local)),
    "tables/analysis/kit-production-by-origin-year.csv, years in which BA is the largest origin other than Nacional")

# Origens que pararam de alimentar a plataforma: última nota datada na base
# inteira, tirada da tabela institucional, porque as origens abaixo do piso não
# estão na amostra analítica.
instituicoes <- ler_csv_carimbado(file.path("tables", "institutions.csv"))
subinstituicoes <- instituicoes$origin[grepl("/", instituicoes$origin, fixed = TRUE) &
                                        !instituicoes$origin %in% c("RS/DMJ", "AM/SES", "AM/SEMSA")]
abandonadas <- instituicoes |>
  filter(!origin %in% c("RJ", subinstituicoes), notes_base >= MIN_NOTAS_ABANDONO,
         as.Date(last_note) < CORTE_ABANDONO) |>
  arrange(desc(notes_base))
anotar("abandoned_origins", juntar_pt(rotulo_origem(abandonadas$origin)),
    sprintf("tables/institutions.csv, origins with last_note before %s and at least %d notes in the base, excluding RJ and the sub-institutions of PR and SP", CORTE_ABANDONO, MIN_NOTAS_ABANDONO))
anotar("abandoned_origins_since", juntar_pt(paste0(rotulo_origem(abandonadas$origin), " desde ", fmt_mes_ano(abandonadas$last_note))),
    "tables/institutions.csv, same rows, last_note")
mg <- instituicoes |> filter(origin == "MG")
anotar("mg_notes_base", fmt_int(mg$notes_base), "tables/institutions.csv, origin MG, notes_base")
anotar("mg_notes_medicines", fmt_int(producao$notes_medicines[producao$origin == "MG"]),
    "tables/analysis/production-by-analysis-origin.csv, origin MG, notes_medicines")
anotar("mg_first_note", fmt_mes_ano(mg$first_note), "tables/institutions.csv, origin MG, first_note")
anotar("mg_last_note", fmt_mes_ano(mg$last_note), "tables/institutions.csv, origin MG, last_note")
ap <- instituicoes |> filter(origin == "AP")
anotar("ap_last_note", fmt_mes_ano(ap$last_note), "tables/institutions.csv, origin AP, last_note")

# Resultados descritivos por origem ---------------------------------------------------

taxas <- ler_csv_carimbado(caminho_final("descriptive-production")) |> arrange(rate)
menor <- taxas[1, ]
segunda_menor <- taxas[2, ]
anotar("lowest_rate_origin", rotulo_origem(menor$origin),
    "tables/analysis/final-analysis-descriptive-production.csv, origin with the lowest rate")
anotar("lowest_rate", fmt_pct(menor$rate),
    "tables/analysis/final-analysis-descriptive-production.csv, lowest rate")
anotar("lowest_rate_share_of_second", fmt_pct(menor$rate / segunda_menor$rate, 0),
    "tables/analysis/final-analysis-descriptive-production.csv, lowest rate divided by the second lowest")
anotar("second_lowest_rate_origin", rotulo_origem(segunda_menor$origin),
    "tables/analysis/final-analysis-descriptive-production.csv, origin with the second lowest rate")
tres_maiores <- taxas |> arrange(desc(rate)) |> slice_head(n = 3)
anotar("top_rate_origins", juntar_pt(rotulo_origem(tres_maiores$origin)),
    "tables/analysis/final-analysis-descriptive-production.csv, three origins with the highest rate")
anotar("top_rate_origins_ratio_to_lowest",
    paste0("entre ", sub(" vezes$", "", fmt_razao(min(tres_maiores$rate) / menor$rate)), " e ", fmt_razao(max(tres_maiores$rate) / menor$rate)),
    "tables/analysis/final-analysis-descriptive-production.csv, rates of the three highest divided by the lowest")
anotar("highest_rate_origin", rotulo_origem(tres_maiores$origin[1]),
    "tables/analysis/final-analysis-descriptive-production.csv, origin with the highest rate")
anotar("highest_rate", fmt_pct(tres_maiores$rate[1]),
    "tables/analysis/final-analysis-descriptive-production.csv, highest rate")
taxa_nacional <- taxas$rate[taxas$origin == "Nacional"]
anotar("national_rate", fmt_pct(taxa_nacional),
    "tables/analysis/final-analysis-descriptive-production.csv, origin Nacional, rate")
anotar("locals_above_national", sum(taxas$rate > taxa_nacional & taxas$origin != "Nacional"),
    "tables/analysis/final-analysis-descriptive-production.csv, origins other than Nacional with rate above Nacional")

anotar("sample_rate", fmt_pct(sum(taxas$favourable) / sum(taxas$notes)),
    "tables/analysis/final-analysis-descriptive-production.csv, sum of favourable over sum of notes")
# O Nacional é a maior parte da amostra, então a taxa agregada fica perto da
# dele; a taxa agregada dos NatJus locais é a referência com que a prosa a
# contrasta.
so_locais <- taxas |> filter(origin != "Nacional")
anotar("local_rate", fmt_pct(sum(so_locais$favourable) / sum(so_locais$notes)),
    "tables/analysis/final-analysis-descriptive-production.csv, origins other than Nacional, sum of favourable over sum of notes")
anotar("local_notes_sample", fmt_int(sum(so_locais$notes)),
    "tables/analysis/final-analysis-descriptive-production.csv, origins other than Nacional, sum of notes")
maior_local <- taxas |> filter(origin != "Nacional") |> arrange(desc(notes)) |> slice_head(n = 1)
anotar("largest_local_origin", rotulo_origem(maior_local$origin),
    "tables/analysis/final-analysis-descriptive-production.csv, origin other than Nacional with most notes")
# As duas instituições do Rio Grande do Sul somadas: o estado, não a
# instituição, é a unidade da observação da dissertação sobre o maior produtor
# local.
rs_estado <- taxas |> filter(origin %in% c("RS/DMJ", "TelessaúdeRS-UFRGS"))
stopifnot(nrow(rs_estado) == 2)
anotar("rs_state_notes", fmt_int(sum(rs_estado$notes)),
    "tables/analysis/final-analysis-descriptive-production.csv, notes of RS/DMJ plus TelessaúdeRS-UFRGS")
anotar("largest_local_origin_notes", fmt_int(maior_local$notes),
    "tables/analysis/final-analysis-descriptive-production.csv, notes of that origin")
anotar("national_notes_base", fmt_int(producao$notes_base[producao$origin == "Nacional"]),
    "tables/analysis/production-by-analysis-origin.csv, origin Nacional, notes_base")
anotar("origins_notes_base_sum", fmt_int(sum(producao$notes_base)),
    "tables/analysis/production-by-analysis-origin.csv, sum of notes_base over the named origins")
anotar("base_without_named_origin", fmt_int(funil$remaining[2] - sum(producao$notes_base)),
    "tables/analysis/attrition.csv step emitted by the series end, remaining, minus the sum of notes_base in production-by-analysis-origin.csv")
notas_nacional <- taxas$notes[taxas$origin == "Nacional"]
anotar("national_notes", fmt_int(notas_nacional),
    "tables/analysis/final-analysis-descriptive-production.csv, origin Nacional, notes")
anotar("national_share", fmt_pct(notas_nacional / total_amostra),
    "tables/analysis/final-analysis-descriptive-production.csv, origin Nacional, notes over the sample total")

# Marcadores da amostra e populações de estimação -------------------------------------

marcadores <- ler_csv_carimbado(file.path(DIR_ANALISE, "sample-flags.csv")) |>
  group_by(flag) |>
  summarise(notes = sum(flagged_notes), .groups = "drop")
conta_marcador <- function(nome) fmt_int(marcadores$notes[marcadores$flag == nome])
anotar("flag_anvisa_label", conta_marcador("dcb_is_anvisa_label"),
    "tables/analysis/sample-flags.csv, flag dcb_is_anvisa_label, sum of flagged_notes")
anotar("flag_icd_missing", conta_marcador("icd_recorded"),
    "tables/analysis/sample-flags.csv, flag icd_recorded, sum of flagged_notes")
anotar("flag_sex_missing", conta_marcador("sex_recorded"),
    "tables/analysis/sample-flags.csv, flag sex_recorded, sum of flagged_notes")
anotar("flag_date_not_strict", conta_marcador("emission_date_strict"),
    "tables/analysis/sample-flags.csv, flag emission_date_strict, sum of flagged_notes")
anotar("flags_total", fmt_int(sum(marcadores$notes)),
    "tables/analysis/sample-flags.csv, sum of flagged_notes over the four flags")

populacoes <- ler_csv_carimbado(caminho_final("fit-populations"))
fixest <- populacoes |> filter(model == "model_1_1")
anotar("fixest_discarded_notes", fmt_int(fixest$discarded_notes),
    "tables/analysis/final-analysis-fit-populations.csv, model model_1_1, discarded_notes")
anotar("fixest_discarded_share", fmt_pct(1 - fixest$coverage),
    "tables/analysis/final-analysis-fit-populations.csv, model model_1_1, one minus coverage")

# Pares medicamento × CID no piso de exibição -----------------------------------------

limiares <- ler_csv_carimbado(caminho_final("descriptive-pair-range-thresholds")) |> filter(floor == PISO_EXIBICAO)
pares_em <- function(t) {
  linha <- limiares |> filter(abs(threshold - t) < 1e-9)
  stopifnot(nrow(linha) == 1)
  linha$pairs_at_or_above_threshold
}
anotar("pair_floor", PISO_EXIBICAO, "constant of R/10-manuscript-numbers.R: display floor per medicine × CID cell")
anotar("pairs_eligible", fmt_int(limiares$eligible_pairs[1]),
    sprintf("tables/analysis/final-analysis-descriptive-pair-range-thresholds.csv, floor %d, eligible_pairs", PISO_EXIBICAO))
anotar("pairs_range_100", fmt_int(pares_em(1)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-range-thresholds.csv, floor %d, threshold 1", PISO_EXIBICAO))
anotar("pairs_range_80", fmt_int(pares_em(0.8)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-range-thresholds.csv, floor %d, threshold 0.8", PISO_EXIBICAO))
anotar("pairs_range_50", fmt_int(pares_em(0.5)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-range-thresholds.csv, floor %d, threshold 0.5", PISO_EXIBICAO))
anotar("pairs_range_20", fmt_int(pares_em(0.2)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-range-thresholds.csv, floor %d, threshold 0.2", PISO_EXIBICAO))

extremos <- ler_csv_carimbado(caminho_final("descriptive-pair-extremes")) |> filter(floor == PISO_EXIBICAO)
anotar("extreme_pairs", n_distinct(extremos$pair_id),
    sprintf("tables/analysis/final-analysis-descriptive-pair-extremes.csv, floor %d, distinct pair_id", PISO_EXIBICAO))
rotulo_medicamento <- function(x) {
  # Caixa de sentença para a prosa; a base guarda os nomes em caixa alta.
  x <- tolower(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}
anotar("extreme_pair_medicines", juntar_pt(rotulo_medicamento(sort(unique(extremos$medicine)))),
    sprintf("tables/analysis/final-analysis-descriptive-pair-extremes.csv, floor %d, distinct medicine", PISO_EXIBICAO))
sempre <- extremos |> filter(rate == 1) |> count(origin, sort = TRUE) |> filter(n >= MIN_MENCAO_EXTREMOS)
nunca <- extremos |> filter(rate == 0) |> count(origin, sort = TRUE) |> filter(n >= MIN_MENCAO_EXTREMOS)
anotar("always_favourable_origins", juntar_pt(rotulo_origem_com_artigo(sempre$origin)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-extremes.csv, floor %d, origins with rate 1 in at least %d pairs", PISO_EXIBICAO, MIN_MENCAO_EXTREMOS))
anotar("always_favourable_counts", juntar_pt(as.character(sempre$n)),
    "tables/analysis/final-analysis-descriptive-pair-extremes.csv, same rows, number of pairs")
anotar("never_favourable_origins", juntar_pt(rotulo_origem_com_artigo(nunca$origin)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-extremes.csv, floor %d, origins with rate 0 in at least %d pairs", PISO_EXIBICAO, MIN_MENCAO_EXTREMOS))
anotar("never_favourable_counts", juntar_pt(as.character(nunca$n)),
    "tables/analysis/final-analysis-descriptive-pair-extremes.csv, same rows, number of pairs")
anotar("extreme_mention_min", MIN_MENCAO_EXTREMOS,
    "constant of R/10-manuscript-numbers.R: minimum extreme pairs for an origin to be named")

celulas <- ler_csv_carimbado(caminho_final("descriptive-pair-cells")) |> filter(floor == PISO_EXIBICAO)
celulas_nacional <- celulas |> filter(origin == "Nacional") |> select(pair_id, national_rate = rate)
comparacoes <- celulas |>
  filter(origin != "Nacional") |>
  inner_join(celulas_nacional, by = "pair_id") |>
  mutate(difference = rate - national_rate, gap = abs(difference))
ao_menos <- function(t) sum(comparacoes$gap >= t - 1e-9)
anotar("ln_comparisons", fmt_int(nrow(comparacoes)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-cells.csv, floor %d, cells of origins other than Nacional in pairs where Nacional has a cell", PISO_EXIBICAO))
anotar("ln_pairs", fmt_int(n_distinct(comparacoes$pair_id)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-cells.csv, floor %d, distinct pairs with a Nacional cell and at least one other", PISO_EXIBICAO))
anotar("ln_gap_100", fmt_int(ao_menos(1)), "same comparisons, absolute difference of rates equal to 1")
anotar("ln_gap_80", fmt_int(ao_menos(0.8)), "same comparisons, absolute difference of rates at least 0.8")
anotar("ln_gap_50", fmt_int(ao_menos(0.5)), "same comparisons, absolute difference of rates at least 0.5")
anotar("ln_gap_20", fmt_int(ao_menos(0.2)), "same comparisons, absolute difference of rates at least 0.2")
anotar("ln_gap_20_national_higher", fmt_int(sum(comparacoes$gap >= 0.2 - 1e-9 & comparacoes$difference < 0)),
    "same comparisons with gap at least 0.2, Nacional rate above the local rate")
anotar("ln_gap_20_local_higher", fmt_int(sum(comparacoes$gap >= 0.2 - 1e-9 & comparacoes$difference > 0)),
    "same comparisons with gap at least 0.2, local rate above the Nacional rate")

exemplos_locais <- ler_csv_carimbado(caminho_final("descriptive-local-national-extremes")) |>
  filter(floor == PISO_EXIBICAO, local_origin == "MS")
for (direcao in c("largest_local_advantage", "largest_national_advantage")) {
  linha <- exemplos_locais |> filter(.data$direction == .env$direcao)
  stopifnot(nrow(linha) == 1)
  prefixo <- if (direcao == "largest_local_advantage") "ln_ms_local" else "ln_ms_national"
  fonte <- paste("tables/analysis/final-analysis-descriptive-local-national-extremes.csv, floor", PISO_EXIBICAO,
                 "local_origin MS, direction", direcao)
  anotar(paste0(prefixo, "_medicine"), tolower(linha$medicine), paste(fonte, "medicine"))
  anotar(paste0(prefixo, "_cid"), linha$cid, paste(fonte, "cid"))
  for (coluna in c("local_favourable", "local_total", "national_favourable", "national_total")) {
    anotar(paste0(prefixo, "_", coluna), fmt_int(linha[[coluna]]), paste(fonte, coluna))
  }
  anotar(paste0(prefixo, "_local_rate"), fmt_pct(linha$rate), paste(fonte, "rate"))
  anotar(paste0(prefixo, "_national_rate"), fmt_pct(linha$national_rate), paste(fonte, "national_rate"))
}

# Divergência ajustada, Modelo 1.2 -----------------------------------------------------
#
# O corpo reporta a probabilidade padronizada (a taxa favorável que um NatJus
# teria sobre a composição de casos da amostra inteira), a dispersão do
# intercepto de origem e a razão de chances mediana; os contrastes no logito
# ficam no apêndice.

padronizadas <- ler_csv_carimbado(caminho_final("origin-effect-candidates")) |>
  # A probabilidade padronizada não depende da âncora; mantém uma âncora só
  # para ter uma linha por origem, como o kit de escrita faz.
  filter(model == "model_1_2", anchor == "pooled_outcome_logit") |>
  arrange(standardized_probability)
anotar("std_prob_lowest_origin", rotulo_origem(padronizadas$origin[1]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, origin with the lowest standardized_probability")
anotar("std_prob_lowest", fmt_pct(padronizadas$standardized_probability[1]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, lowest standardized_probability")
anotar("std_prob_highest_origin", rotulo_origem(padronizadas$origin[nrow(padronizadas)]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, origin with the highest standardized_probability")
anotar("std_prob_highest", fmt_pct(padronizadas$standardized_probability[nrow(padronizadas)]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, highest standardized_probability")
anotar("std_prob_range_pp", round(100 * (padronizadas$standardized_probability[nrow(padronizadas)] - padronizadas$standardized_probability[1])),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, highest minus lowest standardized_probability, percentage points")
anotar("std_prob_national", fmt_pct(padronizadas$standardized_probability[padronizadas$origin == "Nacional"]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, origin Nacional, standardized_probability")
anotar("std_prob_locals_above_national",
    sum(padronizadas$standardized_probability > padronizadas$standardized_probability[padronizadas$origin == "Nacional"] & padronizadas$origin != "Nacional"),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, origins other than Nacional with standardized_probability above Nacional")
# Taxa bruta menos probabilidade padronizada, a parte da taxa observada que a
# composição de casos explica, nas duas pontas.
anotar("std_prob_highest_raw", fmt_pct(padronizadas$raw_rate[nrow(padronizadas)]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, raw_rate of the origin with the highest standardized_probability")
anotar("std_prob_lowest_raw", fmt_pct(padronizadas$raw_rate[1]),
    "tables/analysis/final-analysis-origin-effect-candidates.csv, model model_1_2, raw_rate of the origin with the lowest standardized_probability")

fmt_num <- function(x, digitos = 2) formatC(x, format = "f", digits = digitos, decimal.mark = ",")
efeitos_aleatorios <- ler_csv_carimbado(caminho_final("fit-random-effects")) |> filter(model == "model_1_2")
dp_de <- function(g) efeitos_aleatorios$standard_deviation[efeitos_aleatorios$group == g]
anotar("sd_origin", fmt_num(dp_de("origin")), "tables/analysis/final-analysis-fit-random-effects.csv, model model_1_2, group origin, standard_deviation")
anotar("sd_medicine", fmt_num(dp_de("medicine")), "tables/analysis/final-analysis-fit-random-effects.csv, model model_1_2, group medicine, standard_deviation")
anotar("sd_cid", fmt_num(dp_de("cid")), "tables/analysis/final-analysis-fit-random-effects.csv, model model_1_2, group cid, standard_deviation")
perfil <- ler_csv_carimbado(caminho_final("profile-interval-candidates")) |>
  filter(model == "model_1_2", parameter == "origin_standard_deviation", status == "ok")
stopifnot(nrow(perfil) == 1)
anotar("sd_origin_profile_interval", paste0("[", fmt_num(perfil$conf_low), "; ", fmt_num(perfil$conf_high), "]"),
    "tables/analysis/final-analysis-profile-interval-candidates.csv, model model_1_2, origin_standard_deviation, conf_low and conf_high")
metricas <- ler_csv_carimbado(caminho_final("metric-candidates"))
mor <- metricas |> filter(model == "model_1_2", metric == "mor")
stopifnot(nrow(mor) == 1)
anotar("mor", fmt_num(mor$estimate, 1), "tables/analysis/final-analysis-metric-candidates.csv, model model_1_2, metric mor, estimate")
# Teste da razão de verossimilhanças da variância de NatJus: o Modelo 1 contra
# o mesmo modelo sem o intercepto de NatJus. A hipótese nula põe a variância na
# fronteira do espaço dela, então a distribuição de referência é a mistura meio
# a meio de qui-quadrado com 0 e 1 graus de liberdade, não a qui-quadrado
# comum; o executor publica os dois valores-p e a prosa cita o da fronteira. O
# valor aparece como limite porque a estatística está muito além do que um
# double distingue de zero. A estatística sai com uma casa decimal e sem
# separador de milhar: "17.692" seria lido como dezessete vírgula seis.
lrt_origem <- ler_csv_carimbado(caminho_final("likelihood-ratio-candidates")) |>
  filter(comparison == "random_origin_variance")
stopifnot(nrow(lrt_origem) == 1, lrt_origem$null_on_boundary, lrt_origem$technically_comparable,
          lrt_origem$p_value_boundary_mixture < 0.001)
anotar("origin_lrt_statistic", fmt_num(lrt_origem$statistic, 1),
    "tables/analysis/final-analysis-likelihood-ratio-candidates.csv, comparison random_origin_variance, statistic")

# Parte fixa do Modelo 1.2 em razões de chances, para a prosa da subseção
# explicativa; a tabela completa é escrita por R/09-writing-kit.R.
coeficientes <- ler_csv_carimbado(caminho_final("fit-coefficients")) |> filter(model == "model_1_2")
rc_de <- function(termo) {
  linha <- coeficientes |> filter(.data$term == .env$termo)
  stopifnot(nrow(linha) == 1)
  linha
}
anotar_rc <- function(chave, termo, rotulo) {
  linha <- rc_de(termo)
  anotar(chave, fmt_num(linha$odds_ratio), sprintf("tables/analysis/final-analysis-fit-coefficients.csv, model model_1_2, term %s, odds_ratio", termo))
  anotar(paste0(chave, "_interval"), paste0("[", fmt_num(linha$odds_ratio_low), "; ", fmt_num(linha$odds_ratio_high), "]"),
      sprintf("tables/analysis/final-analysis-fit-coefficients.csv, model model_1_2, term %s, odds_ratio_low and odds_ratio_high", termo))
}
anotar_rc("or_conformity", "cov_selIndicacaoConformidadeSim")
anotar_rc("or_protocol", "cov_selPrevistoProtocoloSim")
anotar_rc("or_sus_available", "cov_selDisponivelSusSim")
anotar_rc("or_oncology", "cov_selOncologicoSim")
anotar_rc("or_anvisa", "cov_selRegistroAnvisaSim")
anotar_rc("or_biosimilar", "cov_selExisteBiossimilarSim")
anotar_rc("or_conitec_favourable", "cov_selRecomendacaoConitecFavoravel")
anotar_rc("or_conitec_unfavourable", "cov_selRecomendacaoConitecDesfavoravel")
anotar_rc("or_male", "cov_selStaGeneroMasculino")
anotar_rc("or_public_prosecutor", "cov_selDefensoriaPublicaMinisterio Publico")
linha_tempo <- rc_de("ano_mes")
anotar("or_time_monthly", fmt_num(linha_tempo$odds_ratio, 3),
    "tables/analysis/final-analysis-fit-coefficients.csv, model model_1_2, term ano_mes, odds_ratio")
# Leitura anual da tendência mensal: exp(12 × coeficiente mensal no logito), a
# convenção que o apêndice enuncia para a escala de tempo.
anotar("or_time_yearly", fmt_num(exp(12 * linha_tempo$estimate_logit)),
    "tables/analysis/final-analysis-fit-coefficients.csv, model model_1_2, term ano_mes, exp(12 × estimate_logit)")
anotar("time_yearly_decline", fmt_pct(1 - exp(12 * linha_tempo$estimate_logit), 0),
    "tables/analysis/final-analysis-fit-coefficients.csv, model model_1_2, term ano_mes, 1 − exp(12 × estimate_logit)")

# Taxas da Conitec e dispersão dos pares que a prosa lê ------------

# Taxa favorável por manifestação da Conitec, amostra inteira: as contagens
# somadas entre origens e a taxa recalculada sobre as somas, como faz a tabela
# do kit.
taxas_conitec <- ler_csv_carimbado(caminho_final("descriptive-conitec-rates")) |>
  filter(substantive) |>
  group_by(conitec_level) |>
  summarise(total = sum(total), favourable = sum(favourable), .groups = "drop")
taxa_conitec_de <- function(nivel) {
  linha <- taxas_conitec |> filter(conitec_level == nivel)
  stopifnot(nrow(linha) == 1)
  fmt_pct(linha$favourable / linha$total)
}
anotar("conitec_rate_not_evaluated", taxa_conitec_de("Nao avaliada"),
    "tables/analysis/final-analysis-descriptive-conitec-rates.csv, level Nao avaliada, sum of favourable over sum of total")
anotar("conitec_rate_favourable", taxa_conitec_de("Favoravel"),
    "tables/analysis/final-analysis-descriptive-conitec-rates.csv, level Favoravel, sum of favourable over sum of total")
anotar("conitec_rate_unfavourable", taxa_conitec_de("Desfavoravel"),
    "tables/analysis/final-analysis-descriptive-conitec-rates.csv, level Desfavoravel, sum of favourable over sum of total")

# Distância mediana entre o NatJus mais favorável e o menos favorável dentro do
# par, taxas brutas no piso de exibição: o valor que a figura de dispersão
# rotula.
dispersao <- ler_csv_carimbado(caminho_final("descriptive-pair-dispersion")) |>
  filter(floor == PISO_EXIBICAO, estimate_type == "raw")
anotar("pair_gap_median_pp", round(100 * median(dispersao$range)),
    sprintf("tables/analysis/final-analysis-descriptive-pair-dispersion.csv, floor %d, estimate_type raw, median of range, percentage points", PISO_EXIBICAO))
anotar("pair_gap_below_10_share", fmt_pct(mean(dispersao$range < 0.10), 0),
    sprintf("tables/analysis/final-analysis-descriptive-pair-dispersion.csv, floor %d, estimate_type raw, share of pairs with range below 0.10", PISO_EXIBICAO))

# NatJus locais sem par comparável contra o Nacional no piso de exibição.
local_nacional <- ler_csv_carimbado(caminho_final("descriptive-local-national-extremes")) |>
  filter(floor == PISO_EXIBICAO)
locais_sem_par <- setdiff(setdiff(taxas$origin, "Nacional"), local_nacional$local_origin)
anotar("ln_locals_without_pair", if (length(locais_sem_par) == 0) "nenhum" else juntar_pt(rotulo_origem(locais_sem_par)),
    sprintf("tables/analysis/final-analysis-descriptive-local-national-extremes.csv, floor %d, local NatJus of the production table absent from local_origin", PISO_EXIBICAO))
anotar("ln_locals_with_pair", n_distinct(local_nacional$local_origin),
    sprintf("tables/analysis/final-analysis-descriptive-local-national-extremes.csv, floor %d, distinct local_origin", PISO_EXIBICAO))

# Conitec por NatJus: padronização dentro de cada manifestação (script 11),
# tabela `kit-` carimbada fora do manifesto.
caminho_kit_analise <- function(nome) file.path(DIR_ANALISE, paste0("kit-", nome, ".csv"))
estratificado <- ler_csv_carimbado(caminho_kit_analise("conitec-stratified-standardization"))
extremos_do_estrato <- function(nivel, prefixo) {
  linhas <- estratificado |> filter(conitec_level == nivel)
  stopifnot(nrow(linhas) == n_distinct(estratificado$origin))
  menor <- linhas |> slice_min(standardized_probability, n = 1)
  maior <- linhas |> slice_max(standardized_probability, n = 1)
  fonte_base <- sprintf("tables/analysis/kit-conitec-stratified-standardization.csv, conitec_level %s", nivel)
  anotar(paste0(prefixo, "_std_lowest_origin"), rotulo_origem(menor$origin), paste(fonte_base, "lowest standardized_probability, origin"))
  anotar(paste0(prefixo, "_std_lowest"), fmt_pct(menor$standardized_probability), paste(fonte_base, "lowest standardized_probability"))
  anotar(paste0(prefixo, "_std_highest_origin"), rotulo_origem(maior$origin), paste(fonte_base, "highest standardized_probability, origin"))
  anotar(paste0(prefixo, "_std_highest"), fmt_pct(maior$standardized_probability), paste(fonte_base, "highest standardized_probability"))
  anotar(paste0(prefixo, "_std_range_pp"), round(100 * diff(range(linhas$standardized_probability))), paste(fonte_base, "range of standardized_probability in percentage points"))
  anotar(paste0(prefixo, "_raw_range_pp"), round(100 * diff(range(linhas$raw_rate))), paste(fonte_base, "range of raw_rate in percentage points"))
  anotar(paste0(prefixo, "_notes"), fmt_int(sum(linhas$notes)), paste(fonte_base, "sum of notes"))
}
extremos_do_estrato("Desfavoravel", "conitec_unfav")
extremos_do_estrato("Favoravel", "conitec_fav")
extremos_do_estrato("Nao avaliada", "conitec_not")

# Gravação -----------------------------------------------------------------------------

string_yaml <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
cabecalho <- c(
  "# Manuscript numbers, written by R/10-manuscript-numbers.R. Do not edit by hand:",
  "# rerun the script after a new analysis round. Each key is used in the text as",
  "# {{< var key >}}; the comment above it names the file, filter and column it",
  "# comes from.",
  paste0("# generated_at: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  vapply(sort(entradas), function(p) {
    paste0("# input: ", p, " sha256 ", digest::digest(p, algo = "sha256", file = TRUE))
  }, character(1)),
  ""
)
corpo <- unlist(lapply(names(variaveis), function(chave) {
  c(paste0("# source: ", variaveis[[chave]]$fonte), paste0(chave, ": ", string_yaml(variaveis[[chave]]$valor)), "")
}))
writeLines(c(cabecalho, corpo), CAMINHO_SAIDA)
cat("gravou ", CAMINHO_SAIDA, " com ", length(variaveis), " variáveis\n", sep = "")
