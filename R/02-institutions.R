# Montar as covariáveis institucionais por NatJus -------------------------
#
# O que este script faz. Ele junta três coisas numa tabela com uma linha por
# NatJus na base: os fatos curados à mão em `data-raw/institutions.csv`
# (quando cada centro foi fundado e a que instituições está ligado, cada
# célula com a fonte dela), as datas derivadas da base completa do e-NatJus
# (primeira e última nota de cada origem, qualquer tecnologia, qualquer
# conclusão) e o status de inclusão de `tables/production-by-natjus.csv`. Ele
# escreve `tables/institutions.csv`, que os modelos explicativos leem, e
# `tables/institutions-coverage.csv`, que diz, para cada variável, quantos
# NatJus da amostra têm valor.
#
# Rodar a partir da raiz do repositório, depois de `R/01-sample.R`:
#
#     Rscript R/02-institutions.R
#
# Por que dois arquivos. O CSV curado guarda só o que alguém leu numa fonte e
# digitou, para que cada célula dali possa ser conferida contra a coluna
# `*_source` correspondente. Tudo que pode ser calculado é calculado aqui, e
# nunca digitado no arquivo curado, porque uma derivação digitada envelhece
# em silêncio quando a base é remontada (a coleta é estendida mês a mês) e
# ninguém a recalcula.
#
# A data de fundação vem em três colunas porque as fontes a dão em três
# formatos: um ano explícito (`founded_year`), "já existia em X"
# (`founded_by`) e "criado depois de X" (`founded_after`). A categórica
# `pre_2016` é derivada da que estiver preenchida. O corte é 2016, o ano em
# que a Resolução 238 do CNJ mandou cada tribunal criar um NatJus: um centro
# fundado antes dela existia por iniciativa do próprio estado, um fundado
# depois existia porque foi mandado. `pre_2016` é TRUE quando se sabe que o
# centro já existia até 2015, FALSE quando se sabe que foi fundado em 2016 ou
# depois, e vazio quando as fontes não resolvem.
#
# Data de fundação e primeira nota na base são variáveis diferentes de
# propósito. A primeira diz quando o centro foi criado; a segunda diz quando
# alguém começou a lançar as notas dele na plataforma e-NatJus, o que vários
# centros só fizeram anos depois de criados, e alguns pararam de fazer.
# `last_note` está aí pela mesma razão: um centro cuja última nota é de anos
# atrás não é comparável a um que ainda lança.

suppressPackageStartupMessages(library(dplyr))
source("R/enatjus.R")
# `emitted_after_series_end()`: a base do artigo são as notas emitidas até o
# fim da série, a mesma de 01-sample.R, para que a primeira e a última nota de
# cada centro não caiam num dia que a amostra corta.
source("R/01-sample.R")

CURATED_PATH <- "data-raw/institutions.csv"
PRODUCTION_PATH <- "tables/production-by-natjus.csv"
OUTPUT_PATH <- "tables/institutions.csv"
COVERAGE_PATH <- "tables/institutions-coverage.csv"


# O ano da resolução do CNJ que mandou ter um NatJus por tribunal.
RESOLUTION_YEAR <- 2016L

# As colunas que os modelos leem, na ordem em que são escritas.
CURATED_VARIABLES <- c(
  "founded_year", "founded_by", "founded_after",
  "court", "health_secretariat", "university", "non_university_hospital"
)

# Derivações -----------------------------------------------------------------

#' Primeira e última nota de cada origem na base completa.
#'
#' Qualquer nota conta, seja qual for a tecnologia ou a conclusão, desde que
#' tenha origem e data de emissão registrada: a pergunta é quando a origem
#' começou e parou de alimentar a plataforma, não quando começou a produzir
#' as notas que o artigo modela. A data é interpretada do mesmo jeito que em
#' `01-sample.R`.
#'
#' @param base A base completa, como devolvida por `read_analysis_base()`.
#' @return Um tibble com `origin`, `first_note`, `last_note` (datas) e
#'   `notes_dated`, o número de notas por trás delas.
note_dates <- function(base) {
  base |>
    filter(
      !is.na(origem_tratada), nzchar(origem_tratada),
      !origem_tratada %in% ORIGIN_SENTINELS,
      data_emissao != NOT_FILLED
    ) |>
    mutate(emission_date = as.Date(lubridate::dmy_hms(data_emissao))) |>
    filter(!is.na(emission_date)) |>
    group_by(origin = origem_tratada) |>
    summarise(
      first_note = min(emission_date),
      last_note = max(emission_date),
      notes_dated = n(),
      .groups = "drop"
    )
}

#' `pre_2016` a partir das três colunas de fundação.
#'
#' TRUE quando se sabe que o centro existia antes do ano da resolução: um ano
#' explícito abaixo dele, ou um limite de "existia em" abaixo dele. FALSE
#' quando se sabe que foi fundado no ano da resolução ou depois: um ano
#' explícito igual ou acima dele, ou um limite de "criado depois" igual ou
#' acima do ano anterior a ele (criado depois de 2015 quer dizer 2016 ou
#' depois). NA nos demais casos. Os limites são inclusivos do lado que a
#' fonte afirma, e é por isso que `founded_by` é comparado com `<` (existia
#' em 2015 é anterior) e `founded_after` com `>=` (depois de 2015 é
#' posterior).
#'
#' @param founded_year,founded_by,founded_after Vetores inteiros com NA.
#' @return Um vetor lógico com NA.
derive_pre_2016 <- function(founded_year, founded_by, founded_after) {
  case_when(
    !is.na(founded_year) & founded_year < RESOLUTION_YEAR ~ TRUE,
    !is.na(founded_year) & founded_year >= RESOLUTION_YEAR ~ FALSE,
    !is.na(founded_by) & founded_by < RESOLUTION_YEAR ~ TRUE,
    !is.na(founded_after) & founded_after >= RESOLUTION_YEAR - 1L ~ FALSE,
    TRUE ~ NA
  )
}

#' Ler o CSV curado e conferi-lo contra a tabela de produção.
#'
#' Para quando o conjunto de origens difere, para que um NatJus acrescentado
#' à base por uma remontagem não fique sem a linha dele em silêncio.
read_curated <- function(path, production) {
  curated <- readr::read_csv(
    path, show_col_types = FALSE,
    col_types = readr::cols(
      founded_year = readr::col_integer(),
      founded_by = readr::col_integer(),
      founded_after = readr::col_integer(),
      court = readr::col_logical(),
      health_secretariat = readr::col_logical(),
      university = readr::col_logical(),
      non_university_hospital = readr::col_logical(),
      .default = readr::col_character()
    )
  )
  missing <- setdiff(production$origin, curated$origin)
  extra <- setdiff(curated$origin, production$origin)
  if (length(missing) > 0 || length(extra) > 0) {
    stop(
      "The curated file and the production table do not list the same NatJus.",
      if (length(missing) > 0) paste0("\n  missing from the curated file: ", paste(missing, collapse = ", ")),
      if (length(extra) > 0) paste0("\n  not in the production table: ", paste(extra, collapse = ", ")),
      call. = FALSE
    )
  }
  curated
}

#' Montar a tabela final.
#'
#' @return Um tibble com uma linha por NatJus na tabela de produção.
build_institutions <- function(curated, dates, production) {
  production |>
    transmute(origin, in_sample = inclusion == "in sample", inclusion, notes_base) |>
    left_join(curated, by = "origin") |>
    left_join(dates, by = "origin") |>
    mutate(pre_2016 = derive_pre_2016(founded_year, founded_by, founded_after)) |>
    select(
      origin, in_sample, inclusion, notes_base,
      all_of(CURATED_VARIABLES), pre_2016, first_note, last_note, notes_dated,
      ends_with("_source"), curation_notes
    ) |>
    arrange(desc(in_sample), desc(notes_base))
}

#' Cobertura de cada variável sobre os NatJus da amostra.
#'
#' Conta, para cada variável que os modelos podem ler, quantos dos NatJus
#' amostrados têm valor não ausente. As colunas derivadas entram para que o
#' leitor veja o que `pre_2016` acrescenta sobre o ano explícito.
coverage_table <- function(institutions) {
  sampled <- filter(institutions, in_sample)
  variables <- c(CURATED_VARIABLES, "pre_2016", "first_note", "last_note")
  tibble(
    variable = variables,
    natjus_with_value = vapply(variables, function(v) sum(!is.na(sampled[[v]])), integer(1)),
    natjus_in_sample = nrow(sampled)
  ) |>
    mutate(share = round(natjus_with_value / natjus_in_sample, 3))
}

# Principal -------------------------------------------------------------------

main <- function() {
  production <- readr::read_csv(PRODUCTION_PATH, show_col_types = FALSE)
  curated <- read_curated(CURATED_PATH, production)

  base_path <- analysis_base_path()
  base <- read_analysis_base(base_path)
  message("Loaded ", format(nrow(base), big.mark = ","), " technical notes.")
  base <- base[!emitted_after_series_end(base$data_emissao), ]
  dates <- note_dates(base)

  institutions <- build_institutions(curated, dates, production)
  coverage <- coverage_table(institutions)

  readr::write_csv(institutions, OUTPUT_PATH, na = "")
  readr::write_csv(coverage, COVERAGE_PATH, na = "")

  message(
    "\nInstitutions: ", nrow(institutions), " NatJus, ",
    sum(institutions$in_sample), " in the sample."
  )
  message("\nCoverage over the sampled NatJus:\n",
          paste(utils::capture.output(print(coverage, n = Inf)), collapse = "\n"))
}

if (sys.nframe() == 0) {
  main()
}
