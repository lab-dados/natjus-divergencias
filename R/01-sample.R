# Montar a amostra de análise ----------------------------------------------
#
# O que este script faz, num parágrafo. Ele lê a base completa do e-NatJus
# (todas as notas técnicas que a plataforma guarda), aplica uma sequência
# fixa de filtros que a reduz às notas que o artigo analisa, e escreve três
# arquivos: a amostra em si (`data/sample.parquet`, não versionada), um
# livro-razão dizendo quantas notas cada filtro removeu (`tables/attrition.csv`),
# e uma tabela com uma linha por NatJus dizendo quantas notas ele produziu e
# se está na amostra (`tables/production-by-natjus.csv`). As análises
# seguintes leem essa amostra; a reconstrução diagnóstica anterior ao piso
# escreve tabelas separadas e nunca substitui esses três arquivos.
#
# Rodar a partir da raiz do repositório, depois de `R/00-setup.R`:
#
#     Rscript R/01-sample.R
#
# Outros scripts também podem dar `source()` neste arquivo para reusar
# `build_sample()` (a análise de robustez precisa da variante que mantém o
# NatJus RJ). O source define as funções e não escreve nada; só uma chamada
# direta por `Rscript` executa o bloco no fim do arquivo.
#
# Os filtros não são inventados aqui. Eles compõem duas funções do pacote
# `enatjus` (copiadas em R/enatjus/), `start_attrition()` e `keep()`, na ordem do pipeline de
# arrumação do próprio pacote (`data-raw/1-tidy.R`), com três mudanças feitas
# de propósito. Os filtros rodam da maior remoção para a menor, e o
# livro-razão se lê assim; a amostra é a mesma em qualquer ordem, porque todo
# filtro é um predicado no nível da linha, e só o piso de 100 notas precisa
# vir por último. Quatro dos filtros do pacote não são aplicados: notas cujo
# campo de nome do medicamento traz um rótulo de situação na Anvisa, notas
# sem código de diagnóstico, notas sem sexo registrado, e notas cuja data de
# emissão veio da tabela de finalização ficam na amostra e são *marcadas* em
# vez disso (ver `build_sample()`).
# A regra por trás disso é remover só o que a análise não consegue dispensar
# (o desfecho, o objeto, a unidade, uma data, um mês completo) e carregar
# todo o resto como um nível "não informado", porque uma exclusão pode
# esconder um viés que ninguém mediu. Para o código de diagnóstico o viés
# aparece nos perfis de origem e de tempo: descartar diagnósticos ausentes
# pode remover o começo da série de um centro, enquanto sexo ausente pode
# remover o fim dela.
# Os perfis atuais são calculados por `03-covariates.R` a partir desta
# amostra. O começo da série, que o pacote impõe com uma parada dura, aqui é
# um filtro com linha própria no livro-razão, aplicado antes do piso de 100
# notas para que o piso conte o que está dentro da janela.
#
# `keep()` é um filtro que registra o que removeu, e `attrition_table()`
# devolve esse registro; é isso que torna o N final explicável linha a linha
# em vez de aparecer do nada.

suppressPackageStartupMessages(library(dplyr))
source("R/enatjus.R")

# Parâmetros ---------------------------------------------------------------

# Onde a série temporal começa e termina. Os dois são meses de calendário,
# escritos como literais em vez de derivados dos dados: um começo ou fim
# tirado de `min()`/`max()` das notas filtradas se moveria sempre que um
# filtro anterior mudasse, e ninguém perceberia. O fim é agosto de 2026
# porque a base foi coletada em 6 de setembro de 2026: a base do artigo são
# as notas emitidas até o último dia de agosto, e as de setembro, que teriam
# seis dias de coleta, saem logo na leitura (ver `build_sample()`).
SERIES_START <- as.Date("2019-05-01")
SERIES_END <- as.Date("2026-08-01")
SERIES_END_DAY <- lubridate::ceiling_date(SERIES_END, "month") - 1L

# Um NatJus entra na amostra só se tiver pelo menos esta quantidade de notas
# depois de todos os outros filtros. Um centro com três notas mostraria 0% ou
# 100% de favoráveis ao lado do número nacional. Mesmo valor do pacote.
ORIGIN_FLOOR <- 100L

# Nível de confiança do intervalo de Wilson em torno da taxa de favoráveis de
# cada NatJus na tabela de produção.
WILSON_LEVEL <- 0.95

# Caminhos de saída, relativos à raiz do repositório.
SAMPLE_PATH <- "data/sample.parquet"
ATTRITION_PATH <- "tables/attrition.csv"
PRODUCTION_PATH <- "tables/production-by-natjus.csv"

# Rótulos que o formulário usa para "este campo não foi preenchido", lidos do
# pacote para que a amostra e o pacote sempre concordem no vocabulário.
# Eles são internos ao pacote (não exportados), daí o `:::`.

# Proveniência ---------------------------------------------------------------

#' Descrever o arquivo de base de que a amostra foi montada
#'
#' Os números do artigo dependem de qual build da base foi lida. Isto
#' registra o bastante para identificar o arquivo e refazer o download: o
#' dataset, a revisão e o caminho no Hugging Face, o SHA-256 do arquivo (uma
#' impressão digital que muda se um único byte mudar), o tamanho, a contagem
#' de linhas, quando e de que commit do coletor ele saiu, e o commit do pacote
#' `enatjus` de que as funções de R/enatjus/ foram copiadas.
#'
#' Só o nome do arquivo é registrado, nunca a pasta: a pasta é um caminho na
#' máquina de uma pessoa e não tem lugar num arquivo versionado.
#'
#' @param base_path Caminho completo do parquet, como resolvido pelo pacote.
#' @param base_rows Número de linhas lidas dele.
#' @return Um vetor de caracteres nomeado, uma entrada por linha do carimbo.
base_stamp <- function(base_path, base_rows) {
  c(
    base_dataset = BASE_HF_DATASET,
    base_revision = BASE_HF_REVISION,
    base_file = BASE_HF_FILE,
    base_sha256 = digest::digest(base_path, algo = "sha256", file = TRUE),
    base_bytes = format(file.size(base_path), scientific = FALSE),
    base_rows = format(base_rows, scientific = FALSE),
    base_generated_at = BASE_BUILT_AT,
    collector_commit = BASE_COLLECTOR_COMMIT,
    enatjus_commit = ENATJUS_COMMIT,
    sample_built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

# Amostra -------------------------------------------------------------------

#' Rótulos de situação na Anvisa que vazaram para a coluna de nome do medicamento
#'
#' Em algumas centenas de notas o coletor capturou o campo errado, então a
#' coluna de nome do medicamento (`txtDcb`) traz uma situação de registro na
#' Anvisa como "VÁLIDO" em vez de um medicamento. A regra que os encontra lê
#' o vocabulário de situações da própria coluna de situação
#' (`selSituacaoAnvisa`), menos os marcadores de "não preenchido", e sinaliza
#' qualquer nome de medicamento igual a uma dessas palavras depois de tirar
#' caixa e acentos. Ler o vocabulário dos dados, em vez de digitar os dois
#' rótulos conhecidos, faz com que um rótulo novo acrescentado ao formulário
#' depois também seja pego.
#'
#' Mesma regra do pipeline de arrumação do pacote; reescrita aqui em poucas
#' linhas porque o pacote não a expõe como função reusável.
#'
#' @param base A base completa.
#' @return Vetor de caracteres com os rótulos de situação achatados.
anvisa_status_labels <- function(base) {
  placeholders <- c(NOT_FILLED, NOT_APPLICABLE)
  sentinels <- unique(flatten_label(c(placeholders, gsub("_", " ", placeholders))))
  vocabulary <- unique(flatten_label(base$selSituacaoAnvisa))
  vocabulary <- vocabulary[!is.na(vocabulary) & nzchar(vocabulary)]
  labels <- setdiff(vocabulary, sentinels)
  if (length(labels) == 0L) {
    stop("`selSituacaoAnvisa` carries no status label; the txtDcb rule would filter nothing.",
         call. = FALSE)
  }
  labels
}

#' Notas emitidas depois do último dia da série
#'
#' A base é coletada alguns dias depois do fim do mês em que a série
#' termina, e essas notas de poucos dias ficam fora da base do artigo. Uma
#' nota sem data de emissão (o marcador de "não preenchido") não é posterior
#' a nada: fica, e sai no passo que exige data. Se nenhuma nota for
#' posterior, o literal SERIES_END deixou de cortar alguma coisa e alguém tem
#' de olhar por quê.
#'
#' @param data_emissao A coluna de data de emissão como vem da base.
#' @return Vetor lógico, TRUE para as notas emitidas depois de SERIES_END_DAY.
emitted_after_series_end <- function(data_emissao) {
  emission_date <- as.Date(lubridate::dmy_hms(
    ifelse(data_emissao == NOT_FILLED, NA_character_, data_emissao), quiet = TRUE))
  after <- !is.na(emission_date) & emission_date > SERIES_END_DAY
  if (!any(after)) {
    warning("No note is dated after ", format(SERIES_END_DAY),
            "; SERIES_END no longer cuts anything.", call. = FALSE)
  }
  after
}

#' Reduzir a base completa à amostra de análise
#'
#' Cada `keep()` abaixo é um filtro. O texto entre aspas é o rótulo que
#' aparece no livro-razão de atrição, e a ordem importa: uma nota removida
#' por um filtro anterior não é contada de novo por um posterior, então o
#' livro-razão só fecha quando os filtros são lidos em sequência.
#'
#' @param base A base completa, como devolvida por `read_analysis_base()`.
#' @param include_rj Manter o NatJus RJ? `FALSE` para a amostra do artigo.
#'   `TRUE` é para a análise de robustez: os pareceres do RJ terminam com uma
#'   fórmula que devolve a pergunta ao juiz em vez de recomendar a favor ou
#'   contra, então o campo binário favorável/desfavorável mede outra coisa
#'   ali.
#' @param series_start Primeiro mês da série. Notas emitidas antes dele saem
#'   com linha própria no livro-razão. O padrão é a âncora do artigo; a
#'   análise de robustez passa outras datas para ver o que se move.
#' @param apply_origin_floor FALSE expõe a população antes do último filtro.
#' @return A amostra, com as colunas da base mais: `emission_date` (uma
#'   Date), `month_of_emission` (primeiro dia daquele mês), `y` (1 se a
#'   conclusão é favorável, 0 caso contrário), e quatro marcas que scripts
#'   posteriores usam para montar níveis "não informado" ou para restringir
#'   uma análise específica: `dcb_is_anvisa_label` (o campo de nome do
#'   medicamento traz um rótulo de situação, então o medicamento é
#'   desconhecido), `icd_recorded` (a nota traz um código de diagnóstico),
#'   `sex_recorded` (o campo de sexo está preenchido), e
#'   `emission_date_strict` (a data veio do aviso de emissão; quando FALSE é
#'   a data de finalização, e a análise de tendência temporal deixa essas
#'   notas de fora). As variáveis dos modelos são derivadas por scripts
#'   posteriores, não aqui.
build_sample <- function(base, include_rj = FALSE, series_start = SERIES_START,
                         apply_origin_floor = TRUE) {
  # Calculado antes do pipe de propósito: `keep()` avalia a condição dentro
  # dos dados primeiro, e uma chamada de função ali só funcionaria por cair
  # no ambiente de chamada.
  anvisa_labels <- anvisa_status_labels(base)
  after_series_end <- emitted_after_series_end(base$data_emissao)

  sample <- start_attrition(base, "technical notes loaded")

  # O corte de data define a base do artigo e por isso vem antes de todos os
  # outros: o texto e a tabela de seleção partem do que ele deixa. Notas sem
  # data ficam aqui e saem adiante, no passo que exige data de emissão.
  sample <- keep(sample, !after_series_end,
                 paste0("emitted by ", format(SERIES_END_DAY)))

  # Ordem dos filtros: as maiores remoções primeiro, para o livro-razão se
  # ler dos cortes grandes (objeto, desfecho, RJ) aos pequenos. Todo filtro é
  # um predicado no nível da linha, então a amostra é a mesma em qualquer
  # ordem; só as contagens do livro-razão mudam. O piso é a exceção e fica
  # por último.
  sample <- sample |>
    # O artigo é sobre medicamentos; procedimentos e produtos saem.
    keep(selTipoTecnologia == "Medicamento", "medicines only") |>
    # A nota tem de dizer se a recomendação foi favorável ou não. Quase todas
    # as notas que isto remove também não têm data de emissão nem origem: são
    # registros vazios, não notas mal preenchidas.
    keep(selConclusao != NOT_FILLED, "conclusion recorded")

  # O `is.na()` fica de propósito: notas sem origem nenhuma não podem ser
  # removidas aqui; o passo "origin mapped" abaixo é onde elas são contadas.
  if (!include_rj) {
    sample <- keep(sample, is.na(origem_tratada) | origem_tratada != "RJ",
                   "NatJus RJ excluded")
  }

  sample <- sample |>
    # Notas sem data nenhuma não podem ser colocadas na série. Notas cuja
    # data veio da tabela de finalização em vez do aviso de emissão ficam,
    # marcadas abaixo, porque uma data ainda é uma data para tudo menos para
    # a tendência temporal.
    keep(data_emissao != NOT_FILLED, "emission date recorded") |>
    mutate(
      emission_date = as.Date(lubridate::dmy_hms(data_emissao)),
      month_of_emission = lubridate::floor_date(emission_date, "month"),
      y = as.integer(selConclusao == "Favoravel"),
      # O coletor às vezes escreveu uma situação da Anvisa ("VÁLIDO",
      # "CADUCO/CANCELADO") onde vai o nome do medicamento. O medicamento não
      # é recuperável do formulário, então scripts posteriores leem o
      # medicamento como "não informado" nessas notas; os campos
      # regulatórios sobre o medicamento continuam lá.
      dcb_is_anvisa_label = flatten_label(txtDcb) %in% anvisa_labels,
      # Duas grafias de "sem diagnóstico": um marcador em texto livre
      # contendo "PREENCHER" e os sentinelas do próprio formulário.
      icd_recorded = !stringr::str_detect(txtCid, "PREENCHER") &
        !txtCid %in% SENTINEL_LEVELS,
      # Sexo é covariável do modelo, lida como "não informado" onde o campo
      # está em branco. Quase todos os em branco são do NatJus BA de 31 de
      # março de 2026 em diante, quando aquele centro voltou à plataforma e
      # parou de preencher o campo, então o nível "não informado" de sexo é
      # na prática um marcador daquele centro naquele período; o texto tem de
      # dizer isso.
      sex_recorded = selStaGenero %in% c("Feminino", "Masculino"),
      emission_date_strict = data_emissao_fonte == EMISSION_SOURCE_KEPT
    )

  # O corte do começo usou o texto bruto da data; aqui a data já está lida, e
  # nenhuma nota pode ter passado de SERIES_END.
  stopifnot(max(sample$month_of_emission) <= SERIES_END)

  # A outra ponta. Os modelos medem o tempo a partir da âncora, então uma
  # nota anterior a ela tornaria o intercepto uma extrapolação. O pipeline do
  # pacote para a execução se existir nota assim; aqui elas são removidas com
  # linha própria no livro-razão, porque sem o filtro de diagnóstico um
  # punhado de notas do fim de 2018 e do começo de 2019 (uma a cinco por mês)
  # agora chega a este ponto. A âncora é um argumento para que a análise de
  # robustez possa remontar a amostra a partir de um mês anterior e ver o que
  # se move.
  sample <- keep(sample, month_of_emission >= series_start,
                 paste0("series starts ", format(series_start, "%Y-%m")))

  # Dois problemas diferentes, duas linhas: uma origem que o dicionário não
  # conseguiu mapear (`NA`: o campo bruto diz "não preenchido") versus uma
  # nota que traz o marcador "Não informado". De um jeito ou de outro a nota
  # não tem NatJus, e o NatJus é a unidade de análise. A tabela do manuscrito
  # mostra as duas como uma linha só.
  sample <- sample |>
    keep(!is.na(origem_tratada) & nzchar(origem_tratada),
         "origin mapped by the NatJus dictionary") |>
    keep(!origem_tratada %in% ORIGIN_SENTINELS, "note carries an origin")

  # O piso é aplicado por último, no que sobrou, para contar as notas que
  # estariam de fato na amostra em vez das notas que entraram.
  if (apply_origin_floor) {
    above_floor <- sample |>
      count(origem_tratada) |>
      filter(n >= ORIGIN_FLOOR) |>
      pull(origem_tratada)
    sample <- keep(sample, origem_tratada %in% above_floor,
                   paste0("NatJus with at least ", ORIGIN_FLOOR, " notes"))
  }

  sample
}

# Tabela de produção ---------------------------------------------------------

#' Uma linha por NatJus: o que ele produziu e se está na amostra
#'
#' O artigo usa esta tabela para explicar quem está dentro e quem está fora.
#' Para cada origem na base do artigo, as notas emitidas até SERIES_END_DAY
#' (menos o marcador "não informado" e as notas sem origem nenhuma,
#' reportadas à parte no texto), ela dá as notas na base, as notas de
#' medicamento, quantas notas de medicamento não
#' têm sexo registrado, as notas que chegaram à amostra, o primeiro e o
#' último mês na amostra, a contagem e a taxa de favoráveis na amostra com um
#' intervalo de Wilson, e a regra que decidiu a inclusão.
#'
#' A taxa de favoráveis é calculada só na amostra, com o mesmo denominador
#' que os modelos usam. As notas na base e as de medicamento entram como
#' contagens, nada além.
#'
#' @param base A base completa.
#' @param sample A amostra devolvida por `build_sample()`.
#' @param include_rj O RJ foi mantido na amostra? Decide o rótulo de inclusão
#'   do RJ.
#' @return Um tibble, uma linha por origem, ordenado pelas notas na amostra.
production_table <- function(base, sample, include_rj = FALSE) {
  origins <- base[!emitted_after_series_end(base$data_emissao), ] |>
    filter(!is.na(origem_tratada), !origem_tratada %in% ORIGIN_SENTINELS) |>
    group_by(origin = origem_tratada) |>
    summarise(
      notes_base = n(),
      notes_medicines = sum(selTipoTecnologia == "Medicamento"),
      # Notas de medicamento sem sexo registrado. Elas ficam na amostra,
      # marcadas; a coluna está aqui porque quase todas são de um centro (BA)
      # a partir de uma data, e o leitor deve ver isso de relance.
      notes_sex_missing = sum(selTipoTecnologia == "Medicamento" &
                                !selStaGenero %in% c("Feminino", "Masculino")),
      .groups = "drop"
    )

  in_sample <- sample |>
    group_by(origin = origem_tratada) |>
    summarise(
      notes_sample = n(),
      first_month = min(month_of_emission),
      last_month = max(month_of_emission),
      favourable = sum(y),
      .groups = "drop"
    )

  table <- origins |>
    left_join(in_sample, by = "origin") |>
    mutate(
      notes_sample = coalesce(notes_sample, 0L),
      favourable = coalesce(favourable, 0L)
    )

  # Intervalo de Wilson só onde há notas; o auxiliar do pacote recusa
  # denominador zero, que é o comportamento certo, então as linhas fora da
  # amostra ficam com NA.
  has_notes <- table$notes_sample > 0L
  wilson <- cnj_audit_wilson(
    table$favourable[has_notes], table$notes_sample[has_notes], level = WILSON_LEVEL
  )
  table$rate <- NA_real_
  table$wilson_low <- NA_real_
  table$wilson_high <- NA_real_
  table$rate[has_notes] <- wilson$estimate
  table$wilson_low[has_notes] <- wilson$conf_low
  table$wilson_high[has_notes] <- wilson$conf_high

  # A regra de inclusão, em palavras. A ordem dos casos é a ordem em que as
  # razões se aplicam: um centro sem notas de medicamento nunca alcança o
  # piso, e o RJ é excluído antes de qualquer contagem.
  table |>
    mutate(
      inclusion = case_when(
        notes_sample > 0L ~ "in sample",
        origin == "RJ" & !include_rj ~ "RJ: non-decisional opinions",
        notes_medicines == 0L ~ "no medicine notes",
        .default = paste0("below floor of ", ORIGIN_FLOOR)
      )
    ) |>
    arrange(desc(notes_sample), desc(notes_base))
}

# Escrita ---------------------------------------------------------------------

#' Escrever a amostra, o livro-razão e a tabela de produção
#'
#' O CSV do livro-razão começa com linhas de comentário (prefixadas por `#`)
#' carregando o carimbo de proveniência, e depois as quatro colunas que o
#' pacote registra para cada filtro: o rótulo do passo, notas removidas
#' porque a condição era falsa, notas removidas porque ela não pôde ser
#' avaliada (um valor ausente), e notas restantes. Quem lê tem de pular as
#' linhas `#`; `assert_sample()` pula.
write_outputs <- function(sample, attrition, production, stamp) {
  dir.create(dirname(SAMPLE_PATH), showWarnings = FALSE)
  dir.create(dirname(ATTRITION_PATH), showWarnings = FALSE)

  arrow::write_parquet(sample, SAMPLE_PATH)

  stamp_lines <- paste0("# ", names(stamp), ": ", stamp)
  ledger_lines <- readr::format_csv(attrition)
  writeLines(c(stamp_lines, sub("\n$", "", ledger_lines)), ATTRITION_PATH)

  readr::write_csv(production, PRODUCTION_PATH, na = "")
}

# Principal -------------------------------------------------------------------

main <- function() {
  source("R/assert-sample.R")

  base_path <- analysis_base_path()
  base <- read_analysis_base(base_path)
  message("Loaded ", format(nrow(base), big.mark = ","), " technical notes.")

  stamp <- base_stamp(base_path, nrow(base))
  sample <- build_sample(base, include_rj = FALSE)
  attrition <- attrition_table()
  production <- production_table(base, sample, include_rj = FALSE)

  write_outputs(sample, attrition, production, stamp)

  # Lê de volta o que foi escrito e confere contra o livro-razão, do mesmo
  # jeito que todo script seguinte vai fazer.
  assert_sample(arrow::read_parquet(SAMPLE_PATH), ATTRITION_PATH, PRODUCTION_PATH)

  message("\nProvenance:\n", paste0("  ", names(stamp), ": ", stamp, collapse = "\n"))
  message("\nAttrition:\n", paste(utils::capture.output(print(attrition, n = Inf)), collapse = "\n"))
  message(
    "\nSample: ", format(nrow(sample), big.mark = ","), " notes, ",
    n_distinct(sample$origem_tratada), " NatJus, months ",
    format(min(sample$month_of_emission)), " to ", format(max(sample$month_of_emission)), "."
  )
}

# Roda só quando o arquivo é executado direto (`Rscript R/01-sample.R`), não
# quando outro script dá `source()` nele para pegar emprestadas as funções.
if (sys.nframe() == 0L) {
  main()
}
