# Adaptado de lab-dados/enatjus, R/read-base.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# É o único arquivo desta pasta que não repete o pacote: lá a leitura aponta a
# base interna da coleta, que tem campos restritos e não é distribuída; aqui
# aponta a base pública publicada no Hugging Face, fixada por revisão e por
# hash. `read_analysis_base()` mantém a leitura por colunas e a validação do
# pacote. Licença MIT, em R/enatjus/LICENSE.md.

#' A base do artigo no Hugging Face
#'
#' O corte de 06/09/2026 da base pública do e-NatJus, no build de 24/09/2026:
#' só notas que a plataforma exibe sem login, sem as colunas de número de
#' processo e de data de nascimento. Os demais campos da página pública, como o
#' nome e a OAB do advogado, estão na base, e o texto livre das notas pode citar
#' processo ou nascimento; a leitura abaixo seleciona só as colunas que a
#' análise usa. A revisão fixa o conjunto de arquivos do dataset naquele
#' momento, e o SHA-256 fixa o arquivo em si; os dois juntos garantem que quem
#' baixa hoje ou daqui a um ano lê os mesmos bytes que o artigo leu.
BASE_HF_DATASET <- "BrunoDCDO/enatjus_v2"
BASE_HF_REVISION <- "be990d4dc17ec24274419c6ff0460844700270a9"
BASE_HF_FILE <- "data/base_enatjus_tratada.parquet"
BASE_SHA256 <- "eec865c49d09ae57ed606b531312ca7d8379726a30e06225ff27763965aa2390"
# Quando e de que commit do coletor o arquivo saiu, do sidecar
# base_enatjus_tratada.parquet.json do build de 24/09/2026 e do CHANGELOG do
# dataset.
BASE_BUILT_AT <- "2026-09-24T18:49:57-03:00"
BASE_COLLECTOR_COMMIT <- "ad40ae9a97623af141c43479ad70374ffdc76a47"

#' Caminho da base
#'
#' Onde `tools/baixar-base.sh` grava o arquivo, dentro do repositório e fora do
#' Git. `NATJUS_BASE` aponta outro lugar quando a pessoa já tem o arquivo; o
#' hash é conferido de qualquer jeito, na leitura.
#'
#' @return O caminho do parquet.
analysis_base_path <- function() {
  Sys.getenv("NATJUS_BASE", unset = file.path("data", "hf", BASE_HF_FILE))
}

#' Ler a base
#'
#' Lê só as colunas que o pipeline usa e as valida contra o contrato de
#' vocabulário antes de devolver.
#'
#' Ler só essas colunas não é cosmético: o parquet tem cerca de 500 MB
#' comprimidos e é dominado por campos de texto livre (`txaConclusao`,
#' `txaEficaciaSeguranca`, `txaReferencia`) que nenhum modelo usa. Lido
#' inteiro, ele ocupa vários gigabytes na sessão; as colunas mantidas aqui são
#' campos curtos do formulário e identificadores.
#'
#' @param path Caminho do parquet. O padrão é [analysis_base_path()].
#' @return Um tibble com [REQUIRED_COLUMNS], validado por [validate_contract()].
read_analysis_base <- function(path = analysis_base_path()) {
  if (!file.exists(path)) {
    stop(
      "Base não encontrada em ", path, ". Rode tools/baixar-base.sh, que baixa `",
      BASE_HF_FILE, "` de ", BASE_HF_DATASET, " na revisão ", BASE_HF_REVISION,
      ", ou aponte NATJUS_BASE para o arquivo.",
      call. = FALSE
    )
  }
  # Outro arquivo com o mesmo nome (um corte anterior ou posterior da base)
  # rodaria sem erro e mudaria todos os números em silêncio.
  observado <- digest::digest(path, algo = "sha256", file = TRUE)
  if (!identical(observado, BASE_SHA256)) {
    stop(
      "A base em ", path, " não é a do artigo: SHA-256 ", observado,
      ", esperado ", BASE_SHA256, ".",
      call. = FALSE
    )
  }

  # O HTML de origem às vezes traz bytes NUL no texto livre, o que derruba o
  # leitor do arrow. A opção vale só nesta chamada, para que nada mais na
  # sessão descarte caracteres em silêncio: as colunas lidas aqui são campos
  # curtos, em que um NUL seria ele mesmo um sinal de alerta.
  previous <- options(arrow.skip_nul = TRUE)
  on.exit(options(previous), add = TRUE)

  # `any_of`, e não `all_of`: uma coluna ausente tem de chegar a
  # `validate_contract()`, que a nomeia; `all_of` falharia antes, dentro do
  # arrow, com uma mensagem que não diz qual base foi lida.
  base <- tibble::as_tibble(
    arrow::read_parquet(path, col_select = dplyr::any_of(REQUIRED_COLUMNS))
  )
  validate_contract(base)
  base
}
