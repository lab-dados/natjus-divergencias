# Conferir que uma amostra em memória é a amostra que o livro-razão descreve --
#
# Todo script depois de `01-sample.R` lê `data/sample.parquet` e chama
# `assert_sample()` nele antes de fazer qualquer outra coisa. A conferência
# compara o data frame em mãos com as duas tabelas versionadas: o número de
# linhas tem de ser igual à última contagem de "remaining" no livro-razão de
# atrição, e os NatJus da amostra têm de bater com a tabela de produção. Se o
# parquet foi remontado de outra base, ou editado, ou está velho em relação às
# tabelas, o script para aqui com uma mensagem que nomeia os dois números em
# vez de produzir uma tabela com o N errado.
#
# Ainda não há framework de teste neste repositório; este auxiliar é a
# conferência. Usar `source("R/assert-sample.R")` para ter acesso a ele.

#' Parar a menos que a amostra bata com o livro-razão e a tabela de produção versionados
#'
#' @param sample Data frame lido de `data/sample.parquet`.
#' @param attrition_path Caminho do CSV do livro-razão de atrição. Linhas que
#'   começam com `#` são o carimbo de proveniência e são puladas.
#' @param production_path Caminho do CSV de produção por NatJus.
#' @return `sample`, de forma invisível, para que a chamada caiba num pipe.
assert_sample <- function(sample,
                          attrition_path = "tables/attrition.csv",
                          production_path = "tables/production-by-natjus.csv") {
  attrition <- readr::read_csv(attrition_path, comment = "#", show_col_types = FALSE)
  production <- readr::read_csv(production_path, show_col_types = FALSE)

  expected_rows <- attrition$remaining[nrow(attrition)]
  if (nrow(sample) != expected_rows) {
    stop(
      "The sample has ", nrow(sample), " rows but the attrition ledger ends at ",
      expected_rows, " (", attrition_path, "). Rebuild it with `Rscript R/01-sample.R`.",
      call. = FALSE
    )
  }

  origins_in_sample <- sort(unique(sample$origem_tratada))
  origins_in_table <- sort(production$origin[production$inclusion == "in sample"])
  if (!identical(origins_in_sample, origins_in_table)) {
    stop(
      "The sample has ", length(origins_in_sample), " NatJus but the production table lists ",
      length(origins_in_table), " as \"in sample\" (", production_path, ").",
      call. = FALSE
    )
  }

  total_in_table <- sum(production$notes_sample)
  if (total_in_table != nrow(sample)) {
    stop(
      "The production table's sample notes add up to ", total_in_table,
      " but the sample has ", nrow(sample), " rows.",
      call. = FALSE
    )
  }

  invisible(sample)
}

#' Ler a amostra e conferi-la numa chamada só
#'
#' @param path Caminho do parquet da amostra.
#' @return A amostra conferida como tibble.
read_sample <- function(path = "data/sample.parquet") {
  if (!file.exists(path)) {
    stop("Sample not found at ", path, ". Build it with `Rscript R/01-sample.R`.",
         call. = FALSE)
  }
  assert_sample(tibble::as_tibble(arrow::read_parquet(path)))
}
