# Compara o resultado de uma rodada dos níveis 3 ou 4 com os arquivos
# versionados. Chamado por tools/replicate.sh; ver REPLICATION.md.
#
#   Rscript tools/compare-replication.R
#
# O nível 2 exige igualdade byte a byte em dados, tabelas e números, porque só
# relê CSVs versionados, e só lista as figuras (ver tools/replicate.sh). Os
# níveis 3 e 4 reajustam os modelos, e o glmmTMB não é determinístico entre
# processos: a função objetivo, no mesmo ponto e com os mesmos dados, já sai
# diferente na 15.ª casa decimal. O R/08 leva cada ajuste até gradiente abaixo
# de 1e-8 (nlminb_with_newton_polish), o que segura essa diferença perto da
# 11.ª casa nas estimativas, mas não a zera, e os CSV guardam 15 dígitos. A
# comparação byte a byte falharia sempre. Por isso, aqui:
#
# - data/ continua exato: amostra e covariáveis não passam por modelo;
# - CSV compara as colunas inteiras exatamente, as demais numéricas com
#   tolerância e as de texto exatamente, e ignora as colunas de
#   COLUNA_IGNORADA e o carimbo de data;
# - tabela Markdown, figura e _variables.yml são exibição: uma estimativa que
#   cai na fronteira de arredondamento pode virar o último dígito. O script
#   lista cada linha que mudou e não falha por isso, para que a pessoa confira.

# Três rodadas nesta máquina, com o polimento, diferiram em no máximo 1,7e-7,
# no limite do intervalo de perfil, que a busca de raiz acha com tolerância
# própria; coeficientes e erros-padrão, em no máximo 1e-11. A folga cobre
# outra máquina, com outra BLAS, e continua milhares de vezes abaixo da casa
# que as tabelas exibem. A tolerância de um elemento é
# TOLERANCIA_ABSOLUTA + TOLERANCIA_RELATIVA * |versionado|.
TOLERANCIA_ABSOLUTA <- 1e-5
TOLERANCIA_RELATIVA <- 1e-5
# Colunas que não são resultado: hash ou tamanho de outro arquivo, o tempo do
# ajuste e o gradiente final, que depois do polimento é ruído de ponto
# flutuante abaixo de 1e-8 e muda de uma rodada para outra.
COLUNA_IGNORADA <- "(^|_)(sha256|bytes)$|^elapsed_seconds$|^max_abs_gradient$"
LINHA_IGNORADA <- "generated_at:|_built_at:|sha256"

git <- function(...) system2("git", c(...), stdout = TRUE)

mudados <- git("diff", "-I", "generated_at:", "-I", "_built_at:", "--name-only", "--",
               "data", "tables", "figures", "_variables.yml")
falhas <- character()
exibicao <- character()

ler_csv <- function(linhas) {
  corpo <- linhas[!startsWith(linhas, "#")]
  utils::read.csv(text = paste(corpo, collapse = "\n"), check.names = FALSE,
                  stringsAsFactors = FALSE, na.strings = c("NA", ""))
}

cabecalho <- function(linhas) {
  cab <- linhas[startsWith(linhas, "#")]
  cab[!grepl(LINHA_IGNORADA, cab)]
}

comparar_csv <- function(caminho) {
  antes <- git("show", paste0("HEAD:", caminho))
  depois <- readLines(caminho, warn = FALSE)
  if (!identical(cabecalho(antes), cabecalho(depois))) {
    return("cabeçalho de proveniência diferente")
  }
  a <- ler_csv(antes)
  b <- ler_csv(depois)
  if (!identical(dim(a), dim(b)) || !identical(names(a), names(b))) {
    return(sprintf("forma diferente: %s contra %s", paste(dim(a), collapse = "x"),
                   paste(dim(b), collapse = "x")))
  }
  problemas <- character()
  for (coluna in names(a)) {
    if (grepl(COLUNA_IGNORADA, coluna)) next
    x <- a[[coluna]]
    y <- b[[coluna]]
    if (is.integer(x) && is.integer(y)) {
      # Contagem não passa pelo otimizador: igualdade exata. Com a tolerância
      # relativa, uma contagem de 273 mil poderia variar em 2 e passar.
      if (!identical(x, y)) {
        problemas <- c(problemas, paste0(coluna, ": contagem diferente"))
      }
    } else if (is.numeric(x) && is.numeric(y)) {
      if (!identical(is.na(x), is.na(y))) {
        problemas <- c(problemas, paste0(coluna, ": NA em linhas diferentes"))
        next
      }
      desvio <- abs(x - y)
      limite <- TOLERANCIA_ABSOLUTA + TOLERANCIA_RELATIVA * abs(x)
      fora <- which(!is.na(desvio) & desvio > limite)
      if (length(fora)) {
        pior <- fora[which.max(desvio[fora])]
        problemas <- c(problemas, sprintf("%s: %d valores fora da tolerância, o maior %.15g contra %.15g na linha %d",
                                          coluna, length(fora), y[pior], x[pior], pior))
      }
    } else if (!identical(as.character(x), as.character(y))) {
      problemas <- c(problemas, paste0(coluna, ": valores diferentes numa coluna não numérica"))
    }
  }
  problemas
}

for (caminho in mudados) {
  if (startsWith(caminho, "data/")) {
    falhas <- c(falhas, paste0(caminho, ": dado diferente do versionado"))
  } else if (endsWith(caminho, ".csv")) {
    problemas <- comparar_csv(caminho)
    if (length(problemas)) falhas <- c(falhas, paste0(caminho, ": ", problemas))
  } else {
    exibicao <- c(exibicao, caminho)
  }
}

cat("Arquivos que diferem dos versionados:", length(mudados), "\n")
if (length(exibicao)) {
  cat("\nExibição que mudou (confira se é só o último dígito de um arredondamento):\n")
  for (caminho in exibicao) {
    if (endsWith(caminho, ".png")) {
      cat("  ", caminho, ": figura regravada\n", sep = "")
      next
    }
    linhas <- git("diff", "-U0", "--", caminho)
    linhas <- linhas[grepl("^[-+][^-+]", linhas) & !grepl(LINHA_IGNORADA, linhas)]
    if (!length(linhas)) {
      cat("  ", caminho, ": só carimbos e hashes\n", sep = "")
    } else {
      cat("  ", caminho, ":\n", paste0("    ", linhas, collapse = "\n"), "\n", sep = "")
    }
  }
}
if (length(falhas)) {
  cat("\nFALHA DE REPLICAÇÃO:\n", paste0("  ", falhas, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}
cat("\nDados idênticos e resultados numéricos dentro da tolerância.\n")
