# Figuras do manuscrito -------------------------------------------------------
#
# Lê as tabelas carimbadas em tables/analysis/ e grava em figures/ as figuras
# que o manuscrito inclui. Renderizar o manuscrito não roda R, então cada
# figura é um arquivo versionado produzido aqui; rode de novo depois de
# qualquer mudança nas tabelas de análise.
#
# Rode a partir da raiz do repositório:
#
#     Rscript R/06-figures.R
#
# A apresentação é em português (títulos, eixos, rótulos), como o manuscrito;
# nomes de coluna e nomes de arquivo continuam em inglês.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
source("R/figure-theme.R")

DIR_FIGURAS <- dir_figuras_preset("figures")
CAMINHO_PRODUCAO <- "tables/analysis/production-by-analysis-origin.csv"
dir.create(DIR_FIGURAS, showWarnings = FALSE)

ler_csv_carimbado <- function(caminho) {
  readr::read_csv(caminho, comment = "#", show_col_types = FALSE)
}

# Taxa favorável por NatJus com o intervalo de Wilson --------------------------
#
# Uma linha por origem analítica da amostra, ordenada pela taxa. Toda origem
# recebe o mesmo marcador: o Nacional é identificado pelo rótulo da linha, e
# uma forma distinta para ele acrescentava legenda sem acrescentar informação.

producao <- ler_csv_carimbado(CAMINHO_PRODUCAO) |>
  filter(inclusion == "in sample")

dados_figura_taxa <- producao |>
  mutate(
    label = sprintf("%s (n = %s)", origin, format(notes_sample, big.mark = ".", decimal.mark = ",", trim = TRUE)),
    label = factor(label, levels = label[order(rate)])
  )

figura_taxa <- ggplot(dados_figura_taxa, aes(x = rate, y = label)) +
  geom_errorbar(aes(xmin = wilson_low, xmax = wilson_high), width = 0.35, orientation = "y", colour = CORES_RELATORIO[["principal"]]) +
  geom_point(size = 2.2, colour = CORES_RELATORIO[["principal"]]) +
  scale_x_continuous(labels = function(x) paste0(format(100 * x, decimal.mark = ","), "%"),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  labs(
    x = "Proporção de notas técnicas com conclusão favorável",
    y = NULL
  ) +
  tema_relatorio(posicao_legenda = "none")

dimensoes_taxa <- dimensoes_preset(7.2, 6.2)
ggsave(file.path(DIR_FIGURAS, "favourable-rate-by-natjus.png"), figura_taxa,
       width = dimensoes_taxa[["largura"]], height = dimensoes_taxa[["altura"]], dpi = 200, bg = "white")
cat("gravou ", file.path(DIR_FIGURAS, "favourable-rate-by-natjus.png"), "\n", sep = "")
