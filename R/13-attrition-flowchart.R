# Fluxograma da seleção da amostra ---------------------------------------------
#
# Desenha a seleção da amostra analítica como um fluxo: à esquerda, o que
# sobra depois de cada filtro; à direita, o que o filtro removeu. As contagens
# e os rótulos da coluna da esquerda são lidos da tabela do manuscrito
# (tables/manuscript/attrition.md), que 07-manuscript-tables.R grava a partir
# do livro-razão carimbado, então a figura não consegue divergir da tabela: os
# filtros agrupados (as duas grafias de "sem NatJus identificado") são
# agrupados lá, uma vez só. Só a redação das
# caixas da direita mora aqui, indexada pelo rótulo da esquerda, para que uma
# tabela com rótulo trocado ou ordem trocada pare o script em vez de rotular
# uma caixa errado.
#
# Uso, a partir da raiz do repositório:
#   Rscript R/13-attrition-flowchart.R

suppressPackageStartupMessages(library(ggplot2))
source("R/figure-theme.R")

CAMINHO_TABELA <- "tables/manuscript/attrition.md"
CAMINHO_FIGURA <- "figures/kit-attrition-flowchart.png"

ROTULOS_EXCLUIDAS <- c(
  "Notas sobre medicamentos" = "Excluídas: outras tecnologias\n(produtos e procedimentos)",
  "Com conclusão favorável ou desfavorável registrada" = "Excluídas: sem conclusão favorável\nou desfavorável registrada",
  "Sem as notas do NatJus RJ, que não concluem pela procedência" = "Excluídas: notas do NatJus RJ",
  "Com data de emissão registrada" = "Excluídas: sem data de emissão",
  "Emitidas a partir de maio de 2019" = "Excluídas: emitidas\nantes de 05/2019",
  "Com NatJus emissor identificado" = "Excluídas: sem NatJus emissor identificado",
  "De NatJus com ao menos 100 notas na amostra" = "Excluídas: NatJus com menos\nde 100 notas na amostra"
)

linhas <- readLines(CAMINHO_TABELA, encoding = "UTF-8")
linhas_tabela <- grep("^\\|", linhas, value = TRUE)
linhas_tabela <- linhas_tabela[!grepl("^\\|[:-]", linhas_tabela)][-1]
celulas <- lapply(strsplit(linhas_tabela, "|", fixed = TRUE), function(x) trimws(x[-1]))
etapas <- data.frame(
  label = vapply(celulas, `[`, "", 1),
  remaining = vapply(celulas, `[`, "", 2),
  removed = vapply(celulas, `[`, "", 3),
  stringsAsFactors = FALSE
)
# A tabela fecha com uma linha de "amostra final" que repete o último saldo.
etapas <- etapas[etapas$label != "Amostra final", ]
filtros <- etapas[-1, ]
stopifnot(identical(filtros$label, names(ROTULOS_EXCLUIDAS)))

n <- nrow(etapas)
quebrar <- function(x, largura) vapply(x, function(frase) paste(strwrap(frase, largura), collapse = "\n"), "")
mantidas <- data.frame(
  x = 0, y = rev(seq_len(n)),
  text = paste0(quebrar(etapas$label, 38), "\n(n = ", etapas$remaining, ")")
)
mantidas$text[n] <- paste0("Amostra analítica: notas de NatJus com\nao menos 100 notas (n = ", etapas$remaining[n], ")")
removidas <- data.frame(
  x = 1.12, y = mantidas$y[-1] + 0.5,
  text = paste0(unname(ROTULOS_EXCLUIDAS), "\n(n = ", filtros$removed, ")")
)

LARGURA_CAIXA <- 0.46
ALTURA_CAIXA <- 0.36
tinta <- CORES_RELATORIO[["tinta"]]
grafico <- ggplot() +
  # fluxo vertical entre as caixas do que ficou
  geom_segment(
    data = data.frame(y = mantidas$y[-n] - ALTURA_CAIXA, yend = mantidas$y[-1] + ALTURA_CAIXA),
    aes(x = 0, xend = 0, y = y, yend = yend), colour = tinta, linewidth = 0.4,
    arrow = arrow(length = unit(1.6, "mm"), type = "closed")
  ) +
  # ramo para o que cada filtro removeu
  geom_segment(
    data = removidas, aes(x = 0, xend = x - LARGURA_CAIXA - 0.02, y = y, yend = y),
    colour = tinta, linewidth = 0.4, arrow = arrow(length = unit(1.6, "mm"), type = "closed")
  ) +
  geom_tile(data = mantidas, aes(x = x, y = y), width = 2 * LARGURA_CAIXA, height = 2 * ALTURA_CAIXA,
            fill = "white", colour = CORES_RELATORIO[["principal"]], linewidth = 0.6) +
  geom_tile(data = removidas, aes(x = x, y = y), width = 2 * LARGURA_CAIXA, height = 2 * ALTURA_CAIXA * 0.8,
            fill = "white", colour = CORES_RELATORIO[["cinza"]], linewidth = 0.4, linetype = "22") +
  geom_text(data = mantidas, aes(x = x, y = y, label = text), colour = tinta,
            family = fonte_relatorio(), size = 2.9, lineheight = 0.95) +
  geom_text(data = removidas, aes(x = x, y = y, label = text), colour = tinta,
            family = fonte_relatorio(), size = 2.7, lineheight = 0.95) +
  coord_cartesian(xlim = c(-LARGURA_CAIXA - 0.02, 1.12 + LARGURA_CAIXA + 0.02), ylim = c(0.5, n + 0.5), expand = FALSE) +
  theme_void()

ggsave(CAMINHO_FIGURA, grafico, width = 7.2, height = 8.6, dpi = 200, bg = "white")
cat("gravou ", CAMINHO_FIGURA, "\n", sep = "")
