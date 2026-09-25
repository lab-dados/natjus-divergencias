# Tabelas do manuscrito --------------------------------------------------------
#
# Converte os CSVs carimbados de tables/analysis/ nas tabelas Markdown que o
# manuscrito inclui, com cabeçalhos em português e formatos numéricos
# brasileiros. Os CSVs mantêm os nomes de coluna em inglês e os valores
# canônicos; só a apresentação é traduzida aqui. Renderizar o manuscrito não
# roda R, então estes arquivos são versionados e refeitos depois de qualquer
# mudança nas tabelas.
#
# Rode a partir da raiz do repositório:
#
#     Rscript R/07-manuscript-tables.R
#
# As saídas ficam em tables/manuscript/, um arquivo Markdown por tabela, cada
# um começando com um comentário HTML que nomeia o CSV de origem.

suppressPackageStartupMessages(library(dplyr))

DIR_MANUSCRITO <- "tables/manuscript"
dir.create(DIR_MANUSCRITO, showWarnings = FALSE)

ler_csv_carimbado <- function(caminho) {
  readr::read_csv(caminho, comment = "#", show_col_types = FALSE)
}

fmt_int <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
fmt_pct <- function(x, digitos = 1) {
  paste0(formatC(100 * x, format = "f", digits = digitos, decimal.mark = ","), "%")
}

# Linha de fonte que a revista exige sob cada tabela; um div de custom-style do
# Pandoc que o docx de referência renderiza no estilo "Fonte".
NOTA_FONTE <- "Fonte: elabora\u00e7\u00e3o pr\u00f3pria a partir das notas t\u00e9cnicas do e-NatJus (CNJ)."

escrever_tabela_markdown <- function(dados, nome, fonte, legenda = NULL, alinhamento = NULL,
                                     nota_fonte = NOTA_FONTE, titulo = NULL) {
  caminho <- file.path(DIR_MANUSCRITO, paste0(nome, ".md"))
  cabecalho <- paste0("| ", paste(names(dados), collapse = " | "), " |")
  if (is.null(alinhamento)) {
    # Colunas com cara de número (contagens, taxas, intervalos, diferenças)
    # alinham à direita; todo o resto, à esquerda. O Pandoc renderiza no Word
    # uma coluna sem marca como centralizada, então toda coluna recebe marca
    # explícita.
    # Expressão de colchetes POSIX: "]" primeiro e "-" por último, para que os
    # dois sejam lidos literalmente.
    parece_numero <- "^[][0-9.,%() ;\u2013\u2212/=p+-]*$"
    alinhamento <- vapply(dados, function(coluna) {
      celulas <- as.character(coluna)
      celulas <- celulas[!is.na(celulas) & nzchar(celulas)]
      if (length(celulas) && all(grepl(parece_numero, celulas))) "r" else "l"
    }, character(1))
  }
  # O Pandoc define as larguras relativas das colunas de uma pipe table pelo
  # comprimento de cada sequência de traços na linha separadora sempre que uma
  # linha é mais longa que a página, então as sequências acompanham o conteúdo
  # mais largo de cada coluna em vez de serem iguais, o que espremia as colunas
  # de texto.
  # O cabeçalho pode quebrar em várias linhas, então só a palavra mais longa
  # dele conta, não o rótulo inteiro.
  # Largura da coluna: nunca menor que a palavra mais longa da coluna, para que
  # o Word não quebre um nome no meio, e nunca maior que um teto, para que uma
  # coluna de frases longas não sufoque as outras; a célula então quebra nos
  # espaços.
  larguras <- vapply(seq_len(ncol(dados)), function(i) {
    palavras <- unlist(strsplit(c(names(dados)[i], as.character(dados[[i]])), " ", fixed = TRUE))
    palavra_mais_longa <- max(nchar(palavras, type = "width"))
    # O cabeçalho quebra livremente, então só a palavra mais longa dele conta;
    # as células do corpo definem a largura até o teto.
    celula_mais_longa <- max(nchar(as.character(dados[[i]]), type = "width"), 0L)
    # Quatro caracteres de folga em torno da palavra mais longa, para as
    # margens da célula e para o cabeçalho em negrito; um piso de quatorze para
    # que uma coluna curta ainda comporte uma palavra como "Nacional" ou
    # "+28 p.p."; um teto de vinte e oito para que uma coluna de frases longas
    # quebre em vez de sufocar as curtas.
    max(palavra_mais_longa + 4L, min(celula_mais_longa, 28L), 14L)
  }, integer(1))
  linha_separadora <- paste0("|", paste(vapply(seq_along(alinhamento), function(i) {
    tracos <- strrep("-", larguras[i])
    switch(alinhamento[i], l = paste0(":", tracos), r = paste0(tracos, ":"), c = paste0(":", tracos, ":"))
  }, character(1)), collapse = "|"), "|")
  linhas <- apply(dados, 1L, function(linha) paste0("| ", paste(linha, collapse = " | "), " |"))
  saida <- c(paste0("<!-- source: ", fonte, " -->"), "", cabecalho, linha_separadora, linhas)
  notas_tabela <- NULL
  if (!is.null(titulo)) {
    rotulo <- regmatches(legenda, regexpr("\\{#tbl-[^}]+\\}$", legenda))
    notas_tabela <- trimws(sub("\\s*\\{#tbl-[^}]+\\}$", "", legenda))
    legenda <- paste(titulo, rotulo)
  }
  if (!is.null(legenda)) saida <- c(saida, "", paste0(": ", legenda))
  if (!is.null(nota_fonte)) saida <- c(saida, "", "::: {custom-style=\"Fonte\"}", nota_fonte, ":::")
  if (!is.null(notas_tabela)) saida <- c(saida, "", "::: {custom-style=\"Notas\"}", paste0("Notas: ", notas_tabela), ":::")
  writeLines(saida, caminho)
  cat("gravou ", caminho, "\n", sep = "")
}

# Funil de seleção ---------------------------------------------------------------

# O registro tem uma linha por filtro; a tabela do manuscrito mostra as duas
# grafias de "nenhum NatJus identificado" (um campo bruto não preenchido, um
# marcador de "não informado") como uma linha só, porque o leitor não precisa
# da separação. O campo `etapas` nomeia as etapas que cada linha exibida soma;
# as contagens de removidas se somam e a contagem de restantes é a da última
# etapa. As duas primeiras linhas do registro não aparecem como filtro: a
# leitura da base e o corte das notas emitidas depois do fim da série juntos
# definem a base do artigo, que abre a tabela.
LINHAS_DO_FUNIL <- list(
  list(rotulo = "Notas sobre medicamentos", etapas = "medicines only"),
  list(rotulo = "Com conclusão favorável ou desfavorável registrada", etapas = "conclusion recorded"),
  list(rotulo = "Sem as notas do NatJus RJ, que não concluem pela procedência", etapas = "NatJus RJ excluded"),
  list(rotulo = "Com data de emissão registrada", etapas = "emission date recorded"),
  list(rotulo = "Emitidas a partir de maio de 2019", etapas = "series starts 2019-05"),
  list(rotulo = "Com NatJus emissor identificado",
       etapas = c("origin mapped by the NatJus dictionary", "note carries an origin")),
  list(rotulo = "De NatJus com ao menos 100 notas na amostra", etapas = "analytical origins with at least 100 notes")
)

funil <- ler_csv_carimbado("tables/analysis/attrition.csv")
stopifnot(identical(funil$step[-(1:2)], unlist(lapply(LINHAS_DO_FUNIL, `[[`, "etapas"))))
# O último dia da base vem do rótulo do próprio corte no registro, então a
# primeira linha da tabela não pode divergir da base que ela conta.
stopifnot(grepl("^emitted by \\d{4}-\\d{2}-\\d{2}$", funil$step[2]))
ultimo_dia <- format(as.Date(sub("^emitted by ", "", funil$step[2])), "%d/%m/%Y")
linhas_funil <- bind_rows(lapply(LINHAS_DO_FUNIL, function(linha) {
  linhas <- funil[match(linha$etapas, funil$step), ]
  tibble::tibble(
    `Etapa` = linha$rotulo,
    `Notas restantes` = fmt_int(linhas$remaining[nrow(linhas)]),
    `Notas removidas` = fmt_int(sum(linhas$removed_false + linhas$removed_na))
  )
}))
tabela_funil <- bind_rows(
  tibble::tibble(`Etapa` = paste0("Notas técnicas emitidas no e-NatJus até ", ultimo_dia),
                 `Notas restantes` = fmt_int(funil$remaining[2]),
                 `Notas removidas` = fmt_int(0L)),
  linhas_funil,
  tibble::tibble(`Etapa` = "Amostra final",
                 `Notas restantes` = fmt_int(funil$remaining[nrow(funil)]),
                 `Notas removidas` = fmt_int(0L))
)
escrever_tabela_markdown(
  tabela_funil, "attrition", "tables/analysis/attrition.csv",
  titulo = "Seleção das notas técnicas para a amostra analítica",
  legenda = paste(
    "Construção da amostra analítica a partir da base do e-NatJus.",
    "As etapas se aplicam na ordem das linhas, cada uma sobre o que a anterior deixou: Notas restantes é o saldo após a etapa e Notas removidas é o que ela retirou.",
    "O NatJus RJ sai porque suas notas não registram conclusão favorável ou desfavorável ao pedido.",
    "A última etapa aplica o piso de 100 notas por NatJus depois de reunir as instituições do Paraná em PR e o Hospital das Clínicas da USP em SP. {#tbl-attrition}"
  ),
  alinhamento = c("l", "r", "r")
)

# Produção e taxa favorável por NatJus -------------------------------------------

producao_toda <- ler_csv_carimbado("tables/analysis/production-by-analysis-origin.csv")
# As linhas ficam em ordem alfabética de estado para que o leitor encontre um
# NatJus sem vasculhar, com o Nacional primeiro porque ele é a referência de
# toda comparação e não é um estado. A chave de estado vem do rótulo: as duas
# instituições do Rio Grande do Sul compartilham a chave "RS" para ficarem
# juntas (o rótulo TelessaúdeRS começa com T e cairia longe de RS/DMJ),
# "AM/SES" ordena sob AM e "DFT" sob DF.
chave_estado <- function(rotulo) {
  dplyr::case_when(
    rotulo == "Nacional" ~ "",
    rotulo == "TelessaúdeRS-UFRGS" ~ "RS",
    rotulo == "DFT" ~ "DF",
    TRUE ~ sub("/.*$", "", rotulo)
  )
}
producao <- producao_toda |>
  filter(inclusion == "in sample") |>
  arrange(chave_estado(origin), origin)
# A legenda nomeia o que a tabela deixa de fora, lido do mesmo CSV para que a
# lista não divirja da amostra: o RJ, excluído por razão própria, e os rótulos
# abaixo do piso de 100 notas.
excluidos_abaixo_do_piso <- producao_toda |>
  filter(grepl("^below floor", inclusion)) |>
  arrange(desc(notes_medicines)) |>
  pull(origin)
excluidos_abaixo_do_piso <- sub("^RS$", "RS genérico", sub("^AM$", "AM genérico", excluidos_abaixo_do_piso))
stopifnot(any(producao_toda$origin == "RJ" & producao_toda$inclusion != "in sample"))
juntar_pt <- function(x) if (length(x) == 1) x else paste0(paste(x[-length(x)], collapse = ", "), " e ", x[length(x)])
tabela_producao <- producao |>
  transmute(
    `NatJus` = origin,
    `Notas` = fmt_int(notes_sample),
    `Primeiro mês` = format(as.Date(first_month), "%m/%Y"),
    `Último mês` = format(as.Date(last_month), "%m/%Y"),
    `Favoráveis` = fmt_int(favourable),
    `Taxa favorável` = fmt_pct(rate),
    `IC 95% (Wilson)` = paste0(formatC(100 * wilson_low, format = "f", digits = 1, decimal.mark = ","), "–", fmt_pct(wilson_high))
  )
linha_total <- tibble::tibble(
  `NatJus` = "Total",
  `Notas` = fmt_int(sum(producao$notes_sample)),
  `Primeiro mês` = format(min(as.Date(producao$first_month)), "%m/%Y"),
  `Último mês` = format(max(as.Date(producao$last_month)), "%m/%Y"),
  `Favoráveis` = fmt_int(sum(producao$favourable)),
  `Taxa favorável` = fmt_pct(sum(producao$favourable) / sum(producao$notes_sample)),
  `IC 95% (Wilson)` = ""
)
escrever_tabela_markdown(
  bind_rows(tabela_producao, linha_total), "production-by-natjus",
  "tables/analysis/production-by-analysis-origin.csv",
  titulo = sprintf("Notas técnicas e taxa de conclusão favorável por NatJus, %s a %s",
                   format(min(as.Date(producao$first_month)), "%m/%Y"),
                   format(max(as.Date(producao$last_month)), "%m/%Y")),
  legenda = sprintf(paste(
    "Produção e taxa de conclusão favorável por NatJus na amostra analítica: notas técnicas sobre medicamentos com conclusão registrada, emitidas entre %s e %s.",
    "O NatJus Nacional está na primeira linha e os %d NatJus locais seguem em ordem alfabética de estado.",
    "Notas é o total do NatJus na amostra; Primeiro e Último mês são os meses de emissão extremos, e %s é o fim da série;",
    "Taxa favorável é a fração de notas com conclusão favorável, com intervalo de confiança de Wilson a 95%%; a linha Total agrega os %d NatJus.",
    "PR reúne as cinco instituições conveniadas do Paraná e SP inclui as notas do Hospital das Clínicas da USP; os rótulos originais ficam preservados nos dados.",
    "Fora da tabela: o NatJus RJ, cujas notas não concluem pela procedência do pedido, e %d rótulos com menos de 100 notas no período (%s), retirados pelo piso de 100 notas por NatJus. {#tbl-production}"),
    format(min(as.Date(producao$first_month)), "%m/%Y"), format(max(as.Date(producao$last_month)), "%m/%Y"),
    nrow(producao) - 1L, format(max(as.Date(producao$last_month)), "%m/%Y"), nrow(producao),
    length(excluidos_abaixo_do_piso), juntar_pt(excluidos_abaixo_do_piso)),
  alinhamento = c("l", "r", "c", "c", "r", "r", "c")
)
