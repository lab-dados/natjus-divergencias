# Tabelas e figuras do kit de escrita ---------------------------------------------
#
# Transforma os CSVs carimbados da rodada definitiva de análise (tables/analysis/
# final-analysis-*.csv) nas demais apresentações que o manuscrito inclui: o
# bloco descritivo por manifestação da Conitec e por par medicamento × CID, as
# tabelas por NatJus dos dois modelos, a série anual e as tabelas do apêndice. Tudo aqui é apresentação: nenhum modelo é reajustado e
# nenhuma estimativa nova é produzida, com a exceção da agregação das
# contagens entre origens para a tabela da Conitec, cujo intervalo de Wilson é
# recalculado sobre as contagens somadas.
#
# As saídas usam o prefixo `kit-` para ficarem fora do manifesto que o executor
# da análise definitiva (R/08) reconcilia por hash.
#
# Rode a partir da raiz do repositório, depois de R/08-final-analysis.R ter
# publicado:
#
#     Rscript R/09-writing-kit.R
#
# A apresentação é em português (cabeçalhos, legendas, eixos); os nomes de
# coluna dos CSVs e os nomes de arquivo continuam em inglês.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
source("R/figure-theme.R")

DIR_MANUSCRITO <- "tables/manuscript"
DIR_FIGURAS <- dir_figuras_preset("figures")
DIR_ANALISE <- "tables/analysis"
# Piso de exibição por célula medicamento × CID nas apresentações por par. A
# análise publica os pisos 5, 10, 20 e 50 sem escolher; 10 é a escolha de
# exibição do kit de escrita, e os outros pisos aparecem nas legendas para que
# o leitor veja o que muda com eles.
PISO_EXIBICAO <- 10
dir.create(DIR_MANUSCRITO, showWarnings = FALSE)
dir.create(DIR_FIGURAS, showWarnings = FALSE)

caminho_final <- function(nome) file.path(DIR_ANALISE, paste0("final-analysis-", nome, ".csv"))
# Tabelas carimbadas que o script 11 publica fora do manifesto de R/08.
caminho_analise_kit <- function(nome) file.path(DIR_ANALISE, paste0("kit-", nome, ".csv"))

ler_csv_carimbado <- function(caminho) {
  readr::read_csv(caminho, comment = "#", show_col_types = FALSE)
}

fmt_inteiro <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
fmt_pct <- function(x, digitos = 1) {
  sub("^-", "−", paste0(formatC(100 * x, format = "f", digits = digitos, decimal.mark = ","), "%"))
}
fmt_num <- function(x, digitos = 3) {
  # Menos tipográfico, para que um limite negativo nunca seja lido como um traço.
  sub("^-", "−", formatC(x, format = "f", digits = digitos, decimal.mark = ","))
}
fmt_intervalo_pct <- function(inferior, superior, digitos = 1, colchetes = FALSE) {
  # Forma com colchetes para as diferenças, cujos limites podem ser negativos;
  # traço médio para as proporções, como fazem as tabelas de R/07.
  if (colchetes) {
    paste0("[", fmt_pct(inferior, digitos), "; ", fmt_pct(superior, digitos), "]")
  } else {
    paste0(sub("%$", "", fmt_pct(inferior, digitos)), "–", fmt_pct(superior, digitos))
  }
}
fmt_pp <- function(x, digitos = 0) {
  # Diferença com sinal em pontos percentuais, com menos tipográfico.
  v <- formatC(100 * x, format = "f", digits = digitos, decimal.mark = ",")
  paste0(if_else(x > 0, "+", ""), sub("^-", "−", v), " p.p.")
}
fmt_intervalo_num <- function(inferior, superior, digitos = 3) {
  # Colchetes e ponto e vírgula em vez de traço médio, porque os limites dos
  # contrastes no logito podem ser negativos.
  paste0("[", fmt_num(inferior, digitos), "; ", fmt_num(superior, digitos), "]")
}

# Intervalo de Wilson, o mesmo que as tabelas de análise trazem para cada
# origem; recalculado aqui só para contagens somadas entre origens.
wilson <- function(sucessos, total, z = qnorm(0.975)) {
  p <- sucessos / total
  centro <- (p + z^2 / (2 * total)) / (1 + z^2 / total)
  metade <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) / (1 + z^2 / total)
  list(inferior = pmax(0, centro - metade), superior = pmin(1, centro + metade))
}

# Linha de fonte que a revista exige sob cada tabela e figura; uma div de
# custom-style do Pandoc que o docx de referência renderiza no estilo "Fonte".
NOTA_FONTE <- "Fonte: elaboração própria a partir das notas técnicas do e-NatJus (CNJ)."
linhas_nota_fonte <- function(texto = NOTA_FONTE) c("", "::: {custom-style=\"Fonte\"}", texto, ":::")

# Os nomes de medicamento chegam da base em caixa alta; as tabelas os mostram
# em caixa de sentença, que lê melhor e ocupa menos largura.
rotulo_medicamento <- function(x) {
  x <- tolower(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

gravar_tabela_markdown <- function(dados, nome, fonte, legenda = NULL, alinhamento = NULL,
                                   nota_fonte = NOTA_FONTE, titulo = NULL) {
  caminho <- file.path(DIR_MANUSCRITO, paste0(nome, ".md"))
  cabecalho <- paste0("| ", paste(names(dados), collapse = " | "), " |")
  if (is.null(alinhamento)) {
    # Colunas com cara de número (contagens, taxas, intervalos, diferenças)
    # alinham à direita; o resto, à esquerda. O Pandoc renderiza no Word uma
    # coluna sem marca como centralizada, então toda coluna recebe marca
    # explícita.
    # Expressão de colchetes POSIX: "]" primeiro e "-" por último, para que os
    # dois sejam lidos literalmente.
    parece_numero <- "^[][0-9.,%() ;–−/=p+-]*$"
    alinhamento <- vapply(dados, function(coluna) {
      celulas <- as.character(coluna)
      celulas <- celulas[!is.na(celulas) & nzchar(celulas)]
      if (length(celulas) && all(grepl(parece_numero, celulas))) "r" else "l"
    }, character(1))
  }
  # Mesma regra de largura de R/07: o Pandoc deriva as larguras relativas das
  # colunas das sequências de traços sempre que uma linha é mais longa que a
  # página, então as sequências acompanham o conteúdo mais largo de cada coluna.
  # Largura da coluna: nunca menor que a maior palavra da coluna, para que o
  # Word não quebre um nome no meio, e nunca maior que um teto, para que uma
  # coluna de frases longas não sufoque as outras; a célula então quebra nos
  # espaços.
  larguras <- vapply(seq_len(ncol(dados)), function(i) {
    palavras <- unlist(strsplit(c(names(dados)[i], as.character(dados[[i]])), " ", fixed = TRUE))
    maior_palavra <- max(nchar(palavras, type = "width"))
    # O cabeçalho quebra livremente, então só a maior palavra dele conta; as
    # células do corpo definem a largura até o teto.
    maior_celula <- max(nchar(as.character(dados[[i]]), type = "width"), 0L)
    # Quatro caracteres de folga em torno da maior palavra, para as margens da
    # célula e o cabeçalho em negrito; um piso de catorze para que uma coluna
    # curta ainda comporte uma palavra como "Nacional" ou "+28 p.p."; um teto de
    # vinte e oito para que uma coluna de frases longas quebre em vez de
    # sufocar as curtas.
    max(maior_palavra + 4L, min(maior_celula, 28L), 14L)
  }, integer(1))
  regua <- paste0("|", paste(vapply(seq_along(alinhamento), function(i) {
    tracos <- strrep("-", larguras[i])
    switch(alinhamento[i], l = paste0(":", tracos), r = paste0(tracos, ":"), c = paste0(":", tracos, ":"))
  }, character(1)), collapse = "|"), "|")
  linhas_dados <- apply(dados, 1L, function(linha) paste0("| ", paste(linha, collapse = " | "), " |"))
  linhas <- c(paste0("<!-- source: ", fonte, " -->"), "", cabecalho, regua, linhas_dados)
  notas <- NULL
  if (!is.null(titulo)) {
    rotulo <- regmatches(legenda, regexpr("\\{#tbl-[^}]+\\}$", legenda))
    notas <- trimws(sub("\\s*\\{#tbl-[^}]+\\}$", "", legenda))
    legenda <- paste(titulo, rotulo)
  }
  if (!is.null(legenda)) linhas <- c(linhas, "", paste0(": ", legenda))
  if (!is.null(nota_fonte)) linhas <- c(linhas, linhas_nota_fonte(nota_fonte))
  if (!is.null(notas)) linhas <- c(linhas, "", "::: {custom-style=\"Notas\"}", paste0("Notas: ", notas), ":::")
  writeLines(linhas, caminho)
  cat("gravou ", caminho, "\n", sep = "")
}

gravar_figura <- function(grafico, arquivo, largura, altura) {
  caminho <- file.path(DIR_FIGURAS, arquivo)
  # no preset de slides as dimensões viram as de tela (R/figure-theme.R)
  dimensoes <- dimensoes_preset(largura, altura)
  ggsave(caminho, grafico, width = dimensoes[["largura"]], height = dimensoes[["altura"]], dpi = 200, bg = "white")
  cat("gravou ", caminho, "\n", sep = "")
}

tema_kit <- function() tema_relatorio()

# Nomes de exibição decididos em 11 de setembro de 2026: o modelo misto (id
# interno `model_1_2`) é o modelo principal do artigo e aparece como "Modelo
# 1"; o modelo de efeitos fixos (`model_1_1`) fica no apêndice como "Modelo 2".
# Os ids internos são o vocabulário dos CSVs final-*, que não mudam, então o id
# não acompanha o nome de exibição.
ROTULOS_MODELO <- c(model_1_2 = "Modelo 1", model_1_1 = "Modelo 2")
ROTULOS_CONITEC <- c(
  "Nao avaliada" = "Não avaliada",
  "Favoravel" = "Favorável",
  "Desfavoravel" = "Desfavorável",
  "N/I" = "N/I"
)
# 1. Taxa favorável por manifestação da Conitec, amostra inteira ----------------

taxas_conitec <- ler_csv_carimbado(caminho_final("descriptive-conitec-rates"))
stopifnot(all(taxas_conitec$conitec_level %in% names(ROTULOS_CONITEC)))
agregado_conitec <- taxas_conitec |>
  group_by(conitec_level) |>
  summarise(
    origins_with_level = sum(total > 0),
    total = sum(total), favourable = sum(favourable), .groups = "drop"
  ) |>
  # O nível auxiliar N/I existe no contrato, mas não tem nota na população
  # final; uma linha vazia só imprimiria NaN.
  filter(total > 0) |>
  mutate(
    rate = favourable / total,
    low = wilson(favourable, total)$inferior,
    high = wilson(favourable, total)$superior,
    conitec_level = factor(conitec_level, levels = names(ROTULOS_CONITEC))
  ) |>
  arrange(conitec_level)
tabela_taxas_conitec <- agregado_conitec |>
  transmute(
    `Manifestação da Conitec` = unname(ROTULOS_CONITEC[as.character(conitec_level)]),
    `Notas` = fmt_inteiro(total),
    `Favoráveis` = fmt_inteiro(favourable),
    `Taxa favorável` = fmt_pct(rate),
    `IC 95%` = fmt_intervalo_pct(low, high)
  )
gravar_tabela_markdown(
  tabela_taxas_conitec, "kit-conitec-rates", caminho_final("descriptive-conitec-rates"),
  titulo = "Conclusões favoráveis por manifestação da Conitec na amostra analítica",
  legenda = paste(
    "Taxa de conclusão favorável por manifestação da Conitec sobre o medicamento, amostra inteira.",
    "A manifestação é a registrada pelo próprio NatJus no formulário da nota, não uma consulta dos autores à Conitec.",
    "Notas e Favoráveis somam as 25 unidades da amostra; Taxa favorável é a fração de conclusões favoráveis, com intervalo de confiança a 95% recalculado sobre as somas.",
    "O nível auxiliar N/I, das notas sem o campo preenchido, não tem nota na população final e por isso não aparece; ele não se funde a nenhuma das três manifestações. {#tbl-conitec-rates}"
  ),
  alinhamento = c("l", "r", "r", "r", "c")
)

# 2. Taxa favorável por NatJus dentro de cada manifestação da Conitec -----------
#
# A figura do corpo do texto para o argumento de tese de que a carteira de
# medicamentos não explica a divergência: um NatJus por linha e um painel por
# manifestação da Conitec, numa única escala de 0 a 100. A comparação de
# interesse é entre NatJus dentro de uma manifestação, e com as três
# manifestações deslocadas num painel só os 75 intervalos se sobrepunham e
# nenhuma coluna de pontos podia ser seguida; um painel por manifestação
# alinha os pontos para comparar. Cor e forma repetem o que o título do painel
# já diz, então nada é lido só pela cor. O corpo do texto mostra a taxa bruta
# com o intervalo de Wilson (tabela final-*); o apêndice mostra a
# probabilidade padronizada dentro do estrato pelo Modelo 1 (script 11): cada
# nota daquela manifestação prevista sob cada NatJus, mantidos o medicamento,
# a CID, o sexo, os campos clínicos e o mês dela, e depois a média. A figura
# padronizada não tem intervalo porque os intervalos por reamostragem ficaram
# para depois.

taxas_conitec_por_origem <- ler_csv_carimbado(caminho_final("descriptive-conitec-rates")) |>
  filter(conitec_level %in% c("Nao avaliada", "Favoravel", "Desfavoravel"))
conitec_estratificada <- ler_csv_carimbado(caminho_analise_kit("conitec-stratified-standardization"))
stopifnot(nrow(conitec_estratificada) == 3 * n_distinct(taxas_conitec_por_origem$origin))
# Linhas ordenadas pela taxa favorável geral, a ordem da figura da taxa.
producao <- ler_csv_carimbado(caminho_final("descriptive-production"))
ordem_taxa <- producao$origin[order(producao$rate)]
CORES_CONITEC <- c(`Não avaliada` = CORES_RELATORIO[["cinza"]], `Favorável` = CORES_RELATORIO[["principal"]], `Desfavorável` = CORES_RELATORIO[["secundaria"]])
FORMAS_CONITEC <- c(`Não avaliada` = 15, `Favorável` = 16, `Desfavorável` = 17)
fator_nivel_conitec <- function(x) factor(unname(ROTULOS_CONITEC[x]), levels = names(CORES_CONITEC))
paineis_conitec <- bind_rows(
  taxas_conitec_por_origem |>
    transmute(origin, level = fator_nivel_conitec(conitec_level), value = rate,
              low = wilson_low, high = wilson_high, panel = "Taxa bruta, com intervalo de Wilson a 95%"),
  conitec_estratificada |>
    transmute(origin, level = fator_nivel_conitec(conitec_level), value = standardized_probability,
              low = NA_real_, high = NA_real_, panel = "Ajustada pelo Modelo 1, mesma carteira de casos")
) |>
  mutate(origin = factor(origin, levels = ordem_taxa),
         panel = factor(panel, levels = c("Taxa bruta, com intervalo de Wilson a 95%",
                                          "Ajustada pelo Modelo 1, mesma carteira de casos")))
grafico_conitec_por_natjus <- function(nome_painel) ggplot(filter(paineis_conitec, panel == nome_painel), aes(x = value, y = origin, colour = level, shape = level)) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0, linewidth = 0.45, na.rm = TRUE) +
  geom_point(size = 2) +
  facet_wrap(~level, nrow = 1, labeller = as_labeller(function(x) paste("Conitec:", tolower(x)))) +
  scale_colour_manual(values = CORES_CONITEC, guide = "none") +
  scale_shape_manual(values = FORMAS_CONITEC, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(format(100 * x, decimal.mark = ","), "%"),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Proporção de notas técnicas com conclusão favorável", y = NULL) +
  tema_kit() +
  # Espaço entre painéis para que o 100% de um não encoste no 0% do seguinte.
  theme(panel.spacing.x = unit(9, "mm"), plot.margin = margin(5.5, 14, 5.5, 5.5))
gravar_figura(grafico_conitec_por_natjus(levels(paineis_conitec$panel)[1]), "kit-conitec-rates-by-natjus.png", 7.2, 6.2)
gravar_figura(grafico_conitec_por_natjus(levels(paineis_conitec$panel)[2]), "kit-conitec-adjusted-by-natjus.png", 7.2, 6.2)

# 3. Pares medicamento × CID por limiar da distância entre os extremos ----------

limiares <- ler_csv_carimbado(caminho_final("descriptive-pair-range-thresholds")) |>
  mutate(threshold = round(threshold, 2)) |>
  # Os pisos 5 e 20 foram calculados como sensibilidade e estão no CSV final-*;
  # o texto mostra só o piso de exibição desde 11 de setembro de 2026, e os
  # pisos alternativos esperam uma decisão sobre se voltam.
  filter(floor == PISO_EXIBICAO)
suporte <- ler_csv_carimbado(caminho_final("descriptive-pair-support"))
tabela_limiares <- limiares |>
  mutate(cell = paste0(fmt_inteiro(pairs_at_or_above_threshold), " (", fmt_pct(fraction_at_or_above_threshold, 0), ")")) |>
  arrange(threshold) |>
  transmute(
    `Distância mínima entre extremos` = paste0(formatC(100 * threshold, format = "f", digits = 0), " p.p."),
    `Pares (fração dos elegíveis)` = cell
  )
pares_no_piso <- suporte |> filter(floor == PISO_EXIBICAO)
gravar_tabela_markdown(
  tabela_limiares, "kit-pair-thresholds", caminho_final("descriptive-pair-range-thresholds"),
  titulo = "Pares medicamento × CID por distância entre as taxas favoráveis dos NatJus",
  legenda = sprintf(paste(
    "Pares medicamento × CID em que a distância entre o NatJus mais favorável e o menos favorável alcança cada limiar, com o piso de %d notas por célula.",
    "Célula é a combinação NatJus × medicamento × CID; um par entra quando dois ou mais NatJus, o Nacional incluído, têm células com o piso, e a distância é a maior taxa favorável bruta menos a menor entre essas células, em pontos percentuais.",
    "Cada linha conta os pares cuja distância é igual ou superior ao limiar, e a fração é sobre os %s pares elegíveis; a primeira linha contém todos. {#tbl-pair-thresholds}"),
    PISO_EXIBICAO, fmt_inteiro(pares_no_piso$retained_pairs[1])),
  alinhamento = c("l", "r")
)

# 4. Distribuição da dispersão entre origens por par -----------------------------

# Só as taxas brutas desde 11 de setembro de 2026: as taxas contraídas
# (células puxadas para a média do par por um modelo hierárquico) nunca foram
# discutidas com o autor e saíram do texto; o CSV final-* ainda as traz, e a
# tabela agregada por piso e tipo de estimativa abaixo não é gravada pelo mesmo
# motivo, até uma decisão sobre se voltam.
dispersao <- ler_csv_carimbado(caminho_final("descriptive-pair-dispersion")) |>
  filter(floor == PISO_EXIBICAO, estimate_type == "raw") |>
  mutate(estimate_label = factor("Taxas brutas"))
medianas_dispersao <- dispersao |>
  group_by(estimate_label) |>
  summarise(median_range = median(range), pairs = n(), .groups = "drop")
grafico_dispersao <- ggplot(dispersao, aes(x = range)) +
  geom_histogram(binwidth = 0.05, boundary = 0, fill = CORES_RELATORIO[["principal"]], colour = "#FFFFFF", linewidth = 0.3) +
  # Halo branco sob uma linha tracejada preta: a linha tem que contrastar com
  # as barras azuis que atravessa e com o fundo branco dos dois lados.
  geom_vline(data = medianas_dispersao, aes(xintercept = median_range),
             colour = "#FFFFFF", linewidth = 1.6) +
  geom_vline(data = medianas_dispersao, aes(xintercept = median_range),
             linetype = "dashed", colour = CORES_RELATORIO[["tinta"]], linewidth = 0.7) +
  geom_text(data = medianas_dispersao,
            aes(x = median_range, y = Inf, label = paste0("mediana ", formatC(100 * median_range, format = "f", digits = 0), " p.p.")),
            hjust = -0.1, vjust = 1.5, size = 3.4, colour = CORES_RELATORIO[["tinta"]]) +
  scale_x_continuous(labels = function(x) paste0(format(100 * x), " p.p."), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(xlim = c(0, 1.05)) +
  labs(x = "Distância entre o NatJus mais e o menos favorável no par medicamento × CID", y = "Pares") +
  tema_kit() +
  theme(panel.grid.major.y = element_line(colour = CORES_RELATORIO[["grade"]]))
gravar_figura(grafico_dispersao, "kit-pair-dispersion.png", 7.2, 4.4)

# 5. Células extremas: um NatJus nunca concede, outro sempre concede ------------

extremos_todos <- ler_csv_carimbado(caminho_final("descriptive-pair-extremes"))
extremos <- extremos_todos |>
  filter(floor == PISO_EXIBICAO) |>
  mutate(
    cell_label = paste0(origin, " ", favourable, "/", total, " (IC ", fmt_intervalo_pct(wilson_low, wilson_high, 0), ")"),
    side = if_else(raw_rate == 0, "never", "always")
  )
tabela_extremos <- extremos |>
  group_by(pair_id, medicine, cid, pair_origins) |>
  summarise(
    never = paste(cell_label[side == "never"], collapse = "; "),
    always = paste(cell_label[side == "always"], collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(medicine, cid) |>
  transmute(
    `Medicamento` = rotulo_medicamento(medicine),
    `CID` = cid,
    `Nunca favorável` = never,
    `Sempre favorável` = always,
    `NatJus no par` = as.character(pair_origins)
  )
gravar_tabela_markdown(
  tabela_extremos, "kit-pair-extremes", caminho_final("descriptive-pair-extremes"),
  titulo = "Pares medicamento × CID com conclusões opostas e unânimes entre NatJus",
  legenda = sprintf(paste(
    "Pares medicamento × CID em que ao menos um NatJus nunca concluiu favoravelmente e outro sempre concluiu, no piso de %d notas por célula, em ordem alfabética de medicamento e CID.",
    "Cada célula traz o NatJus, as notas favoráveis sobre o total e o intervalo de Wilson a 95%%, que mostra quanto uma unanimidade em célula pequena ainda é compatível com taxas intermediárias; quando mais de um NatJus está do mesmo lado, as células vêm separadas por ponto e vírgula.",
    "NatJus no par é o número de NatJus com célula no piso, contados os que ficam entre os extremos. O NatJus Nacional entrou na busca, mas não apresenta unanimidade oposta à de outro NatJus no mesmo par entre as células que atendem ao piso. {#tbl-pair-extremes}"),
    PISO_EXIBICAO),
  alinhamento = c("l", "l", "l", "l", "r")
)

# 6. Local contra Nacional dentro do mesmo par -----------------------------------

local_nacional_todos <- ler_csv_carimbado(caminho_final("descriptive-local-national-extremes"))
local_nacional <- local_nacional_todos |>
  filter(floor == PISO_EXIBICAO) |>
  mutate(direction_label = factor(
    recode(direction,
      largest_local_advantage = "Maior vantagem do local",
      largest_national_advantage = "Maior vantagem do Nacional"
    ),
    levels = c("Maior vantagem do local", "Maior vantagem do Nacional")
  )) |>
  arrange(local_origin, direction_label)
tabela_local_nacional <- local_nacional |>
  transmute(
    `NatJus local` = local_origin,
    `Par` = if_else(direction == "largest_local_advantage", "maior diferença", "menor diferença"),
    `Medicamento` = rotulo_medicamento(medicine),
    `CID` = cid,
    `Local` = paste0(fmt_inteiro(local_favourable), "/", fmt_inteiro(local_total), " = ", fmt_pct(rate), " (", fmt_intervalo_pct(local_wilson_low, local_wilson_high), ")"),
    `Nacional` = paste0(fmt_inteiro(national_favourable), "/", fmt_inteiro(national_total), " = ", fmt_pct(national_rate), " (", fmt_intervalo_pct(national_wilson_low, national_wilson_high), ")"),
    `Dif. (p.p.)` = fmt_pp(local_minus_national)
  )
locais_por_piso <- local_nacional_todos |>
  group_by(floor) |>
  summarise(locals = n_distinct(local_origin), .groups = "drop")
locais_em <- function(f) {
  linha <- locais_por_piso[locais_por_piso$floor == f, ]
  if (nrow(linha) == 0) "0" else as.character(linha$locals)
}
# Os NatJus locais sem um único par comparável no piso de exibição, para que a
# legenda diga quem está de fora em vez de deixar o leitor contar.
locais_sem_par <- setdiff(
  setdiff(ler_csv_carimbado(caminho_final("descriptive-production"))$origin, "Nacional"),
  local_nacional$local_origin
)
texto_locais_sem_par <- if (length(locais_sem_par) == 0) "" else
  paste0(" Fica de fora ", if (length(locais_sem_par) == 1) "o NatJus " else "os NatJus ",
         paste(locais_sem_par, collapse = ", "), ", sem par comparável neste piso.")
gravar_tabela_markdown(
  tabela_local_nacional, "kit-local-national", caminho_final("descriptive-local-national-extremes"),
  titulo = "Pares com maior e menor diferença de taxa favorável entre cada NatJus local e o Nacional",
  legenda = sprintf(paste(
    "Para cada NatJus local, em ordem alfabética, o par medicamento × CID em que a diferença entre sua taxa favorável e a do NatJus Nacional é maior (linha maior diferença: o par em que o local mais excede o Nacional) e o par em que é menor (linha menor diferença: o par em que mais fica abaixo dele), entre os pares em que os dois têm ao menos %d notas.",
    "Local e Nacional trazem notas favoráveis sobre o total, a taxa e o intervalo de Wilson a 95%%; a diferença é a taxa local menos a nacional, em pontos percentuais.",
    "Quando o NatJus tem poucos pares comparáveis, as duas diferenças podem ter o mesmo sinal, e com um único par as duas linhas coincidem.",
    "%s NatJus locais têm ao menos um par comparável neste piso.%s {#tbl-local-national}"),
    PISO_EXIBICAO, locais_em(PISO_EXIBICAO), texto_locais_sem_par),
  alinhamento = c("l", "l", "l", "l", "l", "l", "r")
)

# 7. Tabelas por NatJus dos dois modelos ------------------------------------------

candidatos_origem <- ler_csv_carimbado(caminho_final("origin-effect-candidates")) |>
  # Contraste, probabilidade padronizada e taxa bruta não dependem da âncora;
  # fico com uma âncora para ter uma linha por origem.
  filter(anchor == "pooled_outcome_logit")
wald <- ler_csv_carimbado(caminho_final("origin-effect-wald-intervals")) |>
  select(model, origin, conf_low, conf_high)
populacoes <- ler_csv_carimbado(caminho_final("fit-populations-by-origin")) |>
  select(model, origin, coverage)
tabelas_origem <- candidatos_origem |>
  left_join(wald, by = c("model", "origin")) |>
  left_join(populacoes, by = c("model", "origin"))

for (id_modelo in names(ROTULOS_MODELO)) {
  linhas <- tabelas_origem |>
    filter(model == id_modelo) |>
    arrange(desc(standardized_probability))
  tabela <- linhas |>
    transmute(
      `NatJus` = origin,
      `Notas` = fmt_inteiro(notes),
      `Taxa bruta` = fmt_pct(raw_rate),
      `Probabilidade padronizada` = fmt_pct(standardized_probability),
      `Contraste vs. Nacional (logito)` = if_else(origin == "Nacional", "referência", fmt_num(contrast_vs_nacional_logit, 2)),
      `IC 95% (Wald)` = if_else(origin == "Nacional", "", fmt_intervalo_num(conf_low, conf_high, 2))
    )
  if (id_modelo == "model_1_1") {
    tabela[["Cobertura da estimação"]] <- fmt_pct(linhas$coverage)
  }
  nome <- paste0("kit-", gsub("_", "-", id_modelo), "-origins")
  legenda <- sprintf(paste(
    "Taxa favorável bruta, probabilidade padronizada e contraste em relação ao NatJus Nacional por NatJus, %s, ajuste definitivo, em ordem decrescente de probabilidade padronizada.",
    "Notas é o total do NatJus na amostra e Taxa bruta a fração observada de conclusões favoráveis; a probabilidade padronizada é a que o NatJus teria sobre a composição de casos da amostra inteira, mantidas as demais variáveis;",
    "o contraste é o efeito de NatJus na escala do logito, com o Nacional como referência em zero (positivo é mais favorável que o Nacional para o mesmo pedido), e o intervalo é de Wald a 95%%.%s {#tbl-%s-origins}"),
    if (id_modelo == "model_1_1") "Modelo 2 (indicadores de NatJus, efeitos fixos de medicamento e CID)" else "Modelo 1 (interceptos aleatórios de NatJus, medicamento e CID)",
    if (id_modelo == "model_1_1") " Cobertura da estimação é a fração das notas do NatJus que o estimador conservou depois de remover níveis de medicamento e CID sem variação." else "",
    gsub("_", "-", id_modelo)
  )
  gravar_tabela_markdown(
    tabela, nome,
    paste(c(caminho_final("origin-effect-candidates"), caminho_final("origin-effect-wald-intervals"),
            if (id_modelo == "model_1_1") caminho_final("fit-populations-by-origin")), collapse = "; "),
    titulo = paste("Taxas favoráveis e contrastes por NatJus, Modelo", if (id_modelo == "model_1_1") "2" else "1"),
    legenda = legenda,
    alinhamento = c("l", "r", "r", "r", "r", "c", if (id_modelo == "model_1_1") "r" else NULL)
  )
}

# 8. Contrastes ajustados de origem do Modelo 1 (id model_1_2), sem a linha de referência ---
#
# Cópia da figura definitiva para o manuscrito. O Nacional é a referência do
# modelo, então o contraste dele é zero por construção e não tem intervalo;
# desenhá-lo como ponto sem barra era lido como estimativa sem incerteza. A
# linha sai e a linha tracejada no zero a representa, o que a legenda diz no
# manuscrito. A figura final fica intacta.

efeitos_ajustados <- ler_csv_carimbado(caminho_final("origin-effect-wald-intervals")) |>
  filter(model == "model_1_2", origin != "Nacional") |>
  mutate(origin = factor(origin, levels = origin[order(contrast_vs_nacional)]))
grafico_ajustado <- ggplot(efeitos_ajustados, aes(x = contrast_vs_nacional, y = origin)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = CORES_RELATORIO[["cinza"]], linewidth = 0.5) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), orientation = "y", width = 0.32,
                colour = CORES_RELATORIO[["principal"]], linewidth = 0.55) +
  geom_point(size = 2.6, colour = CORES_RELATORIO[["principal"]]) +
  labs(x = "Contraste ajustado no logito em relação ao NatJus Nacional", y = NULL) +
  tema_kit() +
  theme(legend.position = "none")
gravar_figura(grafico_ajustado, "kit-adjusted-origin-effects.png", 7.2, 6.0)

# 9. Taxa bruta contra probabilidade padronizada, Modelo 1 (id model_1_2) -------------------
#
# A manchete do corpo do texto sobre a divergência ajustada, para um leitor sem
# estatística: para cada NatJus, a taxa favorável que ele teve (bruta) e a taxa
# que ele teria sobre a composição de casos da amostra inteira (padronizada). A
# distância entre as duas é o que a composição de casos explica; o espalhamento
# dos pontos padronizados é o que sobra depois dos controles. O Nacional mantém
# a linha dele, e uma linha tracejada na probabilidade padronizada dele é a
# referência. Forma e cor distinguem as duas grandezas.

padronizadas <- tabelas_origem |>
  filter(model == "model_1_2") |>
  mutate(origin = factor(origin, levels = origin[order(standardized_probability)]))
padronizada_nacional <- padronizadas$standardized_probability[padronizadas$origin == "Nacional"]
padronizadas_longo <- padronizadas |>
  select(origin, `Taxa bruta` = raw_rate, `Probabilidade padronizada` = standardized_probability) |>
  pivot_longer(-origin, names_to = "quantity", values_to = "value") |>
  mutate(quantity = factor(quantity, levels = c("Taxa bruta", "Probabilidade padronizada")))
grafico_padronizadas <- ggplot() +
  geom_vline(xintercept = padronizada_nacional, linetype = "dashed", colour = CORES_RELATORIO[["cinza"]], linewidth = 0.5) +
  geom_segment(data = padronizadas,
               aes(x = raw_rate, xend = standardized_probability, y = origin, yend = origin),
               colour = CORES_RELATORIO[["cinza_claro"]], linewidth = 0.6) +
  geom_point(data = padronizadas_longo, aes(x = value, y = origin, shape = quantity, colour = quantity, fill = quantity),
             size = 2.6, stroke = 0.8) +
  scale_shape_manual(values = c(`Taxa bruta` = 21, `Probabilidade padronizada` = 16), name = NULL) +
  scale_colour_manual(values = c(`Taxa bruta` = CORES_RELATORIO[["cinza"]], `Probabilidade padronizada` = CORES_RELATORIO[["principal"]]), name = NULL) +
  scale_fill_manual(values = c(`Taxa bruta` = "white", `Probabilidade padronizada` = CORES_RELATORIO[["principal"]]), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(format(100 * x, decimal.mark = ","), "%"),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Proporção de notas técnicas com conclusão favorável", y = NULL) +
  tema_kit()
gravar_figura(grafico_padronizadas, "kit-standardized-probability.png", 7.2, 6.2)

# 10. Parte fixa dos dois modelos como razões de chances ----------------------------
#
# Os coeficientes das covariáveis comuns aos dois modelos, sem os indicadores
# de origem do Modelo 2 (id model_1_1), que são a tabela por NatJus. A
# tendência temporal mensal também é lida por ano, exp(12 × coeficiente), a
# convenção que o apêndice declara para a escala de tempo.

ROTULOS_TERMO <- c(
  cov_selRecomendacaoConitecFavoravel = "Conitec: favorável (vs. não avaliada)",
  cov_selRecomendacaoConitecDesfavoravel = "Conitec: desfavorável (vs. não avaliada)",
  cov_selIndicacaoConformidadeSim = "Indicação em conformidade: sim (vs. não)",
  `cov_selIndicacaoConformidadeN/I` = "Indicação em conformidade: N/I",
  cov_selPrevistoProtocoloSim = "Previsto em protocolo: sim (vs. não)",
  `cov_selPrevistoProtocoloN/I` = "Previsto em protocolo: N/I",
  cov_selRegistroAnvisaSim = "Registro na Anvisa: sim (vs. não)",
  `cov_selRegistroAnvisaN/I` = "Registro na Anvisa: N/I",
  cov_selDisponivelSusSim = "Disponível no SUS: sim (vs. não)",
  `cov_selDisponivelSusN/I` = "Disponível no SUS: N/I",
  cov_selOncologicoSim = "Oncológico: sim (vs. não)",
  `cov_selOncologicoN/I` = "Oncológico: N/I",
  cov_selExisteBiossimilarSim = "Existe biossimilar: sim (vs. não)",
  `cov_selExisteBiossimilarN/I` = "Existe biossimilar: N/I",
  cov_selStaGeneroMasculino = "Sexo: masculino (vs. feminino)",
  `cov_selStaGeneroN/I` = "Sexo: N/I",
  `cov_selDefensoriaPublicaMinisterio Publico` = "Representação: Ministério Público (vs. Defensoria Pública)",
  `cov_selDefensoriaPublicaN/I` = "Representação: N/I",
  ano_mes = "Mês de emissão, por mês"
)
coeficientes <- ler_csv_carimbado(caminho_final("fit-coefficients")) |>
  filter(!startsWith(term, "origin::"), term != "(Intercept)")
stopifnot(all(coeficientes$term %in% names(ROTULOS_TERMO)))
tendencia_anual <- coeficientes |>
  filter(term == "ano_mes") |>
  mutate(
    term = "ano_mes_yearly",
    odds_ratio = exp(12 * estimate_logit),
    odds_ratio_low = exp(12 * conf_low_logit),
    odds_ratio_high = exp(12 * conf_high_logit)
  )
ROTULOS_TERMO <- c(ROTULOS_TERMO, ano_mes_yearly = "Mês de emissão, por ano (12 × coeficiente mensal)")
efeitos_fixos <- bind_rows(coeficientes, tendencia_anual) |>
  mutate(
    term = factor(term, levels = names(ROTULOS_TERMO)),
    # Três decimais para a tendência mensal, que com duas arredonda para 0,98.
    digits = if_else(term == "ano_mes", 3L, 2L),
    # formatC recebe um único valor de digits, então as células são montadas
    # linha a linha.
    # Marcas de significância do teste de Wald do coeficiente contra zero, na
    # convenção das tabelas de regressão. A leitura anual da tendência é o
    # coeficiente mensal reescalado, então ela carrega o mesmo teste.
    p_value = 2 * pnorm(-abs(estimate_logit / standard_error)),
    stars = as.character(cut(p_value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", ""), right = FALSE)),
    cell = mapply(function(estimativa, inferior, superior, digitos, estrelas) {
      paste0(fmt_num(estimativa, digitos), " ", fmt_intervalo_num(inferior, superior, digitos), estrelas)
    }, odds_ratio, odds_ratio_low, odds_ratio_high, digits, stars, USE.NAMES = FALSE)
  ) |>
  select(model, term, cell) |>
  pivot_wider(names_from = model, values_from = cell) |>
  arrange(term) |>
  transmute(
    `Variável` = unname(ROTULOS_TERMO[as.character(term)]),
    `Modelo 1: RC [IC 95%]` = model_1_2,
    `Modelo 2: RC [IC 95%]` = model_1_1
  )
gravar_tabela_markdown(
  efeitos_fixos, "kit-fixed-effects", caminho_final("fit-coefficients"),
  titulo = "Razões de chances da parte fixa dos Modelos 1 e 2",
  legenda = paste(
    "Razões de chances (RC) da parte fixa dos Modelos 1 e 2, com intervalo de Wald a 95%, ajustes definitivos, variáveis em ordem alfabética do termo do modelo.",
    "Cada razão compara a chance de conclusão favorável do nível indicado com a do nível de referência entre parênteses, mantidas as demais variáveis, o NatJus, o medicamento e a categoria CID;",
    "uma razão acima de 1 eleva a chance, abaixo de 1 reduz, e um intervalo que contém 1 não distingue o nível da referência.",
    "N/I é o nível auxiliar do campo não preenchido. Os indicadores de NatJus do Modelo 2 estão na tabela por NatJus do apêndice.",
    "A tendência temporal é estimada por mês e lida também por ano.",
    "Teste de Wald do coeficiente contra zero: \\* p < 0,05; \\*\\* p < 0,01; \\*\\*\\* p < 0,001. {#tbl-fixed-effects}"
  ),
  alinhamento = c("l", "r", "r")
)

# 11. Notas por ano, o Nacional contra os NatJus locais somados ----------------
#
# A série anual da amostra de análise, publicada por 10-manuscript-numbers.R.
# Duas barras por ano para o leitor comparar o Nacional com a soma dos locais;
# o valor fica sobre cada barra. O Nacional sai cheio e os locais vazados, com
# contorno, para que a legenda não dependa só da cor: o azul e o vermelhão
# têm luminância parecida e se confundem em escala de cinza. O
# último ano da série é parcial e diz isso no rótulo dele.

anual <- ler_csv_carimbado(file.path(DIR_ANALISE, "kit-production-by-origin-year.csv"))
ultimo_ano <- max(anual$year)
unidades_anuais <- anual |>
  mutate(unit = if_else(origin == "Nacional", "NatJus Nacional", "NatJus locais")) |>
  count(year, unit, wt = notes, name = "notes") |>
  mutate(
    unit = factor(unit, levels = c("NatJus Nacional", "NatJus locais")),
    year_label = if_else(year == ultimo_ano, paste0(year, "\n(até ago.)"), as.character(year))
  )
grafico_anual <- ggplot(unidades_anuais, aes(x = year_label, y = notes, fill = unit, colour = unit)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, linewidth = 0.6) +
  geom_text(aes(label = fmt_inteiro(notes)), position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3, colour = CORES_RELATORIO[["tinta"]]) +
  scale_fill_manual(values = c("NatJus Nacional" = CORES_RELATORIO[["principal"]], "NatJus locais" = "white"), name = NULL) +
  scale_colour_manual(values = c("NatJus Nacional" = CORES_RELATORIO[["principal"]], "NatJus locais" = CORES_RELATORIO[["secundaria"]]), name = NULL) +
  scale_y_continuous(labels = fmt_inteiro, expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Notas técnicas na amostra") +
  tema_kit() +
  theme(panel.grid.major.y = element_line(colour = CORES_RELATORIO[["grade"]]), panel.grid.major.x = element_blank())
gravar_figura(grafico_anual, "kit-production-by-year.png", 8.4, 4.4)

# 12. Níveis que o estimador de efeitos fixos removeu ------------------------------

removidos <- ler_csv_carimbado(caminho_final("fixest-removed-levels")) |>
  group_by(variable) |>
  summarise(levels = n(), notes = sum(notes), .groups = "drop")
tabela_removidos <- removidos |>
  transmute(
    `Variável` = recode(variable, medicine = "Medicamento", cid = "Categoria CID"),
    `Níveis removidos` = fmt_inteiro(levels),
    `Notas nesses níveis` = fmt_inteiro(notes)
  )
gravar_tabela_markdown(
  tabela_removidos, "kit-fixest-removed", caminho_final("fixest-removed-levels"),
  titulo = "Níveis de medicamento e CID removidos pelo estimador do Modelo 2",
  legenda = "Níveis de medicamento e de categoria CID que o estimador de efeitos fixos do Modelo 2 removeu por ausência de variação na conclusão (todas as notas do nível favoráveis ou todas desfavoráveis), e as notas que deixaram de ser estimadas por pertencerem a um nível removido; uma nota pode contar nas duas linhas. Nenhum tratamento de nível raro foi aplicado nesta versão. {#tbl-fixest-removed}",
  alinhamento = c("l", "r", "r")
)

# 13. Tabela de ajustes da rodada definitiva, apresentada em português ---------
#
# Os CSVs de ajuste da rodada definitiva trazem valores canônicos em inglês em
# algumas células; o apêndice inclui esta versão traduzida e transposta.

ajustes <- ler_csv_carimbado(caminho_final("fit-summary")) |>
  left_join(ler_csv_carimbado(caminho_final("fit-populations")) |>
              select(model, input_notes, estimated_notes, discarded_notes, coverage), by = "model") |>
  left_join(ler_csv_carimbado(caminho_final("fit-diagnostics")) |>
              select(model, convergence_status, identifiability), by = "model")
tabela_ajustes <- ajustes |>
  transmute(
    `Modelo` = unname(ROTULOS_MODELO[model]),
    `Notas de entrada` = fmt_inteiro(input_notes),
    `Notas estimadas` = fmt_inteiro(estimated_notes),
    `Descartes internos` = fmt_inteiro(discarded_notes),
    `Cobertura` = fmt_pct(coverage),
    `Convergência` = recode(convergence_status, converged = "convergiu", .default = convergence_status),
    `Identificação` = recode(identifiability, identified = "identificado", .default = identifiability),
    `Log-verossimilhança` = sub("^-", "\u2212", format(round(log_likelihood, 1), big.mark = ".", decimal.mark = ",", nsmall = 1, trim = TRUE)),
    `Parâmetros` = fmt_inteiro(parameters),
    `AIC` = format(round(aic, 1), big.mark = ".", decimal.mark = ",", nsmall = 1, trim = TRUE)
  )
# Dois modelos e nove grandezas: transposta, para que a página comporte três
# colunas em vez de dez. As linhas vêm na ordem do CSV (ids internos), e as
# colunas têm que ler Modelo 1, Modelo 2, a ordem de exibição.
tabela_ajustes <- tabela_ajustes[order(match(tabela_ajustes$Modelo, ROTULOS_MODELO)), ]
ajustes_transpostos <- data.frame(
  Quantidade = names(tabela_ajustes)[-1],
  lapply(seq_len(nrow(tabela_ajustes)), function(i) unlist(tabela_ajustes[i, -1], use.names = FALSE)),
  check.names = FALSE, stringsAsFactors = FALSE
)
names(ajustes_transpostos)[-1] <- tabela_ajustes$Modelo
gravar_tabela_markdown(
  ajustes_transpostos, "kit-fits",
  titulo = "Amostras estimadas e diagnósticos de ajuste dos Modelos 1 e 2",
  paste(caminho_final("fit-summary"), caminho_final("fit-populations"), caminho_final("fit-diagnostics"), sep = "; "),
  legenda = paste(
    "Ajustes definitivos dos Modelos 1 e 2. Notas de entrada é a amostra analítica; Notas estimadas, as que o estimador conservou; Descartes internos, as que ele removeu por pertencerem a nível de medicamento ou CID sem variação na conclusão; Cobertura, a razão entre as duas primeiras.",
    "Convergência e Identificação são os diagnósticos do estimador; Log-verossimilhança, Parâmetros e AIC descrevem o ajuste.",
    "Os valores de AIC não comparam os dois modelos entre si: são calculados sobre conjuntos de notas diferentes, porque o Modelo 2 descarta as notas de níveis sem variação na conclusão, e o do Modelo 1 vem da verossimilhança marginal, que integra os efeitos aleatórios, enquanto o do Modelo 2 vem da verossimilhança com um parâmetro por nível de medicamento e de CID. {#tbl-fits}"
  ),
  alinhamento = c("l", "r", "r")
)
