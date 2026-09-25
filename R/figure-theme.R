# Aparência comum das figuras do manuscrito -----------------------------------
#
# Carregado por 06-figures.R, 09-writing-kit.R e 13-attrition-flowchart.R;
# nunca roda sozinho. As cores
# das séries vêm da paleta de Okabe e Ito, desenhada para continuar
# distinguível por quem tem daltonismo: azul para a série principal,
# vermelhão para uma segunda série ao lado dele, cinza para linhas de
# referência. Sobre branco, o azul tem 5,2:1 e o vermelhão 3,9:1, acima do
# piso de 3:1 da WCAG para objetos gráficos. Duas séries nunca dependem só da
# cor: quem chama combina a cor com forma ou tipo de linha.
#
# A fonte do texto é a do PDF, a IBM Plex Sans, quando o fontconfig
# consegue resolvê-la (o dispositivo PNG do cairo a desenha pelo fontconfig);
# caso contrário, a sans padrão, com uma mensagem, para que uma máquina sem a
# fonte ainda produza todas as figuras.

CORES_RELATORIO <- c(
  principal = "#0072B2",
  secundaria = "#D55E00",
  cinza = "#4a4a4a",
  cinza_claro = "#767676",
  tinta = "#1a1a1a",
  grade = "#e5e5e5"
)

FONTE_RELATORIO <- "IBM Plex Sans"

fonte_relatorio <- local({
  resolvida <- NULL
  function() {
    if (!is.null(resolvida)) return(resolvida)
    familia <- FONTE_RELATORIO
    correspondencia <- tryCatch(
      system2("fc-match", c(shQuote(familia), "family"), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0), warning = function(w) character(0)
    )
    if (length(correspondencia) > 0 && any(grepl(familia, correspondencia, fixed = TRUE))) {
      resolvida <<- familia
    } else {
      message("A fonte ", familia, " não está disponível para o fontconfig; as figuras usam a sans padrão.")
      resolvida <<- ""
    }
    resolvida
  }
})

# Preset de slides -------------------------------------------------------------
#
# Com NATJUS_FIGURAS_PRESET=slides no ambiente, os scripts que gravam figuras
# escrevem em figures/slides/ (pasta ignorada pelo Git, fora do manuscrito)
# versões em proporção de tela, 11 polegadas de largura e no máximo 6,2 de
# altura, com o corpo do texto em 15 em vez de 11: projetada num slide 16:9,
# a figura do manuscrito fica com o texto miúdo. Nada muda sem a variável, e
# os PNG de figures/ nunca são tocados no preset. O deck que consome essas
# figuras mora fora deste repositório.

preset_figuras <- function() {
  preset <- Sys.getenv("NATJUS_FIGURAS_PRESET", unset = "")
  if (preset == "") "manuscrito" else preset
}

dir_figuras_preset <- function(padrao = "figures") {
  if (preset_figuras() == "slides") file.path(padrao, "slides") else padrao
}

dimensoes_preset <- function(largura, altura) {
  if (preset_figuras() != "slides") return(c(largura = largura, altura = altura))
  c(largura = 11, altura = min(altura * 11 / largura, 6.2))
}

tema_relatorio <- function(tamanho_base = if (preset_figuras() == "slides") 15 else 11,
                           posicao_legenda = "top") {
  ggplot2::theme_minimal(base_size = tamanho_base, base_family = fonte_relatorio()) +
    ggplot2::theme(
      legend.position = posicao_legenda,
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = CORES_RELATORIO[["grade"]]),
      axis.text = ggplot2::element_text(colour = CORES_RELATORIO[["tinta"]]),
      axis.title = ggplot2::element_text(colour = CORES_RELATORIO[["tinta"]]),
      legend.text = ggplot2::element_text(colour = CORES_RELATORIO[["tinta"]]),
      strip.text = ggplot2::element_text(colour = CORES_RELATORIO[["tinta"]], face = "bold")
    )
}
