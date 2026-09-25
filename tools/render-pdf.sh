#!/usr/bin/env sh
# Renderiza a diagramação em Typst do artigo com o apêndice em duas versões:
# report.pdf, com a capa da série de working papers, e report-neutro.pdf, o
# mesmo documento sem capa e sem série, para publicar fora da série. As duas
# saem do mesmo report.qmd; a neutra só liga o metadado `neutro`, que
# typst/typst-show.typ repassa ao perfil.
#
# A tipografia de typst/typst-template.typ foi calibrada na série 0.15 do
# Typst, então um binário 0.15 tem preferência sobre o 0.14 que o Quarto
# embute: primeiro a instalação avulsa do projeto revista-automatizada, depois
# qualquer typst no PATH. QUARTO_TYPST já definido no ambiente vence.
set -eu
cd "$(dirname "$0")/.."

if [ -z "${QUARTO_TYPST:-}" ]; then
  for candidato in "$HOME/.local/share/typst-0.15/typst" "$(command -v typst || true)"; do
    if [ -n "$candidato" ] && [ -x "$candidato" ] \
       && "$candidato" --version 2>/dev/null | grep -q ' 0\.15\.'; then
      QUARTO_TYPST="$candidato"
      export QUARTO_TYPST
      break
    fi
  done
fi

# Só as fontes de typst/fonts/: com as do sistema, uma IBM Plex Sans variável
# instalada na máquina ganhava das estáticas do repositório, e o PDF mudava de
# fonte conforme o computador que o compilava.
export TYPST_IGNORE_SYSTEM_FONTS=true

# A neutra vem primeiro: o Quarto compila sempre para report.pdf e só depois
# renomeia para o --output, então na ordem inversa ela levaria junto o
# report.pdf da série.
quarto render report.qmd --to typst -M neutro=true --output report-neutro.pdf "$@"
quarto render report.qmd --to typst "$@"
