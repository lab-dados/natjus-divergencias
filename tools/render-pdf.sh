#!/usr/bin/env sh
# Renderiza a diagramação em Typst do artigo com o apêndice em report.pdf,
# com a capa da série de working papers.
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

quarto render report.qmd --to typst "$@"
