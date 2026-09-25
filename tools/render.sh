#!/usr/bin/env bash
# Renderiza o manuscrito depois de guardar uma cópia de todo documento Word que
# já existe. Os docx derivam dos fontes .qmd, mas o autor pode ter editado um
# deles à mão; o quarto render os sobrescreve sem avisar, e a cópia em backups/
# (ignorada pelo git) é o único caminho de volta.
set -euo pipefail
cd "$(dirname "$0")/.."
destino="backups/docx-$(date +%Y-%m-%d-%H%M%S)"
mapfile -t existentes < <(ls paper.docx appendix.docx sections/*.docx 2>/dev/null || true)
if [ "${#existentes[@]}" -gt 0 ]; then
  mkdir -p "$destino/sections"
  for f in "${existentes[@]}"; do cp -p "$f" "$destino/$f"; done
  echo "${#existentes[@]} docx guardados em $destino"
fi
quarto render "$@"
# Um arquivo só, com artigo e apêndice, para quem quer ler num documento único.
if [ "$#" -eq 0 ]; then
  uv run tools/compose-docx.py paper.docx appendix.docx paper-with-appendix.docx
fi
