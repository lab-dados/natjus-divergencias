#!/usr/bin/env bash
# Baixa do Hugging Face a base que o artigo analisa e confere o SHA-256.
#
#   tools/baixar-base.sh
#
# O dataset tem acesso sob aprovação: peça acesso na página do dataset e faça
# login antes (`uvx --from huggingface_hub hf auth login`). O arquivo vai para
# data/hf/, fora do Git, onde R/enatjus/read-base.R o procura. Dataset,
# revisão, caminho e hash são lidos de R/enatjus/read-base.R, a fonte única.
set -euo pipefail
este_script=$(readlink -f "$0")
cd "$(dirname "$este_script")/.."

constante() { sed -n "s/^$1 <- \"\(.*\)\"$/\1/p" R/enatjus/read-base.R; }
dataset=$(constante BASE_HF_DATASET)
revisao=$(constante BASE_HF_REVISION)
arquivo=$(constante BASE_HF_FILE)
hash=$(constante BASE_SHA256)
[[ -n $dataset && -n $revisao && -n $arquivo && -n $hash ]] || { echo "constantes da base não encontradas em R/enatjus/read-base.R" >&2; exit 1; }

uvx --from huggingface_hub hf download "$dataset" "$arquivo" \
  --repo-type dataset --revision "$revisao" --local-dir data/hf
echo "$hash  data/hf/$arquivo" | sha256sum --check
