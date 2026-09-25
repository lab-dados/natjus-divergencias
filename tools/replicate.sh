#!/usr/bin/env bash
# Roda um nível da replicação, e os de baixo, na ordem. Ver REPLICATION.md.
#
#   tools/replicate.sh 1   renderiza o manuscrito a partir do que está versionado
#   tools/replicate.sh 2   antes disso, reconstrói figuras, tabelas e números
#   tools/replicate.sh 3   antes disso, reajusta os modelos a partir dos parquets versionados
#   tools/replicate.sh 4   antes disso, reconstrói a amostra a partir da base no Hugging Face
#
# Os níveis 2 a 4 terminam comparando o resultado com os arquivos versionados.
# No nível 2 a comparação de dados, tabelas e _variables.yml é byte a byte,
# fora os carimbos de data e hora (linhas generated_at e *_built_at) e o hash
# de um deles que o _variables.yml registra. As figuras só são listadas: os
# bytes de um PNG dependem da fonte e das versões de cairo e freetype da
# máquina, e exigir igualdade reprovaria em outra máquina uma rodada correta.
# Nos níveis 3 e 4, que reajustam os modelos, quem compara é
# tools/compare-replication.R, que diz por que não pode ser byte a byte.
# Qualquer diferença além dessas é falha de replicação, e o script termina com
# erro.
set -euo pipefail
este_script=$(readlink -f "$0")
cd "$(dirname "$este_script")/.."
nivel=${1:-}
[[ $nivel =~ ^[1234]$ ]] || { sed -n '2,7p' "$este_script"; exit 2; }

rodar() { echo "== Rscript R/$1"; Rscript "R/$1"; }

# O commit e o estado da árvore vão para o log, que de outro modo não diria de
# que versão do código saiu.
echo "== nível $nivel, commit $(git rev-parse HEAD)"
git status --short --untracked-files=no | sed 's/^/   alterado antes da rodada: /'

if (( nivel == 4 )); then
  # A base não é versionada: tools/baixar-base.sh a baixa, se ainda não estiver
  # em data/hf/, e R/enatjus/read-base.R confere o hash a cada leitura.
  constante() { sed -n "s/^$1 <- \"\(.*\)\"$/\1/p" R/enatjus/read-base.R; }
  [[ -f ${NATJUS_BASE:-data/hf/$(constante BASE_HF_FILE)} ]] || tools/baixar-base.sh
  # O 04 confere que os produtos dos três anteriores não mudam enquanto ele roda.
  for script in 00-setup.R 01-sample.R 02-institutions.R 03-covariates.R 04-origins.R; do
    rodar "$script"
  done
fi
if (( nivel >= 3 )); then
  # O 08 ajusta as etapas e publica; o 11 lê o Modelo 1 que o 08 grava.
  for script in 08-final-analysis.R 11-conitec-stratified-standardization.R; do
    rodar "$script"
  done
fi
if (( nivel >= 2 )); then
  # O 10 vem antes do 09: o 09 lê a série anual que o 10 grava. Na ordem
  # inversa, as figuras sairiam com a série da rodada anterior, e a comparação
  # abaixo não perceberia.
  for script in 06-figures.R 07-manuscript-tables.R 10-manuscript-numbers.R 09-writing-kit.R 13-attrition-flowchart.R; do
    rodar "$script"
  done
  if (( nivel >= 3 )); then
    # Os modelos reajustados não saem idênticos byte a byte: o comparador
    # confere os dados exatamente e os resultados numéricos com tolerância.
    echo "== comparação com os arquivos versionados"
    Rscript tools/compare-replication.R
  else
    echo "== diferenças contra os arquivos versionados, fora os carimbos de geração"
    if git diff -I 'generated_at:' -I '_built_at:' -I 'kit-production-by-origin-year\.csv sha256' --stat --exit-code -- data tables _variables.yml; then
      echo "nenhuma"
    else
      echo "FALHA DE REPLICAÇÃO: os arquivos acima diferem dos versionados" >&2
      exit 1
    fi
    figuras=$(git diff --name-only -- figures)
    if [[ -n $figuras ]]; then
      echo "figuras regravadas com bytes diferentes (confira visualmente):"
      echo "$figuras" | sed 's/^/   /'
    fi
  fi
fi
tools/render.sh
tools/render-pdf.sh
