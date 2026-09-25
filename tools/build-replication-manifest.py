# /// script
# requires-python = ">=3.10"
# ///
"""Grava replication-manifest.csv: cada tabela e figura que o manuscrito
inclui, os scripts que a gravam ou leem e onde ela aparece.

Uso: uv run tools/build-replication-manifest.py

A lista vem dos includes dos fontes qmd, então acompanha o texto. Tabela ou
figura versionada que nenhum fonte inclui é análise que rodamos e não está no
artigo; essas vão para o segundo bloco do arquivo, para o leitor distinguir
uma coisa da outra sem ler os scripts.
"""
import csv
import re
import subprocess
from pathlib import Path

FONTES = ["paper.qmd", "appendix.qmd", *sorted(str(p) for p in Path("sections").glob("*.qmd"))]
# Scripts que gravam itens de apresentação. Os scripts de ajuste gravam CSV e
# modelos, que estes leem.
SCRIPTS_DE_APRESENTACAO = ["R/06-figures.R", "R/07-manuscript-tables.R", "R/08-final-analysis.R",
                           "R/09-writing-kit.R", "R/13-attrition-flowchart.R"]
POR_PREFIXO = {"kit-": "R/09-writing-kit.R",
               "final-": "R/08-final-analysis.R"}
ITEM = re.compile(r"(?:tables/manuscript|figures)/[\w.-]+\.(?:md|png|svg|pdf)")

texto_dos_scripts = {s: Path(s).read_text(encoding="utf-8") for s in SCRIPTS_DE_APRESENTACAO}


def scripts_do_item(caminho):
    """Scripts que citam o item pelo nome. A busca é por trecho de texto, então
    pega quem grava, quem lê e quem só menciona em comentário."""
    nome = Path(caminho).stem
    achados = [s for s, texto in texto_dos_scripts.items() if nome in texto]
    if achados:
        return ";".join(achados)
    # As tabelas por modelo recebem o nome por paste0(), então o nome do arquivo
    # não está no script. O cabeçalho do próprio arquivo diz quando ele não tem
    # script nenhum.
    if caminho.endswith(".md") and "written by hand" in Path(caminho).read_text(encoding="utf-8")[:600]:
        return "written by hand"
    return next((s for prefixo, s in POR_PREFIXO.items() if nome.startswith(prefixo)), "not found")


usados = {}
for fonte in FONTES:
    for numero, linha in enumerate(Path(fonte).read_text(encoding="utf-8").splitlines(), 1):
        for item in ITEM.findall(linha):
            usados.setdefault(item, (fonte, numero))

versionados = subprocess.run(["git", "ls-files", "-z", "tables/manuscript", "figures"],
                             capture_output=True, text=True, check=True).stdout.split("\0")
fora_do_artigo = sorted(v for v in versionados if ITEM.fullmatch(v) and v not in usados)

with open("replication-manifest.csv", "w", newline="", encoding="utf-8") as arquivo:
    saida = csv.writer(arquivo)
    saida.writerow(["item", "in_article", "included_at", "scripts"])
    for item, (fonte, numero) in sorted(usados.items(), key=lambda par: par[1]):
        saida.writerow([item, "yes", f"{fonte}:{numero}", scripts_do_item(item)])
    for item in fora_do_artigo:
        saida.writerow([item, "no", "", scripts_do_item(item)])
print(f"{len(usados)} itens no artigo, {len(fora_do_artigo)} versionados e não incluídos")
