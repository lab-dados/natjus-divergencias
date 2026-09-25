# /// script
# requires-python = ">=3.10"
# dependencies = ["python-docx==1.2.0"]
# ///

from io import BytesIO
from pathlib import Path
import subprocess

from docx import Document
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt
from docx.enum.text import WD_ALIGN_PARAGRAPH


raiz = Path(__file__).resolve().parent.parent
referencia = subprocess.run(
    ["quarto", "pandoc", "--print-default-data-file", "reference.docx"],
    check=True,
    capture_output=True,
).stdout
documento = Document(BytesIO(referencia))


TIMES = "Times New Roman"


def definir_fontes(estilo, nome=TIMES):
    estilo.font.name = nome
    fontes = estilo.element.get_or_add_rPr().get_or_add_rFonts()
    for atributo in ("ascii", "hAnsi", "eastAsia", "cs"):
        fontes.set(qn(f"w:{atributo}"), nome)
        # Uma referência de fonte de tema vence a fonte nomeada; os títulos do
        # Pandoc carregam uma, e é por isso que eles saíam sempre em sem-serifa.
        atributo_tema = qn(f"w:{atributo}Theme")
        if atributo_tema in fontes.attrib:
            del fontes.attrib[atributo_tema]


for nome in ("Normal", "Body Text", "First Paragraph"):
    definir_fontes(documento.styles[nome])

for nome in ("Normal", "Body Text", "First Paragraph"):
    estilo = documento.styles[nome]
    estilo.font.size = Pt(12)
    estilo.paragraph_format.line_spacing = 1.5
    estilo.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
    # ABNT NBR 14724: o parágrafo é marcado por recuo de primeira linha, não
    # por espaço extra entre parágrafos; o Body Text do Pandoc traz 9 pt acima
    # e abaixo por padrão, que o espaçamento entre linhas já cobre.
    estilo.paragraph_format.space_before = Pt(0)
    estilo.paragraph_format.space_after = Pt(0)
    if nome != "Normal":
        estilo.paragraph_format.first_line_indent = Cm(1.25)

# ABNT NBR 6023: a lista de referências é alinhada à esquerda, com espaçamento
# simples, sem recuo, com uma linha em branco entre as entradas.
bibliografia = documento.styles["Bibliography"]
bibliografia.font.size = Pt(12)
bibliografia.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
bibliografia.paragraph_format.line_spacing = 1.0
bibliografia.paragraph_format.first_line_indent = Cm(0)
bibliografia.paragraph_format.left_indent = Cm(0)
bibliografia.paragraph_format.space_before = Pt(0)
bibliografia.paragraph_format.space_after = Pt(12)



# Títulos: o padrão do Pandoc é sem-serifa azul. O manuscrito segue o visual
# serifado, preto e em negrito de revista de direito; os tamanhos descem a
# partir do título.
from docx.shared import RGBColor

PRETO = RGBColor(0, 0, 0)
for nome, tamanho, antes, depois in (
    ("Title", 16, 0, 6),
    ("Subtitle", 12, 0, 12),
    ("Heading 1", 14, 24, 8),
    ("Heading 2", 12, 18, 6),
    ("Heading 3", 12, 12, 6),
):
    estilo = documento.styles[nome]
    definir_fontes(estilo)
    estilo.font.size = Pt(tamanho)
    estilo.font.bold = True
    estilo.font.italic = nome == "Heading 3"
    estilo.font.color.rgb = PRETO
    estilo.paragraph_format.space_before = Pt(antes)
    estilo.paragraph_format.space_after = Pt(depois)
    estilo.paragraph_format.line_spacing = 1.0
    estilo.paragraph_format.keep_with_next = True
documento.styles["Title"].paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
documento.styles["Subtitle"].paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
documento.styles["Subtitle"].font.bold = False
documento.styles["Subtitle"].font.italic = True
for nome in ("Author", "Date"):
    estilo = documento.styles[nome]
    definir_fontes(estilo)
    estilo.font.size = Pt(12)
    estilo.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    estilo.paragraph_format.line_spacing = 1.0

# Tabelas: os 12 pt do corpo com espaçamento 1,5 transformam cada célula em
# três linhas e espremem as colunas. "Compact" é o estilo de parágrafo que o
# Pandoc dá às células de tabela; "Table Caption" e "Image Caption" guardam as
# legendas.
compacto = documento.styles["Compact"]
definir_fontes(compacto)
compacto.font.size = Pt(9.5)
compacto.paragraph_format.line_spacing = 1.0
compacto.paragraph_format.space_before = Pt(1)
compacto.paragraph_format.space_after = Pt(1)
for nome in ("Table Caption", "Image Caption"):
    estilo = documento.styles[nome]
    definir_fontes(estilo)
    estilo.font.size = Pt(10)
    estilo.font.italic = False
    estilo.paragraph_format.line_spacing = 1.0
    estilo.paragraph_format.space_before = Pt(6)
    estilo.paragraph_format.space_after = Pt(6)
    estilo.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    # Uma legenda nunca fecha a página com a tabela dela na página seguinte.
    estilo.paragraph_format.keep_with_next = True
# Código embutido nas células e na prosa (nomes de campo) no tamanho do texto
# da célula.
verbatim = documento.styles["Verbatim Char"]
verbatim.font.size = Pt(9)
estilo_tabela = documento.styles["Table"]
estilo_tabela.font.size = Pt(9.5)


def definir_bordas_tabela(estilo):
    """Fios abertos nas laterais: uma linha acima da tabela, uma sob a linha de
    cabeçalho e uma embaixo, sem linhas verticais, como são as tabelas das
    revistas de direito brasileiras."""
    tbl_pr = estilo.element.find(qn("w:tblPr"))
    bordas = OxmlElement("w:tblBorders")
    for lado, valor, tamanho in (("top", "single", "8"), ("bottom", "single", "8"),
                                 ("left", "nil", "0"), ("right", "nil", "0"),
                                 ("insideH", "nil", "0"), ("insideV", "nil", "0")):
        elemento = OxmlElement(f"w:{lado}")
        elemento.set(qn("w:val"), valor)
        elemento.set(qn("w:sz"), tamanho)
        elemento.set(qn("w:space"), "0")
        elemento.set(qn("w:color"), "000000")
        bordas.append(elemento)
    tbl_pr.append(bordas)
    margens = tbl_pr.find(qn("w:tblCellMar"))
    for lado, largura in (("top", "30"), ("bottom", "30"), ("left", "80"), ("right", "80")):
        elemento = margens.find(qn(f"w:{lado}"))
        elemento.set(qn("w:w"), largura)
    # A linha de cabeçalho: em negrito, com o fio embaixo dela um pouco mais grosso.
    primeira_linha = estilo.element.find(qn("w:tblStylePr"))
    tc_pr = primeira_linha.find(qn("w:tcPr"))
    tc_pr.find(qn("w:tcBorders")).find(qn("w:bottom")).set(qn("w:sz"), "8")
    r_pr = OxmlElement("w:rPr")
    r_pr.append(OxmlElement("w:b"))
    primeira_linha.insert(0, r_pr)


definir_bordas_tabela(estilo_tabela)

# As citações longas (quatro linhas ou mais) seguem a revista: recuo de 2 cm,
# 10 pt, espaçamento simples. O Pandoc dá às citações em bloco o estilo
# "Block Text".
bloco = documento.styles["Block Text"]
definir_fontes(bloco)
bloco.font.size = Pt(10)
bloco.paragraph_format.left_indent = Cm(2)
bloco.paragraph_format.right_indent = Cm(0)
bloco.paragraph_format.line_spacing = 1.0
bloco.paragraph_format.space_before = Pt(6)
bloco.paragraph_format.space_after = Pt(12)
bloco.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
definir_fontes(documento.styles["Body Text"])

# "Fonte" é o estilo de parágrafo da linha de fonte sob cada tabela e figura; o
# manuscrito o seleciona com uma div de estilo customizado do Pandoc.
from docx.enum.style import WD_STYLE_TYPE

estilo_fonte = documento.styles.add_style("Fonte", WD_STYLE_TYPE.PARAGRAPH)
estilo_fonte.base_style = documento.styles["Normal"]
definir_fontes(estilo_fonte)
estilo_fonte.font.size = Pt(10)
estilo_fonte.paragraph_format.line_spacing = 1.0
estilo_fonte.paragraph_format.space_before = Pt(6)
estilo_fonte.paragraph_format.space_after = Pt(2)
estilo_fonte.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
estilo_fonte.paragraph_format.keep_with_next = True

estilo_notas = documento.styles.add_style("Notas", WD_STYLE_TYPE.PARAGRAPH)
estilo_notas.base_style = estilo_fonte
estilo_notas.paragraph_format.space_before = Pt(0)
estilo_notas.paragraph_format.space_after = Pt(12)
estilo_notas.paragraph_format.keep_with_next = False

# "Autor" é o parágrafo de cada autor na primeira página (nome em negrito,
# afiliação e ORCID): corpo do texto, sem recuo, com vão depois.
estilo_autor = documento.styles.add_style("Autor", WD_STYLE_TYPE.PARAGRAPH)
estilo_autor.base_style = documento.styles["Normal"]
definir_fontes(estilo_autor)
estilo_autor.paragraph_format.first_line_indent = Pt(0)
estilo_autor.paragraph_format.space_after = Pt(6)

nota_rodape = documento.styles["Footnote Text"]
nota_rodape.font.name = "Times New Roman"
nota_rodape.font.size = Pt(10)
nota_rodape.paragraph_format.line_spacing = 1.0
# ABNT NBR 10520: as notas são justificadas, com espaçamento simples, sem
# espaço entre elas, e a segunda linha em diante alinha sob a primeira palavra,
# o que o recuo deslocado aproxima para números de nota de um e dois dígitos.
nota_rodape.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
nota_rodape.paragraph_format.first_line_indent = Cm(-0.5)
nota_rodape.paragraph_format.left_indent = Cm(0.5)
nota_rodape.paragraph_format.space_before = Pt(0)
nota_rodape.paragraph_format.space_after = Pt(0)

for secao in documento.sections:
    secao.page_width = Cm(21)
    secao.page_height = Cm(29.7)
    secao.top_margin = Cm(2.5)
    secao.bottom_margin = Cm(2.5)
    secao.left_margin = Cm(2.5)
    secao.right_margin = Cm(2.5)
    rodape = secao.footer.paragraphs[0]
    rodape.clear()
    rodape.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    campo = OxmlElement("w:fldSimple")
    campo.set(qn("w:instr"), "PAGE")
    rodape._p.append(campo)

documento.core_properties.author = ""
documento.core_properties.last_modified_by = ""
documento.save(raiz / "_reference.docx")
