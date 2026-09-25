// Mapeia os metadados Quarto do report.qmd na função relatorio() do perfil
// LabDados Working Paper Series (typst/perfis/labdados-wp.typ).
// A data da capa vem de `date` (o Quarto a formata com `date-format` e o
// idioma do documento), e `report-date` segue aceito como string fixa.
// Com o metadado `neutro` (tools/render-pdf.sh passa `-M neutro=true`), o
// mesmo documento sai fora da série: sem capa e sem nome e número da série no
// cabeçalho.
// "NatJus" não hifeniza. A proteção de palavras capitalizadas da base só
// reconhece maiúscula seguida de minúsculas, e o nome, com a maiúscula no
// meio, escapa dela: o título em inglês da folha de rosto saía "Nat-Jus".
// Fica aqui, e não na base, porque mudar a regra da base repagina as
// outras publicações que a usam.
#show "NatJus": set text(hyphenate: false)
#show: doc => relatorio(
$if(title)$
  titulo: [$title$],
$endif$
$if(subtitle)$
  subtitulo: [$subtitle$],
$endif$
$if(neutro)$
  neutro: true,
$else$
$if(series)$
  serie: [$series$],
$endif$
$if(series-number)$
  numero: [$series-number$],
$endif$
$endif$
$if(short-title)$
  titulo-curto: [$short-title$],
$endif$
$if(by-author)$
  autores: (
$for(by-author)$
$if(it.name.literal)$
    [$it.name.literal$],
$endif$
$endfor$
  ),
$endif$
$if(institution)$
  instituicao: [$institution$],
$endif$
$if(report-date)$
  data: [$report-date$],
$else$
$if(date)$
  data: [$date$],
$endif$
$endif$
$if(lang)$
  lang: "$lang$",
$endif$
$if(region)$
  region: "$region$",
$endif$
$if(section-numbering)$
  numeracao-secoes: "$section-numbering$",
$endif$
$if(secoes-continuas)$
  secoes-em-pagina-nova: false,
  // Com as seções no fluxo, abrem página só estas. As declarações entram na
  // lista porque o SciELO Preprints as lê como bloco próprio, separado do
  // corpo do artigo.
  secoes-que-abrem-pagina: ("Introdução", "Declarações", "Referências", "Apêndices"),
$endif$
  doc,
)
