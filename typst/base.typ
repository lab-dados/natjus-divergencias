// Copied verbatim from lab-dados/revista-automatizada, poc-typst/base.typ,
// at commit 71d6ac5. The source of truth is that repository: change it there
// first, then refresh this copy (tools/render-pdf.sh reads it from here).
// ===========================================================================
// Base geral de composição — o que vale para qualquer publicação
// ===========================================================================
// Regras que não dependem do projeto gráfico: grade de baseline emulada,
// justificação com micro-tracking, custo de hifenização, proteção de
// palavras capitalizadas, título/legenda/célula sem hífen, nota de rodapé
// com número deslocado, snapping de blocos de altura arbitrária à pauta e
// float por medida. Nada aqui carrega valor da Revista Direito GV: corpo,
// pauta, cap-height, fonte e recuo entram como parâmetros, e cada perfil
// fixa os seus (template.typ para a RDGV; perfis/labdados-wp.typ para a
// LabDados Working Paper Series). Os números de calibração e as medições
// que justificam cada regra ficam no perfil que as mediu e no README.
//
// Uso: `#import "base.typ"` e chamar `base.compor`, `base.nota-deslocada`,
// `base.snap-pauta`, `base.flutuar` (com `base.local-do-float`, para as
// listas de figuras e tabelas); os perfis costumam embrulhar as
// funções de grade com os próprios valores (`leading-de`, `espaco-ate`).
// ===========================================================================

// ---- Grade de baseline emulada ----
// Com top-edge "cap-height" e bottom-edge "baseline", o passo entre baselines
// é leading + cap-height, e o vão entre blocos vai da baseline anterior ao
// topo das versais do bloco seguinte. Logo:
// - o leading que crava um passo `alvo` num corpo é alvo − cap-em × corpo;
// - o espaço que põe a 1ª baseline de um bloco n pautas abaixo da baseline
//   anterior é n × pauta − cap-em × corpo.
// cap-em é a cap-height da fonte em em (sCapHeight / unitsPerEm, lida do
// arquivo da fonte): EB Garamond 0,65; IBM Plex Sans 0,698.
#let leading-de(alvo, corpo, cap-em) = alvo - cap-em * corpo
#let espaco-ate(pautas, corpo, pauta, cap-em) = pautas * pauta - cap-em * corpo

// ---- Proteções de hifenização ----
// Palavras capitalizadas não hifenizam (regra de justificação herdada do
// fluxo InDesign), até um teto de letras: acima dele a palavra pode não
// caber na medida e precisa poder hifenizar. Uma show-set definida antes
// vence as posteriores, então o teto fica na própria regra.
#let capitalizadas-sem-hifen(letras: 15, body) = {
  show regex("\b\p{Lu}\p{Ll}{1," + str(letras - 1) + "}\b"): set text(hyphenate: false)
  body
}
// Título, legenda e célula de tabela compõem em medida curta e ragged: um
// hífen ali é sempre pior que a linha desigual, e numa célula dimensionada
// pelo conteúdo o Typst só quebra a palavra num hífen que ela já tenha.
#let rotulos-sem-hifen(body) = {
  show heading: set text(hyphenate: false)
  show figure.caption: set text(hyphenate: false)
  show table: set text(hyphenate: false)
  body
}

// ---- Texto corrido ----
// Aplicar com `show: base.compor.with(...)` no ponto em que o perfil
// define a mancha. `justification-limits` fixa a banda de espaço entre
// palavras e o micro-tracking, o substituto do glyph scaling do InDesign
// que o Typst não tem; `costs.hyphenation` abaixo de 100% deixa o
// compositor usar o hífen em vez de abrir rios. `texto` recebe opções de
// `text` que só o perfil conhece (number-type, fill).
#let compor(
  fonte: "EB Garamond",
  corpo: 14pt,
  pauta: 19.125pt,
  cap-em: 0.65,
  lang: "pt",
  region: "BR",
  custo-hifen: 60%,
  recuo: 0pt,
  espacamento: (min: 95%, max: 110%),
  tracking: (min: -0.02em, max: 0.02em),
  letras-capitalizada: 15,
  texto: (:),
  body,
) = {
  set text(font: fonte, size: corpo, lang: lang, region: region,
           hyphenate: true, costs: (hyphenation: custo-hifen),
           top-edge: "cap-height", bottom-edge: "baseline", ..texto)
  set par(justify: true, linebreaks: "optimized",
          justification-limits: (spacing: espacamento, tracking: tracking),
          leading: leading-de(pauta, corpo, cap-em),
          spacing: leading-de(pauta, corpo, cap-em),
          first-line-indent: (amount: recuo, all: true))
  set smartquote(enabled: true)
  show: capitalizadas-sem-hifen.with(letras: letras-capitalizada)
  show: rotulos-sem-hifen
  body
}

// ---- Nota de rodapé com número deslocado ----
// Devolve a show rule de `footnote.entry`: número numa caixa de largura
// `recuo` na margem, 1ª linha e continuações alinhadas ao recuo. O
// hanging-indent de `par` não atravessa o corpo da nota, daí o pad com o
// deslocamento negativo. `numero` desenha o marcador a partir do inteiro.
#let nota-deslocada(
  recuo: 19pt,
  tamanho: 10pt,
  entrelinha: 12pt - 0.65 * 10pt,
  numero: n => [#str(n).],
) = it => {
  let n = counter(footnote).at(it.note.location()).first()
  set text(size: tamanho)
  set par(justify: true, first-line-indent: 0pt,
          leading: entrelinha, spacing: entrelinha)
  pad(left: recuo)[#h(-recuo)#box(width: recuo, numero(n))#it.note.body]
}

// ---- Snapping de bloco de altura arbitrária à pauta ----
// Mede o conteúdo em contexto e completa a caixa até imitar a geometria de
// um parágrafo do corpo: topo na fase cap-top, base na fase de baseline
// (altura = m × pauta + cap(corpo)). Assim os above/below calibrados do
// perfil compõem sem caso especial, inclusive quando o bloco abre página.
// Completar até múltiplo seco da pauta deixaria a base na fase errada.
// Limites: breakable: false (conteúdo mais alto que a mancha transborda) e
// o conteúdo é instanciado duas vezes (medida e caixa final), então
// conteúdo cuja altura depende da posição na página pode medir diferente.
#let snap-pauta(pauta: 19.125pt, cap-em: 0.65, corpo: 14pt,
                antes: 2, depois: 2, conteudo) = block(
  width: 100%,
  breakable: false,
  above: espaco-ate(antes, corpo, pauta, cap-em),
  below: espaco-ate(depois, corpo, pauta, cap-em),
  layout(tam => {
    let h = measure(block(width: tam.width, conteudo)).height
    // m·pauta + cap ≥ h; o ε protege alturas quase-múltiplas (float)
    let m = calc.max(0, calc.ceil((h - cap-em * corpo) / pauta - 0.0001))
    block(width: 100%, height: m * pauta + cap-em * corpo,
          above: 0pt, below: 0pt, align(top + left, conteudo))
  }),
)

// ---- Float por medida ----
// Devolve a show rule de `figure`. Tabela ou figura que cabe numa página
// vira placement flutuante: o Typst a põe no topo ou no pé da página atual
// ou da seguinte e o texto corre em volta, sem deixar página pela metade
// antes de um bloco que não caberia. A que não cabe (tabela longa) fica em
// fluxo como bloco quebrável, com a fila de floats esvaziada antes, para
// que nenhuma figura pendente caia no meio dela. `place(float: true)` é o
// que `figure(placement:)` faz por dentro, então numeração, sumário e
// referências cruzadas seguem funcionando. `largura` e `altura` são as da
// mancha; `folga` é o clearance entre o float e o texto; `posicao` é a do
// `place` (`auto` escolhe topo ou pé; `top` mantém a ordem de leitura
// quando uma tabela longa em fluxo vem logo depois, porque um float posto
// no pé da página cairia entre as linhas dela).
// O `figure` continua localizado onde a show rule o recebe, no fluxo, e não
// onde o float é posto: quando o float não cabe na página da citação e vai
// para a seguinte, a localização do elemento (a que o `outline` lê) fica
// na página anterior. Por isso o float leva dentro dele uma âncora
// `<ancora-float>` com a localização da figura, e `local-do-float` devolve
// a localização da âncora, que fica na página e no alto do float.
#let flutuar(largura, altura, folga: 0pt, posicao: auto) = it => context {
  let h = measure(block(width: largura, it)).height
  if h + folga <= altura {
    let figura = it.location()
    place(posicao, float: true, clearance: folga, {
      [#metadata(figura) <ancora-float>]
      it
    })
  } else {
    place.flush()
    it
  }
}

// Onde a figura aparece de fato: a âncora que `flutuar` pôs no float dela
// ou, se ela não flutuou, a própria figura. Chamar em contexto; serve às
// entradas de lista de figuras e de tabelas, que precisam da página e do
// destino de link do float.
#let local-do-float(figura) = {
  let ancora = query(<ancora-float>).find(a => a.value == figura.location())
  if ancora == none { figura.location() } else { ancora.location() }
}
