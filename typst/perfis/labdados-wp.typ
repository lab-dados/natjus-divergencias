// Perfil da LabDados Working Paper Series. Nasceu como cópia do perfil de
// mesmo nome do repositório lab-dados/revista-automatizada e divergiu dele:
// aqui a identidade é neutra (texto e fios em preto e cinza, capa só
// tipográfica, número de página simples) e a fonte é a IBM Plex Sans.
// Mudança na composição geral vai primeiro a base.typ.
//
// Relatório de pesquisa sobre a base geral de composição (base.typ): capa
// com o nome da série e o número, folha de rosto, sumário, listas de
// figuras e tabelas, seções de nível 1 abrindo página própria ou correndo
// no fluxo, título corrente, tabelas com fios, floats por medida.
//
// O perfil é consumido pela saída Typst do Quarto: os tipos de float
// `quarto-float-tbl` e `quarto-float-fig` e o rótulo `<refs>` da lista de
// referências são os que o Quarto emite, e ficam parametrizados em
// `relatorio()` para um emissor próprio poder usar outros. As notas de fonte
// e de tabela entram como `#fonte-legenda[...]` e `#notas-legenda[...]`, de
// preferência dentro do float (o filtro Lua do Quarto faz isso), para
// viajarem com ele.
// ===========================================================================

#import "../base.typ"

// ---- paleta (razões de contraste sobre branco, calculadas) ----
// Só preto e cinzas: o documento não carrega cor de identidade. Os papéis
// são o que cada cor faz; `enfase` marca links, marcadores de lista,
// números de nota e fios de tabela.
#let tinta = rgb("#1a1a1a")         // 17,4:1
#let tinta-suave = rgb("#4a4a4a")   // 8,9:1
#let paleta = (titulo: tinta, enfase: rgb("#333333"),  // 12,6:1
  fio-suave: rgb("#cccccc"), fundo-codigo: rgb("#eeeeee"))

// `relatorio(neutro: true)` compõe o mesmo documento fora da série, para
// quem publica o texto em outro lugar: sem capa e sem o nome da série no
// título corrente. O resto não muda.
// `relatorio` grava aqui o título e os tipos de float, que as peças chamadas pelo
// corpo, fora dele (folha de rosto, pré-textuais), leem em contexto
#let identidade-wp = state("identidade-wp", (titulo: none,
  tipo-figura: "quarto-float-fig", tipo-tabela: "quarto-float-tbl"))

// ---- fonte ----
// IBM Plex Sans (OFL), em typst/fonts/. A grade depende da cap-height da
// fonte (sCapHeight / unitsPerEm, lida do arquivo), por isso ela vem junto
// do nome: trocar a fonte sem remedir desalinha a pauta.
#let fonte = (nome: "IBM Plex Sans", cap-em: 0.698)

// ---- grade ----
#let corpo = 10pt
#let pauta = 16pt
#let cap-em = fonte.cap-em
#let leading-de(alvo, tam) = base.leading-de(alvo, tam, cap-em)
#let espaco-ate(pautas, tam) = base.espaco-ate(pautas, tam, pauta, cap-em)
#let entrelinha = leading-de(pauta, corpo)

// ---- página ----
#let margem-x = 2.3cm
#let margem-topo = 2.5cm
#let margem-base = 2.8cm
// ---- blocos de fonte e notas sob tabelas e figuras ----
// Fonte e notas alinhadas à esquerda, justificadas como o corpo, com a
// mesma entrelinha e o rótulo em negrito: centrar a fonte e justificar só
// as notas, cada uma com sua entrelinha, dava dois blocos que não pareciam
// da mesma tabela, e a bandeira destoava do corpo justificado ao lado.
// A entrelinha de 5pt dá passo de ~11pt no corpo 8.5pt, legível em nota
// longa; 3.5pt ficava apertado.
#let bloco-de-legenda(above, below, body) = block(width: 100%,
  above: above, below: below, {
  set par(justify: true, first-line-indent: 0pt, leading: 5pt,
          spacing: 5pt)
  set text(size: 8.5pt, fill: tinta-suave)
  show regex("^(Fonte|Fontes|Nota|Notas):"): set text(weight: 700)
  align(left, body)
})
#let fonte-legenda(body) = bloco-de-legenda(7pt, 3pt, body)
#let notas-legenda(body) = bloco-de-legenda(3pt, espaco-ate(2, corpo), body)

// ---- folha de rosto ----
// A primeira página de conteúdo, logo depois da capa, no formato que o SciELO
// Preprints pede: o título do documento no topo e, abaixo, os autores. Sem
// recuo de primeira linha, porque aqui nenhum parágrafo continua o anterior,
// e em página própria para o sumário começar limpo. O título vem de
// `relatorio`, e não do corpo, para não ser escrito duas vezes; na versão
// neutra, sem capa, esta é a única página que o traz.
#let folha-de-rosto(body) = page(header: none, {
  set par(first-line-indent: 0pt)
  context {
    let titulo = identidade-wp.get().titulo
    if titulo != none {
      block(width: 100%, below: espaco-ate(2.5, corpo),
        par(justify: false, leading: 9pt, text(size: 20pt, weight: 700,
          fill: paleta.enfase, hyphenate: false, titulo)))
    }
  }
  body
})

// ---- bloco de autoria ----
// O SciELO Preprints pede, na primeira página, o nome de cada autor com a
// afiliação completa logo abaixo e o ORCID com link ativado; não pede
// biografia. Um `autor` por pessoa, sem página própria: o bloco abre a
// primeira página de conteúdo, antes do resumo. Dentro de `autor`, o primeiro
// trecho em negrito é o nome (o Word o mostra em negrito no mesmo parágrafo, e
// aqui ele ganha linha própria), e o ponto que o fecha no parágrafo sai junto,
// porque a quebra já separa o nome da afiliação.
#let autor(body) = block(width: 100%, below: espaco-ate(1.8, corpo), {
  set par(first-line-indent: 0pt, justify: false)
  // O link do ORCID não se parte entre linhas: a triagem do SciELO lê o
  // texto extraído do PDF, e um iD quebrado no hífen não é reconhecido.
  show link: box
  // O ícone iD do ORCID vem com altura fixa; sem `width: auto`, a largura
  // de 100% que o perfil dá às imagens o esticaria pela linha inteira. O box
  // desce o ícone 0,18em abaixo da linha de base, para centrá-lo na altura
  // das letras.
  set image(width: auto)
  show image: box.with(baseline: 0.18em)
  show strong: s => {
    text(weight: 700, if s.body.has("text") { s.body.text.trim(".", at: end) } else { s.body })
    linebreak()
  }
  body
})

// ---- capa ----
// Só tipografia, em preto sobre branco: uma faixa entre dois fios com o
// nome da série à esquerda e o número à direita, o título no terço
// superior, autoria abaixo dele e a data no pé, sobre um fio.
#let capa(titulo: [], subtitulo: none, serie: none, numero: none,
          autores: (), instituicao: none, data: none) = page(
  header: none, footer: none,
  margin: (x: margem-x, top: margem-topo, bottom: margem-base),
  {
    set par(justify: false, first-line-indent: 0pt)
    let rotulo(c) = text(size: 9pt, weight: 700, fill: tinta,
      tracking: 1.5pt, spacing: 200%, upper(c))
    line(length: 100%, stroke: 1.2pt + tinta)
    v(7pt)
    grid(columns: (1fr, auto), align: (left, right),
      rotulo(if serie != none { serie } else { [Working paper] }),
      if numero != none { rotulo[n.#h(0.35em)#numero] })
    v(5pt)
    line(length: 100%, stroke: 0.4pt + tinta)
    v(62mm)
    block(par(leading: 9pt, text(size: 25pt, weight: 700, fill: tinta,
                                 hyphenate: false, titulo)))
    if subtitulo != none {
      v(10pt)
      block(text(size: 14pt, fill: tinta-suave, subtitulo))
    }
    if autores != () {
      v(26pt)
      for a in autores { block(below: 7pt, text(size: 12.5pt, fill: tinta, a)) }
    }
    if instituicao != none {
      v(4pt)
      block(text(size: 10pt, fill: tinta-suave, instituicao))
    }
    v(1fr)
    line(length: 100%, stroke: 0.4pt + tinta)
    v(6pt)
    if data != none { block(text(size: 9.5pt, fill: tinta-suave, data)) }
  })

// ---- pré-textuais: sumário e listas ----
#let titulo-pretextual(t) = block(width: 100%, below: 14pt, context {
  set par(first-line-indent: 0pt)
  text(size: 20pt, weight: 700, fill: paleta.titulo, upper(t))
  v(6pt)
  line(length: 100%, stroke: 1.5pt + paleta.enfase)
})

// Chamado pelo corpo, no fim da folha de rosto. Os tipos das listas vêm de
// `relatorio`, que é quem os define para o resto do documento, pelo estado
// `identidade-wp`; passá-los aqui só serve para uma lista fora do padrão.
#let pre-textuais(tipo-figura: auto, tipo-tabela: auto) = {
  // Nota de rodapé ligada a um título (a de origem do artigo, na
  // Introdução) não se repete na entrada do sumário: `show footnote: none`
  // esconde a chamada, mas a nota ainda sai no pé da página do sumário,
  // então a entrada é remontada sem ela.
  // A entrada de seção de nível 1 tira página e destino da âncora que a
  // regra do título emite (ver a regra do título em relatorio), não da
  // etiqueta do título. Na seção que abre página a âncora fica dentro do
  // float do título, e a etiqueta fica abaixo dele e, quando nenhuma linha
  // de fluxo fica na página de abertura, vai para a página seguinte. Na
  // seção que corre no fluxo as duas ficam juntas. A página sai no formato
  // de numeração que vale na página da âncora. Título e âncora andam na
  // mesma ordem; se as contagens divergirem (um título de nível 1 composto
  // por outra regra), a entrada aponta a etiqueta do título.
  // A entrada de figura e de tabela também é remontada: página e destino
  // vêm de `base.local-do-float`, porque o float pode sair na página
  // seguinte à da citação, e a figura fica localizada na citação.
  let sem-nota(c) = if c.has("children") {
    c.children.filter(x => x.func() != footnote).join()
  } else { c }
  // uma entrada apontando `alvo`, com a página no formato de numeração que
  // vale lá
  let entrada(it, alvo, corpo) = {
    let formato = alvo.page-numbering()
    let pagina = numbering(if formato == none { "1" } else { formato },
      ..counter(page).at(alvo))
    link(alvo, it.indented(it.prefix(),
      [#corpo #box(width: 1fr, it.fill) #sym.wj#pagina]))
  }
  show outline.entry: it => {
    if it.element.func() == figure {
      return context entrada(it, base.local-do-float(it.element), it.body())
    }
    if it.element.func() != heading { return it }
    let corpo = it.element.body
    let tem-nota = corpo.has("children") and corpo.children.any(c => c.func() == footnote)
    if it.level != 1 and not tem-nota { return it }
    context {
      let alvo = it.element.location()
      if it.level == 1 {
        let titulos = query(heading.where(level: 1))
        let ancoras = query(<ancora-titulo>)
        let i = titulos.position(h => h.location() == alvo)
        if i != none and ancoras.len() == titulos.len() {
          alvo = ancoras.at(i).location()
        }
      }
      entrada(it, alvo, sem-nota(corpo))
    }
  }
  show outline.entry.where(level: 1): set text(weight: 700)
  show outline.entry.where(level: 1): set block(above: 12pt)
  show outline.entry.where(level: 2): set block(above: 5pt)
  page(header: none, {
    set par(first-line-indent: 0pt, justify: false)
    titulo-pretextual[Sumário]
    outline(title: none, depth: 2, indent: 1.2em)
  })
  page(header: none, {
    set par(first-line-indent: 0pt, justify: false)
    context {
      let id = identidade-wp.get()
      let fig = if tipo-figura == auto { id.tipo-figura } else { tipo-figura }
      let tab = if tipo-tabela == auto { id.tipo-tabela } else { tipo-tabela }
      titulo-pretextual[Lista de figuras]
      outline(title: none, target: figure.where(kind: fig))
      v(espaco-ate(3, corpo))
      titulo-pretextual[Lista de tabelas]
      outline(title: none, target: figure.where(kind: tab))
    }
  })
}

// ---- o documento ----
#let relatorio(
  titulo: [],
  subtitulo: none,
  serie: none,
  numero: none,
  titulo-curto: none,
  autores: (),
  instituicao: none,
  data: none,
  lang: "pt",
  region: "BR",
  numeracao-secoes: none,
  tipo-figura: "quarto-float-fig",
  tipo-tabela: "quarto-float-tbl",
  rotulo-referencias: <refs>,
  // true: toda seção de nível 1 abre página própria, como
  // num relatório. false: as seções correm no fluxo, como num artigo, e só
  // as de `secoes-que-abrem-pagina` (pelo texto do título) abrem página.
  secoes-em-pagina-nova: true,
  // true: o documento fora da série, sem capa e sem o nome da série no
  // título corrente
  neutro: false,
  secoes-que-abrem-pagina: ("Introdução", "Referências", "Apêndices"),
  // figura ou tabela com altura até esta fração da mancha fica no fluxo, no
  // lugar em que é citada; acima disso flutua (base.flutuar)
  fracao-minima-para-flutuar: 1 / 3,
  // imagem mais alta que esta fração da mancha é reduzida até ela, com a
  // largura proporcional, para que sobre espaço de texto na página
  fracao-maxima-de-imagem: 0.7,
  corpo-doc,
) = {
  set document(title: if titulo-curto != none { titulo-curto } else { titulo })
  let p = paleta
  // recuo de 1ª linha de 1 cm em todos os parágrafos, sem espaço entre eles
  // banda de espaçamento, micro-tracking e custo de hifenização são os
  // calibrados na revista para a EB Garamond a 14 pt; passam explícitos
  // porque as fontes deste perfil a 10 pt ainda não foram medidas nas réguas
  show: base.compor.with(
    fonte: fonte.nome, corpo: corpo, pauta: pauta, cap-em: cap-em,
    lang: lang, region: region, custo-hifen: 60%, recuo: 1cm,
    espacamento: (min: 95%, max: 110%),
    tracking: (min: -0.02em, max: 0.02em),
    letras-capitalizada: 15,
    texto: (fill: tinta),
  )
  // links externos sublinhados, porque a cor sozinha não os separaria do
  // texto em preto; os internos (citações, referências cruzadas) ficam como
  // o texto
  show link: it => if type(it.dest) == str {
    underline(stroke: 0.4pt + p.enfase, offset: 1.5pt, it)
  } else { it }
  set image(width: 100%)
  // o hífen U+2010 que o texto usa em alguns compostos não existe na IBM
  // Plex Sans e caía na fonte de reserva; o hífen ASCII tem o mesmo desenho
  show "\u{2010}": "-"
  // código inline (nomes de campo de formulário) na fonte do texto, em caixa
  show raw.where(block: false): it => box(fill: p.fundo-codigo,
    inset: (x: 2.5pt, y: 0pt), outset: (y: 2.5pt), radius: 1.5pt,
    text(font: fonte.nome, size: 1em, it.text))

  // ---- títulos ----
  set heading(numbering: numeracao-secoes)
  // nota de rodapé dentro do título fica de fora, senão o texto do título
  // deixa de casar com `secoes-que-abrem-pagina`
  let texto-de(c) = if type(c) == str { c }
    else if c.func() == footnote { "" }
    else if c.has("text") { c.text }
    else if c.has("children") { c.children.map(texto-de).join() }
    else if c.has("body") { texto-de(c.body) }
    else { "" }
  let abre-pagina(it) = secoes-em-pagina-nova or texto-de(it.body) in secoes-que-abrem-pagina
  // Toda seção de nível 1 emite uma âncora `<ancora-titulo>`, com `abre`
  // dizendo se ela abre página: o sumário casa títulos e âncoras pela
  // ordem, e o cabeçalho corrente só some nas páginas de abertura.
  let numero-e-titulo(it) = {
    if it.numbering != none {
      counter(heading).display(it.numbering)
      h(0.5em)
    }
    upper(it.body)
  }
  show heading.where(level: 1): it => if abre-pagina(it) {
    // Nada antes da quebra. A etiqueta que o sumário e o cabeçalho
    // corrente consultam fica no começo do que esta regra emite, e o Typst
    // só a leva para a página nova quando nada de tamanho real a precede:
    // com um place.flush() antes da quebra, o sumário apontava a página
    // anterior em todas as seções, e a primeira seção depois das listas
    // ganhava uma página em branco. Float que ainda espera página quando a
    // seção acaba sai mesmo assim antes da abertura, numa página só dele
    // (medido em varreduras de texto e altura de figura, sem o flush).
    pagebreak(weak: true)
    // O título é ele mesmo um float de topo, emitido antes de qualquer
    // float da seção: floats de topo empilham na ordem em que chegam, e
    // uma tabela citada logo depois do título fica abaixo dele. Com o
    // título em fluxo, ela ia para o topo da página, acima do título. O
    // texto que segue começa na mesma linha da grade que começava com o
    // título em fluxo (v(28mm) + bloco + espaco-ate(3)).
    // Custo: a etiqueta do título continua no fluxo, e o fluxo começa
    // abaixo dos floats de topo. O marcador do PDF e a referência cruzada
    // abrem a página certa, mas rolada para baixo do título (e da tabela,
    // quando há uma). Se nenhuma linha de fluxo fica na página de abertura,
    // a etiqueta vai para a página seguinte, que é a que o marcador e a
    // referência abrem. Isso acontece quando o título e um float logo
    // depois dele enchem a página, e quando a seção é só um float que cabe
    // numa página mas não abaixo do título: o float vai para a página
    // seguinte e a abertura fica só com o título. Sumário e cabeçalho
    // corrente escapam dos dois casos lendo a âncora abaixo, que fica
    // sempre na página e no alto do título.
    place(top, float: true, clearance: espaco-ate(3, corpo),
      block(width: 100%, above: 0pt, below: 0pt, inset: (top: 28mm), {
      [#metadata((abre: true)) <ancora-titulo>]
      // entrelinha de 10pt: com 7pt as duas linhas de um título longo
      // ("Apêndice B. Especificações e estimativas dos modelos") colavam
      set par(justify: false, leading: 10pt, first-line-indent: 0pt)
      set text(size: 24pt, weight: 700, fill: p.titulo, hyphenate: false)
      numero-e-titulo(it)
    }))
  } else {
    // seção no fluxo: menor que a de abertura de página; a
    // âncora fica junto da etiqueta, na mesma página. Como num artigo, um
    // float de topo citado nela sobe acima do título quando a seção começa
    // no meio da página, e numa página de abertura ele fica entre o título
    // da abertura e o desta seção.
    block(width: 100%, sticky: true,
      above: espaco-ate(4, corpo), below: espaco-ate(2, corpo), {
      [#metadata((abre: false)) <ancora-titulo>]
      set par(justify: false, leading: 10pt, first-line-indent: 0pt)
      set text(size: 18pt, weight: 700, fill: p.titulo, hyphenate: false,
               top-edge: cap-em * corpo)
      numero-e-titulo(it)
    })
  }
  show heading.where(level: 2): it => block(width: 100%, sticky: true,
    above: espaco-ate(2, corpo), below: espaco-ate(1, corpo), {
    set par(justify: false, leading: 7pt, first-line-indent: 0pt)
    set text(size: 13pt, weight: 700, fill: p.enfase, hyphenate: false,
             top-edge: cap-em * corpo)
    if it.numbering != none {
      counter(heading).display(it.numbering)
      h(0.5em)
    }
    it.body
  })
  show heading.where(level: 3): it => block(width: 100%, sticky: true,
    above: espaco-ate(2, corpo), below: entrelinha, {
    set par(justify: false, first-line-indent: 0pt)
    set text(size: 10.5pt, weight: 700, hyphenate: false,
             top-edge: cap-em * corpo)
    it.body
  })

  // ---- listas e citações ----
  set list(indent: 0.6em, body-indent: 0.6em, spacing: entrelinha,
           marker: text(fill: p.enfase)[•])
  set enum(indent: 0.6em, body-indent: 0.6em, spacing: entrelinha,
           numbering: n => text(fill: p.enfase, weight: 700, str(n) + "."))
  show quote.where(block: true): it => block(width: 100%,
    inset: (left: 1.5cm, right: 1.5cm),
    above: espaco-ate(1, corpo), below: espaco-ate(1, corpo), {
    set par(first-line-indent: 0pt, leading: 4pt)
    set text(size: 9pt)
    it.body
  })

  // ---- tabelas e figuras ----
  // Float por medida (base.flutuar): o que cabe numa página flutua, o que
  // não cabe (tabela longa) fica em fluxo como bloco quebrável, com o
  // cabeçalho repetido. Fonte e notas viajam dentro do float.
  show figure: set block(breakable: true, width: 100%,
    above: espaco-ate(2, corpo), below: espaco-ate(2, corpo))
  let largura-mancha = 210mm - 2 * margem-x
  let altura-mancha = 297mm - margem-topo - margem-base
  // imagem alta (gráfico de pontos com um NatJus por linha) tomava a página
  // inteira e deixava o texto da seção picado em restos de 14 linhas
  // A imagem é medida como o autor a pediu, com a largura relativa
  // resolvida contra o espaço que ela tem de fato (a mancha, uma coluna de
  // grid, uma célula), e, se passa do teto, é a própria imagem que se
  // reduz por escala: reconstruí-la com outra altura perderia o texto
  // alternativo e os demais campos, e o caminho relativo resolveria a
  // partir deste arquivo, não do documento. Sem reflow, a escala ocupa o
  // tamanho original; a caixa reserva o tamanho reduzido, e o `place` no
  // canto dela impede que o alinhamento de fora (o centro da figura)
  // desloque o desenho. O bloco escalado tem a altura natural da imagem
  // declarada: sem ela, ele seria composto na região da caixa, da altura
  // do teto, e só o pedaço de cima da imagem sairia no PDF. A imagem fica
  // à esquerda do bloco para ficar na origem da escala.
  show image: it => layout(disp => {
    let tam = measure(it, width: disp.width)
    let teto = fracao-maxima-de-imagem * altura-mancha
    if tam.height <= teto { return it }
    let f = teto / tam.height
    box(width: tam.width * f, height: teto,
      place(top + left, scale(f * 100%, origin: top + left, reflow: false,
        block(width: disp.width, height: tam.height, breakable: false,
          align(left, it)))))
  })
  show figure: it => context {
    let h = measure(block(width: largura-mancha, it)).height
    if h <= fracao-minima-para-flutuar * altura-mancha {
      // pequena: fica onde é citada, inteira
      block(width: 100%, breakable: false, it)
    } else {
      // no topo também na página que abre seção: o título é um float de
      // topo que chega antes, e a figura empilha abaixo dele
      base.flutuar(largura-mancha, altura-mancha,
        folga: espaco-ate(2, corpo), posicao: top)(it)
    }
  }
  // sticky: legenda posta acima de uma tabela quebrável nunca fecha página
  show figure.caption: it => block(width: 100%, below: 6pt, sticky: true, {
    set par(justify: false, first-line-indent: 0pt, leading: 4pt)
    set text(size: 9pt, fill: tinta, hyphenate: false)
    align(center)[#strong[#it.supplement #context it.counter.display(it.numbering)#it.separator]#h(0.3em)#it.body]
  })
  // fio grosso acima do cabeçalho, fio médio abaixo dele e fios claros
  // entre as linhas; o cabeçalho vai em negrito, sem preenchimento
  set table(
    stroke: (x, y) => if y == 0 { (top: 0.9pt + tinta, bottom: 0.6pt + p.enfase) }
      else { (bottom: 0.3pt + p.fio-suave) },
    inset: (x: 5pt, y: 5pt))
  set table.hline(stroke: 0.6pt + p.enfase)
  // colunas dimensionadas pelo conteúdo (sem larguras declaradas); a base
  // já tira a hifenização das células
  show table: set text(size: 9pt)
  show table: set par(justify: false, leading: 4pt, first-line-indent: 0pt)
  show table.cell.where(y: 0): set text(fill: tinta, weight: 700, size: 9pt)

  // ---- notas de rodapé ----
  set footnote.entry(separator: line(length: 30%, stroke: 0.5pt + p.enfase),
                     clearance: 3mm, gap: 4pt, indent: 0pt)
  show footnote.entry: base.nota-deslocada(
    recuo: 12pt, tamanho: 8pt, entrelinha: 3.5pt,
    numero: n => text(weight: 700, fill: p.enfase, str(n)))

  // ---- lista de referências ----
  show rotulo-referencias: it => {
    set text(size: 9pt)
    set par(first-line-indent: 0pt, justify: true, leading: 4pt)
    set block(spacing: 9pt)
    it
  }

  set page(
    paper: "a4",
    margin: (x: margem-x, top: margem-topo, bottom: margem-base),
    footer-descent: 35%,
    header: context {
      // sem título corrente na página que abre seção; a consulta filtra
      // por página porque o cabeçalho é composto antes do corpo, e usa a
      // âncora do título, que fica na página dele, e não a etiqueta, que
      // pode ir para a página seguinte
      let pg = here().page()
      let abre = query(<ancora-titulo>)
        .filter(a => a.value.abre and a.location().page() == pg).len() > 0
      if not abre {
        set text(size: 8pt, fill: tinta-suave)
        let esquerda = if serie != none {
          serie + if numero != none { [ · n. #numero] } else { [] }
        } else { [] }
        grid(columns: (1fr, auto), align: bottom,
          esquerda,
          if titulo-curto != none { titulo-curto } else { [] })
      }
    },
    footer: align(right, text(size: 9pt, weight: 700,
      fill: tinta-suave, context counter(page).display())),
  )

  identidade-wp.update((titulo: titulo,
    tipo-figura: tipo-figura, tipo-tabela: tipo-tabela))
  if not neutro {
    capa(titulo: titulo, subtitulo: subtitulo, serie: serie, numero: numero,
         autores: autores, instituicao: instituicao, data: data)
  }
  corpo-doc
}
