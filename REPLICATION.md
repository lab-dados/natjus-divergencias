# Replicação

Este arquivo diz o que dá para reconstruir a partir de um clone deste repositório, o que cada nível exige e o que foi conferido. Nenhum nível depende de material privado: a base está publicada no Hugging Face e as funções do pacote de coleta que os scripts usam estão copiadas em `R/enatjus/`.

Código, comentários e documentação estão em português. Nomes de arquivo, nomes de coluna e chaves de `_variables.yml` ficam em inglês: são identificadores, e renomeá-los mudaria os produtos carimbados e os hashes registrados nos logs.

## O que o artigo usa

`replication-manifest.csv` lista as tabelas e figuras que o manuscrito inclui, a linha do fonte que inclui cada uma e os scripts que a gravam ou leem. `tools/build-replication-manifest.py` monta o arquivo a partir dos includes dos `.qmd`, então a lista acompanha o texto. As linhas com `in_article = no` são produtos versionados que nenhum fonte inclui; `tables/manuscript/attrition.md` é um deles e não entra como tabela, mas é o insumo do fluxograma, que está no artigo.

Os scripts `02` a `04` gravam também diagnósticos de controle de qualidade da construção dos dados, que o artigo não cita: a reconciliação da amostra, os mapas e contagens de origem antes do piso, a junção com o campo de instituição, as contagens de medicamentos, CID, indicadores clínicos, biossimilares e níveis raros, e a cobertura das covariáveis institucionais. Estão em `tables/` e `tables/analysis/`, com o carimbo de proveniência de cada rodada. São checagens de que a amostra e as covariáveis saíram como especificado, não análises, e ficam no repositório porque documentam essa conferência; o script `04` para se faltar algum dos que o `03` grava.

Todo número da prosa vem de `_variables.yml`, gravado por `R/10-manuscript-numbers.R`. O comentário acima de cada chave diz o arquivo, o filtro e a coluna de onde ela vem.

## Os quatro níveis

`tools/replicate.sh N` roda o nível `N` e todos os de baixo, na ordem. Os níveis 2 a 4 terminam comparando o resultado com os arquivos versionados e saem com erro se a comparação falhar. Rodar esses níveis regrava arquivos versionados, então o clone fica com alterações locais; `git checkout -- data tables figures _variables.yml` desfaz.

No nível 2 a comparação de dados, tabelas e números do texto é byte a byte, fora os carimbos de data e hora (`generated_at`, `*_built_at`) e os hashes que decorrem deles. As figuras ficam de fora dessa exigência: os bytes de um PNG dependem da fonte instalada e das versões de cairo e freetype, então o nível 2 lista as figuras regravadas sem falhar, para conferência visual. As figuras usam a IBM Plex Sans quando o fontconfig a encontra; para reproduzi-las com a fonte do artigo, instale as fontes de `typst/fonts/` no sistema. Nos níveis 3 e 4 não pode ser: o glmmTMB, que ajusta os modelos, não é determinístico entre processos. No mesmo ponto e com os mesmos dados, a função objetivo sai diferente na 15.ª casa decimal de uma execução para outra. Fixar a semente não muda isso, porque o ajuste não sorteia nada. Com o critério de parada padrão, o otimizador parava longe o bastante do ótimo para essa diferença chegar a 1e-4 nas estimativas, e uma célula da tabela de efeitos fixos trocava de último dígito entre rodadas. Por isso `R/08-final-analysis.R` termina cada ajuste dos modelos com passos de Newton até o gradiente ficar abaixo de 1e-8, o que leva as rodadas a concordar perto da 11.ª casa, e interrompe a rodada se algum ajuste, inclusive os modelos restritos da razão de verossimilhança, não chegar a essa tolerância, terminar com Hessiana não positiva definida ou tiver, no ponto que o glmmTMB reporta, gradiente acima de 1e-6; o log registra o gradiente final de cada um. O encolhimento da etapa descritiva usa o otimizador padrão, e ali a diferença entre rodadas foi da ordem de 1e-16. Os CSV guardam 15 dígitos, então esses níveis usam `tools/compare-replication.R`: igualdade exata em `data/`, nas colunas inteiras e nas de texto dos CSV, e tolerância nas demais colunas numéricas (1e-5 mais 1e-5 do valor versionado). O comparador ignora as colunas que não são resultado: hashes e tamanhos de outros arquivos, o tempo de cada ajuste e o gradiente final, que depois do polimento é ruído abaixo de 1e-8. O script também lista, sem falhar, as tabelas, figuras e números do texto que mudaram, para que a pessoa confira se alguma estimativa virou o último dígito exibido.

### Nível 1: renderizar o manuscrito

Exige Quarto, uv e, para o PDF, Typst. Não exige R.

Renderiza `paper.docx`, `appendix.docx`, `paper-with-appendix.docx`, um docx por seção em `sections/`, `report.pdf` e `report-neutro.pdf`, a partir das tabelas, figuras e números versionados.

### Nível 2: reconstruir figuras, tabelas e números

Exige R com os pacotes de `replication-r-packages.csv`. Não exige a base nem os modelos ajustados.

Roda os scripts `06`, `07`, `10`, `09` e `13`, nessa ordem, porque o `09` lê a série anual que o `10` grava. Eles leem os CSV carimbados de `tables/analysis/`, `tables/institutions.csv` e `data/analysis-sample.parquet`; o `13` lê `tables/manuscript/attrition.md`, que o `07` grava na mesma rodada.

`R/10-manuscript-numbers.R` só atualiza a contagem de grafias dos nomes de NatJus quando `ENATJUS_DICTIONARY` aponta para o dicionário mantido pelo repositório de coleta. Sem ele, o script lê o `tables/analysis/kit-origin-spellings.csv` carimbado, e o resultado é o mesmo.

### Nível 3: reajustar os modelos

Exige o mesmo que o nível 2. Não exige a base: os modelos partem dos parquets versionados em `data/`.

Roda `08` e `11` e depois o nível 2. `R/08-final-analysis.R` ajusta as etapas da análise final, grava os modelos em `data/model-fits/` (fora do versionamento, porque os arquivos passam do limite de 100 MB do GitHub) e publica as tabelas `tables/analysis/final-analysis-*.csv`, com um manifesto que registra o SHA-256 de cada produto. Cada etapa guarda um checkpoint em `data/model-fits/checkpoints/`, identificado pelo hash das entradas, pelas versões de R, fixest e glmmTMB e por um rótulo fixo da implementação da etapa, que quem muda o cálculo tem de trocar; os checkpoints da razão de verossimilhança e do perfil levam também a chave dos ajustes principais, de que partem. Uma segunda rodada reaproveita os checkpoints válidos e refaz só o que mudou. `R/11-conitec-stratified-standardization.R` lê o ajuste salvo do Modelo 1.

As funções do pacote de coleta usadas aqui vêm de `R/enatjus/`, copiadas do commit registrado em `R/enatjus.R`. Os scripts `08` e `11` as usam, inclusive funções internas do pacote, e por isso a cópia fixa o código exato da rodada do artigo em vez de depender de uma versão instalada.

### Nível 4: reconstruir a amostra a partir da base

Exige, além do nível 3, acesso à base no Hugging Face. A base é o arquivo `data/base_enatjus_tratada.parquet` do dataset `BrunoDCDO/enatjus_v2`, na revisão e com o SHA-256 registrados em `R/enatjus/read-base.R`. O dataset tem acesso sob aprovação: peça acesso na página dele e faça login (`uvx --from huggingface_hub hf auth login`).

Baixa a base com `tools/baixar-base.sh`, se ela ainda não estiver em `data/hf/`, e roda `00` a `04` antes do nível 3. `R/00-setup.R` confere os pacotes e o hash da base; `R/enatjus/read-base.R` confere o hash de novo a cada leitura e para se o arquivo não for o do artigo. Quem já tem o arquivo em outro lugar aponta a variável `NATJUS_BASE` para ele.

A base publicada é a pública: só as notas que o e-NatJus exibe sem login, sem as colunas de número de processo e de data de nascimento do paciente, que a plataforma só mostra com login. Ela traz os demais campos da página pública, inclusive o nome e o número de OAB do advogado, e o texto livre das notas pode citar número de processo ou data de nascimento. A amostra do artigo usa só essas notas, e todas as notas dos parquets versionados estão na base na revisão fixada. O primeiro corte de `R/01-sample.R` restringe a base às notas emitidas até 31 de agosto de 2026; a coleta rodou em 6 de setembro.

## O que foi conferido

Em 24 de setembro de 2026, o nível 4 rodou num clone limpo do commit registrado na primeira linha do log, com a base da revisão fixada conferida pelo SHA-256, e terminou em 35 minutos, com pico de 10,2 GiB de memória residente; o log está em `logs/replicacao-nivel-4-2026-09-24.log`. Os arquivos versionados são a saída dessa rodada. A primeira linha do log foi atualizada depois da rodada: o histórico do repositório foi reescrito para tirar versões antigas dos dados, e o commit que rodou passou a ter outro hash, com a mesma árvore (`8e7494e`).

No mesmo dia, outras duas rodadas fizeram o mesmo cálculo em processos separados, antes de o R/08 ganhar a guarda do gradiente, que não muda o cálculo: o nível 4 em outro clone limpo e os níveis 3 e 2 no repositório de trabalho. Nas três, a amostra, as covariáveis, as tabelas e figuras do manuscrito e todos os números da prosa saíram idênticos byte a byte. Nos CSV dos modelos, a maior diferença entre rodadas foi de 9e-12 nos coeficientes, 5e-10 na estatística da razão de verossimilhança, que é da ordem de 17 mil, e 1,7e-7 no limite do intervalo de perfil do desvio-padrão dos NatJus, que a busca de raiz acha com tolerância própria.

## Limites

A data de emissão vem da página pública da nota tal como foi capturada. Uma nota capturada antes de ser emitida, ou reemitida depois da captura, fica com a data daquela captura. Em 24 de setembro de 2026, as páginas públicas cuja data de emissão faltava ou divergia da página vista com login foram baixadas de novo, e a base publicada na revisão fixada já traz as datas corrigidas. A conferência entre as duas páginas é do repositório de coleta; aqui só se lê o resultado.

Os parquets versionados em `data/` guardam o identificador da nota, os campos do formulário usados nos modelos, o medicamento, os códigos CID e o NatJus emissor. Não têm nome ou número de OAB de advogado, número de processo, data de nascimento nem os campos de texto longo da nota, como a conclusão. O medicamento (`txtDcb`) e o CID (`txtCid`) vêm como a nota os registra; a leitura desses dois campos não achou número de processo nem sequência de seis ou mais dígitos.

## Ambiente

A rodada registrada aqui usou R 4.1.2 em Ubuntu 22.04, Quarto 1.9.38, Typst 0.15.0 e uv 0.12.11. `replication-r-packages.csv` tem a versão de cada pacote R que os scripts carregam; `Rscript tools/record-r-packages.R` regrava o arquivo. O projeto não tem lockfile. As ferramentas em Python fixam as dependências no próprio arquivo e rodam por `uv run`.

## Licença e citação

O código está sob a licença MIT (`LICENSE`), e as funções copiadas em `R/enatjus/` mantêm a licença MIT do pacote de origem (`R/enatjus/LICENSE.md`). Texto, figuras, tabelas e dados derivados estão sob CC BY 4.0 (`LICENSE-CONTENT.md`, que também lista o material de terceiros). `CITATION.cff` traz a citação.
