<!-- source: R/model-contract.R; tables/analysis/covariate-summary.csv; written by hand from the contract, not generated -->

| Grupo | Variável apresentada | Codificação | Categorias ou escala |
|:------------|:---------------|:----------------------------------|:--------------------|
| Desfecho | Conclusão da nota | Binária; evento é a conclusão favorável | Favorável; Desfavorável |
| Regulatório e política pública | Manifestação da Conitec | Três respostas substantivas preservadas; ausência vira N/I, nível auxiliar fora dos contrastes | Não avaliada (referência); Favorável; Desfavorável; N/I |
| Regulatório e política pública | Registro na Anvisa | Binária com N/I auxiliar | Não (referência); Sim; N/I |
| Regulatório e política pública | Disponível no SUS | Binária com N/I auxiliar | Não (referência); Sim; N/I |
| Regulatório e política pública | Medicamento oncológico | Binária com N/I auxiliar | Não (referência); Sim; N/I |
| Regulatório e política pública | Existe biossimilar | Binária com N/I auxiliar; ausência não vira "Não" | Não (referência); Sim; N/I |
| Caso e pedido | Indicação em conformidade | Binária com N/I auxiliar; é juízo do próprio NatJus sobre o caso | Não (referência); Sim; N/I |
| Caso e pedido | Previsto em protocolo | Binária com N/I auxiliar | Não (referência); Sim; N/I |
| Caso e pedido | Sexo do paciente | Dois níveis e N/I auxiliar; N/I concentra-se numa origem e num período | Feminino (referência); Masculino; N/I |
| Caso e pedido | Representação jurídica | Dois níveis e N/I auxiliar | Defensoria Pública (referência); Ministério Público; N/I |
| Medicamento e diagnóstico | Medicamento | Normalizado só por caixa, acento e espaços; sem união de sais ou associações; rótulo de situação Anvisa no campo vira N/I | {{< var medicine_levels >}} níveis na amostra |
| Medicamento e diagnóstico | Categoria CID | Primeiro código sintático, pontuação normalizada, categoria de três caracteres; ausência vira N/I | {{< var icd_categories >}} categorias na amostra |
| Tempo | Mês de emissão | Meses decorridos desde maio de 2019, tendência linear; leitura anual por 12 × coeficiente mensal | 0 a {{< var month_index_max >}} |
| Origem | NatJus | Origem analítica depois da classificação e das agregações; piso de 100 notas | {{< var n_origins >}} origens; Nacional é a referência do Modelo 2 |

: Variáveis e codificação dos Modelos 1 e 2 {#tbl-variables}

::: {custom-style="Fonte"}
Fonte: elaboração própria a partir do contrato de codificação dos modelos e do resumo das covariáveis.
:::

::: {custom-style="Notas"}
Notas: a tabela apresenta o campo de origem e a codificação de cada variável. N/I marca campo não preenchido ou não aplicável e nunca é convertido em resposta negativa. Nenhum tratamento de nível raro foi aplicado a medicamento ou CID nesta versão.
:::
