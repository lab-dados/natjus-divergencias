<!-- source: tables/analysis/final-analysis-fit-summary.csv; tables/analysis/final-analysis-fit-populations.csv; tables/analysis/final-analysis-fit-diagnostics.csv -->

| Quantidade | Modelo 1 | Modelo 2 |
|:-----------------------|----------------:|----------------:|
| Notas de entrada | 273.734 | 273.734 |
| Notas estimadas | 273.734 | 260.833 |
| Descartes internos | 0 | 12.901 |
| Cobertura | 100,0% | 95,3% |
| Convergência | convergiu | convergiu |
| Identificação | identificado | identificado |
| Log-verossimilhança | −113.676,0 | −105.674,5 |
| Parâmetros | 23 | 2.471 |
| AIC | 227.397,9 | 216.291,0 |

: Amostras estimadas e diagnósticos de ajuste dos Modelos 1 e 2 {#tbl-fits}

::: {custom-style="Fonte"}
Fonte: elaboração própria a partir das notas técnicas do e-NatJus (CNJ).
:::

::: {custom-style="Notas"}
Notas: Ajustes definitivos dos Modelos 1 e 2. Notas de entrada é a amostra analítica; Notas estimadas, as que o estimador conservou; Descartes internos, as que ele removeu por pertencerem a nível de medicamento ou CID sem variação na conclusão; Cobertura, a razão entre as duas primeiras. Convergência e Identificação são os diagnósticos do estimador; Log-verossimilhança, Parâmetros e AIC descrevem o ajuste. Os valores de AIC não comparam os dois modelos entre si: são calculados sobre conjuntos de notas diferentes, porque o Modelo 2 descarta as notas de níveis sem variação na conclusão, e o do Modelo 1 vem da verossimilhança marginal, que integra os efeitos aleatórios, enquanto o do Modelo 2 vem da verossimilhança com um parâmetro por nível de medicamento e de CID.
:::
