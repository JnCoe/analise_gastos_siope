---
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Codebook

Arquivo ``ideb_sp.Rdata`` é um dataframe contendo 17104 linhas e 8 colunas. Trata-se das notas do ideb obtidas pelas escolas do estado de São Paulo em 2017.

### O Ideb:

É o Índice de Desenvolvimento da Educação Básica, calculado a partir de duas variáveis: as taxas de
aprovação e evasão, levantadas pelo censo anual da educação, e as médias nos dois exames padronizados do Inep que compõem o Saeb (Prova Brasil e Aneb). O Ideb é divulgado a cada dois anos e tem escala de zero a 10, servindo como um ranking das escolas.

[Fonte](http://www.clicrbs.com.br/pdf/17745467.pdf)

### Variáveis:

|Nome variável| Descrição|
|--|--|
|sigla_da_uf|sigla da uf|
|codigo_do_municipio|código do município de acordo com ibge|
|nome_do_municipio||
|codigo_da_escola|código do MEC|
|nome_da_escola|nome do MEC|
|rede|Estadual, Municipal ou Federal|
|ideb_2017_n_x_p|Nota do IDEB em 2017|
|periodo|Séries que estão sendo avaliadas por esta nota|  
