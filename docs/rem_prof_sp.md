---
title: "Análise remuneralção professores no estado de São Paulo"
author: "Jessica Voigt"
date: "31 de janeiro de 2020"
output: 
  html_document:
    theme: paper
    toc: true 
    toc_depth: 3 
    code_folding: hide 
    toc_float: true
---

```{r, warning=FALSE, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, warning=FALSE, message=FALSE}
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(janitor)
library(kableExtra)

load("~/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data/rem_prof_sp.Rdata")
```


## Introdução

Esse documento é uma análise do padrão de remuneração dos profissionais de educação em São Paulo. Trata-se de uma análise exploratória para definirmos o que podemos fazer em nível nacional.<br>

O link para o download de todos os arquivos está aqui: [https://www.fnde.gov.br/index.php/fnde_sistemas/siope/relatorios/arquivos-dados-analiticos](https://www.fnde.gov.br/index.php/fnde_sistemas/siope/relatorios/arquivos-dados-analiticos)

Como a última data de atualização dos arquivos do SIOPE consta como 30/08/2019, iríamos fazer uma análise exploratória do arquivo [Remuneração de Profissionais do Magistério de São Paulo – 2018](ftp://ftp2.fnde.gov.br/dadosabertos/SIOPE/REMUNERACAO_PROFISSIONAIS_EDUCACAO_SP_2018.CSV) . No entanto, verifiquei que todos os links para os arquivos dos anos de 2018 tinham, na verdade, arquivos para os anos de 2019 ([veja a verificação aqui](https://voigtjessica.github.io/analise_gastos_siope/revisao_dados_2018)), de forma que decidi trabalhar apenas com o arquivo *Remuneração de Profissionais do Magistério de São Paulo – 2019*

<br><br>

## Dicionário de dados
<br>

|Nome da Coluna| Descrição do Dado|
|-----|------------|
|AN_EXERCICIO| Ano base da declaração|
|NU_PERIODO| Período do ano base a que se refere a declaração (1,2,3,4,5,6). De 2008 a 2016, o período 1 refere-se à declaração anual. A partir de 2017, os dados passaram a ser bimestrais, sendo o período 6 a consolidação anual|
|ME_EXERCICIO| Mês do ano em que foram pagos os vencimentos para o profissional de educação (1 a 12)|
|CO_UF| Código da UF. Mesmo código usado pelo IBGE.|
|NO_UF| Nome da unidade federativa|
|CO_MUNICIPIO| Código do município.É o código completo usado pelo IBGE.|
|NO_MUNICIPIO| Nome do município.|
|NU_REMUNERACAO| Sequencial de remuneração|
|NO_PROFISSIONAL| Nome do profissional do magistério/Educação.|
|LOCAL_ EXERCICIO| Escola onde o profissional exerce sua função / Entidade|
|CARGA_HORARIA| Carga Horária de trabalho do profissional|
|TP_CATEGORIA| Código do tipo de categoria do profissional: 1 - Profissionais do magistério; 2 - Outros profissionais da educação.|
|CATEG_PROFISSIONAL| Número sequencial da categoria do profissional do magistério/Educação.|
|VL_SALARIO| Salário ou Vencimento Básico do profissional da educação|
|VL_PARC_MINIMA_FUNDEB| Vencimento Bruto da Remuneração - Com Parcela mínima de 60% do FUNDEB|
|VL_PARC_MAXIMA_FUNDEB| Valor Bruto da Remuneração - Com parcela máxima de 40% do FUNDEB|
|VL_OUTRAS_RECEITAS| Valor Bruto da Remuneração - Com outros recursos|
|VL_TOTAL| VL_PARC_MINIMA_FUNDEB + VL_PARC_MAXIMA_FUNDEB + VL_OUTRAS_RECEITAS| 

<br><br>

## 1. Análise exploratória

### Conferindo os anos de execução contidos no arquivo:
```{r, warning=FALSE}
unique(rem_prof_sp$AN_EXERCICIO)
```

E os bimestres contidos:

```{r}
unique(rem_prof_sp$NU_PERIODO)
```
<br>
E os meses:

```{r}
unique(rem_prof_sp$ME_EXERCICIO)
```
<br>
###2. Quantidade de missings por variável

```{r}
as.data.frame(colSums(is.na(rem_prof_sp)))
```
<br>

### Quantas escolas constam no arquivo?
```{r}
rem_prof_sp %>%
  distinct(CO_MUNICIPIO, LOCAL_EXERCICIO) %>%
  nrow()
```


### Quais são as categorias profissionais que constam no arquivo?

```{r}

categorias <- unique(rem_prof_sp$CATEG_PROFISSIONAL)

data.frame("Categorias" = categorias) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"))%>%
  scroll_box(width = "700px", height = "200px")

```


### Distribuição de funcionários e escolas por município

```{r}
rem_prof_sp %>%
  filter(ME_EXERCICIO == "06") %>%
  group_by(CO_MUNICIPIO ,NO_MUNICIPIO) %>%
  summarise(qtde_funcionarios = n_distinct(NO_PROFISSIONAL),
            qtde_escolas = n_distinct(LOCAL_EXERCICIO),
            media_funcionario_escola = round(qtde_funcionarios/qtde_escolas, 0)) %>%
  ungroup() %>%
  select(-c(CO_MUNICIPIO)) %>%
  arrange(desc(qtde_funcionarios)) %>%
  adorn_totals("row") %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive")) %>%
  scroll_box(width = "700px", height = "200px")

```

###Descrição da variável VL_SALARIO:

```{r}

summary(rem_prof_sp$VL_SALARIO)
  
```

### Verificando se a variável NU_REMUNERACAO se repete:

Admito que o `NU_REMUNERACAO` é único por município:
```{r}
rem_prof_sp %>%
  group_by(CO_MUNICIPIO, NU_REMUNERACAO) %>%
  summarise(total_nu_remuneracao = n()) %>%
  filter(total_nu_remuneracao > 1) %>%
  nrow()
  
```

Eles não se repetem.

Eu não sei ao certo do que se trata essa variável. Fiz um pedido LAI para o FNDE solicitando maiores esclarecimentos. *Acredito* ser um "protocolo de pagamento", algo que registra uma transição específica.

##2. Inconsistências

### Mais de um registro de pagamento por mês:

Assumindo que cada linha consista em um pagamento dado a uma pessoa `NO_PROFISSIONAL` em um determinado `ME_EXERCICIO`, existem pessoas com mais de um registro de pagamento na mesma função, local e mês?

```{r}
rem_prof_sp %>%
  group_by(CO_MUNICIPIO,LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL, ME_EXERCICIO ) %>%
  summarise(total_registros = n()) %>%
  filter(total_registros > 2) %>%
  distinct(CO_MUNICIPIO,LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL) %>% 
  nrow()
```

Foram registradas 281 pessoas que tiveram ao menos dois registros de pagamento em um mesmo mês, local e exercendo a mesma função.

```{r}
rem_prof_sp %>%
  group_by(CO_MUNICIPIO,LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL, ME_EXERCICIO ) %>%
  summarise(total_registros = n()) %>%
  filter(total_registros > 3) %>%
  distinct(CO_MUNICIPIO,LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL) %>% 
  nrow()
```

Das 281 pessoas, 126 registraram 3 pagamentos para um mesmo local, mÊs e função.

```{r}
rem_prof_sp %>%
  group_by(CO_MUNICIPIO,LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL, ME_EXERCICIO ) %>%
  summarise(total_registros = n()) %>%
  filter(total_registros > 4) %>%
  distinct(CO_MUNICIPIO,LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL) %>% 
  nrow()
```
E cinco pessoas tiveram amis de 4 pagamentos registrados para a mesma função, local e mês.

Esses pagamentos se concentram em algum município específico? E quantos funcionários receberam duas ou mais vezes por município?

```{r}

p <- rem_prof_sp %>%
  group_by(CO_MUNICIPIO,NO_MUNICIPIO, LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL, ME_EXERCICIO ) %>%
  summarise(total_registros = n()) %>%
  filter(total_registros > 2) %>%
  ungroup() %>%
  group_by(CO_MUNICIPIO, LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL) %>%
  summarise(total_repeticoes = n()) %>%
  ungroup() %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(total_funcionarios = n())

rem_prof_sp %>%
  group_by(CO_MUNICIPIO,NO_MUNICIPIO, LOCAL_EXERCICIO, TP_CATEGORIA, 
           CATEG_PROFISSIONAL, NO_PROFISSIONAL, ME_EXERCICIO ) %>%
  summarise(total_registros = n()) %>%
  filter(total_registros > 2) %>%
  ungroup() %>%
  group_by(CO_MUNICIPIO, NO_MUNICIPIO) %>%
  summarise(ocorrencias_registros_repetidos = n()) %>%
  arrange(desc(ocorrencias_registros_repetidos)) %>%
  left_join(p) %>%
  kable() %>%
  kable_styling()
```

###Outliers nos pagamentos

### Salários que constam como menores que 1 real:

```{r}
rem_prof_sp %>%
  filter(VL_SALARIO < 1) %>%
  nrow()
```

### Quem são os maiores salários? (> 100 mil)

```{r}
rem_prof_sp %>%
  filter(VL_SALARIO > 100000) %>%
  select(NO_MUNICIPIO, NO_PROFISSIONAL, CATEG_PROFISSIONAL, ME_EXERCICIO, VL_SALARIO) %>% 
  arrange(NO_PROFISSIONAL) %>%
  kable() %>%
  kable_styling()
```

Vimos que pagamentos acima de R$ 100 mil ocorreram apenas para duas pessoas, sendo duas vezes para uma pessoa e uma para outra pessoa. O arquivo de 2019 não contém dados consolidados, ou seja, esses valores (se corretos) correspontem ao pagamento de um único mês e não o consolidado do ano.

Ainda é possível que seja apenas um erro de digitação. Vamos ver o quanto essas duas funcionárias receberam nos outros meses:

```{r}

rem_prof_sp %>%
  filter(NO_PROFISSIONAL %in% c("Karina Aparecida Pavanelli Horacio",
                  "Patricia Solano Pereira de Souza")) %>%
  select(NO_MUNICIPIO, NO_PROFISSIONAL, CATEG_PROFISSIONAL, ME_EXERCICIO, VL_SALARIO) %>% 
  arrange(NO_PROFISSIONAL) %>%
  kable() %>%
  kable_styling()
```

**Como podemos interpretar esses dados?**

[Voltar](https://voigtjessica.github.io/analise_gastos_siope/index)
