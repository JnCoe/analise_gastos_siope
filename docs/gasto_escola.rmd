---
title: "Os gastos refletem as desigualdades?"
author: "Jessica Voigt"
date: "3 de fevereiro de 2020"
output: html_document
---

```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
```

## Desigualdade

Queremos verificar se os gastos com pagamentos de professores no Estado de São Paulo de alguma forma refletem desigualdades.

Há duas abordagens possíveis para aferir essas desigualdade: 
* Desigualdade regional : IDH, saneamento básico, etc;
* Desigualdade em desempenho: Índices

É provável que esses dois tipos de desigualdades estão fortemente correlacionados, mas neste momento ainda não vamos testar essa hipótese. Optamos por iniciar o debate sobre desigualdade com a seguinda abordagem, olhando primeiramente os resultados do IDEB

### IDEB:

Do site do INEP: <br>

*O Índice de Desenvolvimento da Educação Básica (Ideb) é uma iniciativa do Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (Inep) para mensurar o desempenho do sistema educacional brasileiro a partir da combinação entre a proficiência dos estudantes, obtida no Sistema de Avaliação da Educação Básica (Saeb), e o indicador de taxa de aprovação, que tem influência na eficiência do fluxo escolar e é obtido por meio do Censo Escolar.*

*Essas duas dimensões, que refletem problemas estruturais da educação básica brasileira, precisam ser aprimoradas para que o país alcance níveis educacionais compatíveis com seu potencial de desenvolvimento e para garantia do direito educacional expresso em nossa constituição federal. Pela própria construção matemática do indicador (taxa de troca entre as duas dimensões), para elevar o Ideb, as redes de ensino e as escolas precisam melhorar as duas dimensões do indicador, simultaneamente, uma vez que a natureza do Ideb dificulta a sua elevação, considerando apenas a melhoria de uma dimensão em detrimento da outra.*

Mais informações [aqui]("http://download.inep.gov.br/educacao_basica/portal_ideb/press-kit/2017/press-kit_ideb2017.pdf")

## Cruzamentos

```{r, message=FALSE, warning=FALSE}

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(janitor)
library(kableExtra)

#Dados SIOPE
load("~/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data/rem_prof_sp.Rdata")

#Tabela com os códigos das escolas
escolas = fread("~/R-Projects/qualidade_gastos_educação/analise_gastos_siope/tabela_escolas_sp/tabela_end_escolas_completa.csv", encoding = "UTF-8")

#Juntando:
escolas2 <- escolas %>%
  distinct(nome_escola_siope , municipio, .keep_all = TRUE) %>%
  select(nome_escola_siope , municipio, end2, lon, lat, codigo_escola, codigo_mec) %>%
  rename(NO_MUNICIPIO = municipio,
         LOCAL_EXERCICIO = nome_escola_siope ) 

df <- rem_prof_sp %>%
  left_join(escolas2, by=c("NO_MUNICIPIO", "LOCAL_EXERCICIO"))

```

Distribuição do gasto médio com salário por escola:


```{r, message=FALSE, warning=FALSE}

bx <- 0

for(i in 1:10){
  a <- i*25
  bx <- c(bx, a)
} 

df %>%
  mutate(valor_hora = round(VL_SALARIO / CARGA_HORARIA,0 )) %>%
  group_by(codigo_escola) %>%
  summarise(valor_medio_hora = mean(valor_hora)) %>%
  ggplot(aes(valor_medio_hora)) +
  geom_histogram(show.legend = TRUE) +
  labs(x = "Valor médio da hora paga na escola", y = "Qtde escolas") +
  theme_minimal() + xlim(0,500) +
  ggtitle("Distribuição da média salarial em São Paulo")

```


Tabela com a categoria profissional e o valor médio por hora:

```{r, message=FALSE, warning=FALSE}

df %>%
  mutate(valor_hora = round(VL_SALARIO / CARGA_HORARIA,0 )) %>%
  group_by(CATEG_PROFISSIONAL) %>%
  summarise(valor_medio_hora = mean(valor_hora)) %>%
  kable()
```



