---
title: "Análise dados remuneração de profissionais do magistério 2018 - SIOPE"
date: "30 de janeiro de 2020"
output: 
    html_document: 
        theme: paper 
        toc: true
        toc_depth: 2
        code_folding: hide
        toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)/remuneracao_2018")

load(file="arquivos.Rdata")
library(kableExtra)
```

## Relatório

Após verificar que os dados de remuneração de profissionais do magistério do estado de São Paulo para o ano de 2018 continha, na verdade, os dados do ano de 2019, decidi executar um script em R para verificar se o mesmo acontecia com todos os anos. 
O script abaixo realiza as seguintes etapas:

1. Define o link de download do arquivo do SIOPE (substituindo a UF)
2. Download do arquivo
3. Verifica os valores únicos contidos na variável AN_EXERCICIO
4. exporta as informações para uma tabela.

```{r, eval=FALSE}
library(dplyr)
library(data.table)


ufs <- c("AC", "AL", 'AP', "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT",
         "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
         "RR", 'SC', 'SP', "SE", "TO")

arquivos <- data_frame(uf = "", ano_exercicio = "")

for(i in 1:length(ufs)){
  print(i)
  uf <- ufs[i]
  
  url <- gsub("UF", uf, 
              "ftp://ftp2.fnde.gov.br/dadosabertos/SIOPE/REMUNERACAO_PROFISSIONAIS_EDUCACAO_UF_2018.CSV")
  
  destino = paste0("~/remuneracao_2018/", uf, "_2018.csv")
  
  
  download.file(url, destfile = destino)
  
  x <- fread(destino)
  
  ano_exercicio <- paste(unique(x$AN_EXERCICIO),collapse=" ")
  
  y <- data_frame(uf, ano_exercicio)
  
  arquivos <- rbind(arquivos, y)
}
```

<br><br>
O resultado dessa operação gerou a seguinte tabela abaixo.
Como podemos ver, todos os arquivos possuem registros do exercício do ano de 2019, exceto o arquivo do distrito federal, que não possui nenhuma informação.

```{r, message=FALSE, warning=FALSE}

arquivos %>%
  kable() %>%
  kable_styling()

```

-----
