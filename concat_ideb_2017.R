# Ideb_df

library(tidyverse)
library(readxl)
library(stringi)
library(janitor)

# Origem: http://inep.gov.br/educacao-basica/ideb/resultados
# Juntando todos os dataframes do IDEB 2017:

dir <- "C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data/IDEB"

setwd(dir)

pastas <- list.files()

h <- snakecase::to_any_case(c("Sigla da UF", "Código do Município",
                              "Nome do Município","Código da Escola", "Nome da Escola", "Rede", "aprovacao 2005 6º a 9º ano",
                              "aprovacao 2005 6º", "aprovacao 2005 7º", "aprovacao 2005 8º", "aprovacao 2005 9º",
                              "Indicador de Rendimento (P) 2005", "aprovacao 2007 6º a 9º ano", "aprovacao 2007 6º",
                              "aprovacao 2007 7º", "aprovacao 2007 8º", "aprovacao 2007 9º", "Indicador de Rendimento (P) 2007",
                              "aprovacao 2009 6º a 9º ano", "aprovacao 2009 6º", "aprovacao 2009 7º", "aprovacao 2009 8º",
                              "aprovacao 2009 9º", "Indicador de Rendimento (P) 2009", "aprovacao 2011 6º a 9º ano",
                              "aprovacao 2011 6º", "aprovacao 2011 7º", "aprovacao 2011 8º", "aprovacao 2011 9º",
                              "Indicador de Rendimento (P) 2011", "aprovacao 2013 6º a 9º ano", "aprovacao 2013 6º",
                              "aprovacao 2013 7º", "aprovacao 2013 8º", "aprovacao 2013 9º", "Indicador de Rendimento (P) 2013",
                              "aprovacao 2015 6º a 9º ano", "aprovacao 2015 6º", "aprovacao 2015 7º", "aprovacao 2015 8º",
                              "aprovacao 2015 9º", "Indicador de Rendimento (P) 2015", "aprovacao 2017 6º a 9º ano",
                              "aprovacao 2017 6º", "aprovacao 2017 7º", "aprovacao 2017 8º", "aprovacao 2017 9º",
                              "Indicador de Rendimento (P) 2017", "Nota SAEB - 2005 Matemática",
                              "Nota SAEB - 2005 Língua Portuguesa", "Nota SAEB - 2005 Nota Média Padronizada (N)",
                              "Nota SAEB - 2007 Matemática", "Nota SAEB - 2007 Língua Portuguesa",
                              "Nota SAEB - 2007 Nota Média Padronizada (N)", "Nota SAEB - 2009 Matemática",
                              "Nota SAEB - 2009 Língua Portuguesa", "Nota SAEB - 2009 Nota Média Padronizada (N)",
                              "Nota SAEB - 2011 Matemática", "Nota SAEB - 2011 Língua Portuguesa",
                              "Nota SAEB - 2011 Nota Média Padronizada (N)", "Nota SAEB - 2013 Matemática",
                              "Nota SAEB - 2013 Língua Portuguesa", "Nota SAEB - 2013 Nota Média Padronizada (N)",
                              "Nota SAEB - 2015 Matemática", "Nota SAEB - 2015 Língua Portuguesa",
                              "Nota SAEB - 2015 Nota Média Padronizada (N)", "Nota SAEB - 2017 Matemática",
                              "Nota SAEB - 2017 Língua Portuguesa", "Nota SAEB - 2017 Nota Média Padronizada (N)",
                              "IDEB2005(N x P)", "IDEB2007(N x P)", "IDEB2009(N x P)", "IDEB2011(N x P)",
                              "IDEB2013(N x P)", "IDEB2015(N x P)", "IDEB2017(N x P)",
                              "Projeções 2007",	"Projeções 2009",	"Projeções 2011",	"Projeções 2013",	"Projeções 2015",	
                              "Projeções 2017",	"Projeções 2019", "Projeções 2021"))
h <- gsub("_º", "o", h)
h <- stri_trans_general(h, "Latin-ASCII")

i =1
setwd(paste0(dir, "/", pastas[i]))
n_obj <- gsub("divulgacao_anos_", "", pastas[i])
n_obj <- snakecase::to_any_case(n_obj)
assign(n_obj, read_excel(paste0(pastas[i], ".xlsx"), skip = 8, col_names = h))

finais_escolas_2017 <- finais_escolas_2017 %>%
  slice(3:43612)

## terminei as importações, vou limpar.
## Primeiro, duplicatas:

finais_escolas_2017 %>%
  distinct() %>% nrow() #não tem

#missings estão como "-" e "ND" , tenho que substituir or missing.
finais_escolas_2017[] <- lapply(finais_escolas_2017, gsub, pattern='-', replacement=NA)
finais_escolas_2017[] <- lapply(finais_escolas_2017, gsub, pattern='ND', replacement=NA)
missings_finais <- as.data.frame(colSums(is.na(finais_escolas_2017)))

#Deixar todos os campos numéricos como numéricos:

hf <- names(finais_escolas_2017)
hf <- hf[-6:-1]

finais_escolas_2017 <- finais_escolas_2017 %>%
  mutate(nota_saeb_2015_matematica = gsub("\\*", "", nota_saeb_2015_matematica),
         nota_saeb_2015_matematica = gsub(",", "\\.", nota_saeb_2015_matematica),
         nota_saeb_2015_lingua_portuguesa = gsub("\\*", "", nota_saeb_2015_lingua_portuguesa),
         nota_saeb_2015_lingua_portuguesa = gsub(",", "\\.", nota_saeb_2015_lingua_portuguesa)) %>%
  mutate_at(hf, as.numeric)

#validar quantos missings temps no final
mf <- as.data.frame(colSums(is.na(finais_escolas_2017))) %>%
  bind_cols(missings_finais) %>%
  clean_names() %>%
  mutate(dif = col_sums_is_na_finais_escolas_2017_1 - col_sums_is_na_finais_escolas_2017)

#Deu certo =D
rm(mf)
rm(missings_finais)
rm(hf)

i =2
setwd(paste0(dir, "/", pastas[i]))
n_obj <- "iniciais_escolas_2017"
assign(n_obj, read_excel("divulgacao_anos_iniciais-escolas-2017_copia.xlsx"))

# As três últimas linhas são legendas

iniciais_escolas_2017 <- iniciais_escolas_2017 %>%
  clean_names() %>%
  slice(1:59922)

## Limpeza:

#Duplicatas:
iniciais_escolas_2017 %>% distinct() %>% nrow()  #Não tem

#Substituindo os padrões de NA que eu já encontrei aqui e nas outras tabelas:
iniciais_escolas_2017[] <- lapply(iniciais_escolas_2017, gsub, pattern='-', replacement=NA)
iniciais_escolas_2017[] <- lapply(iniciais_escolas_2017, gsub, pattern='ND', replacement=NA)

#Missings:
missings_finais <- as.data.frame(colSums(is.na(iniciais_escolas_2017)))

#Selecionando os nomes das colunas numéricas
hf <- names(iniciais_escolas_2017)
hf <- hf[-6:-1]

#Limpando: fazendo as substituições e transformando nossas variáveis em numéricas.
iniciais_escolas_2017 <- iniciais_escolas_2017 %>%
  mutate(nota_saeb_2015_matematica = gsub("\\*", "", nota_saeb_2015_matematica),
         nota_saeb_2015_matematica = gsub(",", "\\.", nota_saeb_2015_matematica),
         nota_saeb_2015_lingua_portuguesa = gsub("\\*", "", nota_saeb_2015_lingua_portuguesa),
         nota_saeb_2015_lingua_portuguesa = gsub(",", "\\.", nota_saeb_2015_lingua_portuguesa)) %>%
  mutate_at(hf, as.numeric) 

#Validando:
mf <- as.data.frame(colSums(is.na(iniciais_escolas_2017))) %>%
  bind_cols(missings_finais) %>%
  clean_names() %>%
  mutate(dif = col_sums_is_na_iniciais_escolas_2017_1 - col_sums_is_na_iniciais_escolas_2017)

#Ok, vamos pra próxima
rm(mf)
rm(missings_finais)
rm(hf)

### Ensino Médio:
i = 3
setwd(paste0(dir, "/", pastas[i]))
n_obj <- "medio_escolas_2017"
assign(n_obj, read_excel("divulgacao_ensino_medio-escolas-2017_copia.xlsx"))

medio_escolas_2017 <- medio_escolas_2017 %>%
  clean_names() %>%
  slice(1:19625)    #Aqui também têm três linhas para legendas

#Substituindo os padrões de NA que eu já encontrei aqui e nas outras tabelas:
medio_escolas_2017[] <- lapply(medio_escolas_2017, gsub, pattern='-', replacement=NA)
medio_escolas_2017[] <- lapply(medio_escolas_2017, gsub, pattern='ND', replacement=NA)

#Missings:
missings_finais <- as.data.frame(colSums(is.na(medio_escolas_2017)))

#Selecionando os nomes das colunas numéricas
hf <- names(medio_escolas_2017)
hf <- hf[-6:-1]

medio_escolas_2017 <- medio_escolas_2017 %>%
  mutate_at(hf, as.numeric) 

#Validando:
mf <- as.data.frame(colSums(is.na(medio_escolas_2017))) %>%
  bind_cols(missings_finais) %>%
  clean_names() %>%
  mutate(dif = col_sums_is_na_medio_escolas_2017_1 - col_sums_is_na_medio_escolas_2017)

#Deu certo.

rm(mf)
rm(missings_finais)
rm(h)
rm(hf)
rm(i)
rm(n_obj)
rm(pastas)

ideb <- list(finais_escolas_2017, iniciais_escolas_2017, medio_escolas_2017)
setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data/IDEB")
save(ideb, file="ideb_2017.Rdata")
