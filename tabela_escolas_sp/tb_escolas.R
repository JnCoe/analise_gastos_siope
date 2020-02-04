library(dplyr)
library(data.table)
library(janitor)
library(stringi)
library(writexl)

#Remuneração de professores - SIOPE
load("~/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data/rem_prof_sp.Rdata")

#Rede de ensino de São Paulo
rede_ensino_sp <- fread("C:/Users/coliv/Documents/Python Scripts/Escolas São Paulo/dados_rede_de_ensino_sp/rede_ensino_sp.csv",
                        encoding = "UTF-8")

#retirando ruplicadas:

rede_ensino_sp <- rede_ensino_sp %>%
  distinct()

rem_prof_sp <- rem_prof_sp %>%
  distinct()

#Não tinha.  

## Missings:
as.data.frame(colSums(is.na(rede_ensino_sp)))

#Sem missings em variáveis relevantes.

## Quantas escolas constam no arquivo por categoria:
rede_ensino_sp %>%
  group_by(`Nome dependencia Administrativa`) %>%
  summarise(total = n()) %>%
  adorn_totals("row")

# No arquivo rem_prof_sp constam 10.955 escolas. 
# Como é um arquivo que trata só de escolas municipais tem uma série de escolas que 
# não constam na remuneração. Tenho que entender o porque.

## Montando meu arquivo de referencia das escolas

data_secretaria <- rede_ensino_sp %>%
  clean_names() %>%
  select(nome_dependencia_administrativa, nome_escola,
         codigo_escola, codigo_mec,
         codigo_mec, municipio,
         municipio, bairro,
         distrito, cep,
         complemento, endereco,
         numero) %>%
  mutate_all(as.character) %>%
  mutate(esc = tolower(stri_trans_general(nome_escola, "Latin-ASCII")),
         municipio = tolower(stri_trans_general(municipio, "Latin-ASCII"))) %>%
  rename(nome_escola_secretaria = nome_escola,
         no_municipio = municipio)

tb_escolas <- rem_prof_sp %>%
  clean_names() %>%
  distinct(no_municipio, local_exercicio) %>%
  mutate(esc = tolower(stri_trans_general(local_exercicio, "Latin-ASCII")),
         no_municipio = tolower(stri_trans_general(no_municipio, "Latin-ASCII"))) %>%
  rename(nome_escola_siope = local_exercicio) %>%
  filter(!grepl("sec mun", esc),
         !grepl("associacao de pais e amigos dos excepcionais", esc)) %>%   #retirando o que não é escola
  left_join(data_secretaria)

#Vamos ver onde deu certo:

tb_escolas %>%
  mutate(achou_end = ifelse(is.na(nome_escola_secretaria), FALSE, TRUE)) %>%
  group_by(achou_end) %>%
  summarise(total = n())

#Vamos ver quantos municípios são:
tb_escolas %>%
  filter(is.na(nome_escola_secretaria))%>%
  group_by(no_municipio) %>%
  summarise(total = n()) %>%
  nrow()

#############################################################################
#Acho que vou exportar para excel e trabalhar com o arquivo exportado.
setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/tabela_escolas_sp")

tb_escolas <- as.data.frame(tb_escolas)

writexl::write_xlsx(tb_escolas, 
                    path = "tb_escolas.xlsx", col_names=TRUE)

#############################################################################


#TB escolas match vai ser o arquivo só com os matches:
tb_escolas_match <- tb_escolas %>%
  filter(!is.na(nome_escola_secretaria))

# O que eu fiz:
# Separei as escolas em que eu achei endereço das que eu não achei endereço em dois 
# objetos diferentes. Verifiquei no excel quais eram os padrões que se repetiam e que eu 
# poderia fazer gsubs tanto nos dados do SIOPE(tb_escolas) quanto nos dados da 
# secretaria(data_secretaria) para aumentar meu número de matchs. No final, consegui o endereço
# de 87% das escolas do SIOPE

ends <- data_secretaria %>%
  mutate(esc = ifelse(no_municipio == "sao paulo", gsub(" creche municipal", "", esc),esc),
         esc = ifelse(no_municipio == "sao paulo", gsub("centro municipal de educação infantil ",
                                                        "cemei ", esc), esc),
         esc = ifelse(no_municipio == "sao paulo", gsub(" ceu cei","", esc),esc),
         esc = ifelse(no_municipio == "sao paulo", gsub(" emef", "", esc),esc),
         esc = ifelse(no_municipio == "sao paulo" & grepl("benno hubert stollenwerk", esc), 
                      "benno hubert stollenwerk", esc),
         esc = ifelse(no_municipio == "sao paulo", gsub(" ceu emei", "", esc), esc),
         esc = ifelse(no_municipio == "sao paulo", gsub(" emef", "", esc), esc),
         esc = ifelse(no_municipio == "sao paulo", gsub(" emei", "", esc), esc))

#excel
tb_esc2 <- tb_escolas %>%
  filter(is.na(nome_escola_secretaria)) %>%
  select(1:3) %>%
  mutate(esc = ifelse(no_municipio == "sao paulo", gsub("cei diret ", "", esc), esc),
         esc = ifelse(no_municipio == "sao paulo",gsub("ceu cei ","", esc),esc),
         esc = ifelse(no_municipio == "sao paulo",gsub("ceu emef ","", esc),esc),
         esc = ifelse(no_municipio == "sao paulo" & grepl("benno hubert stollenwerk", esc), 
                      "benno hubert stollenwerk", esc),
         esc = ifelse(no_municipio == "sao paulo",gsub("ceu emei ","", esc),esc),
         esc = ifelse(no_municipio == "franco da rocha",gsub("emeb ","", esc),esc),
         esc = ifelse(no_municipio == "sao paulo",gsub("emef ","", esc),esc),
         esc = ifelse(no_municipio == "sao paulo",gsub("emei ","", esc),esc),
         esc = ifelse(no_municipio == "teodoro sampaio",gsub("creche municipal do ","", esc),esc),
         esc = ifelse(no_municipio == "teodoro sampaio",gsub("creche municipal ","", esc),esc)) %>%
  left_join(ends)


tb_esc2 %>%
  mutate(find = ifelse(is.na(nome_escola_secretaria), FALSE, TRUE)) %>%
  group_by(find) %>%
  summarise(total = n())


tb_esc2 %>%
  filter(is.na(nome_escola_secretaria)) %>%
  group_by(no_municipio) %>%
  summarise(total = n()) %>%
  arrange(desc(total))


tabela_endereco_escolas <- bind_rows(tb_escolas_match, tb_esc2 ) %>%
  distinct(no_municipio, nome_escola_siope, nome_dependencia_administrativa, .keep_all = TRUE) 
  

tabela_endereco_escolas %>%
  mutate(find = ifelse(is.na(nome_escola_secretaria), FALSE, TRUE)) %>%
  group_by(find) %>%
  summarise(total = n())   #Consegui 87% dos endereços das escolas do SIOPE.


#Salvando
setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/tabela_escolas_sp")
fwrite(tabela_endereco_escolas, file="tabela_endereco_escolas.csv")
