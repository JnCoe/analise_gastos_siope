# tratando os arquivos pra eles ficarem menores:
library(readr)
library(dplyr)
library(stringr)
library(data.table)

fix_nomes <- function(x){
  stopifnot(require(stringr))
  
  x <- str_to_title(x)
  x <- ifelse(grepl("De", x), gsub( "\\ De", " de", x ), x)
  x <- ifelse(grepl("Da", x), gsub( "\\ Da", " da", x ), x)
  x <- ifelse(grepl("Do", x), gsub( "\\ Do", " do", x ), x)
  x <- ifelse(grepl("Dos", x), gsub( "\\ Dos", " dos", x ), x)
  
}

#Remuneração dos professores de São Paulo
setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)")

rem_prof_sp <- read_delim("REMUNERACAO_PROFISSIONAIS_EDUCACAO_SP_2019.CSV", 
                          ";", escape_double = FALSE, col_types = cols(CARGA_HORARIA = col_character(), 
                                                                       CO_MUNICIPIO = col_character(), CO_UF = col_character(), 
                                                                       NU_REMUNERACAO = col_character(), 
                                                                       VL_SALARIO = col_character()), locale = locale(encoding = "ISO-8859-1"), 
                          trim_ws = TRUE)

glimpse(rem_prof_sp)

rem_prof_sp <- rem_prof_sp %>%
  mutate(AN_EXERCICIO = as.character(AN_EXERCICIO),
         NU_PERIODO = as.character(NU_PERIODO),
         NO_PROFISSIONAL = fix_nomes(NO_PROFISSIONAL),
         CARGA_HORARIA = as.numeric(CARGA_HORARIA),
         VL_SALARIO  = as.numeric(gsub(",", "\\.", VL_SALARIO)),
         VL_PARC_MINIMA_FUNDEB = as.numeric(gsub(",", "\\.", VL_PARC_MINIMA_FUNDEB)),
         VL_PARC_MAXIMA_FUNDEB = as.numeric(gsub(",", "\\.", VL_PARC_MAXIMA_FUNDEB)),
         VL_OUTRAS_RECEITAS = as.numeric(gsub(",", "\\.", VL_OUTRAS_RECEITAS)),
         VL_TOTAL = as.numeric(gsub(",", "\\.", VL_TOTAL)))

setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data")
save(rem_prof_sp, file="rem_prof_sp.Rdata")

#Dividindo as despesas totais por estado e salvando os valores:
setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)")

despesa_total_munic <- read_delim("DESPESA_TOTAL_MUNICIPIOS_2019.CSV", 
                                  ";", escape_double = FALSE, col_types = cols(`#AN_EXERCICIO` = col_character(), 
                                                                               CODIGO_EXIBICAO_PASTA_PAI = col_character(), 
                                                                               CODIGO_SUBF_PASTA = col_character(), 
                                                                               CO_CONTA_CONTABIL = col_character(), 
                                                                               CO_MUNICIPIO = col_character(), CO_UF = col_character(), 
                                                                               NU_PERIODO = col_character(), VL_DESPESAS_EMPENHADAS = col_character(), 
                                                                               VL_DESPESAS_LIQUIDADAS = col_character(), 
                                                                               VL_DESPESAS_ORCADA = col_character(), 
                                                                               VL_DESPESAS_PAGAS = col_character(), 
                                                                               VL_DOTACAO_ATUALIZADA = col_character()), 
                                  locale = locale(encoding = "ISO-8859-1"), 
                                  trim_ws = TRUE)

estados <- unique(despesa_total_munic$NO_UF)
estados_nomes <- tolower(iconv(estados, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
estados_nomes <- gsub(" ", "_", estados_nomes)

setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/analise_gastos_siope/data/despesa_total_municipios_por_uf")


for(i in 1:length(estados)){
  
  uf <- estados[i]
  print(uf)
  
  x <- despesa_total_munic %>%
    filter(NO_UF %in% uf) %>%
    rename(ANO_EXERCICIO = `#AN_EXERCICIO`) %>%
    mutate(VL_DOTACAO_ATUALIZADA = as.numeric(gsub(",", "\\.", VL_DOTACAO_ATUALIZADA)),
           VL_DESPESAS_EMPENHADAS = as.numeric(gsub(",", "\\.", VL_DESPESAS_EMPENHADAS)),
           VL_DESPESAS_LIQUIDADAS = as.numeric(gsub(",", "\\.", VL_DESPESAS_LIQUIDADAS)),
           VL_DESPESAS_PAGAS = as.numeric(gsub(",", "\\.", VL_DESPESAS_PAGAS)),
           VL_DESPESAS_ORCADA = as.numeric(gsub(",", "\\.", VL_DESPESAS_ORCADA)))
  
  nome <- paste0(estados_nomes[i], "_despesa_total")          
  
  save(x, file=paste0(nome, "_despesa_total.Rdata"))
}

### Arquivo remuneração professores de São Paulo 2018

rem_prof_sp_2018 <- fread("~/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)/REMUNERACAO_PROFISSIONAIS_EDUCACAO_SP_2018.CSV")

#Esse arquivo está com problemas de encoding, vou tentar salvar.

rem_prof_2018 <- read_delim("~/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)/REMUNERACAO_PROFISSIONAIS_EDUCACAO_SP_2018.CSV", 
                            ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                            trim_ws = TRUE)
