#Latitude e longitude das escolas:

library(readr)
library(dplyr)
library(ggmap)
library(data.table)

gkey <- "AIzaSyCtIIoMer4QzSmlqKiapmdWSzt57j04KCY"
register_google(key = gkey)

tabela_endereco_escolas <- read_csv("~/R-Projects/qualidade_gastos_educação/analise_gastos_siope/tabela_escolas_sp/tabela_endereco_escolas.csv", 
                                    col_types = cols(cep = col_character(), 
                                                     codigo_escola = col_character(), 
                                                     codigo_mec = col_character()))

tabela_end_escolas_completa <- tabela_endereco_escolas 

#retirando NAs
tabela_end_escolas_completa[is.na(tabela_end_escolas_completa)] <- ""

tabela_end_escolas_completa <- tabela_end_escolas_completa %>%
  filter(endereco != "") %>%
  mutate(end2 = paste(tolower(endereco), tolower(numero), tolower(bairro), 
                      ", SP, Brasil", sep=" ")) %>%
  select(-c(esc, no_municipio, nome_escola_secretaria)) 

ends <- tabela_end_escolas_completa$end2

df <- data.frame()

for(i in 1:length(ends)){
  
  print(i)
  
  x <- geocode(ends[i])
  
  x <- x %>%
    mutate(endereco = ends[i])
  
  df <- bind_rows(df, x)
}

setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/analise_gastos_siope/tabela_escolas_sp")
save(df, file="df.Rdata")  
load("df.Rdata")

tabela_end_escolas_completa <- tabela_end_escolas_completa %>%
  left_join(df, by=c('end2' = 'endereco'))

fwrite(tabela_end_escolas_completa, file="tabela_end_escolas_completa.csv")
