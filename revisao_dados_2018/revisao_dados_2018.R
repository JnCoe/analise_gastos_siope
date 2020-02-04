# Análise do SIOPE:
# Verificando quais arquivos de 2018 são de 2019...

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
  
  destino = paste0("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)/remuneracao_2018/", uf, "_2018.csv")
  
  
  download.file(url, destfile = destino)
  
  x <- fread(destino)
  
  ano_exercicio <- paste(unique(x$AN_EXERCICIO),collapse=" ")
  
  y <- data_frame(uf, ano_exercicio)
  
  arquivos <- rbind(arquivos, y)
}

arquivos <- arquivos %>%
  filter(!is.na(ano_exercicio))

setwd("C:/Users/coliv/Documents/R-Projects/qualidade_gastos_educação/arquivos grandes (fora do git)/remuneracao_2018")
save(arquivos, file="arquivos.Rdata")
