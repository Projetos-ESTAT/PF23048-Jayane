source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

##### ORGANIZANDO O BANCO DO MATILDA #####

# setwd('D:/Downloads/1994-2022')
# library(readxl)
# 
# dados<- read_excel('eleicoes1994_2022.xlsx', sheet = 1)
# dados_unicos1 <- dados[!duplicated(dados[c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "SG_PARTIDO")]), ]
# dados_unicos1 <- subset(dados_unicos1, select = -NM_MUNICIPIO)
# 
# 
# dados2<- read_excel('eleicoes1994_2022.xlsx', sheet = 2)
# dados_unicos2 <- dados2[!duplicated(dados2[c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "SG_PARTIDO")]), ]
# dados_unicos2 <- subset(dados_unicos2, select = -NM_MUNICIPIO)
# 
# dados_combinados <- rbind(dados_unicos1, dados_unicos2)
# 
# library(openxlsx)
# 
# # Nome do arquivo de saída
# arquivo_saida <- "resultados.xlsx"
# 
# write.xlsx(dados_combinados, file = arquivo_saida, sheetName = "1994-2022")


setwd('D:/Downloads/ESTAT')
library(readxl)
library(tidyverse)
library(xlsx)  
banco <- read_excel('base_distritais.xlsx')

resultados<-banco%>%
  filter(str_detect(DS_CARGO, "DEPUTADO DISTRITAL"))

arquivo_saida <- "banco_distritais.xlsx"
write.xlsx(resultados, file = arquivo_saida)
