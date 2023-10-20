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

banco <- read.xlsx("banco/banco_candidatos.xlsx", sheetIndex = 1)


banco2 <- banco %>% group_by(ANO_ELEICAO,SG_UF,DS_CARGO) %>% 
  mutate(Ganhou = ifelse(porcentagem_votos==max(porcentagem_votos),"SIM","NÃO"))

#table(banco2$ANO_ELEICAO,banco2$Ganhou, banco2$DS_CARGO)
#table(banco2$ANO_ELEICAO,banco2$SG_UF)

bancoGov <- banco2 %>% filter(DS_CARGO=="GOVERNADOR")
bancoDep <- banco2 %>% filter(DS_CARGO=="DEPUTADO ESTADUAL")


