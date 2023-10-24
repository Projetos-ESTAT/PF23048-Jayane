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

banco <- read_excel("banco/banco_candidatos.xlsx")


banco2 <- banco %>% group_by(ANO_ELEICAO,SG_UF,DS_CARGO) %>% 
  mutate(Ganhou = ifelse(porcentagem_votos==max(porcentagem_votos),"SIM","NÃO"))

#table(banco2$ANO_ELEICAO,banco2$Ganhou, banco2$DS_CARGO)
#table(banco2$ANO_ELEICAO,banco2$SG_UF)

bancoGov <- banco2 %>% filter(DS_CARGO=="GOVERNADOR")
#bancoDep <- banco2 %>% filter(DS_CARGO=="DEPUTADO ESTADUAL")

bancoGov$ANO_ELEICAO <- as.numeric(bancoGov$ANO_ELEICAO)

class(bancoGov$ANO_ELEICAO)

bancoGov2 <- bancoGov %>% group_by(ANO_ELEICAO,SG_UF,SG_PARTIDO) %>%
  mutate(Resultado_anterior = ifelse( when(select(as.numeric(ANO_ELEICAO)-4), Ganhou == "SIM"
                                           ), "Eleito", "Nope"
                                      )
         )

        
 
x <- paste(bancoGov$ANO_ELEICAO,collapse=NULL)        
         
         
         
         
bancoGov <- bancoGov %>% group_by(ANO_ELEICAO,SG_UF,SG_PARTIDO) %>%
  mutate(ANO_ANTERIOR = ANO_ELEICAO-4) %>%
  mutate(Resultado_eleicao_anterior = ifelse((select(ANO_ELEICAO)-4 = ANO_ANTERIOR & Ganhou == "SIM"),"Eleito Anterior", "Nope"))
  

bancoGov2 <- bancoGov %>% group_by(ANO_ELEICAO,SG_UF,SG_PARTIDO) %>%
  mutate(ANO_ANTERIOR = ANO_ELEICAO-4) %>%
  mutate(Bla = ifelse((when(ANO_ANTERIOR %in% ANO_ELEICAO) & Ganhou == "SIM" ),"Eleito Anterior", "Nope"))

table(bancoGov$ANO_ELEICAO)

i <- table(bancoGov$ANO_ELEICAO)

for (x in i){
  if ((x == 2010)){
 break
    }  
  print(x)
}

SG_PARTIDO %in% SG_CONGLOMERADO

bancoGovAC <- bancoGov %>% filter(SG_UF=="AC")
bancoGovAC2 <- bancoGov2 %>% filter(SG_UF=="AC")
