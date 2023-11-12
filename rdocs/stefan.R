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

banco_deputados <- readxl::read_xlsx("banco/Base_deputados estaduais_1986-2022.xlsx")

banco_deputados <- banco_deputados[,1:16]

#Só Deputados Estaduais
table(banco_deputados$DS_CARGO)
banco_deputados <- banco_deputados %>% filter(DS_CARGO == "DEPUTADO ESTADUAL")


#Porcentagem ou decimal?
#banco_deputados <- banco_deputados %>% mutate(porcentagem_votos = porcentagem_votos/100)

banco_deputados2 <- banco_deputados %>% group_by(ANO_ELEICAO,SG_UF) %>%
  summarise(Oi = sum(ifelse(gov_oppos=="O",porcentagem_votos,0)),
            Oi2 = sum((ifelse(gov_oppos=="O",porcentagem_votos,0))^2),
            O=Oi2/Oi,
            Gi = sum(ifelse(gov_oppos=="G",porcentagem_votos,0)),
            Gi2 = sum((ifelse(gov_oppos=="G",porcentagem_votos,0))^2),
            G=Gi2/Gi,
            IC = 1- abs(O-G)/100) 


#gráficos de linha
#Bivariado n vai rolar com 26 estados


#Vários Univariados
for (i in unique(banco_deputados2$SG_UF)) {
banco_deputados3 <- banco_deputados2 %>% filter(SG_UF == i)
ggplot(banco_deputados3) +
  aes(x = ANO_ELEICAO, y = IC, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ano", y = "IC") +
  theme_estat()
b <- paste("resultados/Stefan/Linhas_",i,".pdf", sep = "")
ggsave(filename = file.path(b), width = 158, height = 93, units = "mm")
}
