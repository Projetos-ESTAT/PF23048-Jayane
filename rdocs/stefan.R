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

pacman::p_load(ggcorrplot, knitr,showtext, kableExtra, data.table, tidyr,SnowballC,
               wordcloud,tm,stringr,gridExtra)

banco_deputados <- readxl::read_xlsx("banco/Base_deputados estaduais_1986-2022.xlsx")

banco_deputados <- banco_deputados[,1:16]


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

write.csv2(banco_deputados2, "IC_deputados.xlsx")


#gráficos de linha
#Bivariado n vai rolar com 26 estados


#Vários Univariados
# for (i in unique(banco_deputados2$SG_UF)) {
# banco_deputados3 <- banco_deputados2 %>% filter(SG_UF == i)
# a <- ggplot(banco_deputados3) +
#   aes(x = ANO_ELEICAO, y = IC, group = 1) +
#   geom_line(size = 1, colour = "#A11D21") +
#   geom_point(colour = "#A11D21", size = 2) +
#   labs(x = "Ano", y = "IC") +
#   theme_estat() +
#   ylim(floor(20*min(banco_deputados3$IC))/20,1)
# b <- paste("resultados/Stefan/Linhas_",i,".jpeg", sep = "")
# ggsave(filename = file.path(b), plot = a, width = 158, height = 93, units = "mm")
# }

graph_list <- list()

banco_deputados2$SG_UF <- as.factor(banco_deputados2$SG_UF)

levels(banco_deputados2$SG_UF) <- list("ACRE" ="AC" ,"ALAGOAS"="AL","AMAZONAS"="AM","AMAPÁ"="AP","BAHIA"="BA","CEARÁ"="CE","DISTRITO FEDERAL"="DF","ESPÍRITO SANTO"="ES","GOIÁS"="GO",
                                       "MARANHÃO"="MA","MINAS GERAIS"="MG","MATO GROSSO DO SUL"="MS","MATO GROSSO"="MT","PARÁ"="PA","PARAÍBA"="PB","PIAUÍ"="PI","PARANÁ"="PR",
                                       "RIO DE JANEIRO"="RJ","RIO GRANDE DO NORTE"="RN","RONDÔNIA"="RO","RORAIMA"="RR","RIO GRANDE DO SUL"="RS","SANTA CATARINA"="SC","SERGIPE"="SE",
                                       "SÃO PAULO"="SP","TOCANTINS"="TO")

for (i in unique(banco_deputados2$SG_UF)) {
  banco_deputados3 <- banco_deputados2 %>% filter(SG_UF == i)
  a <- ggplot(banco_deputados3) +
    aes(x = ANO_ELEICAO, y = IC, group = 1) +
    geom_line(size = 1, colour = "#A11D21") +
    geom_point(colour = "#A11D21", size = 2) +
    labs(x = " Election Year", y = "Competitiveness") +
    theme_estat(axis.text.x = element_text(size = 15,angle = 75, vjust = .65),axis.text.y = element_text(size = 15)) +
    ylim(0,1) + ggtitle(i)
  graph_list[[i]] <- a
  b <- paste("resultados/Stefan/Linhas_",i,".jpeg", sep = "")
  ggsave(filename = file.path(b), plot = a, width = 158, height = 93, units = "mm")
}

grid_norte <- grid.arrange(graph_list[[1]],graph_list[[25]],graph_list[[3]],graph_list[[12]],graph_list[[19]],graph_list[[27]],graph_list[[24]], ncol = 4)

ggsave("resultados/Matilda/Linhas_Norte.jpeg", plot = grid_norte)

grid_sul <- grid.arrange(graph_list[[16]],graph_list[[21]],graph_list[[20]], ncol = 2)

ggsave("resultados/Matilda/Linhas_Sul.jpeg", plot = grid_sul)

grid_co <- grid.arrange(graph_list[[7]],graph_list[[11]],graph_list[[10]],graph_list[[26]], ncol = 2)

ggsave("resultados/Matilda/Linhas_CO.jpeg", plot = grid_co)

grid_sud <- grid.arrange(graph_list[[6]],graph_list[[9]],graph_list[[17]],graph_list[[23]], ncol = 2)

ggsave("resultados/Matilda/Linhas_Sudeste.jpeg", plot = grid_sud)

grid_nord <- grid.arrange(graph_list[[2]],graph_list[[4]],graph_list[[5]],graph_list[[8]],graph_list[[13]], graph_list[[14]],
                          graph_list[[15]],graph_list[[18]],graph_list[[22]], ncol = 3)

ggsave("resultados/Matilda/Linhas_Nordeste.jpeg", plot = grid_nord)
