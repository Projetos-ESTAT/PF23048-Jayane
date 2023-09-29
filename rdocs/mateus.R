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

#esse código foi realizado localmente dada a quantidade de arquivos necessárias para a construção do banco
#Nesse código foi criado o banco de dados dos candidatos ----
#diretórios são locais do meu estágio/casa

setwd('Z:/Mateus - Estagiário/est/Jayane')
setwd("C:/Users/Mateu/OneDrive - unb.br/ESTAT/PF23048")
source("source/packages.R")

pacman::p_load(tidyverse, tibble, infer, readxl,janitor, ggcorrplot, knitr,showtext, kableExtra, data.table, tidyr,SnowballC,
               wordcloud,tm,dplyr, stringr,Hmisc,electionsBR,writexl)

## 1982 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_uf_1982')
setwd("C:/Users/Mateu/OneDrive - unb.br/ESTAT/PF23048/votacao_partido_uf_1982")

estados <- c('AC','AL','AM','AP','BA','CE','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP')
bancos <- list()

for (i in estados) {
  banco <- read.table(paste0("VOTACAO_PARTIDO_UF_1982_",i,".txt"), header = FALSE, sep=";", encoding = 'latin1')
                  
  {
    names(banco) [1] <- "dt"
                    
    names(banco) [2] <- "hr"
                    
    names(banco) [3] <- "ano"
                    
    names(banco) [4] <- "turno"
                    
    names(banco) [5] <- "ELEICAO"
                    
    names(banco) [6] <- "uf"
                    
    names(banco) [7] <- "UE"
                    
    names(banco) [8] <- "codcargo"
                    
    names(banco) [9] <- "CARGO"
                    
    names(banco) [10] <- "legenda"
                    
    names(banco) [11] <- "coligacao"
                  
    names(banco) [12] <- "complegenda"
                    
    names(banco) [13] <- "PARTIDO"
                    
    names(banco) [14] <- "numpart"
                    
    names(banco) [15] <- "NOME_PARTIDO"
                    
    names(banco) [16] <- "VOTOS_NOMINAIS"
                    
    names(banco) [17] <- "VOTOS_LEGENDA"
                    
  }
  banco$CARGO <- toupper(banco$CARGO)
  banco <- subset(banco, CARGO == "DEPUTADO ESTADUAL" | CARGO == "GOVERNADOR",select=c("ELEICAO", "ano", "turno", "uf", "codcargo",
                                                                                       "CARGO","legenda","coligacao","complegenda","PARTIDO",
                                                                                       "NOME_PARTIDO","VOTOS_NOMINAIS","VOTOS_LEGENDA"))
  
  bancos[[i]] <- banco
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
es <- bancos[[7]]
go <- bancos[[8]]
ma <- bancos[[9]]
mg <- bancos[[10]]
ms <- bancos[[11]]
mt <- bancos[[12]]
pa <- bancos[[13]]
pb <- bancos[[14]]
pe <- bancos[[15]]
pi <- bancos[[16]]
pr <- bancos[[17]]
rj <- bancos[[18]]
rn <- bancos[[19]]
ro <- bancos[[20]]
rr <- bancos[[21]]
rs <- bancos[[22]]
sc <- bancos[[23]]
se <- bancos[[24]]
sp <- bancos[[25]]

banco1982 <- rbind(ac,al,am,ap,ba,ce,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp)

write_excel_csv2(banco1982,"Z:/Mateus - Estagiário/est/Jayane/eleicoes1982.csv")

# 1986 ----
setwd("C:/Users/Mateu/OneDrive - unb.br/ESTAT/PF23048/votacao_partido_uf_1986")
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_uf_1986')

estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP')
bancos <- list()

for (i in estados) {
  banco <- read.table(paste0("VOTACAO_PARTIDO_UF_1986_",i,".txt"), header = FALSE, sep=";", encoding = 'latin1')
  
  {
    names(banco) [1] <- "dt"
    
    names(banco) [2] <- "hr"
    
    names(banco) [3] <- "ano"
    
    names(banco) [4] <- "turno"
    
    names(banco) [5] <- "ELEICAO"
    
    names(banco) [6] <- "uf"
    
    names(banco) [7] <- "UE"
    
    names(banco) [8] <- "codcargo"
    
    names(banco) [9] <- "CARGO"
    
    names(banco) [10] <- "legenda"
    
    names(banco) [11] <- "coligacao"
    
    names(banco) [12] <- "complegenda"
    
    names(banco) [13] <- "PARTIDO"
    
    names(banco) [14] <- "numpart"
    
    names(banco) [15] <- "NOME_PARTIDO"
    
    names(banco) [16] <- "VOTOS_NOMINAIS"
    
    names(banco) [17] <- "VOTOS_LEGENDA"
    
  }
  banco$CARGO <- toupper(banco$CARGO)
  banco <- subset(banco, CARGO == "DEPUTADO ESTADUAL" | CARGO == "GOVERNADOR",select=c("ELEICAO", "ano", "turno", "uf", "codcargo",
                                                                                       "CARGO","legenda","coligacao","complegenda","PARTIDO",
                                                                                       "NOME_PARTIDO","VOTOS_NOMINAIS","VOTOS_LEGENDA"))
  
  bancos[[i]] <- banco
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]

banco1986 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp)


#1990 ----
setwd("C:/Users/Mateu/OneDrive - unb.br/ESTAT/PF23048/votacao_partido_uf_1990")
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_uf_1990')

estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP')
bancos <- list()
#test <- read.table(paste0("VOTACAO_PARTIDO_UF_1990_AC.txt"), header = FALSE, sep=";")
for (i in estados) {
  banco <- read.table(paste0("VOTACAO_PARTIDO_UF_1990_",i,".txt"), header = FALSE, sep=";",encoding = 'latin1')
  
  {
    names(banco) [1] <- "dt"
    
    names(banco) [2] <- "hr"
    
    names(banco) [3] <- "ano"
    
    names(banco) [4] <- "turno"
    
    names(banco) [5] <- "ELEICAO"
    
    names(banco) [6] <- "uf"
    
    names(banco) [7] <- "UE"
    
    names(banco) [8] <- "codcargo"
    
    names(banco) [9] <- "CARGO"
    
    names(banco) [10] <- "legenda"
    
    names(banco) [11] <- "coligacao"
    
    names(banco) [12] <- "complegenda"
    
    names(banco) [13] <- "PARTIDO"
    
    names(banco) [14] <- "numpart"
    
    names(banco) [15] <- "NOME_PARTIDO"
    
    names(banco) [16] <- "VOTOS_NOMINAIS"
    
    names(banco) [17] <- "VOTOS_LEGENDA"
    
  }
  banco$CARGO <- toupper(banco$CARGO)
  banco <- subset(banco, CARGO == "DEPUTADO ESTADUAL" | CARGO == "GOVERNADOR",select=c("ELEICAO", "ano", "turno", "uf", "codcargo",
                                                                                       "CARGO","legenda","coligacao","complegenda","PARTIDO",
                                                                                       "NOME_PARTIDO","VOTOS_NOMINAIS","VOTOS_LEGENDA"))
  
  bancos[[i]] <- banco
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]

banco1990 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp)


banco1982_90 <- rbind(banco1982,banco1986,banco1990)

#1994 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_1994')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_1994_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                     "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                     "QT_VOTOS_NOMINAIS_VALIDOS","QT_TOTAL_VOTOS_LEG_VALIDOS"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco1994 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco1994 <- banco1994 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS_VALIDOS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_TOTAL_VOTOS_LEG_VALIDOS)) %>% ungroup()

banco1994 <- banco1994 %>% rename("QT_VOTOS_NOMINAIS" = "QT_VOTOS_NOMINAIS_VALIDOS","QT_VOTOS_LEGENDA" = "QT_TOTAL_VOTOS_LEG_VALIDOS" ) 
#1998 ----

setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_1998')
estados <- c('AC','AL','AM','AP','BA','CE','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_1998_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS_VALIDOS","QT_TOTAL_VOTOS_LEG_VALIDOS"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
es <- bancos[[7]]
go <- bancos[[8]]
ma <- bancos[[9]]
mg <- bancos[[10]]
ms <- bancos[[11]]
mt <- bancos[[12]]
pa <- bancos[[13]]
pb <- bancos[[14]]
pe <- bancos[[15]]
pi <- bancos[[16]]
pr <- bancos[[17]]
rj <- bancos[[18]]
rn <- bancos[[19]]
ro <- bancos[[20]]
rr <- bancos[[21]]
rs <- bancos[[22]]
sc <- bancos[[23]]
se <- bancos[[24]]
sp <- bancos[[25]]
to <- bancos[[26]]

banco1998 <- rbind(ac,al,am,ap,ba,ce,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco1998 <- banco1998 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS_VALIDOS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_TOTAL_VOTOS_LEG_VALIDOS)) %>% ungroup()

banco1998 <- banco1998 %>% rename("QT_VOTOS_NOMINAIS" = "QT_VOTOS_NOMINAIS_VALIDOS","QT_VOTOS_LEGENDA" = "QT_TOTAL_VOTOS_LEG_VALIDOS" ) 

#2002 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_2002')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_2002_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS","QT_VOTOS_LEGENDA"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco2002 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco2002 <- banco2002 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_VOTOS_LEGENDA)) %>% ungroup()
# 2006 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_2006')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_2006_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS","QT_VOTOS_LEGENDA"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco2006 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco2006 <- banco2006 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_VOTOS_LEGENDA)) %>% ungroup()
#2010 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_2010')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_2010_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS","QT_VOTOS_LEGENDA"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco2010 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco2010 <- banco2010 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_VOTOS_LEGENDA)) %>% ungroup()
# 2014 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_2014')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_2014_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS","QT_VOTOS_LEGENDA"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco2014 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco2014 <- banco2014 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_VOTOS_LEGENDA)) %>% ungroup()
#2018 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_2018')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_2018_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS_VALIDOS","QT_VOTOS_LEGENDA_VALIDOS"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco2018 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco2018 <- banco2018 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS_VALIDOS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_VOTOS_LEGENDA_VALIDOS)) %>% ungroup()
banco2018 <- banco2018 %>% rename("QT_VOTOS_NOMINAIS"="QT_VOTOS_NOMINAIS_VALIDOS","QT_VOTOS_LEGENDA"="QT_VOTOS_LEGENDA_VALIDOS")
# 2022 ----
setwd('Z:/Mateus - Estagiário/est/Jayane/votacao_partido_munzona_2022')
estados <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
bancos <- list()
for (i in estados) {
  banco <- read.csv(paste0("votacao_partido_munzona_2022_",i,".csv"),  sep = ';', encoding = 'latin1')
  banco$DS_CARGO <- toupper(banco$DS_CARGO)
  banco <- subset(banco, DS_CARGO == "DEPUTADO ESTADUAL" | DS_CARGO == "GOVERNADOR",select= c("ANO_ELEICAO","NR_TURNO","SG_UF","NM_MUNICIPIO","CD_CARGO","DS_CARGO","TP_AGREMIACAO",
                                                                                              "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
                                                                                              "QT_VOTOS_NOMINAIS_VALIDOS","QT_VOTOS_LEGENDA_VALIDOS"))
  bancos[[i]] <- banco
  #bancos <- rbindlist(list(bancos,banco))
}

ac <- bancos[[1]]
al <- bancos[[2]]
am <- bancos[[3]]
ap <- bancos[[4]]
ba <- bancos[[5]]
ce <- bancos[[6]]
df <- bancos[[7]]
es <- bancos[[8]]
go <- bancos[[9]]
ma <- bancos[[10]]
mg <- bancos[[11]]
ms <- bancos[[12]]
mt <- bancos[[13]]
pa <- bancos[[14]]
pb <- bancos[[15]]
pe <- bancos[[16]]
pi <- bancos[[17]]
pr <- bancos[[18]]
rj <- bancos[[19]]
rn <- bancos[[20]]
ro <- bancos[[21]]
rr <- bancos[[22]]
rs <- bancos[[23]]
sc <- bancos[[24]]
se <- bancos[[25]]
sp <- bancos[[26]]
to <- bancos[[27]]

banco2022 <- rbind(ac,al,am,ap,ba,ce,df,es,go,ma,mg,ms,mt,pa,pb,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)

banco2022 <- banco2022 %>%
  group_by(SG_UF,SG_PARTIDO) %>%
  mutate(VOTOS_NOMINAIS_ESTADO = sum(QT_VOTOS_NOMINAIS_VALIDOS),
         VOTOS_LEGENDA_ESTAD0 = sum(QT_VOTOS_LEGENDA_VALIDOS)) %>% ungroup()

banco2022 <- banco2022 %>% rename("QT_VOTOS_NOMINAIS"="QT_VOTOS_NOMINAIS_VALIDOS","QT_VOTOS_LEGENDA"="QT_VOTOS_LEGENDA_VALIDOS")

banco1994_2022 <- rbind(banco1994,banco1998,banco2002,banco2006,banco2006,banco2010,banco2014,banco2018,banco2022)
banco1994_2022 <- banco1994_2022[!duplicated(banco1994_2022[c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "SG_PARTIDO")]), ]

banco1994_2022 <- banco1994_2022 %>% select(-NR_PARTIDO,-QT_VOTOS_LEGENDA,-QT_VOTOS_NOMINAIS,-NM_MUNICIPIO)
banco1982_90 <- banco1982_90 %>% select(-ELEICAO,ano,turno,uf,codcargo,CARGO,legenda,PARTIDO,NOME_PARTIDO,coligacao,complegenda,VOTOS_NOMINAIS,VOTOS_LEGENDA)
colnames(banco1982_90) <- c("ANO_ELEICAO","NR_TURNO","SG_UF","CD_CARGO","DS_CARGO","TP_AGREMIACAO","SG_PARTIDO",
                            "NM_PARTIDO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO","VOTOS_NOMINAIS_ESTADO","VOTOS_LEGENDA_ESTADO")
colnames(banco1994_2022) <- colnames(banco1982_90)
candidatos <- rbind(banco1982_90,banco1994_2022)

candidatos$ANO_ELEICAO <- as.factor(candidatos$ANO_ELEICAO)
candidatos <- candidatos %>% mutate(votos_totais = VOTOS_NOMINAIS_ESTADO + VOTOS_LEGENDA_ESTADO)

adicionar_porcentagem_votos <- function(dataframe, coluna_votos, coluna_estado, coluna_ano, coluna_cargo) {
  dataframe <- dataframe %>%
    group_by({{coluna_estado}}, {{coluna_ano}}, {{coluna_cargo}}) %>%
    mutate(
      porcentagem_votos = {{coluna_votos}} / sum({{coluna_votos}})
    ) %>%
    ungroup()
  
  return(dataframe)
}

candidatos <- adicionar_porcentagem_votos(candidatos,votos_totais,SG_UF,ANO_ELEICAO,CD_CARGO)

write_csv2(banco1994_2022,"Z:/Mateus - Estagiário/est/Jayane/eleicoes1994_2022.csv")

writexl::write_xlsx(candidatos,"Z:/Mateus - Estagiário/est/Jayane/banco_candidatos.xlsx")
remove(ac,al,am,ap,ba,banco,banco1994,banco1998,banco2002,banco2006,banco2010,banco2014,banco2018,banco2022,ce,df,es,go,ma,mg,ms,mt,pa,pe,pi,pr,rj,rn,ro,rr,rs,sc,se,sp,to)



 

  



