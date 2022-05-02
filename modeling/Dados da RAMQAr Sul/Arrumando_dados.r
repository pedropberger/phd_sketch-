##############################################################################
################# IMPORTANTO OS DADOS DE METEOROLOGICOS #######################
##############################################################################
#############################################################################
library(readxl)
library(dplyr)

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

list.files()


meteo_aeroporto_15a19 <- read_excel("meteo_aeroporto_15a19.xlsx") 

dados.data <- meteo_aeroporto_15a19[,1:4]

meteo_aeroporto_15a19 <- group_by(meteo_aeroporto_15a19, ano,mês,dia) %>% summarise(
                                                Direca.Vento = mean(Direcao.Vento),
                                                Veloc.Vento = mean(Veloc.Vento),
                                                Precip.acumulada = mean(Precip.acumulada),
                                                Ponto.Orvalho = mean(Ponto.Orvalho),
                                                Temp.Ar = mean(Temp.Ar),
                                                Temp.minima = max(Temp.minima),
                                                Temp.maxima = max(Temp.maxima),
                                                Umidade.Ar = mean(Umidade.Ar))

##############################################################################
################# EXPORTANTO OS DADOS METEOROLOGICOS ## ############################
#############################################################################
library(writexl)

write_xlsx(meteo_aeroporto_15a19, path = "dados.meteo.aeroporto.15a19.xlsx")

##############################################################################
################# IMPORTANTO OS DADOS DE SAUDE ############################
##############################################################################
library(readxl)
library(dplyr)
dados.saude <- read_excel("C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_SUL.dados.saude.filipe.xlsx")

dados.saude <- group_by(dados.saude,CID_focado, ï..Data_inter) %>% summarise(sum(Contagem_de_Inter))

write.table(dados.saude, "dados.saude.filipe.csv", sep  = ",")

dados.saude <- dados.saude[39:1774,]

summary(dados.saude$`sum(PACIENTE)`)

plot.ts(dados.saude$`sum(PACIENTE)`)