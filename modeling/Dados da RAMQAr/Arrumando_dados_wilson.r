##############################################################################
################# IMPORTANTO OS DADOS DE METEOROLOGICOS #######################
##############################################################################
library(readxl)
library(dplyr)
meteo_aeroporto_15a19 <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/meteo_aeroporto_15a19.xlsx") 

meteo_aeroporto_15a19 <- group_by(meteo_aeroporto_15a19, ano,mês,dia) %>% summarise(mean(Direcao.Vento),
                                                mean(Veloc.Vento),
                                                mean(Precip.acumulada),
                                                mean(Temp.Ar),
                                                mean(Ponto.Orvalho),
                                                max(Temp.minima),
                                                max(Temp.maxima),
                                                mean(Umidade.Ar))
write.table(meteo_aeroporto_15a19, "meteoro_media.csv", sep  = ",")

##############################################################################
################# IMPORTANTO OS DADOS DE SAUDE ############################
##############################################################################
library(readxl)
library(readr)
dados.saude<- Wilson_SIH_GV_IJ_Kid <- read_csv("C:/Users/fisic/Desktop/PPGEA/2020_1/gam/filtrados/Wilson_SIH_GV_IJ_Kid.csv")

#dados.saude <- group_by(dados.saude,CID_focado, ï..Data_inter) %>% summarise(sum(Contagem_de_Inter))

#write.table(dados.saude, "dados.saude.filipe.csv", sep  = ",")

#dados.saude <- dados.saude[39:1774,]

summary(dados.saude)

#plot.ts(dados.saude$`sum(PACIENTE)`)
