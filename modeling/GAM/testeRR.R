######################################################
#####carregando as planilhas##########################
######################################################
library(readxl)

library(dplyr)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

######################################################
##### separando os dados RGC e SUL ###################
######################################################

dados_rgv <- data.frame(dados_pol[,c(6:9,10:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99)], meteo[1:1462, c(9,12)])

dados_sul <- data.frame(dados_pol[,c(2:5,16:21,28:33,40:45,52:57,64:69,76:81,88:93,100:105)], meteo[1:1462, c(9,12)])


######################################################
##### Retirando ozonio dos dados   ###################
######################################################

dados_GV <-data.frame(dados_rgv[,-c(10,16,22,28,34,40,46,52)])

library("gam")

library("gamRR")

library("mgcv")

dat <-data.frame(dados_GV[,c(4:9)])

attach(dat)

fit <- gam(SIH_GV_J_Kid~pm25.gv+pm10.gv+so2.gv+no2.gv+co.gv,family=poisson,dat,na.action = na.gam.replace,method="REML")


summary(fit)


gamRR(fit=fit,c(pm25.gv=dat$pm25.gv[1], pm10.gv=dat$pm10.gv[1], so2.gv=dat$so2.gv[1], no2.gv=dat$no2.gv[1], co.gv=dat$co.gv[1]), est="pm25.gv", data=dat, n.points=1462, plot=TRUE, ylim=NULL)
