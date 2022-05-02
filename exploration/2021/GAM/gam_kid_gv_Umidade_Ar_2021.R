############################################################################################################################
###### IMPORTANDO OS DADOS ######
#################################
library(readxl)
library(dplyr)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

list.files()

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

attach(dados)
###############################
###### MODELOS COM O GAM ######
###############################
library("mgcv")
library("gam")
###############################################################################################################################
#########################################
###### MODELOS J kid gv Umidade.Ar LAG 0 ######
#########################################
modelo.kid.j.Umidade.Ar.gv <- mgcv::gam(SIH_GV_J_Kid~s(Umidade.Ar,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.Umidade.Ar.gv)

plot(modelo.kid.j.Umidade.Ar.gv, main="modelo.kid.j.Umidade.Ar.gv")

residuo.kid.j.Umidade.Ar.gv <- residuals(modelo.kid.j.Umidade.Ar.gv)

par(mfrow=c(2,2))
plot(residuo.kid.j.Umidade.Ar.gv)
acf(residuo.kid.j.Umidade.Ar.gv)
qqnorm(residuo.kid.j.Umidade.Ar.gv)
qqline(residuo.kid.j.Umidade.Ar.gv)
hist(residuo.kid.j.Umidade.Ar.gv)
par(mfrow=c(1,1))

############################################################################################################################
##########################################
###### MODELOS i kid  gv Umidade.Ar LAG 0 ######
##########################################
modelo.kid.i.Umidade.Ar.gv <- mgcv::gam(SIH_GV_I_Kid~s(Umidade.Ar,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.Umidade.Ar.gv)

plot(modelo.kid.i.Umidade.Ar.gv, main="modelo.kid.i.Umidade.Ar.gv")

residuo.kid.i.Umidade.Ar.gv <- residuals(modelo.kid.i.Umidade.Ar.gv)

par(mfrow=c(2,2))
plot(residuo.kid.i.Umidade.Ar.gv)
acf(residuo.kid.i.Umidade.Ar.gv)
qqnorm(residuo.kid.i.Umidade.Ar.gv)
qqline(residuo.kid.i.Umidade.Ar.gv)
hist(residuo.kid.i.Umidade.Ar.gv)
par(mfrow=c(1,1))
