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
###### MODELOS J kid gv Temp.Ar LAG 0 ######
#########################################
modelo.kid.j.Temp.Ar.gv <- mgcv::gam(SIH_GV_J_Kid~s(Temp.Ar,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.Temp.Ar.gv)

plot(modelo.kid.j.Temp.Ar.gv, main="modelo.kid.j.Temp.Ar.gv")

residuo.kid.j.Temp.Ar.gv <- residuals(modelo.kid.j.Temp.Ar.gv)

par(mfrow=c(2,2))
plot(residuo.kid.j.Temp.Ar.gv)
acf(residuo.kid.j.Temp.Ar.gv)
qqnorm(residuo.kid.j.Temp.Ar.gv)
qqline(residuo.kid.j.Temp.Ar.gv)
hist(residuo.kid.j.Temp.Ar.gv)
par(mfrow=c(1,1))

############################################################################################################################
##########################################
###### MODELOS i kid  gv Temp.Ar LAG 0 ######
##########################################
modelo.kid.i.Temp.Ar.gv <- mgcv::gam(SIH_GV_I_Kid~s(Temp.Ar,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.Temp.Ar.gv)

plot(modelo.kid.i.Temp.Ar.gv, main="modelo.kid.i.Temp.Ar.gv")

residuo.kid.i.Temp.Ar.gv <- residuals(modelo.kid.i.Temp.Ar.gv)

par(mfrow=c(2,2))
plot(residuo.kid.i.Temp.Ar.gv)
acf(residuo.kid.i.Temp.Ar.gv)
qqnorm(residuo.kid.i.Temp.Ar.gv)
qqline(residuo.kid.i.Temp.Ar.gv)
hist(residuo.kid.i.Temp.Ar.gv)
par(mfrow=c(1,1))

