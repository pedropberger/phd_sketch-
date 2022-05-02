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
###### MODELOS J old gv Temp.Ar LAG 0 ######
#########################################
modelo.old.j.Temp.Ar.gv <- mgcv::gam(SIH_GV_J_Old~s(Temp.Ar,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.Temp.Ar.gv)

plot(modelo.old.j.Temp.Ar.gv, main="modelo.old.j.Temp.Ar.gv")

residuo.old.j.Temp.Ar.gv <- residuals(modelo.old.j.Temp.Ar.gv)

par(mfrow=c(2,2))
plot(residuo.old.j.Temp.Ar.gv)
acf(residuo.old.j.Temp.Ar.gv)
qqnorm(residuo.old.j.Temp.Ar.gv)
qqline(residuo.old.j.Temp.Ar.gv)
hist(residuo.old.j.Temp.Ar.gv)
par(mfrow=c(1,1))

############################################################################################################################
##########################################
###### MODELOS i old  gv Temp.Ar LAG 0 ######
##########################################
modelo.old.i.Temp.Ar.gv <- mgcv::gam(SIH_GV_I_Old~s(Temp.Ar,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.Temp.Ar.gv)

plot(modelo.old.i.Temp.Ar.gv, main="modelo.old.i.Temp.Ar.gv")

residuo.old.i.Temp.Ar.gv <- residuals(modelo.old.i.Temp.Ar.gv)

par(mfrow=c(2,2))
plot(residuo.old.i.Temp.Ar.gv)
acf(residuo.old.i.Temp.Ar.gv)
qqnorm(residuo.old.i.Temp.Ar.gv)
qqline(residuo.old.i.Temp.Ar.gv)
hist(residuo.old.i.Temp.Ar.gv)
par(mfrow=c(1,1))
