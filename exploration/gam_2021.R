#################################
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
#########################################
###### MODELOS J old gv pm10 LAG 0 ######
#########################################
modelo.old.j.gv <- mgcv::gam(SIH_GV_J_Old~s(pm10.gv,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)
summary(modelo.old.j.gv)

plot(modelo.old.j.gv, main="modelo.old.j.gv")

residuo.old.j.gv <- residuals(modelo.old.j.gv)

par(mfrow=c(2,2))
plot(residuo.old.j.gv)
acf(residuo.old.j.gv)
qqnorm(residuo.old.j.gv)
qqline(residuo.old.j.gv)
hist(residuo.old.j.gv)
dev.off()
###############################
###### MODELOS i old gv ######
###############################
modelo.old.i.gv <- mgcv::gam(SIH_GV_I_Old~s(pm10.gv,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)
summary(modelo.old.i.gv)

plot(modelo.old.i.gv, main="modelo.old.i.gv")

residuo.old.i.gv <- residuals(modelo.old.i.gv)

par(mfrow=c(2,2))
plot(residuo.old.i.gv)
acf(residuo.old.i.gv)
qqnorm(residuo.old.i.gv)
qqline(residuo.old.i.gv)
hist(residuo.old.i.gv)
dev.off()

###############################
###### MODELOS J old sul ######
###############################
modelo.old.j.sul <- mgcv::gam(SIH_Sul_J_Old~s(pm10.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)
summary(modelo.old.j.sul)

plot(modelo.old.j.sul, main="modelo.old.j.sul")

residuo.old.j.sul <- residuals(modelo.old.j.sul)

par(mfrow=c(2,2))
plot(residuo.old.j.sul)
acf(residuo.old.j.sul)
qqnorm(residuo.old.j.sul)
qqline(residuo.old.j.sul)
hist(residuo.old.j.sul)
dev.off()
###############################
###### MODELOS i old sul ######
###############################
modelo.old.i.sul <- mgcv::gam(SIH_Sul_I_Old~s(pm10.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)
summary(modelo.old.i.sul)

plot(modelo.old.i.sul, main="modelo.old.i.sul")

residuo.old.i.sul <- residuals(modelo.old.i.sul)

par(mfrow=c(2,2))
plot(residuo.old.i.sul)
acf(residuo.old.i.sul)
qqnorm(residuo.old.i.sul)
qqline(residuo.old.i.sul)
hist(residuo.old.i.sul)
dev.off()



