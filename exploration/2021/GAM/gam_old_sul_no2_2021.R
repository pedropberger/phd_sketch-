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
###### MODELOS J old sul no2 LAG 0 ######
#########################################
modelo.old.j.no2.sul <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul)

plot(modelo.old.j.no2.sul, main="modelo.old.j.no2.sul")

residuo.old.j.no2.sul <- residuals(modelo.old.j.no2.sul)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul)
acf(residuo.old.j.no2.sul)
qqnorm(residuo.old.j.no2.sul);qqline(residuo.old.j.no2.sul)
hist(residuo.old.j.no2.sul)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 1 ######
#########################################
modelo.old.j.no2.sul.d.1 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.1,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.1)

plot(modelo.old.j.no2.sul.d.1, main="modelo.old.j.no2.sul.d.1")

residuo.old.j.no2.sul.d.1 <- residuals(modelo.old.j.no2.sul.d.1)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.1)
acf(residuo.old.j.no2.sul.d.1)
qqnorm(residuo.old.j.no2.sul.d.1);qqline(residuo.old.j.no2.sul.d.1)
hist(residuo.old.j.no2.sul.d.1)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 2 ######
#########################################
modelo.old.j.no2.sul.d.2 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.2,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.2)

plot(modelo.old.j.no2.sul.d.2, main="modelo.old.j.no2.sul.d.2")

residuo.old.j.no2.sul.d.2 <- residuals(modelo.old.j.no2.sul.d.2)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.2)
acf(residuo.old.j.no2.sul.d.2)
qqnorm(residuo.old.j.no2.sul.d.2);qqline(residuo.old.j.no2.sul.d.2)
hist(residuo.old.j.no2.sul.d.2)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 3 ######
#########################################
modelo.old.j.no2.sul.d.3 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.3,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.3)

plot(modelo.old.j.no2.sul.d.3, main="modelo.old.j.no2.sul.d.3")

residuo.old.j.no2.sul.d.3 <- residuals(modelo.old.j.no2.sul.d.3)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.3)
acf(residuo.old.j.no2.sul.d.3)
qqnorm(residuo.old.j.no2.sul.d.3);qqline(residuo.old.j.no2.sul.d.3)
hist(residuo.old.j.no2.sul.d.3)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 4 ######
#########################################
modelo.old.j.no2.sul.d.4 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.4,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.4)

plot(modelo.old.j.no2.sul.d.4, main="modelo.old.j.no2.sul.d.4")

residuo.old.j.no2.sul.d.4 <- residuals(modelo.old.j.no2.sul.d.4)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.4)
acf(residuo.old.j.no2.sul.d.4)
qqnorm(residuo.old.j.no2.sul.d.4);qqline(residuo.old.j.no2.sul.d.4)
hist(residuo.old.j.no2.sul.d.4)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 5 ######
#########################################
modelo.old.j.no2.sul.d.5 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.5,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.5)

plot(modelo.old.j.no2.sul.d.5, main="modelo.old.j.no2.sul.d.5")

residuo.old.j.no2.sul.d.5 <- residuals(modelo.old.j.no2.sul.d.5)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.5)
acf(residuo.old.j.no2.sul.d.5)
qqnorm(residuo.old.j.no2.sul.d.5);qqline(residuo.old.j.no2.sul.d.5)
hist(residuo.old.j.no2.sul.d.5)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 6 ######
#########################################
modelo.old.j.no2.sul.d.6 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.6,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.6)

plot(modelo.old.j.no2.sul.d.6, main="modelo.old.j.no2.sul.d.6")

residuo.old.j.no2.sul.d.6 <- residuals(modelo.old.j.no2.sul.d.6)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.6)
acf(residuo.old.j.no2.sul.d.6)
qqnorm(residuo.old.j.no2.sul.d.6);qqline(residuo.old.j.no2.sul.d.6)
hist(residuo.old.j.no2.sul.d.6)
par(mfrow=c(1,1))
#########################################
###### MODELOS J old Sul no2 LAG 7######
#########################################
modelo.old.j.no2.sul.d.7 <- mgcv::gam(SIH_Sul_J_Old~s(no2.sul.d.7,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.j.no2.sul.d.7)

plot(modelo.old.j.no2.sul.d.7, main="modelo.old.j.no2.sul.d.7")

residuo.old.j.no2.sul.d.7 <- residuals(modelo.old.j.no2.sul.d.7)

par(mfrow=c(2,2))
plot(residuo.old.j.no2.sul.d.7)
acf(residuo.old.j.no2.sul.d.7)
qqnorm(residuo.old.j.no2.sul.d.7);qqline(residuo.old.j.no2.sul.d.7)
hist(residuo.old.j.no2.sul.d.7)
par(mfrow=c(1,1))
########comparando modelos com lags diferentes##########
################ crianças CID J ########################
#################### no2 ##############################

summary(modelo.old.j.no2.sul)$edf;summary(modelo.old.j.no2.sul)$s.table;summary(modelo.old.j.no2.sul)$r.sq;summary(modelo.old.j.no2.sul)$sp.criterion;
summary(modelo.old.j.no2.sul.d.1)$edf;summary(modelo.old.j.no2.sul.d.1)$s.table;summary(modelo.old.j.no2.sul.d.1)$r.sq;summary(modelo.old.j.no2.sul.d.1)$sp.criterion;
summary(modelo.old.j.no2.sul.d.2)$edf;summary(modelo.old.j.no2.sul.d.2)$s.table;summary(modelo.old.j.no2.sul.d.2)$r.sq;summary(modelo.old.j.no2.sul.d.2)$sp.criterion;
summary(modelo.old.j.no2.sul.d.3)$edf;summary(modelo.old.j.no2.sul.d.3)$s.table;summary(modelo.old.j.no2.sul.d.3)$r.sq;summary(modelo.old.j.no2.sul.d.3)$sp.criterion;
summary(modelo.old.j.no2.sul.d.4)$edf;summary(modelo.old.j.no2.sul.d.4)$s.table;summary(modelo.old.j.no2.sul.d.4)$r.sq;summary(modelo.old.j.no2.sul.d.4)$sp.criterion;
summary(modelo.old.j.no2.sul.d.5)$edf;summary(modelo.old.j.no2.sul.d.5)$s.table;summary(modelo.old.j.no2.sul.d.5)$r.sq;summary(modelo.old.j.no2.sul.d.5)$sp.criterion;
summary(modelo.old.j.no2.sul.d.6)$edf;summary(modelo.old.j.no2.sul.d.6)$s.table;summary(modelo.old.j.no2.sul.d.6)$r.sq;summary(modelo.old.j.no2.sul.d.6)$sp.criterion;
summary(modelo.old.j.no2.sul.d.7)$edf;summary(modelo.old.j.no2.sul.d.7)$s.table;summary(modelo.old.j.no2.sul.d.7)$r.sq;summary(modelo.old.j.no2.sul.d.7)$sp.criterion;

################################################################################
# como interpretar esses resultados do summary do modelo:
#
# edf = explica quanto cada variavel foi suavizada(quanto maior 
# o valor de edf mais complexos são os splines).
#
# p-values =  significancia estatistica da variavel (quanto menor
# melhor).
#
# R-sq. (adj)= R quadrado  ajustado, quanto maior o valor melhor.
#
# GCV = Generalized Cross Validation (usado para estimar parametros 
# de suavizacao ideais e os graus de liberdade,  quanto menor melhor).
#########################################################################

par(mfrow=c(2,4))

plot(modelo.old.j.no2.sul, main="modelo.old.j.no2.sul")
plot(modelo.old.j.no2.sul.d.1, main="modelo.old.j.no2.sul.d.1")
plot(modelo.old.j.no2.sul.d.2, main="modelo.old.j.no2.sul.d.2")
plot(modelo.old.j.no2.sul.d.3, main="modelo.old.j.no2.sul.d.3")
plot(modelo.old.j.no2.sul.d.4, main="modelo.old.j.no2.sul.d.4")
plot(modelo.old.j.no2.sul.d.5, main="modelo.old.j.no2.sul.d.5")
plot(modelo.old.j.no2.sul.d.6, main="modelo.old.j.no2.sul.d.6")
plot(modelo.old.j.no2.sul.d.7, main="modelo.old.j.no2.sul.d.7")

plot(residuo.old.j.no2.sul)
plot(residuo.old.j.no2.sul.d.1)
plot(residuo.old.j.no2.sul.d.2)
plot(residuo.old.j.no2.sul.d.3)
plot(residuo.old.j.no2.sul.d.4)
plot(residuo.old.j.no2.sul.d.5)
plot(residuo.old.j.no2.sul.d.6)
plot(residuo.old.j.no2.sul.d.7)

acf(residuo.old.j.no2.sul)
acf(residuo.old.j.no2.sul.d.1)
acf(residuo.old.j.no2.sul.d.2)
acf(residuo.old.j.no2.sul.d.3)
acf(residuo.old.j.no2.sul.d.4)
acf(residuo.old.j.no2.sul.d.5)
acf(residuo.old.j.no2.sul.d.6)
acf(residuo.old.j.no2.sul.d.7)

qqnorm(residuo.old.j.no2.sul);qqline(residuo.old.j.no2.sul)
qqnorm(residuo.old.j.no2.sul.d.1);qqline(residuo.old.j.no2.sul.d.1)
qqnorm(residuo.old.j.no2.sul.d.2);qqline(residuo.old.j.no2.sul.d.2)
qqnorm(residuo.old.j.no2.sul.d.3);qqline(residuo.old.j.no2.sul.d.3)
qqnorm(residuo.old.j.no2.sul.d.4);qqline(residuo.old.j.no2.sul.d.4)
qqnorm(residuo.old.j.no2.sul.d.5);qqline(residuo.old.j.no2.sul.d.5)
qqnorm(residuo.old.j.no2.sul.d.6);qqline(residuo.old.j.no2.sul.d.6)
qqnorm(residuo.old.j.no2.sul.d.7);qqline(residuo.old.j.no2.sul.d.7)

hist(residuo.old.j.no2.sul)
hist(residuo.old.j.no2.sul.d.1)
hist(residuo.old.j.no2.sul.d.2)
hist(residuo.old.j.no2.sul.d.3)
hist(residuo.old.j.no2.sul.d.4)
hist(residuo.old.j.no2.sul.d.5)
hist(residuo.old.j.no2.sul.d.6)
hist(residuo.old.j.no2.sul.d.7)
par(mfrow=c(1,1))


library(bbmle)

AIC.modelo.old.j.no2.sul <- AICctab(modelo.old.j.no2.sul,modelo.old.j.no2.sul.d.1,modelo.old.j.no2.sul.d.2,modelo.old.j.no2.sul.d.3, 
                                     modelo.old.j.no2.sul.d.4,modelo.old.j.no2.sul.d.5,modelo.old.j.no2.sul.d.6,
                                     modelo.old.j.no2.sul.d.7,
                                     base = T, weights=T)
AIC.modelo.old.j.no2.sul  

anova.gam.modelo.old.j.no2.sul <- anova.gam(modelo.old.j.no2.sul,modelo.old.j.no2.sul.d.1,modelo.old.j.no2.sul.d.2,modelo.old.j.no2.sul.d.3, 
                                             modelo.old.j.no2.sul.d.4,modelo.old.j.no2.sul.d.5,modelo.old.j.no2.sul.d.6,
                                             modelo.old.j.no2.sul.d.7)

anova.gam.modelo.old.j.no2.sul

anova.modelo.old.j.no2.sul<-anova(modelo.old.j.no2.sul,modelo.old.j.no2.sul.d.1,modelo.old.j.no2.sul.d.2,modelo.old.j.no2.sul.d.3, 
                                   modelo.old.j.no2.sul.d.4,modelo.old.j.no2.sul.d.5,modelo.old.j.no2.sul.d.6,
                                   modelo.old.j.no2.sul.d.7)

anova.modelo.old.j.no2.sul

###############################################################################################################################
#########################################
###### MODELOS i old sul no2 LAG 0 ######
#########################################
modelo.old.i.no2.sul <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul)

plot(modelo.old.i.no2.sul, main="modelo.old.i.no2.sul")

residuo.old.i.no2.sul <- residuals(modelo.old.i.no2.sul)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul)
acf(residuo.old.i.no2.sul)
qqnorm(residuo.old.i.no2.sul);qqline(residuo.old.i.no2.sul)
hist(residuo.old.i.no2.sul)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 1 ######
#########################################
modelo.old.i.no2.sul.d.1 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.1,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.1)

plot(modelo.old.i.no2.sul.d.1, main="modelo.old.i.no2.sul.d.1")

residuo.old.i.no2.sul.d.1 <- residuals(modelo.old.i.no2.sul.d.1)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.1)
acf(residuo.old.i.no2.sul.d.1)
qqnorm(residuo.old.i.no2.sul.d.1);qqline(residuo.old.i.no2.sul.d.1)
hist(residuo.old.i.no2.sul.d.1)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 2 ######
#########################################
modelo.old.i.no2.sul.d.2 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.2,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.2)

plot(modelo.old.i.no2.sul.d.2, main="modelo.old.i.no2.sul.d.2")

residuo.old.i.no2.sul.d.2 <- residuals(modelo.old.i.no2.sul.d.2)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.2)
acf(residuo.old.i.no2.sul.d.2)
qqnorm(residuo.old.i.no2.sul.d.2);qqline(residuo.old.i.no2.sul.d.2)
hist(residuo.old.i.no2.sul.d.2)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 3 ######
#########################################
modelo.old.i.no2.sul.d.3 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.3,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.3)

plot(modelo.old.i.no2.sul.d.3, main="modelo.old.i.no2.sul.d.3")

residuo.old.i.no2.sul.d.3 <- residuals(modelo.old.i.no2.sul.d.3)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.3)
acf(residuo.old.i.no2.sul.d.3)
qqnorm(residuo.old.i.no2.sul.d.3);qqline(residuo.old.i.no2.sul.d.3)
hist(residuo.old.i.no2.sul.d.3)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 4 ######
#########################################
modelo.old.i.no2.sul.d.4 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.4,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.4)

plot(modelo.old.i.no2.sul.d.4, main="modelo.old.i.no2.sul.d.4")

residuo.old.i.no2.sul.d.4 <- residuals(modelo.old.i.no2.sul.d.4)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.4)
acf(residuo.old.i.no2.sul.d.4)
qqnorm(residuo.old.i.no2.sul.d.4);qqline(residuo.old.i.no2.sul.d.4)
hist(residuo.old.i.no2.sul.d.4)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 5 ######
#########################################
modelo.old.i.no2.sul.d.5 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.5,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.5)

plot(modelo.old.i.no2.sul.d.5, main="modelo.old.i.no2.sul.d.5")

residuo.old.i.no2.sul.d.5 <- residuals(modelo.old.i.no2.sul.d.5)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.5)
acf(residuo.old.i.no2.sul.d.5)
qqnorm(residuo.old.i.no2.sul.d.5);qqline(residuo.old.i.no2.sul.d.5)
hist(residuo.old.i.no2.sul.d.5)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 6 ######
#########################################
modelo.old.i.no2.sul.d.6 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.6,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.6)

plot(modelo.old.i.no2.sul.d.6, main="modelo.old.i.no2.sul.d.6")

residuo.old.i.no2.sul.d.6 <- residuals(modelo.old.i.no2.sul.d.6)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.6)
acf(residuo.old.i.no2.sul.d.6)
qqnorm(residuo.old.i.no2.sul.d.6);qqline(residuo.old.i.no2.sul.d.6)
hist(residuo.old.i.no2.sul.d.6)
par(mfrow=c(1,1))
#########################################
###### MODELOS i old Sul no2 LAG 7######
#########################################
modelo.old.i.no2.sul.d.7 <- mgcv::gam(SIH_Sul_I_Old~s(no2.sul.d.7,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.i.no2.sul.d.7)

plot(modelo.old.i.no2.sul.d.7, main="modelo.old.i.no2.sul.d.7")

residuo.old.i.no2.sul.d.7 <- residuals(modelo.old.i.no2.sul.d.7)

par(mfrow=c(2,2))
plot(residuo.old.i.no2.sul.d.7)
acf(residuo.old.i.no2.sul.d.7)
qqnorm(residuo.old.i.no2.sul.d.7);qqline(residuo.old.i.no2.sul.d.7)
hist(residuo.old.i.no2.sul.d.7)
par(mfrow=c(1,1))

########comparando modelos com lags diferentes##########
################ idosos CID i ########################
#################### no2 ##############################
summary(modelo.old.i.no2.sul)$edf;summary(modelo.old.i.no2.sul)$s.table;summary(modelo.old.i.no2.sul)$r.sq;summary(modelo.old.i.no2.sul)$sp.criterion;
summary(modelo.old.i.no2.sul.d.1)$edf;summary(modelo.old.i.no2.sul.d.1)$s.table;summary(modelo.old.i.no2.sul.d.1)$r.sq;summary(modelo.old.i.no2.sul.d.1)$sp.criterion;
summary(modelo.old.i.no2.sul.d.2)$edf;summary(modelo.old.i.no2.sul.d.2)$s.table;summary(modelo.old.i.no2.sul.d.2)$r.sq;summary(modelo.old.i.no2.sul.d.2)$sp.criterion;
summary(modelo.old.i.no2.sul.d.3)$edf;summary(modelo.old.i.no2.sul.d.3)$s.table;summary(modelo.old.i.no2.sul.d.3)$r.sq;summary(modelo.old.i.no2.sul.d.3)$sp.criterion;
summary(modelo.old.i.no2.sul.d.4)$edf;summary(modelo.old.i.no2.sul.d.4)$s.table;summary(modelo.old.i.no2.sul.d.4)$r.sq;summary(modelo.old.i.no2.sul.d.4)$sp.criterion;
summary(modelo.old.i.no2.sul.d.5)$edf;summary(modelo.old.i.no2.sul.d.5)$s.table;summary(modelo.old.i.no2.sul.d.5)$r.sq;summary(modelo.old.i.no2.sul.d.5)$sp.criterion;
summary(modelo.old.i.no2.sul.d.6)$edf;summary(modelo.old.i.no2.sul.d.6)$s.table;summary(modelo.old.i.no2.sul.d.6)$r.sq;summary(modelo.old.i.no2.sul.d.6)$sp.criterion;
summary(modelo.old.i.no2.sul.d.7)$edf;summary(modelo.old.i.no2.sul.d.7)$s.table;summary(modelo.old.i.no2.sul.d.7)$r.sq;summary(modelo.old.i.no2.sul.d.7)$sp.criterion;

################################################################################
# como interpretar esses resultados do summary do modelo:
#
# edf = explica quanto cada variavel foi suavizada(quanto maior 
# o valor de edf mais complexos são os splines).
#
# p-values =  significancia estatistica da variavel (quanto menor
# melhor).
#
# R-sq. (adj)= R quadrado  ajustado, quanto maior o valor melhor.
#
# GCV = Generalized Cross Validation (usado para estimar parametros 
# de suavizacao ideais e os graus de liberdade,  quanto menor melhor).
#########################################################################

par(mfrow=c(2,4))

plot(modelo.old.i.no2.sul, main="modelo.old.i.no2.sul")
plot(modelo.old.i.no2.sul.d.1, main="modelo.old.i.no2.sul.d.1")
plot(modelo.old.i.no2.sul.d.2, main="modelo.old.i.no2.sul.d.2")
plot(modelo.old.i.no2.sul.d.3, main="modelo.old.i.no2.sul.d.3")
plot(modelo.old.i.no2.sul.d.4, main="modelo.old.i.no2.sul.d.4")
plot(modelo.old.i.no2.sul.d.5, main="modelo.old.i.no2.sul.d.5")
plot(modelo.old.i.no2.sul.d.6, main="modelo.old.i.no2.sul.d.6")
plot(modelo.old.i.no2.sul.d.7, main="modelo.old.i.no2.sul.d.7")

plot(residuo.old.i.no2.sul)
plot(residuo.old.i.no2.sul.d.1)
plot(residuo.old.i.no2.sul.d.2)
plot(residuo.old.i.no2.sul.d.3)
plot(residuo.old.i.no2.sul.d.4)
plot(residuo.old.i.no2.sul.d.5)
plot(residuo.old.i.no2.sul.d.6)
plot(residuo.old.i.no2.sul.d.7)

acf(residuo.old.i.no2.sul)
acf(residuo.old.i.no2.sul.d.1)
acf(residuo.old.i.no2.sul.d.2)
acf(residuo.old.i.no2.sul.d.3)
acf(residuo.old.i.no2.sul.d.4)
acf(residuo.old.i.no2.sul.d.5)
acf(residuo.old.i.no2.sul.d.6)
acf(residuo.old.i.no2.sul.d.7)

qqnorm(residuo.old.i.no2.sul);qqline(residuo.old.i.no2.sul)
qqnorm(residuo.old.i.no2.sul.d.1);qqline(residuo.old.i.no2.sul.d.1)
qqnorm(residuo.old.i.no2.sul.d.2);qqline(residuo.old.i.no2.sul.d.2)
qqnorm(residuo.old.i.no2.sul.d.3);qqline(residuo.old.i.no2.sul.d.3)
qqnorm(residuo.old.i.no2.sul.d.4);qqline(residuo.old.i.no2.sul.d.4)
qqnorm(residuo.old.i.no2.sul.d.5);qqline(residuo.old.i.no2.sul.d.5)
qqnorm(residuo.old.i.no2.sul.d.6);qqline(residuo.old.i.no2.sul.d.6)
qqnorm(residuo.old.i.no2.sul.d.7);qqline(residuo.old.i.no2.sul.d.7)

hist(residuo.old.i.no2.sul)
hist(residuo.old.i.no2.sul.d.1)
hist(residuo.old.i.no2.sul.d.2)
hist(residuo.old.i.no2.sul.d.3)
hist(residuo.old.i.no2.sul.d.4)
hist(residuo.old.i.no2.sul.d.5)
hist(residuo.old.i.no2.sul.d.6)
hist(residuo.old.i.no2.sul.d.7)
par(mfrow=c(1,1))

library(bbmle)

AIC.modelo.old.i.no2.sul <- AICctab(modelo.old.i.no2.sul,modelo.old.i.no2.sul.d.1,modelo.old.i.no2.sul.d.2,modelo.old.i.no2.sul.d.3, 
                                     modelo.old.i.no2.sul.d.4,modelo.old.i.no2.sul.d.5,modelo.old.i.no2.sul.d.6,
                                     modelo.old.i.no2.sul.d.7,
                                     base = T, weights=T)
AIC.modelo.old.i.no2.sul  

anova.gam.modelo.old.i.no2.sul <- anova.gam(modelo.old.i.no2.sul,modelo.old.i.no2.sul.d.1,modelo.old.i.no2.sul.d.2,modelo.old.i.no2.sul.d.3, 
                                             modelo.old.i.no2.sul.d.4,modelo.old.i.no2.sul.d.5,modelo.old.i.no2.sul.d.6,
                                             modelo.old.i.no2.sul.d.7)

anova.gam.modelo.old.i.no2.sul

anova.modelo.old.i.no2.sul<-anova(modelo.old.i.no2.sul,modelo.old.i.no2.sul.d.1,modelo.old.i.no2.sul.d.2,modelo.old.i.no2.sul.d.3, 
                                   modelo.old.i.no2.sul.d.4,modelo.old.i.no2.sul.d.5,modelo.old.i.no2.sul.d.6,
                                   modelo.old.i.no2.sul.d.7)

anova.modelo.old.i.no2.sul
