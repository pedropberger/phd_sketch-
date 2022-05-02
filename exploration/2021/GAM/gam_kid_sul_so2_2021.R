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
###### MODELOS J kid sul so2 LAG 0 ######
#########################################
modelo.kid.j.so2.sul <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul)

plot(modelo.kid.j.so2.sul, main="modelo.kid.j.so2.sul")

residuo.kid.j.so2.sul <- residuals(modelo.kid.j.so2.sul)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul)
acf(residuo.kid.j.so2.sul)
qqnorm(residuo.kid.j.so2.sul);qqline(residuo.kid.j.so2.sul)
hist(residuo.kid.j.so2.sul)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 1 ######
#########################################
modelo.kid.j.so2.sul.d.1 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.1,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.1)

plot(modelo.kid.j.so2.sul.d.1, main="modelo.kid.j.so2.sul.d.1")

residuo.kid.j.so2.sul.d.1 <- residuals(modelo.kid.j.so2.sul.d.1)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.1)
acf(residuo.kid.j.so2.sul.d.1)
qqnorm(residuo.kid.j.so2.sul.d.1);qqline(residuo.kid.j.so2.sul.d.1)
hist(residuo.kid.j.so2.sul.d.1)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 2 ######
#########################################
modelo.kid.j.so2.sul.d.2 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.2,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.2)

plot(modelo.kid.j.so2.sul.d.2, main="modelo.kid.j.so2.sul.d.2")

residuo.kid.j.so2.sul.d.2 <- residuals(modelo.kid.j.so2.sul.d.2)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.2)
acf(residuo.kid.j.so2.sul.d.2)
qqnorm(residuo.kid.j.so2.sul.d.2);qqline(residuo.kid.j.so2.sul.d.2)
hist(residuo.kid.j.so2.sul.d.2)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 3 ######
#########################################
modelo.kid.j.so2.sul.d.3 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.3,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.3)

plot(modelo.kid.j.so2.sul.d.3, main="modelo.kid.j.so2.sul.d.3")

residuo.kid.j.so2.sul.d.3 <- residuals(modelo.kid.j.so2.sul.d.3)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.3)
acf(residuo.kid.j.so2.sul.d.3)
qqnorm(residuo.kid.j.so2.sul.d.3);qqline(residuo.kid.j.so2.sul.d.3)
hist(residuo.kid.j.so2.sul.d.3)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 4 ######
#########################################
modelo.kid.j.so2.sul.d.4 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.4,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.4)

plot(modelo.kid.j.so2.sul.d.4, main="modelo.kid.j.so2.sul.d.4")

residuo.kid.j.so2.sul.d.4 <- residuals(modelo.kid.j.so2.sul.d.4)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.4)
acf(residuo.kid.j.so2.sul.d.4)
qqnorm(residuo.kid.j.so2.sul.d.4);qqline(residuo.kid.j.so2.sul.d.4)
hist(residuo.kid.j.so2.sul.d.4)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 5 ######
#########################################
modelo.kid.j.so2.sul.d.5 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.5,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.5)

plot(modelo.kid.j.so2.sul.d.5, main="modelo.kid.j.so2.sul.d.5")

residuo.kid.j.so2.sul.d.5 <- residuals(modelo.kid.j.so2.sul.d.5)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.5)
acf(residuo.kid.j.so2.sul.d.5)
qqnorm(residuo.kid.j.so2.sul.d.5);qqline(residuo.kid.j.so2.sul.d.5)
hist(residuo.kid.j.so2.sul.d.5)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 6 ######
#########################################
modelo.kid.j.so2.sul.d.6 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.6,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.6)

plot(modelo.kid.j.so2.sul.d.6, main="modelo.kid.j.so2.sul.d.6")

residuo.kid.j.so2.sul.d.6 <- residuals(modelo.kid.j.so2.sul.d.6)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.6)
acf(residuo.kid.j.so2.sul.d.6)
qqnorm(residuo.kid.j.so2.sul.d.6);qqline(residuo.kid.j.so2.sul.d.6)
hist(residuo.kid.j.so2.sul.d.6)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul so2 LAG 7######
#########################################
modelo.kid.j.so2.sul.d.7 <- mgcv::gam(SIH_Sul_J_Kid~s(so2.sul.d.7,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.so2.sul.d.7)

plot(modelo.kid.j.so2.sul.d.7, main="modelo.kid.j.so2.sul.d.7")

residuo.kid.j.so2.sul.d.7 <- residuals(modelo.kid.j.so2.sul.d.7)

par(mfrow=c(2,2))
plot(residuo.kid.j.so2.sul.d.7)
acf(residuo.kid.j.so2.sul.d.7)
qqnorm(residuo.kid.j.so2.sul.d.7);qqline(residuo.kid.j.so2.sul.d.7)
hist(residuo.kid.j.so2.sul.d.7)
par(mfrow=c(1,1))
########comparando modelos com lags diferentes##########
################ crianças CID J ########################
#################### so2 ##############################

summary(modelo.kid.j.so2.sul)$edf;summary(modelo.kid.j.so2.sul)$s.table;summary(modelo.kid.j.so2.sul)$r.sq;summary(modelo.kid.j.so2.sul)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.1)$edf;summary(modelo.kid.j.so2.sul.d.1)$s.table;summary(modelo.kid.j.so2.sul.d.1)$r.sq;summary(modelo.kid.j.so2.sul.d.1)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.2)$edf;summary(modelo.kid.j.so2.sul.d.2)$s.table;summary(modelo.kid.j.so2.sul.d.2)$r.sq;summary(modelo.kid.j.so2.sul.d.2)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.3)$edf;summary(modelo.kid.j.so2.sul.d.3)$s.table;summary(modelo.kid.j.so2.sul.d.3)$r.sq;summary(modelo.kid.j.so2.sul.d.3)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.4)$edf;summary(modelo.kid.j.so2.sul.d.4)$s.table;summary(modelo.kid.j.so2.sul.d.4)$r.sq;summary(modelo.kid.j.so2.sul.d.4)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.5)$edf;summary(modelo.kid.j.so2.sul.d.5)$s.table;summary(modelo.kid.j.so2.sul.d.5)$r.sq;summary(modelo.kid.j.so2.sul.d.5)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.6)$edf;summary(modelo.kid.j.so2.sul.d.6)$s.table;summary(modelo.kid.j.so2.sul.d.6)$r.sq;summary(modelo.kid.j.so2.sul.d.6)$sp.criterion;
summary(modelo.kid.j.so2.sul.d.7)$edf;summary(modelo.kid.j.so2.sul.d.7)$s.table;summary(modelo.kid.j.so2.sul.d.7)$r.sq;summary(modelo.kid.j.so2.sul.d.7)$sp.criterion;

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

plot(modelo.kid.j.so2.sul, main="modelo.kid.j.so2.sul")
plot(modelo.kid.j.so2.sul.d.1, main="modelo.kid.j.so2.sul.d.1")
plot(modelo.kid.j.so2.sul.d.2, main="modelo.kid.j.so2.sul.d.2")
plot(modelo.kid.j.so2.sul.d.3, main="modelo.kid.j.so2.sul.d.3")
plot(modelo.kid.j.so2.sul.d.4, main="modelo.kid.j.so2.sul.d.4")
plot(modelo.kid.j.so2.sul.d.5, main="modelo.kid.j.so2.sul.d.5")
plot(modelo.kid.j.so2.sul.d.6, main="modelo.kid.j.so2.sul.d.6")
plot(modelo.kid.j.so2.sul.d.7, main="modelo.kid.j.so2.sul.d.7")

plot(residuo.kid.j.so2.sul)
plot(residuo.kid.j.so2.sul.d.1)
plot(residuo.kid.j.so2.sul.d.2)
plot(residuo.kid.j.so2.sul.d.3)
plot(residuo.kid.j.so2.sul.d.4)
plot(residuo.kid.j.so2.sul.d.5)
plot(residuo.kid.j.so2.sul.d.6)
plot(residuo.kid.j.so2.sul.d.7)

acf(residuo.kid.j.so2.sul)
acf(residuo.kid.j.so2.sul.d.1)
acf(residuo.kid.j.so2.sul.d.2)
acf(residuo.kid.j.so2.sul.d.3)
acf(residuo.kid.j.so2.sul.d.4)
acf(residuo.kid.j.so2.sul.d.5)
acf(residuo.kid.j.so2.sul.d.6)
acf(residuo.kid.j.so2.sul.d.7)

qqnorm(residuo.kid.j.so2.sul);qqline(residuo.kid.j.so2.sul)
qqnorm(residuo.kid.j.so2.sul.d.1);qqline(residuo.kid.j.so2.sul.d.1)
qqnorm(residuo.kid.j.so2.sul.d.2);qqline(residuo.kid.j.so2.sul.d.2)
qqnorm(residuo.kid.j.so2.sul.d.3);qqline(residuo.kid.j.so2.sul.d.3)
qqnorm(residuo.kid.j.so2.sul.d.4);qqline(residuo.kid.j.so2.sul.d.4)
qqnorm(residuo.kid.j.so2.sul.d.5);qqline(residuo.kid.j.so2.sul.d.5)
qqnorm(residuo.kid.j.so2.sul.d.6);qqline(residuo.kid.j.so2.sul.d.6)
qqnorm(residuo.kid.j.so2.sul.d.7);qqline(residuo.kid.j.so2.sul.d.7)

hist(residuo.kid.j.so2.sul)
hist(residuo.kid.j.so2.sul.d.1)
hist(residuo.kid.j.so2.sul.d.2)
hist(residuo.kid.j.so2.sul.d.3)
hist(residuo.kid.j.so2.sul.d.4)
hist(residuo.kid.j.so2.sul.d.5)
hist(residuo.kid.j.so2.sul.d.6)
hist(residuo.kid.j.so2.sul.d.7)
par(mfrow=c(1,1))


library(bbmle)

AIC.modelo.kid.j.so2.sul <- AICctab(modelo.kid.j.so2.sul,modelo.kid.j.so2.sul.d.1,modelo.kid.j.so2.sul.d.2,modelo.kid.j.so2.sul.d.3, 
                                   modelo.kid.j.so2.sul.d.4,modelo.kid.j.so2.sul.d.5,modelo.kid.j.so2.sul.d.6,
                                   modelo.kid.j.so2.sul.d.7,
                                   base = T, weights=T)
AIC.modelo.kid.j.so2.sul  

anova.gam.modelo.kid.j.so2.sul <- anova.gam(modelo.kid.j.so2.sul,modelo.kid.j.so2.sul.d.1,modelo.kid.j.so2.sul.d.2,modelo.kid.j.so2.sul.d.3, 
                                           modelo.kid.j.so2.sul.d.4,modelo.kid.j.so2.sul.d.5,modelo.kid.j.so2.sul.d.6,
                                           modelo.kid.j.so2.sul.d.7)

anova.gam.modelo.kid.j.so2.sul

anova.modelo.kid.j.so2.sul<-anova(modelo.kid.j.so2.sul,modelo.kid.j.so2.sul.d.1,modelo.kid.j.so2.sul.d.2,modelo.kid.j.so2.sul.d.3, 
                                 modelo.kid.j.so2.sul.d.4,modelo.kid.j.so2.sul.d.5,modelo.kid.j.so2.sul.d.6,
                                 modelo.kid.j.so2.sul.d.7)

anova.modelo.kid.j.so2.sul

###############################################################################################################################
#########################################
###### MODELOS i kid sul so2 LAG 0 ######
#########################################
modelo.kid.i.so2.sul <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul)

plot(modelo.kid.i.so2.sul, main="modelo.kid.i.so2.sul")

residuo.kid.i.so2.sul <- residuals(modelo.kid.i.so2.sul)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul)
acf(residuo.kid.i.so2.sul)
qqnorm(residuo.kid.i.so2.sul);qqline(residuo.kid.i.so2.sul)
hist(residuo.kid.i.so2.sul)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 1 ######
#########################################
modelo.kid.i.so2.sul.d.1 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.1,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.1)

plot(modelo.kid.i.so2.sul.d.1, main="modelo.kid.i.so2.sul.d.1")

residuo.kid.i.so2.sul.d.1 <- residuals(modelo.kid.i.so2.sul.d.1)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.1)
acf(residuo.kid.i.so2.sul.d.1)
qqnorm(residuo.kid.i.so2.sul.d.1);qqline(residuo.kid.i.so2.sul.d.1)
hist(residuo.kid.i.so2.sul.d.1)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 2 ######
#########################################
modelo.kid.i.so2.sul.d.2 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.2,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.2)

plot(modelo.kid.i.so2.sul.d.2, main="modelo.kid.i.so2.sul.d.2")

residuo.kid.i.so2.sul.d.2 <- residuals(modelo.kid.i.so2.sul.d.2)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.2)
acf(residuo.kid.i.so2.sul.d.2)
qqnorm(residuo.kid.i.so2.sul.d.2);qqline(residuo.kid.i.so2.sul.d.2)
hist(residuo.kid.i.so2.sul.d.2)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 3 ######
#########################################
modelo.kid.i.so2.sul.d.3 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.3,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.3)

plot(modelo.kid.i.so2.sul.d.3, main="modelo.kid.i.so2.sul.d.3")

residuo.kid.i.so2.sul.d.3 <- residuals(modelo.kid.i.so2.sul.d.3)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.3)
acf(residuo.kid.i.so2.sul.d.3)
qqnorm(residuo.kid.i.so2.sul.d.3);qqline(residuo.kid.i.so2.sul.d.3)
hist(residuo.kid.i.so2.sul.d.3)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 4 ######
#########################################
modelo.kid.i.so2.sul.d.4 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.4,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.4)

plot(modelo.kid.i.so2.sul.d.4, main="modelo.kid.i.so2.sul.d.4")

residuo.kid.i.so2.sul.d.4 <- residuals(modelo.kid.i.so2.sul.d.4)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.4)
acf(residuo.kid.i.so2.sul.d.4)
qqnorm(residuo.kid.i.so2.sul.d.4);qqline(residuo.kid.i.so2.sul.d.4)
hist(residuo.kid.i.so2.sul.d.4)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 5 ######
#########################################
modelo.kid.i.so2.sul.d.5 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.5,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.5)

plot(modelo.kid.i.so2.sul.d.5, main="modelo.kid.i.so2.sul.d.5")

residuo.kid.i.so2.sul.d.5 <- residuals(modelo.kid.i.so2.sul.d.5)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.5)
acf(residuo.kid.i.so2.sul.d.5)
qqnorm(residuo.kid.i.so2.sul.d.5);qqline(residuo.kid.i.so2.sul.d.5)
hist(residuo.kid.i.so2.sul.d.5)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 6 ######
#########################################
modelo.kid.i.so2.sul.d.6 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.6,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.6)

plot(modelo.kid.i.so2.sul.d.6, main="modelo.kid.i.so2.sul.d.6")

residuo.kid.i.so2.sul.d.6 <- residuals(modelo.kid.i.so2.sul.d.6)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.6)
acf(residuo.kid.i.so2.sul.d.6)
qqnorm(residuo.kid.i.so2.sul.d.6);qqline(residuo.kid.i.so2.sul.d.6)
hist(residuo.kid.i.so2.sul.d.6)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul so2 LAG 7######
#########################################
modelo.kid.i.so2.sul.d.7 <- mgcv::gam(SIH_Sul_I_Kid~s(so2.sul.d.7,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.so2.sul.d.7)

plot(modelo.kid.i.so2.sul.d.7, main="modelo.kid.i.so2.sul.d.7")

residuo.kid.i.so2.sul.d.7 <- residuals(modelo.kid.i.so2.sul.d.7)

par(mfrow=c(2,2))
plot(residuo.kid.i.so2.sul.d.7)
acf(residuo.kid.i.so2.sul.d.7)
qqnorm(residuo.kid.i.so2.sul.d.7);qqline(residuo.kid.i.so2.sul.d.7)
hist(residuo.kid.i.so2.sul.d.7)
par(mfrow=c(1,1))

########comparando modelos com lags diferentes##########
################ idosos CID i ########################
#################### so2 ##############################
summary(modelo.kid.i.so2.sul)$edf;summary(modelo.kid.i.so2.sul)$s.table;summary(modelo.kid.i.so2.sul)$r.sq;summary(modelo.kid.i.so2.sul)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.1)$edf;summary(modelo.kid.i.so2.sul.d.1)$s.table;summary(modelo.kid.i.so2.sul.d.1)$r.sq;summary(modelo.kid.i.so2.sul.d.1)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.2)$edf;summary(modelo.kid.i.so2.sul.d.2)$s.table;summary(modelo.kid.i.so2.sul.d.2)$r.sq;summary(modelo.kid.i.so2.sul.d.2)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.3)$edf;summary(modelo.kid.i.so2.sul.d.3)$s.table;summary(modelo.kid.i.so2.sul.d.3)$r.sq;summary(modelo.kid.i.so2.sul.d.3)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.4)$edf;summary(modelo.kid.i.so2.sul.d.4)$s.table;summary(modelo.kid.i.so2.sul.d.4)$r.sq;summary(modelo.kid.i.so2.sul.d.4)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.5)$edf;summary(modelo.kid.i.so2.sul.d.5)$s.table;summary(modelo.kid.i.so2.sul.d.5)$r.sq;summary(modelo.kid.i.so2.sul.d.5)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.6)$edf;summary(modelo.kid.i.so2.sul.d.6)$s.table;summary(modelo.kid.i.so2.sul.d.6)$r.sq;summary(modelo.kid.i.so2.sul.d.6)$sp.criterion;
summary(modelo.kid.i.so2.sul.d.7)$edf;summary(modelo.kid.i.so2.sul.d.7)$s.table;summary(modelo.kid.i.so2.sul.d.7)$r.sq;summary(modelo.kid.i.so2.sul.d.7)$sp.criterion;

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

plot(modelo.kid.i.so2.sul, main="modelo.kid.i.so2.sul")
plot(modelo.kid.i.so2.sul.d.1, main="modelo.kid.i.so2.sul.d.1")
plot(modelo.kid.i.so2.sul.d.2, main="modelo.kid.i.so2.sul.d.2")
plot(modelo.kid.i.so2.sul.d.3, main="modelo.kid.i.so2.sul.d.3")
plot(modelo.kid.i.so2.sul.d.4, main="modelo.kid.i.so2.sul.d.4")
plot(modelo.kid.i.so2.sul.d.5, main="modelo.kid.i.so2.sul.d.5")
plot(modelo.kid.i.so2.sul.d.6, main="modelo.kid.i.so2.sul.d.6")
plot(modelo.kid.i.so2.sul.d.7, main="modelo.kid.i.so2.sul.d.7")

plot(residuo.kid.i.so2.sul)
plot(residuo.kid.i.so2.sul.d.1)
plot(residuo.kid.i.so2.sul.d.2)
plot(residuo.kid.i.so2.sul.d.3)
plot(residuo.kid.i.so2.sul.d.4)
plot(residuo.kid.i.so2.sul.d.5)
plot(residuo.kid.i.so2.sul.d.6)
plot(residuo.kid.i.so2.sul.d.7)

acf(residuo.kid.i.so2.sul)
acf(residuo.kid.i.so2.sul.d.1)
acf(residuo.kid.i.so2.sul.d.2)
acf(residuo.kid.i.so2.sul.d.3)
acf(residuo.kid.i.so2.sul.d.4)
acf(residuo.kid.i.so2.sul.d.5)
acf(residuo.kid.i.so2.sul.d.6)
acf(residuo.kid.i.so2.sul.d.7)

qqnorm(residuo.kid.i.so2.sul);qqline(residuo.kid.i.so2.sul)
qqnorm(residuo.kid.i.so2.sul.d.1);qqline(residuo.kid.i.so2.sul.d.1)
qqnorm(residuo.kid.i.so2.sul.d.2);qqline(residuo.kid.i.so2.sul.d.2)
qqnorm(residuo.kid.i.so2.sul.d.3);qqline(residuo.kid.i.so2.sul.d.3)
qqnorm(residuo.kid.i.so2.sul.d.4);qqline(residuo.kid.i.so2.sul.d.4)
qqnorm(residuo.kid.i.so2.sul.d.5);qqline(residuo.kid.i.so2.sul.d.5)
qqnorm(residuo.kid.i.so2.sul.d.6);qqline(residuo.kid.i.so2.sul.d.6)
qqnorm(residuo.kid.i.so2.sul.d.7);qqline(residuo.kid.i.so2.sul.d.7)

hist(residuo.kid.i.so2.sul)
hist(residuo.kid.i.so2.sul.d.1)
hist(residuo.kid.i.so2.sul.d.2)
hist(residuo.kid.i.so2.sul.d.3)
hist(residuo.kid.i.so2.sul.d.4)
hist(residuo.kid.i.so2.sul.d.5)
hist(residuo.kid.i.so2.sul.d.6)
hist(residuo.kid.i.so2.sul.d.7)
par(mfrow=c(1,1))

library(bbmle)

AIC.modelo.kid.i.so2.sul <- AICctab(modelo.kid.i.so2.sul,modelo.kid.i.so2.sul.d.1,modelo.kid.i.so2.sul.d.2,modelo.kid.i.so2.sul.d.3, 
                                   modelo.kid.i.so2.sul.d.4,modelo.kid.i.so2.sul.d.5,modelo.kid.i.so2.sul.d.6,
                                   modelo.kid.i.so2.sul.d.7,
                                   base = T, weights=T)
AIC.modelo.kid.i.so2.sul  

anova.gam.modelo.kid.i.so2.sul <- anova.gam(modelo.kid.i.so2.sul,modelo.kid.i.so2.sul.d.1,modelo.kid.i.so2.sul.d.2,modelo.kid.i.so2.sul.d.3, 
                                           modelo.kid.i.so2.sul.d.4,modelo.kid.i.so2.sul.d.5,modelo.kid.i.so2.sul.d.6,
                                           modelo.kid.i.so2.sul.d.7)

anova.gam.modelo.kid.i.so2.sul

anova.modelo.kid.i.so2.sul<-anova(modelo.kid.i.so2.sul,modelo.kid.i.so2.sul.d.1,modelo.kid.i.so2.sul.d.2,modelo.kid.i.so2.sul.d.3, 
                                 modelo.kid.i.so2.sul.d.4,modelo.kid.i.so2.sul.d.5,modelo.kid.i.so2.sul.d.6,
                                 modelo.kid.i.so2.sul.d.7)

anova.modelo.kid.i.so2.sul

