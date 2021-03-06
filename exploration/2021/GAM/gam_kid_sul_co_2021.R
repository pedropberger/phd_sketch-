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
###### MODELOS J kid sul co LAG 0 ######
#########################################
modelo.kid.j.co.sul <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul)

plot(modelo.kid.j.co.sul, main="modelo.kid.j.co.sul")

residuo.kid.j.co.sul <- residuals(modelo.kid.j.co.sul)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul)
acf(residuo.kid.j.co.sul)
qqnorm(residuo.kid.j.co.sul);qqline(residuo.kid.j.co.sul)
hist(residuo.kid.j.co.sul)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 1 ######
#########################################
modelo.kid.j.co.sul.d.1 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.1,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.1)

plot(modelo.kid.j.co.sul.d.1, main="modelo.kid.j.co.sul.d.1")

residuo.kid.j.co.sul.d.1 <- residuals(modelo.kid.j.co.sul.d.1)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.1)
acf(residuo.kid.j.co.sul.d.1)
qqnorm(residuo.kid.j.co.sul.d.1);qqline(residuo.kid.j.co.sul.d.1)
hist(residuo.kid.j.co.sul.d.1)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 2 ######
#########################################
modelo.kid.j.co.sul.d.2 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.2,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.2)

plot(modelo.kid.j.co.sul.d.2, main="modelo.kid.j.co.sul.d.2")

residuo.kid.j.co.sul.d.2 <- residuals(modelo.kid.j.co.sul.d.2)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.2)
acf(residuo.kid.j.co.sul.d.2)
qqnorm(residuo.kid.j.co.sul.d.2);qqline(residuo.kid.j.co.sul.d.2)
hist(residuo.kid.j.co.sul.d.2)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 3 ######
#########################################
modelo.kid.j.co.sul.d.3 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.3,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.3)

plot(modelo.kid.j.co.sul.d.3, main="modelo.kid.j.co.sul.d.3")

residuo.kid.j.co.sul.d.3 <- residuals(modelo.kid.j.co.sul.d.3)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.3)
acf(residuo.kid.j.co.sul.d.3)
qqnorm(residuo.kid.j.co.sul.d.3);qqline(residuo.kid.j.co.sul.d.3)
hist(residuo.kid.j.co.sul.d.3)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 4 ######
#########################################
modelo.kid.j.co.sul.d.4 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.4,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.4)

plot(modelo.kid.j.co.sul.d.4, main="modelo.kid.j.co.sul.d.4")

residuo.kid.j.co.sul.d.4 <- residuals(modelo.kid.j.co.sul.d.4)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.4)
acf(residuo.kid.j.co.sul.d.4)
qqnorm(residuo.kid.j.co.sul.d.4);qqline(residuo.kid.j.co.sul.d.4)
hist(residuo.kid.j.co.sul.d.4)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 5 ######
#########################################
modelo.kid.j.co.sul.d.5 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.5,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.5)

plot(modelo.kid.j.co.sul.d.5, main="modelo.kid.j.co.sul.d.5")

residuo.kid.j.co.sul.d.5 <- residuals(modelo.kid.j.co.sul.d.5)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.5)
acf(residuo.kid.j.co.sul.d.5)
qqnorm(residuo.kid.j.co.sul.d.5);qqline(residuo.kid.j.co.sul.d.5)
hist(residuo.kid.j.co.sul.d.5)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 6 ######
#########################################
modelo.kid.j.co.sul.d.6 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.6,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.6)

plot(modelo.kid.j.co.sul.d.6, main="modelo.kid.j.co.sul.d.6")

residuo.kid.j.co.sul.d.6 <- residuals(modelo.kid.j.co.sul.d.6)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.6)
acf(residuo.kid.j.co.sul.d.6)
qqnorm(residuo.kid.j.co.sul.d.6);qqline(residuo.kid.j.co.sul.d.6)
hist(residuo.kid.j.co.sul.d.6)
par(mfrow=c(1,1))
#########################################
###### MODELOS J kid Sul co LAG 7######
#########################################
modelo.kid.j.co.sul.d.7 <- mgcv::gam(SIH_Sul_J_Kid~s(co.sul.d.7,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.j.co.sul.d.7)

plot(modelo.kid.j.co.sul.d.7, main="modelo.kid.j.co.sul.d.7")

residuo.kid.j.co.sul.d.7 <- residuals(modelo.kid.j.co.sul.d.7)

par(mfrow=c(2,2))
plot(residuo.kid.j.co.sul.d.7)
acf(residuo.kid.j.co.sul.d.7)
qqnorm(residuo.kid.j.co.sul.d.7);qqline(residuo.kid.j.co.sul.d.7)
hist(residuo.kid.j.co.sul.d.7)
par(mfrow=c(1,1))
########comparando modelos com lags diferentes##########
################ crian�as CID J ########################
#################### co ##############################

summary(modelo.kid.j.co.sul)$edf;summary(modelo.kid.j.co.sul)$s.table;summary(modelo.kid.j.co.sul)$r.sq;summary(modelo.kid.j.co.sul)$sp.criterion;
summary(modelo.kid.j.co.sul.d.1)$edf;summary(modelo.kid.j.co.sul.d.1)$s.table;summary(modelo.kid.j.co.sul.d.1)$r.sq;summary(modelo.kid.j.co.sul.d.1)$sp.criterion;
summary(modelo.kid.j.co.sul.d.2)$edf;summary(modelo.kid.j.co.sul.d.2)$s.table;summary(modelo.kid.j.co.sul.d.2)$r.sq;summary(modelo.kid.j.co.sul.d.2)$sp.criterion;
summary(modelo.kid.j.co.sul.d.3)$edf;summary(modelo.kid.j.co.sul.d.3)$s.table;summary(modelo.kid.j.co.sul.d.3)$r.sq;summary(modelo.kid.j.co.sul.d.3)$sp.criterion;
summary(modelo.kid.j.co.sul.d.4)$edf;summary(modelo.kid.j.co.sul.d.4)$s.table;summary(modelo.kid.j.co.sul.d.4)$r.sq;summary(modelo.kid.j.co.sul.d.4)$sp.criterion;
summary(modelo.kid.j.co.sul.d.5)$edf;summary(modelo.kid.j.co.sul.d.5)$s.table;summary(modelo.kid.j.co.sul.d.5)$r.sq;summary(modelo.kid.j.co.sul.d.5)$sp.criterion;
summary(modelo.kid.j.co.sul.d.6)$edf;summary(modelo.kid.j.co.sul.d.6)$s.table;summary(modelo.kid.j.co.sul.d.6)$r.sq;summary(modelo.kid.j.co.sul.d.6)$sp.criterion;
summary(modelo.kid.j.co.sul.d.7)$edf;summary(modelo.kid.j.co.sul.d.7)$s.table;summary(modelo.kid.j.co.sul.d.7)$r.sq;summary(modelo.kid.j.co.sul.d.7)$sp.criterion;

################################################################################
# como interpretar esses resultados do summary do modelo:
#
# edf = explica quanto cada variavel foi suavizada(quanto maior 
# o valor de edf mais complexos s�o os splines).
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

plot(modelo.kid.j.co.sul, main="modelo.kid.j.co.sul")
plot(modelo.kid.j.co.sul.d.1, main="modelo.kid.j.co.sul.d.1")
plot(modelo.kid.j.co.sul.d.2, main="modelo.kid.j.co.sul.d.2")
plot(modelo.kid.j.co.sul.d.3, main="modelo.kid.j.co.sul.d.3")
plot(modelo.kid.j.co.sul.d.4, main="modelo.kid.j.co.sul.d.4")
plot(modelo.kid.j.co.sul.d.5, main="modelo.kid.j.co.sul.d.5")
plot(modelo.kid.j.co.sul.d.6, main="modelo.kid.j.co.sul.d.6")
plot(modelo.kid.j.co.sul.d.7, main="modelo.kid.j.co.sul.d.7")

plot(residuo.kid.j.co.sul)
plot(residuo.kid.j.co.sul.d.1)
plot(residuo.kid.j.co.sul.d.2)
plot(residuo.kid.j.co.sul.d.3)
plot(residuo.kid.j.co.sul.d.4)
plot(residuo.kid.j.co.sul.d.5)
plot(residuo.kid.j.co.sul.d.6)
plot(residuo.kid.j.co.sul.d.7)

acf(residuo.kid.j.co.sul)
acf(residuo.kid.j.co.sul.d.1)
acf(residuo.kid.j.co.sul.d.2)
acf(residuo.kid.j.co.sul.d.3)
acf(residuo.kid.j.co.sul.d.4)
acf(residuo.kid.j.co.sul.d.5)
acf(residuo.kid.j.co.sul.d.6)
acf(residuo.kid.j.co.sul.d.7)

qqnorm(residuo.kid.j.co.sul);qqline(residuo.kid.j.co.sul)
qqnorm(residuo.kid.j.co.sul.d.1);qqline(residuo.kid.j.co.sul.d.1)
qqnorm(residuo.kid.j.co.sul.d.2);qqline(residuo.kid.j.co.sul.d.2)
qqnorm(residuo.kid.j.co.sul.d.3);qqline(residuo.kid.j.co.sul.d.3)
qqnorm(residuo.kid.j.co.sul.d.4);qqline(residuo.kid.j.co.sul.d.4)
qqnorm(residuo.kid.j.co.sul.d.5);qqline(residuo.kid.j.co.sul.d.5)
qqnorm(residuo.kid.j.co.sul.d.6);qqline(residuo.kid.j.co.sul.d.6)
qqnorm(residuo.kid.j.co.sul.d.7);qqline(residuo.kid.j.co.sul.d.7)

hist(residuo.kid.j.co.sul)
hist(residuo.kid.j.co.sul.d.1)
hist(residuo.kid.j.co.sul.d.2)
hist(residuo.kid.j.co.sul.d.3)
hist(residuo.kid.j.co.sul.d.4)
hist(residuo.kid.j.co.sul.d.5)
hist(residuo.kid.j.co.sul.d.6)
hist(residuo.kid.j.co.sul.d.7)
par(mfrow=c(1,1))


library(bbmle)

AIC.modelo.kid.j.co.sul <- AICctab(modelo.kid.j.co.sul,modelo.kid.j.co.sul.d.1,modelo.kid.j.co.sul.d.2,modelo.kid.j.co.sul.d.3, 
                                   modelo.kid.j.co.sul.d.4,modelo.kid.j.co.sul.d.5,modelo.kid.j.co.sul.d.6,
                                   modelo.kid.j.co.sul.d.7,
                                   base = T, weights=T)
AIC.modelo.kid.j.co.sul  

anova.gam.modelo.kid.j.co.sul <- anova.gam(modelo.kid.j.co.sul,modelo.kid.j.co.sul.d.1,modelo.kid.j.co.sul.d.2,modelo.kid.j.co.sul.d.3, 
                                           modelo.kid.j.co.sul.d.4,modelo.kid.j.co.sul.d.5,modelo.kid.j.co.sul.d.6,
                                           modelo.kid.j.co.sul.d.7)

anova.gam.modelo.kid.j.co.sul

anova.modelo.kid.j.co.sul<-anova(modelo.kid.j.co.sul,modelo.kid.j.co.sul.d.1,modelo.kid.j.co.sul.d.2,modelo.kid.j.co.sul.d.3, 
                                 modelo.kid.j.co.sul.d.4,modelo.kid.j.co.sul.d.5,modelo.kid.j.co.sul.d.6,
                                 modelo.kid.j.co.sul.d.7)

anova.modelo.kid.j.co.sul

###############################################################################################################################
#########################################
###### MODELOS i kid sul co LAG 0 ######
#########################################
modelo.kid.i.co.sul <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul)

plot(modelo.kid.i.co.sul, main="modelo.kid.i.co.sul")

residuo.kid.i.co.sul <- residuals(modelo.kid.i.co.sul)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul)
acf(residuo.kid.i.co.sul)
qqnorm(residuo.kid.i.co.sul);qqline(residuo.kid.i.co.sul)
hist(residuo.kid.i.co.sul)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 1 ######
#########################################
modelo.kid.i.co.sul.d.1 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.1,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.1)

plot(modelo.kid.i.co.sul.d.1, main="modelo.kid.i.co.sul.d.1")

residuo.kid.i.co.sul.d.1 <- residuals(modelo.kid.i.co.sul.d.1)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.1)
acf(residuo.kid.i.co.sul.d.1)
qqnorm(residuo.kid.i.co.sul.d.1);qqline(residuo.kid.i.co.sul.d.1)
hist(residuo.kid.i.co.sul.d.1)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 2 ######
#########################################
modelo.kid.i.co.sul.d.2 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.2,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.2)

plot(modelo.kid.i.co.sul.d.2, main="modelo.kid.i.co.sul.d.2")

residuo.kid.i.co.sul.d.2 <- residuals(modelo.kid.i.co.sul.d.2)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.2)
acf(residuo.kid.i.co.sul.d.2)
qqnorm(residuo.kid.i.co.sul.d.2);qqline(residuo.kid.i.co.sul.d.2)
hist(residuo.kid.i.co.sul.d.2)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 3 ######
#########################################
modelo.kid.i.co.sul.d.3 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.3,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.3)

plot(modelo.kid.i.co.sul.d.3, main="modelo.kid.i.co.sul.d.3")

residuo.kid.i.co.sul.d.3 <- residuals(modelo.kid.i.co.sul.d.3)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.3)
acf(residuo.kid.i.co.sul.d.3)
qqnorm(residuo.kid.i.co.sul.d.3);qqline(residuo.kid.i.co.sul.d.3)
hist(residuo.kid.i.co.sul.d.3)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 4 ######
#########################################
modelo.kid.i.co.sul.d.4 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.4,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.4)

plot(modelo.kid.i.co.sul.d.4, main="modelo.kid.i.co.sul.d.4")

residuo.kid.i.co.sul.d.4 <- residuals(modelo.kid.i.co.sul.d.4)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.4)
acf(residuo.kid.i.co.sul.d.4)
qqnorm(residuo.kid.i.co.sul.d.4);qqline(residuo.kid.i.co.sul.d.4)
hist(residuo.kid.i.co.sul.d.4)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 5 ######
#########################################
modelo.kid.i.co.sul.d.5 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.5,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.5)

plot(modelo.kid.i.co.sul.d.5, main="modelo.kid.i.co.sul.d.5")

residuo.kid.i.co.sul.d.5 <- residuals(modelo.kid.i.co.sul.d.5)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.5)
acf(residuo.kid.i.co.sul.d.5)
qqnorm(residuo.kid.i.co.sul.d.5);qqline(residuo.kid.i.co.sul.d.5)
hist(residuo.kid.i.co.sul.d.5)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 6 ######
#########################################
modelo.kid.i.co.sul.d.6 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.6,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.6)

plot(modelo.kid.i.co.sul.d.6, main="modelo.kid.i.co.sul.d.6")

residuo.kid.i.co.sul.d.6 <- residuals(modelo.kid.i.co.sul.d.6)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.6)
acf(residuo.kid.i.co.sul.d.6)
qqnorm(residuo.kid.i.co.sul.d.6);qqline(residuo.kid.i.co.sul.d.6)
hist(residuo.kid.i.co.sul.d.6)
par(mfrow=c(1,1))
#########################################
###### MODELOS i kid Sul co LAG 7######
#########################################
modelo.kid.i.co.sul.d.7 <- mgcv::gam(SIH_Sul_I_Kid~s(co.sul.d.7,k=-1, fx=T),data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.i.co.sul.d.7)

plot(modelo.kid.i.co.sul.d.7, main="modelo.kid.i.co.sul.d.7")

residuo.kid.i.co.sul.d.7 <- residuals(modelo.kid.i.co.sul.d.7)

par(mfrow=c(2,2))
plot(residuo.kid.i.co.sul.d.7)
acf(residuo.kid.i.co.sul.d.7)
qqnorm(residuo.kid.i.co.sul.d.7);qqline(residuo.kid.i.co.sul.d.7)
hist(residuo.kid.i.co.sul.d.7)
par(mfrow=c(1,1))

########comparando modelos com lags diferentes##########
################ idosos CID i ########################
#################### co ##############################
summary(modelo.kid.i.co.sul)$edf;summary(modelo.kid.i.co.sul)$s.table;summary(modelo.kid.i.co.sul)$r.sq;summary(modelo.kid.i.co.sul)$sp.criterion;
summary(modelo.kid.i.co.sul.d.1)$edf;summary(modelo.kid.i.co.sul.d.1)$s.table;summary(modelo.kid.i.co.sul.d.1)$r.sq;summary(modelo.kid.i.co.sul.d.1)$sp.criterion;
summary(modelo.kid.i.co.sul.d.2)$edf;summary(modelo.kid.i.co.sul.d.2)$s.table;summary(modelo.kid.i.co.sul.d.2)$r.sq;summary(modelo.kid.i.co.sul.d.2)$sp.criterion;
summary(modelo.kid.i.co.sul.d.3)$edf;summary(modelo.kid.i.co.sul.d.3)$s.table;summary(modelo.kid.i.co.sul.d.3)$r.sq;summary(modelo.kid.i.co.sul.d.3)$sp.criterion;
summary(modelo.kid.i.co.sul.d.4)$edf;summary(modelo.kid.i.co.sul.d.4)$s.table;summary(modelo.kid.i.co.sul.d.4)$r.sq;summary(modelo.kid.i.co.sul.d.4)$sp.criterion;
summary(modelo.kid.i.co.sul.d.5)$edf;summary(modelo.kid.i.co.sul.d.5)$s.table;summary(modelo.kid.i.co.sul.d.5)$r.sq;summary(modelo.kid.i.co.sul.d.5)$sp.criterion;
summary(modelo.kid.i.co.sul.d.6)$edf;summary(modelo.kid.i.co.sul.d.6)$s.table;summary(modelo.kid.i.co.sul.d.6)$r.sq;summary(modelo.kid.i.co.sul.d.6)$sp.criterion;
summary(modelo.kid.i.co.sul.d.7)$edf;summary(modelo.kid.i.co.sul.d.7)$s.table;summary(modelo.kid.i.co.sul.d.7)$r.sq;summary(modelo.kid.i.co.sul.d.7)$sp.criterion;

################################################################################
# como interpretar esses resultados do summary do modelo:
#
# edf = explica quanto cada variavel foi suavizada(quanto maior 
# o valor de edf mais complexos s�o os splines).
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

plot(modelo.kid.i.co.sul, main="modelo.kid.i.co.sul")
plot(modelo.kid.i.co.sul.d.1, main="modelo.kid.i.co.sul.d.1")
plot(modelo.kid.i.co.sul.d.2, main="modelo.kid.i.co.sul.d.2")
plot(modelo.kid.i.co.sul.d.3, main="modelo.kid.i.co.sul.d.3")
plot(modelo.kid.i.co.sul.d.4, main="modelo.kid.i.co.sul.d.4")
plot(modelo.kid.i.co.sul.d.5, main="modelo.kid.i.co.sul.d.5")
plot(modelo.kid.i.co.sul.d.6, main="modelo.kid.i.co.sul.d.6")
plot(modelo.kid.i.co.sul.d.7, main="modelo.kid.i.co.sul.d.7")

plot(residuo.kid.i.co.sul)
plot(residuo.kid.i.co.sul.d.1)
plot(residuo.kid.i.co.sul.d.2)
plot(residuo.kid.i.co.sul.d.3)
plot(residuo.kid.i.co.sul.d.4)
plot(residuo.kid.i.co.sul.d.5)
plot(residuo.kid.i.co.sul.d.6)
plot(residuo.kid.i.co.sul.d.7)

acf(residuo.kid.i.co.sul)
acf(residuo.kid.i.co.sul.d.1)
acf(residuo.kid.i.co.sul.d.2)
acf(residuo.kid.i.co.sul.d.3)
acf(residuo.kid.i.co.sul.d.4)
acf(residuo.kid.i.co.sul.d.5)
acf(residuo.kid.i.co.sul.d.6)
acf(residuo.kid.i.co.sul.d.7)

qqnorm(residuo.kid.i.co.sul);qqline(residuo.kid.i.co.sul)
qqnorm(residuo.kid.i.co.sul.d.1);qqline(residuo.kid.i.co.sul.d.1)
qqnorm(residuo.kid.i.co.sul.d.2);qqline(residuo.kid.i.co.sul.d.2)
qqnorm(residuo.kid.i.co.sul.d.3);qqline(residuo.kid.i.co.sul.d.3)
qqnorm(residuo.kid.i.co.sul.d.4);qqline(residuo.kid.i.co.sul.d.4)
qqnorm(residuo.kid.i.co.sul.d.5);qqline(residuo.kid.i.co.sul.d.5)
qqnorm(residuo.kid.i.co.sul.d.6);qqline(residuo.kid.i.co.sul.d.6)
qqnorm(residuo.kid.i.co.sul.d.7);qqline(residuo.kid.i.co.sul.d.7)

hist(residuo.kid.i.co.sul)
hist(residuo.kid.i.co.sul.d.1)
hist(residuo.kid.i.co.sul.d.2)
hist(residuo.kid.i.co.sul.d.3)
hist(residuo.kid.i.co.sul.d.4)
hist(residuo.kid.i.co.sul.d.5)
hist(residuo.kid.i.co.sul.d.6)
hist(residuo.kid.i.co.sul.d.7)
par(mfrow=c(1,1))

library(bbmle)

AIC.modelo.kid.i.co.sul <- AICctab(modelo.kid.i.co.sul,modelo.kid.i.co.sul.d.1,modelo.kid.i.co.sul.d.2,modelo.kid.i.co.sul.d.3, 
                                   modelo.kid.i.co.sul.d.4,modelo.kid.i.co.sul.d.5,modelo.kid.i.co.sul.d.6,
                                   modelo.kid.i.co.sul.d.7,
                                   base = T, weights=T)
AIC.modelo.kid.i.co.sul  

anova.gam.modelo.kid.i.co.sul <- anova.gam(modelo.kid.i.co.sul,modelo.kid.i.co.sul.d.1,modelo.kid.i.co.sul.d.2,modelo.kid.i.co.sul.d.3, 
                                           modelo.kid.i.co.sul.d.4,modelo.kid.i.co.sul.d.5,modelo.kid.i.co.sul.d.6,
                                           modelo.kid.i.co.sul.d.7)

anova.gam.modelo.kid.i.co.sul

anova.modelo.kid.i.co.sul<-anova(modelo.kid.i.co.sul,modelo.kid.i.co.sul.d.1,modelo.kid.i.co.sul.d.2,modelo.kid.i.co.sul.d.3, 
                                 modelo.kid.i.co.sul.d.4,modelo.kid.i.co.sul.d.5,modelo.kid.i.co.sul.d.6,
                                 modelo.kid.i.co.sul.d.7)

anova.modelo.kid.i.co.sul

