##############################################################################################
############################# IMPORTANDO OS DADOS ############################################

library(readxl)
library(dplyr)

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

list.files()

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")


dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

attach(dados)

###########################################################################################
############################# MODELOS COM O GAM ###########################################

############# CID J ##############################

library("gam")
library("mgcv")

modelo.old.gv <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.5 + pm10.gv  + pm10.gv.d.7
                           + co.gv + no2.gv,
                     data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.gv)
residuo <- residuals(modelo.old.gv)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


gamRR(
 fit=modelo.old.gv,
 ref=c(x0=pm25.gv.d.5, x1=pm10.gv, x2=pm10.gv.d.7,
       x3=co.gv,x4=no2.gv),
 est="x1",
 data=dados,
 n.points=10,
 plot=TRUE,
 ylim=NULL)


modelo.kid.gv <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.5 + pm10.gv + no2.gv + co.gv + o3.gv.d.3 ,
                           data = dados, family = poisson, na.action = na.gam.replace)


summary(modelo.kid.gv)
residuo <- residuals(modelo.kid.gv)
plot(residuo)
acf(residuo)  ###período de 7
qqnorm(residuo)
qqline(residuo)
hist(residuo)

############# CID I ##############################

modelo.I.old.gv <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.7 + pm10.gv + o3.gv.d.5 + no2.gv + co.gv,
                            data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.I.old.gv)
residuo <- residuals(modelo.I.old.gv)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)

#modelo.I.kid.gv <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.7 + pm10.gv + o3.gv.d.5 + no2.gv + co.gv,
#                             data = dados, family = poisson, na.action = na.gam.replace)

#summary(modelo.I.kid.gv)
#residuo <- residuals(modelo.I.kid.gv)
#plot(residuo)
#acf(residuo)
#qqnorm(residuo)
#qqline(residuo)
#hist(residuo)
