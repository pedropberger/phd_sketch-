#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brazil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Authors: Dennis, Filipe, Pedro e Wilson
# Advisor: Valderio Anselmo Reisen 
#===============================================================================
# CODE PART 2/2
#===============================================================================
# Installing and loading packages
#===============================================================================
#install.packages("xlsx").
#install.packages("tidyverse").
#install.packages("mtsdi").
library(xlsx)
library(tidyverse)
library(mtsdi)

#===============================================================================
# loading Data(pollutants, temperature and air humidity 2009 to 2018)
#===============================================================================
####Ensuring the correct type of each column
nropollu <-rep("numeric",17)
colunas <-c("Date", nropollu)

####importing the spreadsheets
pollut_2009_2013<- read.csv("C:/Users/fisic/artigo_2021/planilhas_brutas/pollut_2009_2013.csv", header=TRUE, colClasses =colunas)
pollut_2014_2018<- read.csv("C:/Users/fisic/artigo_2021/planilhas_brutas/pollut_2014_2018.csv", header=TRUE, colClasses =colunas)

#===============================================================================
#  data imputation - PM10 - from 2009 to 2013
#===============================================================================
i.pm10.2009_2013 <- mnimput(~pm10jc+pm10es+pm10vc,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.pm10.2009_2013)
summary(i.pm10.2009_2013)

imputadopm10.2009_2013=predict(i.pm10.2009_2013)
plot.ts(imputadopm10.2009_2013)
summary(imputadopm10.2009_2013)

#===============================================================================
#  data imputation - NO2 - from 2009 to 2013
#===============================================================================
i.no2.2009_2013 <- mnimput(~no2jc+no2es+no2vc,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.no2.2009_2013)
summary(i.no2.2009_2013)

imputadono2.2009_2013=predict(i.no2.2009_2013)
plot.ts(imputadono2.2009_2013)
summary(imputadono2.2009_2013)

#===============================================================================
# data imputation - O3 - from 2009 to 2013
#===============================================================================
i.o3.2009_2013 <- mnimput(~o3lar+o3es,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.o3.2009_2013)
summary(i.o3.2009_2013)

imputadoo3.2009_2013=predict(i.o3.2009_2013)
plot.ts(imputadoo3.2009_2013)
summary(imputadoo3.2009_2013)

#===============================================================================
#  data imputation - SO2 - from 2009 to 2013
#===============================================================================
i.so2.2009_2013 <- mnimput(~so2jc+so2es+so2vc,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.so2.2009_2013)
summary(i.so2.2009_2013)

imputadoso2.2009_2013=predict(i.so2.2009_2013)
plot.ts(imputadoso2.2009_2013)
summary(imputadoso2.2009_2013)

#===============================================================================
#data imputation - CO - from 2009 to 2013
#===============================================================================
i.co.2009_2013 <- mnimput(~coes+covc,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.co.2009_2013)
summary(i.co.2009_2013)

imputadoco.2009_2013=predict(i.co.2009_2013)
plot.ts(imputadoco.2009_2013)
summary(imputadoco.2009_2013)

#===============================================================================
# data imputation - temperature - from 2009 to 2013
#===============================================================================
i.temp.2009_2013 <- mnimput(~tempcar+tempcari,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.temp.2009_2013)
summary(i.temp.2009_2013)

imputadotemp.2009_2013=predict(i.temp.2009_2013)
plot.ts(imputadotemp.2009_2013)
summary(imputadotemp.2009_2013)

#===============================================================================
#  data imputation - air Humidity - from 2009 to 2013
#===============================================================================
i.umid.2009_2013 <- mnimput(~umidcar+umidcari,dataset=pollut_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.umid.2009_2013)
summary(i.umid.2009_2013)

imputadoumid.2009_2013=predict(i.umid.2009_2013)
plot.ts(imputadoumid.2009_2013)
summary(imputadoumid.2009_2013)

#===============================================================================
#  data imputation - PM10 - from 2014 to 2018
#===============================================================================
i.pm10.2014_2018 <- mnimput(~pm10jc+pm10es+pm10vc,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.pm10.2014_2018)
summary(i.pm10.2014_2018)

imputadopm10.2014_2018=predict(i.pm10.2014_2018)
plot.ts(imputadopm10.2014_2018)
summary(imputadopm10.2014_2018)

#===============================================================================
#  data imputation - NO2 - from 2014 to 2018
#===============================================================================
i.no2.2014_2018 <- mnimput(~no2jc+no2es+no2vc,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.no2.2014_2018)
summary(i.no2.2014_2018)

imputadono2.2014_2018=predict(i.no2.2014_2018)
plot.ts(imputadono2.2014_2018)
summary(imputadono2.2014_2018)

#===============================================================================
# data imputation - O3 - from 2014 to 2018
#===============================================================================
i.o3.2014_2018 <- mnimput(~o3lar+o3es,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.o3.2014_2018)
summary(i.o3.2014_2018)

imputadoo3.2014_2018=predict(i.o3.2014_2018)
plot.ts(imputadoo3.2014_2018)
summary(imputadoo3.2014_2018)

#===============================================================================
#  data imputation - SO2 - from 2014 to 2018
#===============================================================================
i.so2.2014_2018 <- mnimput(~so2jc+so2es+so2vc,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.so2.2014_2018)
summary(i.so2.2014_2018)

imputadoso2.2014_2018=predict(i.so2.2014_2018)
plot.ts(imputadoso2.2014_2018)
summary(imputadoso2.2014_2018)

#===============================================================================
#data imputation - CO - from 2014 to 2018
#===============================================================================
i.co.2014_2018 <- mnimput(~coes+covc,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.co.2014_2018)
summary(i.co.2014_2018)

imputadoco.2014_2018=predict(i.co.2014_2018)
plot.ts(imputadoco.2014_2018)
summary(imputadoco.2014_2018)

#===============================================================================
# data imputation - temperature - from 2014 to 2018
#===============================================================================
i.temp.2014_2018 <- mnimput(~tempcar+tempcari,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.temp.2014_2018)
summary(i.temp.2014_2018)

imputadotemp.2014_2018=predict(i.temp.2014_2018)
plot.ts(imputadotemp.2014_2018)
summary(imputadotemp.2014_2018)

#===============================================================================
#  data imputation - air Humidity - from 2014 to 2018
#===============================================================================
i.umid.2014_2018 <- mnimput(~umidcar+umidcari,dataset=pollut_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.umid.2014_2018)
summary(i.umid.2014_2018)

imputadoumid.2014_2018=predict(i.umid.2014_2018)
plot.ts(imputadoumid.2014_2018)
summary(imputadoumid.2014_2018)

#===============================================================================
# creating a dataframe with the imputations replacing the NA's
#===============================================================================
Date_2009_2013 <- pollut_2009_2013$Date
Stations_no_NA_2009_2013<- cbind.data.frame(Date_2009_2013, imputadopm10.2009_2013, imputadoso2.2009_2013, imputadono2.2009_2013, imputadoo3.2009_2013, imputadoco.2009_2013, imputadotemp.2009_2013, imputadoumid.2009_2013)

Date_2014_2018 <- pollut_2014_2018Date
Stations_no_NA_2014_2018<- cbind.data.frame(Date_2014_2018, imputadopm10.2014_2018, imputadoso2.2014_2018, imputadono2.2014_2018, imputadoo3.2014_2018, imputadoco.2014_2018, imputadotemp.2014_2018, imputadoumid.2014_2018)

#===============================================================================
# exporting to .csv file
#===============================================================================
write_csv(Stations_no_NA_2009_2013,file="C:/Users/fisic/artigo_2021/planilhas_brutas/Stations_no_NA_2009_2013.csv")
write_csv(Stations_no_NA_2014_2018,file="C:/Users/fisic/artigo_2021/planilhas_brutas/Stations_no_NA_2014_2018.csv")

#==============================END OF CODE=====================================#
