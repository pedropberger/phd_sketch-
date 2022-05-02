#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brazil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Authors: Dennis, Filipe, Pedro e Wilson
# Advisor: Valderio Anselmo Reisen 
#===============================================================================
# loading packages
#===============================================================================
library(xlsx);library(tidyverse)
library(mtsdi);library(readxl)
library(openair)

#===============================================================================
#loading Data (pollutants, temperature and air humidity from 2009 to 2018)
#===============================================================================
####importing the spreadsheets
setwd("C:/Users/fisic/artigo_2021/planilhas_brutas/")
setwd("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/")

####Ensuring the correct type of each column
nropollu <-rep("numeric",36)
colunas <-c("date", nropollu)

pollut_RGV_2009<- read_excel("Data_Stations_2009.xlsx",  col_types = colunas)
pollut_RGV_2010<- read_excel("Data_Stations_2010.xlsx",  col_types = colunas)
pollut_RGV_2011<- read_excel("Data_Stations_2011.xlsx",  col_types = colunas)
pollut_RGV_2012<- read_excel("Data_Stations_2012.xlsx",  col_types = colunas)
pollut_RGV_2013<- read_excel("Data_Stations_2013.xlsx",  col_types = colunas)
pollut_RGV_2014<- read_excel("Data_Stations_2014.xlsx",  col_types = colunas)
pollut_RGV_2015<- read_excel("Data_Stations_2015.xlsx",  col_types = colunas)
pollut_RGV_2016<- read_excel("Data_Stations_2016.xlsx",  col_types = colunas)
pollut_RGV_2017<- read_excel("Data_Stations_2017.xlsx",  col_types = colunas)
pollut_RGV_2018<- read_excel("Data_Stations_2018.xlsx",  col_types = colunas)

setwd("C:/Users/fisic/artigo_2021/planilhas_brutas/")
setwd("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/")

nro_rgv_2009_children_cid_j<- read.csv("nro_rgv_2009_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2010_children_cid_j<- read.csv("nro_rgv_2010_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2011_children_cid_j<- read.csv("nro_rgv_2011_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2012_children_cid_j<- read.csv("nro_rgv_2012_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2013_children_cid_j<- read.csv("nro_rgv_2013_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2014_children_cid_j<- read.csv("nro_rgv_2014_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2015_children_cid_j<- read.csv("nro_rgv_2015_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2016_children_cid_j<- read.csv("nro_rgv_2016_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2017_children_cid_j<- read.csv("nro_rgv_2017_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))
nro_rgv_2018_children_cid_j<- read.csv("nro_rgv_2018_children_cid_j.csv", header=TRUE, colClasses = c("Date", "numeric"))

####combining data from several years into just two objects 
pollut_RGV_2009_2013 <-rbind.data.frame(pollut_RGV_2009,pollut_RGV_2010,pollut_RGV_2011,pollut_RGV_2012,pollut_RGV_2013)
pollut_RGV_2014_2018 <-rbind.data.frame(pollut_RGV_2014,pollut_RGV_2015,pollut_RGV_2016,pollut_RGV_2017,pollut_RGV_2018)

nro_adm_GV_2009_2013 <-rbind.data.frame(nro_rgv_2009_children_cid_j,nro_rgv_2010_children_cid_j,nro_rgv_2011_children_cid_j,nro_rgv_2012_children_cid_j,nro_rgv_2013_children_cid_j)
nro_adm_GV_2014_2018 <-rbind.data.frame(nro_rgv_2014_children_cid_j,nro_rgv_2015_children_cid_j,nro_rgv_2016_children_cid_j,nro_rgv_2017_children_cid_j,nro_rgv_2018_children_cid_j)

#===============================================================================
#  DATA IMPUTATION (dataset = pollut_RGV_2009_2013)
#===============================================================================
####checking NA's in dataset pollut_RGV_2009_2013
summary(pollut_RGV_2009_2013)
mstats(pollut_RGV_2009_2013)

#===============================================================================
#  data imputation - PM10 - from 2009 to 2013
#===============================================================================
i.pm10.RGV.09_13 <- mnimput(~pm10lar+pm10car+pm10jc+pm10es+pm10vc+pm10vvi+pm10vvc+pm10cari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.pm10.RGV.09_13)
summary(i.pm10.RGV.09_13)

imputado.pm10.RGV.09_13=predict(i.pm10.RGV.09_13)
plot.ts(imputado.pm10.RGV.09_13)
summary(imputado.pm10.RGV.09_13)

#===============================================================================
#  data imputation - SO2 - from 2009 to 2013
#===============================================================================
i.so2.RGV.09_13 <- mnimput(~so2lar+so2jc+so2es+so2vc+so2vvi+so2cari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
### so2vvc was excluded because it had 68% of missings.
plot(i.so2.RGV.09_13)
summary(i.so2.RGV.09_13)

imputado.so2.RGV.09_13=predict(i.so2.RGV.09_13)
plot.ts(imputado.so2.RGV.09_13)
summary(imputado.so2.RGV.09_13)

#===============================================================================
#  data imputation - NO2 - from 2009 to 2013
#===============================================================================
i.no2.RGV.09_13 <- mnimput(~no2lar+no2jc+no2es+no2vc+no2vvi+no2cari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.no2.RGV.09_13)
summary(i.no2.RGV.09_13)

imputado.no2.RGV.09_13=predict(i.no2.RGV.09_13)
plot.ts(imputado.no2.RGV.09_13)
summary(imputado.no2.RGV.09_13)

#===============================================================================
#data imputation - CO - from 2009 to 2013
#===============================================================================
i.co.RGV.09_13 <- mnimput(~colar+coes+covc+covvi+cocari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.co.RGV.09_13)
summary(i.co.RGV.09_13)

imputado.co.RGV.09_13=predict(i.co.RGV.09_13)
plot.ts(imputado.co.RGV.09_13)
summary(imputado.co.RGV.09_13)

#===============================================================================
# data imputation - O3 - from 2009 to 2013
#===============================================================================
i.o3.RGV.09_13 <- mnimput(~o3lar+o3es+o3vvi+o3cari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.o3.RGV.09_13)
summary(i.o3.RGV.09_13)

imputado.o3.RGV.09_13=predict(i.o3.RGV.09_13)
plot.ts(imputado.o3.RGV.09_13)
summary(imputado.o3.RGV.09_13)

#===============================================================================
# data imputation - temperature - from 2009 to 2013
#===============================================================================
i.temp.RGV.09_13 <- mnimput(~tempcar+tempcari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.temp.RGV.09_13)
summary(i.temp.RGV.09_13)

imputado.temp.RGV.09_13=predict(i.temp.RGV.09_13)
plot.ts(imputado.temp.RGV.09_13)
summary(imputado.temp.RGV.09_13)

#===============================================================================
#  data imputation - air Humidity - from 2009 to 2013
#===============================================================================
i.umid.RGV.09_13 <- mnimput(~umidcar+umidcari,dataset=pollut_RGV_2009_2013,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.umid.RGV.09_13)
summary(i.umid.RGV.09_13)

imputado.umid.RGV.09_13=predict(i.umid.RGV.09_13)
plot.ts(imputado.umid.RGV.09_13)
summary(imputado.umid.RGV.09_13)

#===============================================================================
#  DATA IMPUTATION (dataset = pollut_RGV_2014_2018)
#===============================================================================
####checking NA's in dataset pollut_RGV_2009_2013
summary(pollut_RGV_2014_2018)
mstats(pollut_RGV_2014_2018)

#===============================================================================
#  data imputation - PM10 - from 2014 to 2018
#===============================================================================
i.pm10.RGV.14_18 <- mnimput(~pm10lar+pm10car+pm10jc+pm10es+pm10vc+pm10vvi+pm10cari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
### pm10vvc was excluded because it had 77% of missings.
plot(i.pm10.RGV.14_18)
summary(i.pm10.RGV.14_18)

imputado.pm10.RGV.14_18=predict(i.pm10.RGV.14_18)
plot.ts(imputado.pm10.RGV.14_18)
summary(imputado.pm10.RGV.14_18)

#===============================================================================
#  data imputation - SO2 - from 2014 to 2018
#===============================================================================
i.so2.RGV.14_18 <- mnimput(~so2lar+so2jc+so2es+so2vc+so2vvi+so2vvc+so2cari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
### so2vvc was excluded because it had 79% of missings.
plot(i.so2.RGV.14_18)
summary(i.so2.RGV.14_18)

imputado.so2.RGV.14_18=predict(i.so2.RGV.14_18)
plot.ts(imputado.so2.RGV.14_18)
summary(imputado.so2.RGV.14_18)

#===============================================================================
#  data imputation - NO2 - from 2014 to 2018
#===============================================================================
i.no2.RGV.14_18 <- mnimput(~no2lar+no2jc+no2es+no2vc+no2vvi+no2cari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.no2.RGV.14_18)
summary(i.no2.RGV.14_18)

imputado.no2.RGV.14_18=predict(i.no2.RGV.14_18)
plot.ts(imputado.no2.RGV.14_18)
summary(imputado.no2.RGV.14_18)

#===============================================================================
#data imputation - CO - from 2014 to 2018
#===============================================================================
i.co.RGV_14_18 <- mnimput(~colar+coes+covc+covvi+cocari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.co.RGV_14_18)
summary(i.co.RGV_14_18)

imputado.co.RGV.14_18=predict(i.co.RGV_14_18)
plot.ts(imputado.co.RGV.14_18)
summary(imputado.co.RGV.14_18)

#===============================================================================
# data imputation - O3 - from 2014 to 2018
#===============================================================================
i.o3.RGV.14_18 <- mnimput(~o3lar+o3es+o3vvi+o3cari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.o3.RGV.14_18)
summary(i.o3.RGV.14_18)

imputado.o3.RGV.14_18=predict(i.o3.RGV.14_18)
plot.ts(imputado.o3.RGV.14_18)
summary(imputado.o3.RGV.14_18)

#===============================================================================
# data imputation - temperature - from 2014 to 2018
#===============================================================================
i.temp.RGV.14_18 <- mnimput(~tempcar+tempcari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.temp.RGV.14_18)
summary(i.temp.RGV.14_18)

imputado.temp.RGV.14_18=predict(i.temp.RGV.14_18)
plot.ts(imputado.temp.RGV.14_18)
summary(imputado.temp.RGV.14_18)

#===============================================================================
#  data imputation - air Humidity - from 2014 to 2018
#===============================================================================
i.umid.RGV.14_18 <- mnimput(~umidcar+umidcari,dataset=pollut_RGV_2014_2018,eps=1e-3,maxit=1e3,ts=T,method="spline")
plot(i.umid.RGV.14_18)
summary(i.umid.RGV.14_18)

imputado.umid.RGV.14_18=predict(i.umid.RGV.14_18)
plot.ts(imputado.umid.RGV.14_18)
summary(imputado.umid.RGV.14_18)

#===============================================================================
# creating dataframes with the imputations replacing the NA's
#===============================================================================
date <-pollut_RGV_2009_2013$Date
RGV_no_NA_09_13 <- cbind.data.frame(date,imputado.pm10.RGV.09_13,imputado.so2.RGV.09_13,imputado.no2.RGV.09_13,imputado.co.RGV.09_13, imputado.o3.RGV.09_13,imputado.temp.RGV.09_13,imputado.umid.RGV.09_13)

date <-pollut_RGV_2014_2018$Date
RGV_no_NA_14_18 <- cbind.data.frame(date,imputado.pm10.RGV.14_18,imputado.so2.RGV.14_18,imputado.no2.RGV.14_18,imputado.co.RGV.14_18, imputado.o3.RGV.14_18,imputado.temp.RGV.14_18,imputado.umid.RGV.14_18)

#===============================================================================
# daily averages of pollutans, temperatures and air humidity
#===============================================================================
daily_mean_09_13 <-timeAverage(RGV_no_NA_09_13, avg.time = "24 hour")

daily_mean_14_18 <-timeAverage(RGV_no_NA_14_18, avg.time = "24 hour")

#===============================================================================
# max daily  averages of pollutants, temperatures and air humidity
#===============================================================================
daily_max_09_13 <-timeAverage(RGV_no_NA_09_13, statistic = "max", avg.time = "24 hour")
daily_max_09_13 <-select(daily_max_09_13,no2lar, no2jc, no2es, no2vc, no2vvi, no2cari)
daily_max_09_13 <-rename(daily_max_09_13, no2lar_max = no2lar, no2jc_max = no2jc, no2es_max = no2es, no2vc_max = no2vc, no2vvi_max = no2vvi, no2cari_max = no2cari)

daily_max_14_18 <-timeAverage(RGV_no_NA_14_18, statistic = "max", avg.time = "24 hour")
daily_max_14_18 <-select(daily_max_14_18,no2lar, no2jc, no2es, no2vc, no2vvi, no2cari)
daily_max_14_18 <-rename(daily_max_14_18, no2lar_max = no2lar, no2jc_max = no2jc, no2es_max = no2es, no2vc_max = no2vc, no2vvi_max = no2vvi, no2cari_max = no2cari)

#===============================================================================
# rolling 8-hour mean of CO and O3 from 2009 to 2013
#===============================================================================
#### CO 
roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "colar", width = 8, new.name = "colar_8hour_max", align = "right")
colar_8h_max<-select(roll_8_mean_09_13, colar_8hour_max)
  
roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "coes", width = 8, new.name = "coes_8hour_max", align = "right")
coes_8h_max<-select(roll_8_mean_09_13, coes_8hour_max)

roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "covc", width = 8, new.name = "covc_8hour_max", align = "right")
covc_8h_max<-select(roll_8_mean_09_13, covc_8hour_max)

roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "covvi", width = 8, new.name = "covvi_8hour_max", align = "right")
covvi_8h_max<-select(roll_8_mean_09_13, covvi_8hour_max)

roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "cocari", width = 8, new.name = "cocari_8hour_max",  align = "right")
cocari_8h_max<-select(roll_8_mean_09_13, cocari_8hour_max)


date <-pollut_RGV_2009_2013$Date

CO_8h_09_13 <- cbind(date,colar_8h_max,coes_8h_max,covc_8h_max,covvi_8h_max, cocari_8h_max)

CO_8h_MAX_09_13 <-timeAverage(CO_8h_09_13, statistic = "max", avg.time = "24 hour")


#### O3 
roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "o3lar", width = 8, new.name = "o3lar_8hour_max", align = "right")
o3lar_8h_max<-select(roll_8_mean_09_13, o3lar_8hour_max)

roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "o3es", width = 8, new.name = "o3es_8hour_max", align = "right")
o3es_8h_max<-select(roll_8_mean_09_13, o3es_8hour_max)

roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "o3vvi", width = 8, new.name = "o3vvi_8hour_max", align = "right")
o3vvi_8h_max<-select(roll_8_mean_09_13, o3vvi_8hour_max)

roll_8_mean_09_13 <- rollingMean(RGV_no_NA_09_13, pollutant = "o3cari", width = 8, new.name = "o3cari_8hour_max",  align = "right")
o3cari_8h_max<-select(roll_8_mean_09_13, o3cari_8hour_max)


date <-pollut_RGV_2009_2013$Date

O3_8h_09_13 <- cbind(date,o3lar_8h_max,o3es_8h_max,o3vvi_8h_max,o3cari_8h_max)

O3_8h_MAX_09_13 <-timeAverage(O3_8h_09_13, statistic = "max", avg.time = "24 hour")

#===============================================================================
# rolling 8-hour mean of CO and O3 from 2014 to 2018
#===============================================================================
#### CO 
roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "colar", width = 8, new.name = "colar_8hour_max", align = "right")
colar_8h_max<-select(roll_8_mean_14_18, colar_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "coes", width = 8, new.name = "coes_8hour_max", align = "right")
coes_8h_max<-select(roll_8_mean_14_18, coes_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "covc", width = 8, new.name = "covc_8hour_max", align = "right")
covc_8h_max<-select(roll_8_mean_14_18, covc_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "covvi", width = 8, new.name = "covvi_8hour_max", align = "right")
covvi_8h_max<-select(roll_8_mean_14_18, covvi_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "cocari", width = 8, new.name = "cocari_8hour_max",  align = "right")
cocari_8h_max<-select(roll_8_mean_14_18, cocari_8hour_max)


date <-pollut_RGV_2014_2018$Date

CO_8h_14_18 <- cbind(date,colar_8h_max,coes_8h_max,covc_8h_max,covvi_8h_max, cocari_8h_max)

CO_8h_MAX_14_18 <-timeAverage(CO_8h_14_18, statistic = "max", avg.time = "24 hour")



#### O3 
roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "o3lar", width = 8, new.name = "o3lar_8hour_max", align = "right")
o3lar_8h_max<-select(roll_8_mean_14_18, o3lar_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "o3es", width = 8, new.name = "o3es_8hour_max", align = "right")
o3es_8h_max<-select(roll_8_mean_14_18, o3es_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "o3vvi", width = 8, new.name = "o3vvi_8hour_max", align = "right")
o3vvi_8h_max<-select(roll_8_mean_14_18, o3vvi_8hour_max)

roll_8_mean_14_18 <- rollingMean(RGV_no_NA_14_18, pollutant = "o3cari", width = 8, new.name = "o3cari_8hour_max",  align = "right")
o3cari_8h_max<-select(roll_8_mean_14_18, o3cari_8hour_max)


date <-pollut_RGV_2014_2018$Date

O3_8h_14_18 <- cbind(date,o3lar_8h_max,o3es_8h_max,o3vvi_8h_max,o3cari_8h_max)

O3_8h_MAX_14_18 <-timeAverage(O3_8h_14_18, statistic = "max", avg.time = "24 hour")
#===============================================================================
# Combining daily concentrations, max daily concentration of the NO2 and 
# 8 hours mean of CO and O3
#===============================================================================

daily_concentrations_09_13 <-cbind(daily_mean_09_13,daily_max_09_13,CO_8h_MAX_09_13[ ,-1],O3_8h_MAX_09_13[ ,-1])

daily_concentrations_14_18 <-cbind(daily_mean_14_18,daily_max_14_18,CO_8h_MAX_14_18[ ,-1],O3_8h_MAX_14_18[ ,-1])

#===============================================================================
# averaging pollutant concentrations per station
#===============================================================================
#pm10vvc e so2vvc datas were removed from the two periods(09_13 and 14_18) .

daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(pm10.gv=mean(c(pm10lar,pm10car,pm10jc,pm10es,pm10vc,pm10vvi,pm10vvc,pm10cari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(so2.gv=mean(c(so2lar,so2jc,so2es,so2vc,so2vvi,so2cari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(no2.gv=mean(c(no2lar,no2jc,no2es,no2vc,no2vvi,no2cari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(co.gv=mean(c(colar,coes,covc,covvi,cocari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(o3.gv=mean(c(o3lar,o3es,o3vvi,o3cari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(temp.gv=mean(c(tempcar,tempcari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(umid.gv=mean(c(umidcar,umidcari)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(no2_max.gv=mean(c(no2lar_max,no2jc_max,no2es_max,no2vc_max,no2vvi_max,no2cari_max)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(co_8h_max.gv=mean(c(colar_8hour_max,coes_8hour_max,covc_8hour_max,covvi_8hour_max, cocari_8hour_max)))
daily_concentrations_09_13<- daily_concentrations_09_13 %>% rowwise() %>% mutate(o3_8h_max.gv=mean(c(o3lar_8hour_max,o3es_8hour_max,o3vvi_8hour_max,o3cari_8hour_max)))


daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(pm10.gv=mean(c(pm10lar,pm10car,pm10jc,pm10es,pm10vc,pm10vvi,pm10cari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(so2.gv=mean(c(so2lar,so2jc,so2es,so2vc,so2vvi,so2cari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(no2.gv=mean(c(no2lar,no2jc,no2es,no2vc,no2vvi,no2cari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(co.gv=mean(c(colar,coes,covc,covvi,cocari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(o3.gv=mean(c(o3lar,o3es,o3vvi,o3cari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(temp.gv=mean(c(tempcar,tempcari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(umid.gv=mean(c(umidcar,umidcari)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(no2_max.gv=mean(c(no2lar_max,no2jc_max,no2es_max,no2vc_max,no2vvi_max,no2cari_max)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(co_8h_max.gv=mean(c(colar_8hour_max,coes_8hour_max,covc_8hour_max,covvi_8hour_max, cocari_8hour_max)))
daily_concentrations_14_18<- daily_concentrations_14_18  %>% rowwise() %>% mutate(o3_8h_max.gv=mean(c(o3lar_8hour_max,o3es_8hour_max,o3vvi_8hour_max,o3cari_8hour_max)))

#===============================================================================
# averaging pollutant concentrations per region (GV)
#===============================================================================
daily_concent_GV_09_13 <-select(daily_concentrations_09_13, date,pm10.gv,so2.gv, no2.gv,co.gv,o3.gv,temp.gv,umid.gv,no2_max.gv,co_8h_max.gv,o3_8h_max.gv)

daily_concent_GV_14_18 <-select(daily_concentrations_14_18, date,pm10.gv,so2.gv, no2.gv,co.gv,o3.gv,temp.gv,umid.gv,no2_max.gv,co_8h_max.gv,o3_8h_max.gv)

#===============================================================================
# Exporting data from 2008 to 2013 and from 2014 to 2018 in two different .csv 
#===============================================================================
setwd("C:/Users/fisic/artigo_2021/planilhas_brutas/")

write_csv(daily_concent_GV_09_13,file="daily_concent_GV_09_13.csv")
write_csv(daily_concent_GV_14_18,file="daily_concent_GV_14_18.csv")

write_csv(nro_adm_GV_2009_2013,file="nro_adm_GV_2009_2013.csv")
write_csv(nro_adm_GV_2014_2018,file="nro_adm_GV_2014_2018.csv")

setwd("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/")

write_csv(daily_concent_GV_09_13,file="daily_concent_GV_09_13.csv")
write_csv(daily_concent_GV_14_18,file="daily_concent_GV_14_18.csv")

write_csv(nro_adm_GV_2009_2013,file="nro_adm_GV_2009_2013.csv")
write_csv(nro_adm_GV_2014_2018,file="nro_adm_GV_2014_2018.csv")
#==============================END OF CODE=====================================#