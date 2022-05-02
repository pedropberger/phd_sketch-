#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brazil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Authors: Dennis, Filipe, Pedro e Wilson
# Advisor: Valderio Anselmo Reisen 
#===============================================================================
# CODE PART 1/2
#===============================================================================
# Installing and loading packages
#===============================================================================
#install.packages("xlsx").
#install.packages("tidyverse").
#install.packages("mtsdi").
#install.packages("readxl").

library(xlsx)
library(tidyverse)
library(mtsdi)
library(readxl)

#===============================================================================
#loading Data (pollutants, temperature and air humidity from 2009 to 2018)
#===============================================================================
####Ensuring the correct type of each column
nropollu <-rep("numeric",34)
colunas <-c("date", nropollu)

####importing the spreadsheets
pollutants_2009<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2009.xlsx",  col_types = colunas)
pollutants_2010<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2010.xlsx",  col_types = colunas)
pollutants_2011<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2011.xlsx",  col_types = colunas)
pollutants_2012<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2012.xlsx",  col_types = colunas)
pollutants_2013<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2013.xlsx",  col_types = colunas)
pollutants_2014<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2014.xlsx",  col_types = colunas)
pollutants_2015<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2015.xlsx",  col_types = colunas)
pollutants_2016<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2016.xlsx",  col_types = colunas)
pollutants_2017<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2017.xlsx",  col_types = colunas)
pollutants_2018<- read_excel("C:/Users/fisic/artigo_2021/planilhas_brutas/Data_Stations_2018.xlsx",  col_types = colunas)

####importing only the columns we will need (only vitoria stations)
pm10vix <- c('pm10jc','pm10es','pm10vc')
so2vix <- c('so2jc','so2es','so2vc')
no2vix <- c('no2jc','no2es','no2vc')
covix <- c('coes','covc')
o3vix <- c('o3lar','o3es')
tempvix <-c('tempcar', 'tempcari')
umidvix <-c('umidcar', 'umidcari')

pollut_2009<- select(pollutants_2009, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2010<- select(pollutants_2010, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2011<- select(pollutants_2011, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2012<- select(pollutants_2012, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix) 
pollut_2013<- select(pollutants_2013, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2014<- select(pollutants_2014, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2015<- select(pollutants_2015, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2016<- select(pollutants_2016, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)
pollut_2017<- select(pollutants_2017, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix) 
pollut_2018<- select(pollutants_2018, 'Date', pm10vix, so2vix,no2vix, covix, o3vix, tempvix, umidvix)

####combining data from several years into just two objects
pollut_2009_2013 <-rbind.data.frame(pollut_2009,pollut_2010,pollut_2011,pollut_2012,pollut_2013)
pollut_2014_2018 <-rbind.data.frame(pollut_2014,pollut_2015,pollut_2016,pollut_2017,pollut_2018)

####Exporting data from 2008 to 2013 and from 2014 to 2018 in two different .csv files
write_csv(pollut_2009_2013,file="C:/Users/fisic/artigo_2021/planilhas_brutas/pollut_2009_2013.csv")
write_csv(pollut_2014_2018,file="C:/Users/fisic/artigo_2021/planilhas_brutas/pollut_2014_2018.csv")

#==============================END OF CODE=====================================#
