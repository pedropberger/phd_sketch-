#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brazil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Authors: Filipe e Wilson
# Advisor: Valderio Anselmo Reisen 
#===============================================================================
# loading packages
#===============================================================================
library(xlsx);library(tidyverse)
library(mtsdi);library(readxl)
library(openair);library(dlookr)
library(xtable)
#===============================================================================
#loading Data 
#===============================================================================
setwd("C:/Users/fisic/Dropbox/Filipe e Wilson/DADOS/")

####Ensuring the correct type of each column
nropollu <-rep("numeric",6)
colunas <-c("Date", nropollu)

####pollutants data
dados_NA_poluentes_GV_15_19 <- read.csv("dados_NA_poluentes_GV_15_19.csv", header=TRUE, colClasses =colunas)
nro_rgv_2015_2019_children_cid_j <- read.csv("nro_rgv_2015_2019_children_cid_j.csv", header=TRUE, colClasses =c("Date", "numeric"))

x<-cbind(nro_rgv_2015_2019_children_cid_j[,-1],dados_NA_poluentes_GV_15_19[,-1])


cor_x <-cor(x, use = "na.or.complete")
cor_x[upper.tri(cor_x)]
cor_x<-as.data.frame(cor_x)
cor_x

xtable(cor_x)

cor_x <-round(cor(x, use = "na.or.complete"), 4)
cor_x[upper.tri(cor_x)]<-""
cor_x<-as.data.frame(cor_x)
cor_x

xtable(cor_x)




pollut_RGV_2015 <- dados_NA_poluentes_GV_15_19[dados_NA_poluentes_GV_15_19$date >= "2015-01-01" & dados_NA_poluentes_GV_15_19$date <= "2015-12-31",]
pollut_RGV_2016 <- dados_NA_poluentes_GV_15_19[dados_NA_poluentes_GV_15_19$date >= "2016-01-01" & dados_NA_poluentes_GV_15_19$date <= "2016-12-31",]
pollut_RGV_2017 <- dados_NA_poluentes_GV_15_19[dados_NA_poluentes_GV_15_19$date >= "2017-01-01" & dados_NA_poluentes_GV_15_19$date <= "2017-12-31",]
pollut_RGV_2018 <- dados_NA_poluentes_GV_15_19[dados_NA_poluentes_GV_15_19$date >= "2018-01-01" & dados_NA_poluentes_GV_15_19$date <= "2018-12-31",]
pollut_RGV_2019 <- dados_NA_poluentes_GV_15_19[dados_NA_poluentes_GV_15_19$date >= "2019-01-01" & dados_NA_poluentes_GV_15_19$date <= "2019-12-31",]

#######

perpolu_15<-mstats(select(pollut_RGV_2015,pm25.gv,pm10.gv,so2.gv,no2.gv,co.gv,o3.gv))
perpolu_15<-round(perpolu_15$columns)
perpolu_15<-data.frame(t(perpolu_15))

xtable(perpolu_15)

perpolu_16<-mstats(select(pollut_RGV_2016,pm25.gv,pm10.gv,so2.gv,no2.gv,co.gv,o3.gv))
perpolu_16<-round(perpolu_16$columns)
perpolu_16<-data.frame(t(perpolu_16))

xtable(perpolu_16)

perpolu_17<-mstats(select(pollut_RGV_2017,pm25.gv,pm10.gv,so2.gv,no2.gv,co.gv,o3.gv))
perpolu_17<-round(perpolu_17$columns)
perpolu_17<-data.frame(t(perpolu_17))

xtable(perpolu_17)

perpolu_18<-mstats(select(pollut_RGV_2018,pm25.gv,pm10.gv,so2.gv,no2.gv,co.gv,o3.gv))
perpolu_18<-round(perpolu_18$columns)
perpolu_18<-data.frame(t(perpolu_18))

xtable(perpolu_18)

perpolu_19<-mstats(select(pollut_RGV_2019,pm25.gv,pm10.gv,so2.gv,no2.gv,co.gv,o3.gv))
perpolu_19<-round(perpolu_19$columns)
perpolu_19<-data.frame(t(perpolu_19))

xtable(perpolu_19)
