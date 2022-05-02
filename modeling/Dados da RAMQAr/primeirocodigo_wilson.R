##############################################################################
################# IMPORTANTO OS DADOS DE POLUICAO ############################
##############################################################################
library(readxl)

dados.laranjeiras <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 1 - Laranjeiras.xlsx")
dados.carapina <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 2 - Carapina.xlsx")
dados.camburi <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 3 - Jardim Camburi.xlsx")
dados.enseada <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 4 - Enseada do Sua.xlsx")
dados.centro <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 5 - Vitoria Centro.xlsx")
dados.ibes <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 6 - Ibes.xlsx")
dados.VilaVelha <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 7 - Vila Velha Centro.xlsx")
dados.vilacapixaba <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr/RAMQAr 8 - Vila Capixaba.xlsx")


##############################################################################
################# ARRUMANDO OS DADOS DE LARANJEIRAS ##########################
##############################################################################
summary(dados.laranjeiras[, c(2,4,6,8)])

co.laranjeiras <- dados.laranjeiras$'Monóxido de Carbono'
no2.laranjeiras <- dados.laranjeiras$'Dióxido de Nitrogênio'
so2.laranjeiras <- dados.laranjeiras$'Dióxido de Enxofre'
pm10.laranjeiras <- dados.laranjeiras$'Partículas Inaláveis (<10µm)'

##############################################################################
################# ARRUMANDO OS DADOS DE CARAPINA #############################
##############################################################################

summary(dados.carapina[,c(2,3)])

pm10.carapina <- dados.carapina$'Partículas Inaláveis (<10µm)'
pts.carapina <- dados.carapina$'Partículas Totais em Suspensão'

##############################################################################
################# ARRUMANDO OS DADOS DE CAMBURI     ##########################
##############################################################################

summary(dados.camburi[,c(2,3,4,6)])

no2.camburi <- dados.camburi$'Dióxido de Nitrogênio'
so2.camburi <- dados.camburi$'Dióxido de Enxofre'
pm10.camburi <- dados.camburi$'Partículas Inaláveis (<10µm)'
pts.camburi <- dados.camburi$'Partículas Totais em Suspensão'

##############################################################################
################# ARRUMANDO OS DADOS ENSEADA  ################################
##############################################################################

summary(dados.enseada[,c(2,3,4,5,7,9,10)])

so2.enseada <- dados.enseada$`Dióxido de Enxofre`
no2.enseada <- dados.enseada$`Dióxido de Nitrogênio`
co.enseada <- dados.enseada$`Monóxido de Carbono`
O3.enseada <- dados.enseada$'Ozônio'
PM25.enseada <-dados.enseada$'Partículas Respiráveis (< 2,5µm)'
pm10.enseada <- dados.enseada$'Partículas Inaláveis (<10µm)'
pts.enseada <- dados.enseada$`Partículas Totais em Suspensão`

##############################################################################
################# ARRUMANDO OS DADOS DE CENTRO ###############################
##############################################################################

summary(dados.centro[,c(2,3,4,6,8)])

so2.centro <- dados.centro$`Dióxido de Enxofre`
no2.centro <- dados.centro$`Dióxido de Nitrogênio`
co.centro <- dados.centro$`Monóxido de Carbono`
pm10.centro <- dados.centro$'Partículas Inaláveis (<10µm)'
pts.centro <- dados.centro$`Partículas Totais em Suspensão`

##############################################################################
################# ARRUMANDO OS DADOS DE IBES #################################
##############################################################################

summary(dados.ibes[,c(2,3,4,5,7,9,10)])

so2.ibes <- dados.ibes$`Dióxido de Enxofre`
no2.ibes <- dados.ibes$`Dióxido de Nitrogênio`
co.ibes <- dados.ibes$`Monóxido de Carbono`
o3.ibes <- dados.ibes$'Ozônio'
pm10.ibes <- dados.ibes$'Partículas Inaláveis (<10µm)'
pm25.ibes <- dados.ibes$`Partículas Respiráveis (< 2.5µm)`
pts.ibes <- dados.ibes$`Partículas Totais em Suspensão`

##############################################################################
################# ARRUMANDO OS DADOS DE VILA VELHA############################
##############################################################################

summary(dados.VilaVelha)

so2.ibes <- dados.VilaVelha$`Dióxido de Enxofre`
pm10.ibes <- dados.VilaVelha$'Partículas Inaláveis (<10µm)'

##############################################################################
################# ARRUMANDO OS DADOS DE VILA CAPIXABA ########################
##############################################################################

summary(dados.vilacapixaba[,c(2,3,4,6,8,9)])
so2.vilacapixaba <- dados.vilacapixaba$`Dióxido de Enxofre`
no2.vilacapixaba <- dados.vilacapixaba$`Dióxido de Nitrogênio`
co.vilacapixaba <- dados.vilacapixaba$`Monóxido de Carbono`
o3.vilacapixaba <- dados.vilacapixaba$'Ozônio'
pm10.vilacapixaba<- dados.vilacapixaba$'Partículas Inaláveis (<10µm)'
pts.vilacapixaba <- dados.vilacapixaba$`Partículas Totais em Suspensão`

##############################################################################
################# AGRUPANDO OS DADOS POR POLUENTE#############################
##############################################################################


#################### CO ###################################################
dados.co <- cbind(co.vilacapixaba, co.ibes, co.centro, co.enseada, co.laranjeiras)

plot(ts(dados.co, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Monoxido de Carbono")

#################### O3 ###################################################
dados.o3 <- cbind(o3.vilacapixaba, o3.ibes, O3.enseada)

plot(ts(dados.o3 , start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Ozonio")

#################### NO2 ###################################################
dados.no2 <- cbind(no2.centro, no2.enseada, no2.ibes,no2.vilacapixaba, no2.laranjeiras,no2.camburi)

plot(ts(dados.no2, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Dioxido de Nitrogenio")

#################### SO2 ###################################################
dados.so2 <- cbind(so2.vilacapixaba,so2.ibes, so2.centro, so2.enseada, so2.laranjeiras,so2.camburi)

plot(ts(dados.so2, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Dioxido de Enxofre")

#################### PM10 ###################################################
dados.pm10 <- cbind(pm10.camburi, pm10.carapina, pm10.vilacapixaba, pm10.centro, pm10.ibes, pm10.laranjeiras, pm10.enseada)

plot(ts(dados.pm10, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Particulas Inalaveis")

#################### PM2,5 ###################################################
dados.pm25 <- cbind(pm25.ibes,PM25.enseada)

plot(ts(dados.pm25, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Particulas Respiraveis")


#################### MEDIA DE CADA POLUENTE #############################################

co.media <- rowMeans(dados.co, na.rm = T)
o3.media <- rowMeans(dados.o3, na.rm = T)
no2.media <- rowMeans(dados.no2, na.rm = T)
so2.media <- rowMeans(dados.so2, na.rm = T)
pm10.media <- rowMeans(dados.pm10, na.rm = T)
pm25.media <- rowMeans(dados.pm25, na.rm = T)
dados.media <- cbind(co.media, o3.media, no2.media, so2.media, pm10.media, pm25.media)

summary(dados.media)

plot(ts(dados.media, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Media de cada Poluente")

##############################################################################
################# IMPORTANTO OS DADOS DE SAUDE de 0-12 anos #################
##############################################################################
library(readxl)

dados.saudekid<- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/Wilson_SIH_GV_IJ_Kid.xlsx")
dados.saudekid <- dados.saudekid[1:1826,] ### CONSIDERANDO APENAS O TEMPO DE JAN/15 A DEZ/19

##############################################################################
################# ARRUMANDO TODOS OS DADOS de 0-12 anos#######################
##############################################################################

dadoskid <- cbind(dados.saudekid, dados.media[1:1826,])

plot(ts(dadoskid, start = c(2015,1), end = c(2020,3), frequency = 365))

summary(dadoskid)

#################### preenchendo NA com o pacote gam
library(splines)
library(foreach)
library(gam)

modelokid <- gam(CID_J ~  co.media + o3.media + so2.media + no2.media  + pm10.media + pm25.media, data = dadoskid, family = poisson, na.action = na.gam.replace)

summary(modelokid)
residuokid <- residuals(modelokid)
plot(residuokid)
acf(residuokid)
qqnorm(residuokid)
qqline(residuokid)
hist(residuokid)

plot.Gam(modelokid)

plot(modelokid)

gam.fit(dadoskid[,5:10], dadoskid$CID_J, smooth.frame= dadoskid$so2.media)



##############################################################################
################# IMPORTANTO OS DADOS DE SAUDE de idosos #####################
##############################################################################
library(readxl)

dados.saudeold<- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/Wilson_SIH_GV_IJ_old.xlsx")
dados.saudeold <- dados.saudeold[1:1826,] ### CONSIDERANDO APENAS O TEMPO DE JAN/15 A DEZ/19

##############################################################################


##############################################################################
################# ARRUMANDO TODOS OS DADOS de idosos    ######################
##############################################################################

dadosold<- cbind(dados.saudeold, dados.media[1:1826,])

plot(ts(dadosold, start = c(2015,1), end = c(2020,3), frequency = 365))

summary(dadosold)

#################### preenchendo NA com o pacote gam
library(splines)
library(foreach)
library(gam)

modeloold <- gam(CID_J ~  co.media + o3.media + so2.media + no2.media  + pm10.media + pm25.media, data = dadosold, family = poisson, na.action = na.gam.replace)

summary(modeloold)
residuoold <- residuals(modeloold)
plot(residuoold)
acf(residuoold)
qqnorm(residuoold)
qqline(residuoold)
hist(residuoold)

plot.Gam(modeloold)

plot(modeloold)

gam.fit(dadosold[,5:10], dadosold$CID_J, smooth.frame= dadosold$so2.media)

