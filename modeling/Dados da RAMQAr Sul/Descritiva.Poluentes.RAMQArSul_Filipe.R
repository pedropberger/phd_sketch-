##############################################################################
################# IMPORTANTO OS DADOS DE POLUICAO ############################
##############################################################################
library(readxl)

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQArSul/')

list.files()

dados.anchieta <- read_excel("RAMQAR - ANCHIETA.xlsx")

dados.belo <- read_excel("RAMQAR - BELO HORIZONTE.xlsx")

dados.guanabara <- read_excel("RAMQAR - GUANABARA.xlsx")

dados.mae <- read_excel("RAMQAR - MAEMBA.xlsx")

dados.meaipe <- read_excel("RAMQAR - MEAIPE.xlsx")

dados.ubu <- read_excel("RAMQAR - UBU.xlsx")

dados.data <- read_excel("Data.xlsx")
 

##############################################################################
################# ARRUMANDO OS DADOS DE ANCHIETA ############################
##############################################################################

summary(dados.anchieta[, c(4,6,8,10,11)])

co.anchieta <- dados.anchieta$`Monoxido de Carbono`

no2.anchieta <- dados.anchieta$`Dioxido de Nitrogenio`

so2.anchieta <- dados.anchieta$`Dioxido de Enxofre`

pm10.anchieta <- dados.anchieta$`Partículas Inalaveis (<10µm)`

pm25.anchieta <- dados.anchieta$`Particulas Inalaveis(<2,5µm)`


##############################################################################
################# ARRUMANDO OS DADOS DE BELO HORIZONTE ########################
##############################################################################

summary(dados.belo[,c(2,3,5,7,9,10)])

co.belo <- dados.belo$`Monoxido de Carbono`

o3.belo <- dados.belo$Ozonio

no2.belo <- dados.belo$`Dioxido de Nitrogenio`

so2.belo <- dados.belo$`Dioxido de Enxofre`

pm10.belo <- dados.belo$`Particulas Inalaveis (<10µm)`

pm25.belo <- dados.belo$`Particulas Inalaveis(<2,5µm)`


##############################################################################
################# ARRUMANDO OS DADOS DE GUANABARA     ########################
##############################################################################

summary(dados.guanabara)

co.guanabara <- dados.guanabara$`Monoxido de Carbono`

no2.guanabara <- dados.guanabara$`Dioxido de Nitrogenio`

so2.guanabara <- dados.guanabara$`Dioxido de Enxofre`

pm25.guanabara <- dados.guanabara$`Particulas Inalaveis(<2,5µm)`


##############################################################################
################# ARRUMANDO OS DADOS DE MAE B?  ##############################
##############################################################################

summary(dados.mae[c(3,6)])

no2.mae <- dados.mae$`Dioxido de Nitrogenio`

pm10.mae <- dados.mae$`Particulas Inalaveis (<10µm)` 

##############################################################################
################# ARRUMANDO OS DADOS DE MEAIPE ###############################
##############################################################################

summary(dados.meaipe[,c(2,4,6,8,9)])

co.meaipe <- dados.meaipe$`Monoxido de Carbono`

no2.meaipe <- dados.meaipe$`Dioxido de Nitrogenio`

so2.meaipe <- dados.meaipe$`Dioxido de Enxofre`

pm10.meaipe <- dados.meaipe$`Particulas Inalaveis (<10µm)`

pm25.meaipe <- dados.meaipe$`Particulas Inalaveis(<2,5µm)`

##############################################################################
################# ARRUMANDO OS DADOS DE UBU  #################################
##############################################################################

summary(dados.ubu[,c(3,5)])

no2.ubu <- dados.ubu$`Dioxido de Nitrogenio`

pm10.ubu <- dados.ubu$`Particulas Inalaveis (<10µm)`


##############################################################################
################# AGRUPANDO OS DADOS POR POLUENTE#############################
##############################################################################


#################### CO ###################################################
dados.co <- data.frame(dados.data, co.anchieta, co.belo, co.guanabara, co.meaipe)

plot(ts(dados.co[,5:8], start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Monoxido de Carbono")


#################### O3 ###################################################
plot(ts(o3.belo, start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Ozonio")

#################### NO2 ###################################################
dados.no2 <- data.frame(dados.data, no2.anchieta, no2.belo, no2.guanabara, no2.mae, 
                        no2.meaipe, no2.ubu)

plot(ts(dados.no2[,-c(1,2,3,4)], start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Di?xido de Nitrog?nio")

#################### SO2 ###################################################
dados.so2 <- cbind(dados.data, so2.anchieta, so2.belo, so2.guanabara, no2.meaipe)

plot(ts(dados.so2[,-c(1,2,3,4)], start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Di?xido de Enxofre")

#################### PM10 ###################################################
dados.pm10 <- cbind(dados.data, pm10.anchieta, pm10.belo, pm10.mae, pm10.meaipe, pm10.ubu)

plot(ts(dados.pm10[,-c(1,2,3,4)], start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Part?culas Inal?veis")

#################### PM2,5 ###################################################
dados.pm25 <- data.frame(dados.data, pm25.anchieta, pm25.belo, pm25.guanabara, pm25.meaipe)

plot(ts(dados.pm25[,-c(1,2,3,4)], start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "Part?culas Respir?veis")



#################### MEDIA DE CADA POLUENTE #############################################
co.media <- rowMeans(dados.co[,-c(1,2,3,4)], na.rm = T)

no2.media <- rowMeans(dados.no2[,-c(1,2,3,4)], na.rm = T)

so2.media <- rowMeans(dados.so2[,-c(1,2,3,4)], na.rm = T)

pm10.media <- rowMeans(dados.pm10[,-c(1,2,3,4)], na.rm = T)

pm25.media <- rowMeans(dados.pm25[,-c(1,2,3,4)], na.rm = T)

dados.media <- data.frame(dados.data, co.media, o3.belo, no2.media, so2.media, pm10.media, pm25.media)

summary(dados.media)

plot(ts(dados.media[,-c(1,2,3,4)], start = c(2015,1), end = c(2020,3), frequency = 365),
     main = "M?dia de cada Poluente")


summary(o3.belo[365:1096])


##############################################################################
################# EXPORTANTO OS DADOS DE POLUICAO ## ############################
#############################################################################
library(writexl)

write_xlsx(dados.media, path = "dados.media.poluente.2015a2020.xlsx")

write_xlsx(dados.pm25, path = "dados.pm25.2015a2020.xlsx")

write_xlsx(dados.pm10, path = "dados.pm10.2015a2020.xlsx")

write_xlsx(dados.so2, path = "dados.so2.2015a2020.xlsx")

write_xlsx(dados.no2, path = "dados.no2.2015a2020.xlsx")

write_xlsx(dados.co, path = "dados.co.2015a2020.xlsx")

write_xlsx(data.frame(dados.data, o3.belo), path = "dados.o3.2015a2020.xlsx")

##############################################################################
################# IMPORTANTO OS DADOS DE SAUDE ############################
##############################################################################
library(readxl)
dados.saude <- read_excel("C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/SIH_SUL.dados.saude.filipe.xlsx")

dados.saude <- dados.saude[1:1826,] ### CONSIDERANDO APENAS O TEMPO DE JAN/15 A DEZ/19


##############################################################################
################# ARRUMANDO TODOS OS DADOS ############################
##############################################################################

dados <- cbind(dados.saude, dados.media[1:1826,])

plot(ts(dados, start = c(2015,1), frequency = 365))

summary(dados)


dados <- na.gam.replace(dados)

#################### preenchendo NA com o pacote gam

modelo <- gam(CID_J ~  co.media + o3.belo + so2.media + no2.media  + pm10.media + pm25.media, data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo)
residuo <- residuals(modelo)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)

plot.Gam(modelo)

plot(modelo)

gam.fit(dados[,5:10], dados$CID_J, smooth.frame= dados$so2.media)

cor.test(dados$CID_J, dados$pm25.media, method = "pearson")

##############################################################################
################# IMPORTANTO OS DADOS DE SAUDE ############################
##############################################################################

modelo <- gam(CID_J ~ co.media + o3.media + no2.media + pm10.media + pm25.media, data = dados)