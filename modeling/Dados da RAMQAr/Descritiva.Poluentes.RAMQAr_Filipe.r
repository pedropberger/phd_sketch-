##############################################################################
################# IMPORTANTO OS DADOS DE POLUICAO ############################
#############################################################################
library(readxl)

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/DADOS DE POLUICAO/RAMQAr')

list.files()

dados.laranjeiras <- read_excel("RAMQAr 1 - Laranjeiras.xlsx",
                                col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                              "numeric", "numeric","numeric", "numeric", "numeric", 
                                              "numeric", "numeric"))

dados.carapina <- read_excel("RAMQAr 2 - Carapina.xlsx",
                             col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                           "numeric"))

dados.camburi <- read_excel("RAMQAr 3 - Jardim Camburi.xlsx",
                            col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                          "numeric", "numeric","numeric", "numeric", "numeric"))

dados.enseada <- read_excel("RAMQAr 4 - Enseada do Sua.xlsx",
                            col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                          "numeric", "numeric","numeric", "numeric", "numeric", 
                                          "numeric", "numeric","numeric", "numeric", "numeric", 
                                          "numeric"))

dados.vixcentro <- read_excel("RAMQAr 5 - Vitoria Centro.xlsx", 
                              col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                            "numeric", "numeric","numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", "numeric"))

dados.ibes <- read_excel("RAMQAr 6 - Ibes.xlsx",
                         col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                       "numeric", "numeric","numeric", "numeric", "numeric", 
                                       "numeric", "numeric","numeric", "numeric", "numeric", 
                                       "numeric"))

dados.vvcentro <- read_excel("RAMQAr 7 - Vila Velha Centro.xlsx",
                             col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                           "numeric"))

dados.capixaba <- read_excel("RAMQAr 8 - Vila Capixaba.xlsx",
                             col_types = c("date", "numeric", "numeric", "numeric","numeric", 
                                           "numeric", "numeric","numeric", "numeric", "numeric", 
                                           "numeric", "numeric"))

#dados.continental <- read_excel("RAMQAr 9 - Cidade Continental.xlsx",
#                                col_types = c("date", "numeric", "numeric", "numeric","numeric", 
#                                              "numeric", "numeric","numeric"))



##############################################################################
################# ARRUMANDO OS DADOS - LARANJEIRAS ###########################
##############################################################################

pm10.laranjeiras <- dados.laranjeiras$`Partículas Inaláveis (<10µm)`

so2.laranjeiras <- dados.laranjeiras$`Dióxido de Enxofre`

no2.laranjeiras <- dados.laranjeiras$`Dióxido de Nitrogênio`

co.laranjeiras <- dados.laranjeiras$`Monóxido de Carbono`

o3.laranjeiras <- dados.laranjeiras$Ozônio

##############################################################################
################# ARRUMANDO OS DADOS - CARAPINA  ###########################
##############################################################################

pm10.carapina <- dados.carapina$`Partículas Inaláveis (<10µm)`

##############################################################################
################# ARRUMANDO OS DADOS - CAMBURI  ###########################
##############################################################################

pm10.camburi <- dados.camburi$`Partículas Inaláveis (<10µm)`

so2.camburi <- dados.camburi$`Dióxido de Enxofre`

no2.camburi <- dados.camburi$`Dióxido de Nitrogênio`

##############################################################################
################# ARRUMANDO OS DADOS - ENSEADA  ###########################
##############################################################################

pm25.enseada <- dados.enseada$`Partículas Respiráveis (< 2,5µm)`

pm10.enseada <- dados.enseada$`Partículas Inaláveis (<10µm)`

so2.enseada <- dados.enseada$`Dióxido de Enxofre`

no2.enseada <- dados.enseada$`Dióxido de Nitrogênio`

co.enseada <- dados.enseada$`Monóxido de Carbono`

o3.enseada <- dados.enseada$Ozônio


##############################################################################
################# ARRUMANDO OS DADOS - VITORIA CENTRO  #######################
##############################################################################

pm10.vixcentro <- dados.vixcentro$`Partículas Inaláveis (<10µm)`

so2.vixcentro <- dados.vixcentro$`Dióxido de Enxofre`

no2.vixcentro <- dados.vixcentro$`Dióxido de Nitrogênio`

co.vixcentro <- dados.vixcentro$`Monóxido de Carbono`

##############################################################################
################# ARRUMANDO OS DADOS - IBES ###########################
##############################################################################

pm25.ibes <- dados.ibes$`Partículas Respiráveis (< 2.5µm)`

pm10.ibes <- dados.ibes$`Partículas Inaláveis (<10µm)`

so2.ibes <- dados.ibes$`Dióxido de Enxofre`

no2.ibes <- dados.ibes$`Dióxido de Nitrogênio`

co.ibes <- dados.ibes$`Monóxido de Carbono`

o3.ibes <- dados.ibes$Ozônio

##############################################################################
################# ARRUMANDO OS DADOS - VV CENTRO ###########################
##############################################################################

pm10.vvcentro <- dados.vvcentro$`Partículas Inaláveis (<10µm)`

so2.vvcentro <- dados.vvcentro$`Dióxido de Enxofre`

##############################################################################
################# ARRUMANDO OS DADOS - VILA CAPIXABA ##########################
##############################################################################

pm10.capixaba <- dados.capixaba$`Partículas Inaláveis (<10µm)`

so2.capixaba <- dados.capixaba$`Dióxido de Enxofre`

no2.capixaba <- dados.capixaba$`Dióxido de Nitrogênio`

co.capixaba <- dados.capixaba$`Monóxido de Carbono`

o3.capixaba <- dados.capixaba$Ozônio

##############################################################################
################# ARRUMANDO OS DADOS - CIDADE CONTINENTAL #####################
##############################################################################

#pm10.continental <- dados.continental$`Partículas Inaláveis (<10µm)`

#so2.continental <- dados.continental$`Dióxido de Enxofre`

#no2.continental <- dados.continental$`Dióxido de Nitrogênio`


##############################################################################
################# AGRUPANDO OS DADOS POR POLUENTE#############################
##############################################################################
library('dplyr')

ano <- dados.camburi$ano
mes <- dados.camburi$mes
dia <- dados.camburi$dia
#data <- 

dados.data <- data.frame(ano, mes, dia)

#############################################################################
#################### PM25 ###################################################

##### DIÁRIO
dados.pm25 <- data.frame(dados.data, pm25.enseada, pm25.ibes)

plot(ts(dados.pm25[, 4:5], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Material Particulado pm25 (diário)")

#### MENSAL
tabela.pm25.mensal <- group_by(dados.pm25, ano, mes) %>%  summarise(Enseada = mean(pm25.enseada, na.rm = T),
                                                                    Ibes = mean(pm25.ibes, na.rm = T))

plot(ts(tabela.pm25.mensal[,3:4], start = c(2015,1), end = c(2019,12), frequency = 12),
     main = "Material Particulado pm25 (mensal)")

### ANUAL
tabela.pm25.anual <- group_by(dados.pm25, ano) %>%  summarise(Enseada = mean(pm25.enseada, na.rm = T),
                                                              Ibes = mean(pm25.ibes, na.rm = T))

plot(ts(tabela.pm25.anual[,2:3], start = 2015, end = 2019),
     main = "Material Particulado pm25 (anual)")

#############################################################################
#################### PM10 ###################################################

dados.pm10 <- data.frame(dados.data, pm10.laranjeiras, pm10.carapina, pm10.camburi, 
                         pm10.enseada, pm10.vixcentro, pm10.ibes, pm10.vvcentro, 
                         pm10.capixaba 
                         )

plot(ts(dados.pm10[,4:11], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Material Particulado pm10 (diário)")

#### MENSAL
tabela.pm10.mensal <- group_by(dados.pm10, ano, mes) %>%  summarise(Laranjeiras = mean(pm10.laranjeiras, na.rm = T),
                                                                    Carapina = mean(pm10.carapina, na.rm = T),
                                                                    Camburi = mean(pm10.camburi, na.rm = T),
                                                                    Enseada = mean(pm10.enseada, na.rm = T),
                                                                    VIXCentro = mean(pm10.vixcentro, na.rm = T),
                                                                    Ibes = mean(pm10.ibes, na.rm = T),
                                                                    VVCentro = mean(pm10.vvcentro, na.rm = T),
                                                                    Capixaba = mean(pm10.capixaba, na.rm = T)
                                                                    )

plot(ts(tabela.pm10.mensal[,3:10], start = c(2015,1), end = c(2019,12), frequency = 12),
     main = "Material Particulado pm10 (mensal)")

### ANUAL
tabela.pm10.anual <- group_by(dados.pm10, ano) %>%  summarise(Laranjeiras = mean(pm10.laranjeiras, na.rm = T),
                                                              Carapina = mean(pm10.carapina, na.rm = T),
                                                              Camburi = mean(pm10.camburi, na.rm = T),
                                                              Enseada = mean(pm10.enseada, na.rm = T),
                                                              VIXCentro = mean(pm10.vixcentro, na.rm = T),
                                                              Ibes = mean(pm10.ibes, na.rm = T),
                                                              VVCentro = mean(pm10.vvcentro, na.rm = T),
                                                              Capixaba = mean(pm10.capixaba, na.rm = T)
                                                              )

plot(ts(tabela.pm10.anual[,2:9], start = 2015, end = 2019),
     main = "Material Particulado pm10 (anual)")


#############################################################################
#################### SO2 ###################################################

dados.so2 <- data.frame(dados.data, so2.laranjeiras, so2.camburi, so2.enseada, 
                        so2.vixcentro, so2.ibes, so2.vvcentro, so2.capixaba
                        )

plot(ts(dados.so2[,4:10], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Dióxido de Enxofre (diário)")

#### MENSAL
tabela.so2.mensal <- group_by(dados.so2, ano, mes) %>%  summarise(Laranjeiras = mean(so2.laranjeiras, na.rm = T),
                                                                  Camburi = mean(so2.camburi, na.rm = T),
                                                                    Enseada = mean(so2.enseada, na.rm = T),
                                                                    VIXCentro = mean(so2.vixcentro, na.rm = T),
                                                                    Ibes = mean(so2.ibes, na.rm = T),
                                                                    VVCentro = mean(so2.vvcentro, na.rm = T),
                                                                    Capixaba = mean(so2.capixaba, na.rm = T)
                                                                    )

plot(ts(tabela.so2.mensal[,3:9], start = c(2015,1), end = c(2019,12), frequency = 12),
     main = "Dióxido de Enxofre (mensal)")

### ANUAL
tabela.so2.anual <- group_by(dados.so2, ano) %>%  summarise(Laranjeiras = mean(so2.laranjeiras, na.rm = T),
                                                              Camburi = mean(so2.camburi, na.rm = T),
                                                              Enseada = mean(so2.enseada, na.rm = T),
                                                              VIXCentro = mean(so2.vixcentro, na.rm = T),
                                                              Ibes = mean(so2.ibes, na.rm = T),
                                                              VVCentro = mean(so2.vvcentro, na.rm = T),
                                                              Capixaba = mean(so2.capixaba, na.rm = T),
                                                              )

plot(ts(tabela.so2.anual[,2:8], start = 2015, end = 2019),
     main = "Material Particulado pm10 (anual)")


#############################################################################
#################### NO2 ###################################################

dados.no2 <- data.frame(dados.data, no2.laranjeiras, no2.camburi, no2.enseada, no2.vixcentro, 
                        no2.ibes, no2.capixaba
                        )

plot(ts(dados.no2[,4:9], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Dióxido de Nitrogênio (diário)")


#### MENSAL
tabela.no2.mensal <- group_by(dados.no2, ano, mes) %>%  summarise(Laranjeiras = mean(no2.laranjeiras, na.rm = T),
                                                                  Camburi = mean(no2.camburi, na.rm = T),
                                                                    Enseada = mean(no2.enseada, na.rm = T),
                                                                    VIXCentro = mean(no2.vixcentro, na.rm = T),
                                                                    Ibes = mean(no2.ibes, na.rm = T),
                                                                    Capixaba = mean(no2.capixaba, na.rm = T),
                                                                    )

plot(ts(tabela.no2.mensal[,3:8], start = c(2015,1), end = c(2019,12), frequency = 12),
     main = "Dióxido de Nitrogênio (mensal)")

### ANUAL
tabela.no2.anual <- group_by(dados.no2, ano) %>%  summarise(Laranjeiras = mean(no2.laranjeiras, na.rm = T),
                                                              Camburi = mean(no2.camburi, na.rm = T),
                                                              Enseada = mean(no2.enseada, na.rm = T),
                                                              VIXCentro = mean(no2.vixcentro, na.rm = T),
                                                              Ibes = mean(no2.ibes, na.rm = T),
                                                              Capixaba = mean(no2.capixaba, na.rm = T),
                                                              )

plot(ts(tabela.no2.anual[,2:7], start = 2015, end = 2019),
     main = "Dióxido de Nitrogênio (anual)")


#############################################################################
#################### CO ###################################################

dados.co <- data.frame(dados.data, co.laranjeiras, co.enseada, co.vixcentro, co.ibes, co.capixaba)

plot(ts(dados.co[,4:8], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Monóxido de Carbono (diário)")

#### MENSAL
tabela.co.mensal <- group_by(dados.co, ano, mes) %>%  summarise(Laranjeiras = mean(co.laranjeiras, na.rm = T),
                                                                    Enseada = mean(co.enseada, na.rm = T),
                                                                    VIXCentro = mean(co.vixcentro, na.rm = T),
                                                                    Ibes = mean(co.ibes, na.rm = T),
                                                                    Capixaba = mean(co.capixaba, na.rm = T),
                                                                    )

plot(ts(tabela.co.mensal[,3:7], start = c(2015,1), end = c(2019,12), frequency = 12),
     main = "Monóxido de Carbono (mensal)")

### ANUAL
tabela.co.anual <- group_by(dados.co, ano) %>%  summarise(Laranjeiras = mean(co.laranjeiras, na.rm = T),
                                                              Enseada = mean(co.enseada, na.rm = T),
                                                              VIXCentro = mean(co.vixcentro, na.rm = T),
                                                              Ibes = mean(co.ibes, na.rm = T),
                                                              Capixaba = mean(co.capixaba, na.rm = T),
                                                              )

plot(ts(tabela.co.anual[,2:6], start = 2015, end = 2019),
     main = "Monóxido de Carbono (anual)")


#############################################################################
#################### O3 ###################################################

dados.o3 <- data.frame(dados.data, o3.laranjeiras, o3.enseada, o3.ibes, o3.capixaba)

plot(ts(dados.pm10[, 4:7], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Ozônio (diário)")

#### MENSAL
tabela.o3.mensal <- group_by(dados.o3, ano, mes) %>%  summarise(Laranjeiras = mean(o3.laranjeiras, na.rm = T),
                                                                    Enseada = mean(o3.enseada, na.rm = T),
                                                                    Ibes = mean(o3.ibes, na.rm = T),
                                                                    Capixaba = mean(o3.capixaba, na.rm = T),
                                                                    )

plot(ts(tabela.o3.mensal[,3:6], start = c(2015,1), end = c(2019,12), frequency = 12),
     main = "Ozônio (mensal)")

### ANUAL
tabela.o3.anual <- group_by(dados.o3, ano) %>%  summarise(Laranjeiras = mean(o3.laranjeiras, na.rm = T),
                                                              Enseada = mean(o3.enseada, na.rm = T),
                                                              Ibes = mean(o3.ibes, na.rm = T),
                                                              Capixaba = mean(o3.capixaba, na.rm = T),
                                                              )

plot(ts(tabela.o3.anual[,2:5], start = 2015, end = 2019),
     main = "Ozônio (anual)")


#################### MEDIA DE CADA POLUENTE #############################################
pm25 <- rowMeans(dados.pm25[,-c(1,2,3)], na.rm = T)

pm10 <- rowMeans(dados.pm10[,-c(1,2,3)], na.rm = T)

so2 <- rowMeans(dados.so2[,-c(1,2,3)], na.rm = T)

no2 <- rowMeans(dados.no2[,-c(1,2,3)], na.rm = T)

co <- rowMeans(dados.co[,-c(1,2,3)], na.rm = T)

o3 <- rowMeans(dados.o3[,-c(1,2,3)], na.rm = T)

dados.media <- data.frame(dados.data, pm25, pm10, so2, no2, co, o3)

summary(dados.media[,-c(1,2,3)])

plot(ts(dados.media[,-c(1,2,3)], start = c(2015,1), end = c(2019,12), frequency = 365),
     main = "Media de cada Poluente 2015-2019")

##############################################################################
################# EXPORTANTO OS DADOS DE POLUICAO ##############################
#############################################################################
library(writexl)

write_xlsx(dados.media, path = "dados.media.poluente.2015a2019.xlsx")

write_xlsx(dados.pm25, path = "dados.pm25.2015a2019.xlsx")

write_xlsx(dados.pm10, path = "dados.pm10.2015a2019.xlsx")

write_xlsx(dados.so2, path = "dados.so2.2015a2019.xlsx")

write_xlsx(dados.no2, path = "dados.no2.2015a2019.xlsx")

write_xlsx(dados.co, path = "dados.co.2015a2019.xlsx")

write_xlsx(dados.o3, path = "dados.o3.2015a2019.xlsx")
