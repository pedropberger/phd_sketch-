#===============================================================================
# clean the memory
#===============================================================================
rm(list=ls(all=TRUE))

#===============================================================================
# installing packages
#===============================================================================
install.packages("devtools")
install.packages("tidyverse")
install.packages("microdatasus")
install.packages("xlsx")
install.packages("padr")
install.packages("lubridate")
#install.packages("rJava")


#===============================================================================
# loading packages
#===============================================================================
library(devtools)
library(tidyverse)
library(microdatasus)
#library(rJava)
library(xlsx)
library(padr)
library(lubridate)

#===============================================================================
# Define work directory
#===============================================================================
setwd("C:/Users/pberger/OneDrive/ProjetosR/Dados/")

#===============================================================================
# Downloading data from DATASUS
#===============================================================================

 data_09 <- fetch_datasus(year_start = 2009, month_start = 1, year_end = 2009, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_10 <- fetch_datasus(year_start = 2010, month_start = 1, year_end = 2010, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_11 <- fetch_datasus(year_start = 2011, month_start = 1, year_end = 2011, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_12 <- fetch_datasus(year_start = 2012, month_start = 1, year_end = 2012, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_13 <- fetch_datasus(year_start = 2013, month_start = 1, year_end = 2013, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_14 <- fetch_datasus(year_start = 2014, month_start = 1, year_end = 2014, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_15 <- fetch_datasus(year_start = 2015, month_start = 1, year_end = 2015, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_16 <- fetch_datasus(year_start = 2016, month_start = 1, year_end = 2016, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_17 <- fetch_datasus(year_start = 2017, month_start = 1, year_end = 2017, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_18 <- fetch_datasus(year_start = 2018, month_start = 1, year_end = 2018, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_19 <- fetch_datasus(year_start = 2019, month_start = 1, year_end = 2019, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_20 <- fetch_datasus(year_start = 2020, month_start = 1, year_end = 2020, month_end = 12, uf = "ES", information_system = "SIH-RD")
 data_21 <- fetch_datasus(year_start = 2021, month_start = 1, year_end = 2021, month_end = 10, uf = "ES", information_system = "SIH-RD")
 

 #bind nas bases
 
 data_09_21 <-bind_rows(data_09,data_10,data_11,data_12,data_13,data_14,data_15,data_16,data_17,data_18,data_19,data_20,data_21)

 #salva e carrega - para agilizar as coisas
 
 save(data_09_21, file = "SIH-RD_ES_09_21")

  load("SIH-RD_ES_09_21")

#===============================================================================
# Processing data from DATASUS using functions from the microdatasus package
#===============================================================================
  
  #ajusta os dados para facilitar a leitura de dados do Datasus
 data_09_21_proc <- process_sih(data_09_21)
 
  #salva e carrega
 save(data_09_21_proc, file = "SIH-RD_ES_09_21_proc")

 load("SIH-RD_ES_09_21_proc")

#===============================================================================
# choosing the important variables for our study
#===============================================================================

admissions_09_21 <- select(data_09_21_proc, DT_INTER, CGC_HOSP ,N_AIH, ESPEC, CEP, munResNome, NASC, SEXO, DT_SAIDA, DIAG_PRINC, DIAG_SECUN, MUNIC_MOV, COD_IDADE, IDADE, DIAS_PERM, MORTE, HOMONIMO, CID_NOTIF, CNES, CNPJ_MANT, RACA_COR, ETNIA)

save(admissions_09_21, file = "admissions_09_21")

load("admissions_09_21")

#===============================================================================
# Only diseases with CID-10 J00-J99 (Respiratory diseases)
#===============================================================================

admissions_09_21_cid_j <- filter(admissions_09_21, grepl('J',DIAG_PRINC))

save(admissions_09_21_cid_j, file = "admissions_09_21_cid_j")

load("admissions_09_21_cid_j")

#===============================================================================
# Only children under 6 years old
#===============================================================================

kid_j_under_6_09_21  <-  filter(admissions_09_21_cid_j, (COD_IDADE=='Dias'& IDADE>=28) | COD_IDADE=='Meses' | (COD_IDADE=='Anos' & IDADE<6))

#===============================================================================
# Only children under 12 years old
#===============================================================================


kid_j_under_12_09_21  <-  filter(admissions_09_21_cid_j, (COD_IDADE=='Dias'& IDADE>=28) | COD_IDADE=='Meses' | (COD_IDADE=='Anos' & IDADE<12))


#===============================================================================
# filtering by Region = SUL
#===============================================================================

#mesorregi�o

kid_SUL_j_under_6_09_21  <- filter(kid_j_under_6_09_21, munResNome=="Anchieta"|munResNome=="Guarapari" | munResNome =="Rio Novo do Sul" | munResNome=="Alfredo Chaves" | munResNome=="Iconha" | munResNome=="Piúma")


kid_SUL_j_under_12_09_21  <- filter(kid_j_under_12_09_21, munResNome=="Anchieta"|munResNome=="Guarapari" | munResNome =="Rio Novo do Sul" | munResNome=="Alfredo Chaves" | munResNome=="Iconha" | munResNome=="Piúma")


#===============================================================================
# filtering by Region = RGV
#===============================================================================


kid_RGV_j_under_6_09_21  <- filter(kid_j_under_6_09_21, munResNome=="Cariacica"|munResNome=="Serra"|munResNome=="Vitória"|munResNome=="Vila Velha")

kid_RGV_j_under_12_09_21  <- filter(kid_j_under_12_09_21, munResNome=="Cariacica"|munResNome=="Serra"|munResNome=="Vitória"|munResNome=="Vila Velha")


#===============================================================================
# removing duplicate rows (observations)
#===============================================================================

kid_SUL_j_under_6_09_21 <-unique(kid_SUL_j_under_6_09_21)

kid_SUL_j_under_12_09_21 <-unique(kid_SUL_j_under_12_09_21)

kid_RGV_j_under_6_09_21 <-unique(kid_RGV_j_under_6_09_21)

kid_RGV_j_under_12_09_21 <-unique(kid_RGV_j_under_12_09_21)

#===============================================================================
# counting admissions by date and rename DT_INTER and n
#===============================================================================

NRO_kid_SUL_j_under_6_09_21 <-kid_SUL_j_under_6_09_21 %>% group_by(DT_INTER) %>% count
NRO_kid_SUL_j_under_12_09_21 <-kid_SUL_j_under_12_09_21 %>% group_by(DT_INTER) %>% count

NRO_kid_SUL_j_under_6_09_21 <-rename(NRO_kid_SUL_j_under_6_09_21, Date = DT_INTER, Admissions = n)
NRO_kid_SUL_j_under_12_09_21 <-rename(NRO_kid_SUL_j_under_12_09_21, Date = DT_INTER, Admissions = n)


NRO_kid_RGV_j_under_6_09_21 <-kid_RGV_j_under_6_09_21 %>% group_by(DT_INTER) %>% count
NRO_kid_RGV_j_under_12_09_21 <-kid_RGV_j_under_12_09_21 %>% group_by(DT_INTER) %>% count

NRO_kid_RGV_j_under_6_09_21 <-rename(NRO_kid_RGV_j_under_6_09_21, Date = DT_INTER, Admissions = n)
NRO_kid_RGV_j_under_12_09_21 <-rename(NRO_kid_RGV_j_under_12_09_21, Date = DT_INTER, Admissions = n)

#===============================================================================
# changing Date format and convert to dataframe
#===============================================================================

NRO_kid_SUL_j_under_6_09_21$Date <-as.Date(NRO_kid_SUL_j_under_6_09_21$Date)
NRO_kid_SUL_j_under_12_09_21$Date <-as.Date(NRO_kid_SUL_j_under_12_09_21$Date)

NRO_kid_SUL_j_under_6_09_21 <- as.data.frame(NRO_kid_SUL_j_under_6_09_21)
NRO_kid_SUL_j_under_12_09_21 <- as.data.frame(NRO_kid_SUL_j_under_12_09_21)


NRO_kid_RGV_j_under_6_09_21$Date <-as.Date(NRO_kid_RGV_j_under_6_09_21$Date)
NRO_kid_RGV_j_under_12_09_21$Date <-as.Date(NRO_kid_RGV_j_under_12_09_21$Date)

NRO_kid_RGV_j_under_6_09_21 <- as.data.frame(NRO_kid_RGV_j_under_6_09_21)
NRO_kid_RGV_j_under_12_09_21 <- as.data.frame(NRO_kid_RGV_j_under_12_09_21)


#===============================================================================
# Insert Rows for Missing Dates and fill n with 0 (zeros)
#===============================================================================

NRO_kid_SUL_j_under_6_09_21 <- NRO_kid_SUL_j_under_6_09_21 %>% pad %>% fill_by_value(Admissions)

NRO_kid_SUL_j_under_12_09_21 <- NRO_kid_SUL_j_under_12_09_21 %>% pad %>% fill_by_value(Admissions)


NRO_kid_RGV_j_under_6_09_21 <- NRO_kid_RGV_j_under_6_09_21 %>% pad %>% fill_by_value(Admissions)

NRO_kid_RGV_j_under_12_09_21 <- NRO_kid_RGV_j_under_12_09_21 %>% pad %>% fill_by_value(Admissions)


#save e load

save(NRO_kid_SUL_j_under_6_09_21, file = "NRO_kid_SUL_j_under_6_09_21")
save(NRO_kid_SUL_j_under_12_09_21, file = "NRO_kid_SUL_j_under_12_09_21")

save(NRO_kid_RGV_j_under_6_09_21, file = "NRO_kid_RGV_j_under_6_09_21")
save(NRO_kid_RGV_j_under_12_09_21, file = "NRO_kid_RGV_j_under_12_09_21")


#===============================================================================
# combining right range of dates
#===============================================================================

adm_kid_SUL_j_under_6_10_19 <- NRO_kid_SUL_j_under_6_09_21[NRO_kid_SUL_j_under_6_09_21$Date >= "2010-01-01" & NRO_kid_SUL_j_under_6_09_21$Date <= "2019-12-31",]
adm_kid_SUL_j_under_12_10_19 <- NRO_kid_SUL_j_under_12_09_21[NRO_kid_SUL_j_under_12_09_21$Date >= "2010-01-01" & NRO_kid_SUL_j_under_12_09_21$Date <= "2019-12-31",]

adm_kid_SUL_j_under_6_09_21 <- NRO_kid_SUL_j_under_6_09_21[NRO_kid_SUL_j_under_6_09_21$Date >= "2009-01-01" & NRO_kid_SUL_j_under_6_09_21$Date <= "2021-09-30",]
adm_kid_SUL_j_under_12_09_21 <- NRO_kid_SUL_j_under_12_09_21[NRO_kid_SUL_j_under_12_09_21$Date >= "2009-01-01" & NRO_kid_SUL_j_under_12_09_21$Date <= "2021-09-30",]

adm_kid_RGV_j_under_6_10_19 <- NRO_kid_RGV_j_under_6_09_21[NRO_kid_RGV_j_under_6_09_21$Date >= "2010-01-01" & NRO_kid_RGV_j_under_6_09_21$Date <= "2019-12-31",]
adm_kid_RGV_j_under_12_10_19 <- NRO_kid_RGV_j_under_12_09_21[NRO_kid_RGV_j_under_12_09_21$Date >= "2010-01-01" & NRO_kid_RGV_j_under_12_09_21$Date <= "2019-12-31",]

adm_kid_RGV_j_under_6_09_21 <- NRO_kid_RGV_j_under_6_09_21[NRO_kid_RGV_j_under_6_09_21$Date >= "2009-01-01" & NRO_kid_RGV_j_under_6_09_21$Date <= "2021-09-30",]
adm_kid_RGV_j_under_12_09_21 <- NRO_kid_RGV_j_under_12_09_21[NRO_kid_RGV_j_under_12_09_21$Date >= "2009-01-01" & NRO_kid_RGV_j_under_12_09_21$Date <= "2021-09-30",]


#===============================================================================
# save files
#===============================================================================

save(adm_kid_SUL_j_under_6_10_19, file = "adm_kid_SUL_j_under_6_10_19")
save(adm_kid_SUL_j_under_12_10_19, file = "adm_kid_SUL_j_under_12_10_19")
save(adm_kid_SUL_j_under_6_09_21, file = "adm_kid_SUL_j_under_6_09_21")
save(adm_kid_SUL_j_under_12_09_21, file = "adm_kid_SUL_j_under_12_09_21")

save(adm_kid_RGV_j_under_6_10_19, file = "adm_kid_RGV_j_under_6_10_19")
save(adm_kid_RGV_j_under_12_10_19, file = "adm_kid_RGV_j_under_12_10_19")
save(adm_kid_RGV_j_under_6_09_21, file = "adm_kid_RGV_j_under_6_09_21")
save(adm_kid_RGV_j_under_12_09_21, file = "adm_kid_RGV_j_under_12_09_21")

#=======================END OF CODE=============================================

#Extra

tail(adm_kid_RGV_j_under_12_09_21)
rownames(adm_kid_RGV_j_under_12_09_21) <- NULL

summary(adm_kid_RGV_j_under_12_09_21)
hist(adm_kid_RGV_j_under_12_09_21$Admissions)
