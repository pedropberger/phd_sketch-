#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brazil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Authors: Dennis, filipe, Pedro e Wilson
# Advisor: Valderio Anselmo Reisen 
#===============================================================================
#===============================================================================
# Installing and loading packages
#===============================================================================
#install.packages("devtools")
#install.packages("tidyverse").
install.packages('xlsx')
#devtools::install_github("rfsaldanha/microdatasus")

install.packages('devtools')
install.packages('tidyverse')
install.packages('microdatasus')
install.packages('xlsx')
devtools::install_github("rfsaldanha/microdatasus")


library(devtools)
library(tidyverse)
library(microdatasus)
library(xlsx)

#===============================================================================
# Downloading data from DATASUS
#===============================================================================
data_before <- fetch_datasus(year_start = 2009, month_start = 1, year_end = 2013, month_end = 12, uf = "ES", information_system = "SIH-RD")
data_after <- fetch_datasus(year_start = 2014, month_start = 1, year_end = 2018, month_end = 12, uf = "ES", information_system = "SIH-RD")

#===============================================================================
# Processing data from DATASUS using functions from the microdatasus package
#===============================================================================
data_before_proc <- process_sih(data_before)
data_after_proc <- process_sih(data_after)

#===============================================================================
# choosing the important variables for our study
#===============================================================================
admissions_2009_2013 <- select(data_before_proc, DT_INTER, CGC_HOSP ,N_AIH, ESPEC, CEP, munResNome, NASC, SEXO, DT_SAIDA, DIAG_PRINC, DIAG_SECUN, MUNIC_MOV, COD_IDADE, IDADE, DIAS_PERM, MORTE, HOMONIMO, CID_NOTIF, CNES, CNPJ_MANT, RACA_COR, ETNIA)
admissions_2014_2018 <- select(data_after_proc,  DT_INTER, CGC_HOSP ,N_AIH, ESPEC, CEP, munResNome, NASC, SEXO, DT_SAIDA, DIAG_PRINC, DIAG_SECUN, MUNIC_MOV, COD_IDADE, IDADE, DIAS_PERM, MORTE, HOMONIMO, CID_NOTIF, CNES, CNPJ_MANT, RACA_COR, ETNIA)

#===============================================================================
# Only diseases with CID-10 J00-J99 (Respiratory diseases)
#===============================================================================
admissions_2009_2013_cid_j <- filter(admissions_2009_2013, grepl('J',DIAG_PRINC))
admissions_2014_2018_cid_j <- filter(admissions_2014_2018, grepl('J',DIAG_PRINC))

#===============================================================================
# Only children under 6 years old
#===============================================================================
adm_2009_2013_children_cid_j  <- filter(admissions_2009_2013_cid_j , COD_IDADE=='Meses' | (COD_IDADE=='Anos' & IDADE<=6))
adm_2014_2018_children_cid_j  <- filter(admissions_2014_2018_cid_j , COD_IDADE=='Meses' | (COD_IDADE=='Anos' & IDADE<=6))

#===============================================================================
# filtering by city = VITORIA
#===============================================================================
adm_vit_09_13_children_cid_j  <- filter(admissions_2009_2013_cid_j , munResNome=='Vitória')
adm_vit_14_18_children_cid_j  <- filter(admissions_2014_2018_cid_j , munResNome=='Vitória')

#===============================================================================
# filtering by Region = RGV
#===============================================================================
rgv <-c("Vitória", "Cariacica","Vila Velha","Serra")
adm_RGV_09_13_children_cid_j  <- filter(admissions_2009_2013_cid_j , munResNome==rgv)
adm_RGV_14_18_children_cid_j  <- filter(admissions_2014_2018_cid_j , munResNome==rgv)

#===============================================================================
# counting admissions by date
#===============================================================================
nro_adm_vit_09_13_child_cid_j<-adm_vit_09_13_children_cid_j %>% group_by(DT_INTER) %>% count
nro_adm_vit_14_18_child_cid_j<-adm_vit_14_18_children_cid_j %>% group_by(DT_INTER) %>% count

nro_adm_RGV_09_13_child_cid_j<-adm_RGV_09_13_children_cid_j %>% group_by(DT_INTER) %>% count
nro_adm_RGV_14_18_child_cid_j<-adm_RGV_14_18_children_cid_j %>% group_by(DT_INTER) %>% count

#===============================================================================
# exporting to .csv file
#===============================================================================

# write_csv(nro_adm_vit_09_13_child_cid_j,file="C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_vit_09_13_child_cid_j.csv")
# write_csv(nro_adm_vit_14_18_child_cid_j,file="C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_vit_14_18_child_cid_j.csv")
# 
# write_csv(nro_adm_RGV_09_13_child_cid_j,file="C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_RGV_09_13_child_cid_j.csv")
# write_csv(nro_adm_RGV_14_18_child_cid_j,file="C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_RGV_14_18_child_cid_j.csv")




write_csv(nro_adm_vit_09_13_child_cid_j,file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_vit_09_13_child_cid_j.csv")
write_csv(nro_adm_vit_14_18_child_cid_j,file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_vit_14_18_child_cid_j.csv")

write_csv(nro_adm_RGV_09_13_child_cid_j,file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_RGV_09_13_child_cid_j.csv")
write_csv(nro_adm_RGV_14_18_child_cid_j,file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_RGV_14_18_child_cid_j.csv")

#===============================================================================
# importing to .csv file
#===============================================================================
# nro_adm_vit_09_13_child_cid_j <- read_csv(file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_vit_09_13_child_cid_j.csv")
# nro_adm_vit_14_18_child_cid_j <- read_csv(file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_vit_14_18_child_cid_j.csv")
# 
# nro_adm_RGV_09_13_child_cid_j <- read_csv(file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_RGV_09_13_child_cid_j.csv")
# nro_adm_RGV_14_18_child_cid_j <- read_csv(file="C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/codigos/2021/nro_adm_RGV_14_18_child_cid_j.csv")
# 
# options("scipen"=100, "digits"=4)
#===============================================================================
#                       END OF CODE
#===============================================================================