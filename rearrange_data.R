# first, data of quantity of students was extracted from:
# https://www.sunedu.gob.pe/sibe/
# filters applied (2021-04-27):
#   ESTADO DEL ESTUDIANTE: "MATRICULADOS"
#   NIVEL DE PROGRAMA DE ESTUDIO: "PREGRADO"
#   PERIODO ANUAL: "2016"
#   TIPO DE GESTIÓN: "(All)"
#   UNIVERSIDAD: "(All)"
#   CLASIFICACIÓN PROGRAMAS: "(All)"
#   SEXO: "(All)"
# saved as: "DetPostIngMat_data.csv"

# install.packages("dplyr")
# install.packages("revtools")
# install.packages("googlesheets4")

library(dplyr)
library(googlesheets4)

dat <- read.csv("DetPostIngMat_data.csv", encoding = "UTF-8")
names(dat) <- c("university","gestion","gender","n")
dat <- dat[,c("university","gender","n")]

new_dat <- dat %>% group_by(university) %>% summarise(n = sum(n)) %>% arrange(desc(n))
ss <- gs4_get("https://docs.google.com/spreadsheets/d/13wL3Jbk-FcNpTePgcb10M0Xowj_PKCXSm5vNgQuiN40/edit#gid=0")
range_write(ss, new_dat) # sent to a google sheet for screening

