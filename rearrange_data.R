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
range_write(ss, new_dat)

write_sheet( "selected_universities" , ss = id)

