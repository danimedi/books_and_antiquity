library(googlesheets4)
library(tibble)
library(dplyr)
library(ggplot2)

##################################################
### DOWNLOAD AND ARRANGE DATA FROM DATASET #######
##################################################
data <- read_sheet("https://docs.google.com/spreadsheets/d/1RVD96GNMStJdIvPzOMomu-fg9VY7L6SiAH8a1VFSWGw/edit#gid=0")
1
# characters -> factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           as.factor)

##################################################
################### TABLES #######################
##################################################

# Table 1. Features of the the selected syllabus by universities
table1 <- get_data_features_by(data, "university", "course (original name)", "year of syllabus")

# Table 2. Features of the the selected syllabus by courses
table2 <- get_data_features_by(data, "course", "course (original name)", "year of syllabus")

# Table 3. years of version cited, and latest version (until 2020) in English and Spanish
table3 <- wilcox_compare_by(data, "course", "last year in spanish", "last year in english", "year")

#################################################
################### FIGURES #####################
#################################################

# distribution of years
hist(unlist(data[, "year"]), main = "Distribución en años")

ggplot(data) + 
  geom_density(aes(year, color = course), size=0.8)

ggplot(data) + 
  geom_density(aes(`last year in spanish`, color = course), size=0.8)

ggplot(data) + 
  geom_density(aes(`last year in english`, color = course))


##################################################
############# additional findings ################
##################################################

# total of books included (and percentage)
sum(!is.na(data[,"title"]))
paste(round(mean(!is.na(data[,"title"]))*100, 2), "%")

# for comparisons by nature of courses, lets group by basic sciences and clinical ones:
BS <- data[data$course %in% c("anatomy", "physiology", "microbiology", "pathology"), ]
CS <- data[data$course %in% c("medicine","surgery","pediatrics","ginecology"), ]

############# according to syllabus ################
# antiquity of basic sciences and clinical sciences, according to the syllabuses
bs_data <- unlist(BS[ ,"year"])
cs_data <- unlist(CS[ ,"year"])
wilcox.test(bs_data, cs_data)
# MAD
mad(unlist(data[ ,"year"]), na.rm = T)

################ last edition in Spanish #################
# antiquity of basic sciences and clinical sciences, according to the last editions
bs_data_sp <-unlist(BS[ ,"last year in spanish"])
cs_data_sp <- unlist(CS[ ,"last year in spanish"])
wilcox.test(bs_data_sp, cs_data_sp)
# MAD
mad(unlist(data[ ,"last year in spanish"]), na.rm = T)

################ last edition in English #################
# antiquity of basic sciences and clinical sciences, according to the last editions
bs_data_en <- unlist(data[data$course %in% c("anatomy", "physiology", "microbiology", "pathology"),
                          "last year in english"])
cs_data_en <- unlist(data[data$course %in% c("medicine","surgery","pediatrics","ginecology"),
                          "last year in english"])
wilcox.test(bs_data_en, cs_data_en)
# MAD
mad(unlist(data[ ,"last year in english"]), na.rm = T)

# total median
median(unlist(data[, "year"]), na.rm = T)

# total difference between last version in Spanish and English
(unlist(data[,"last year in spanish"]))
t.test(data[,"last year in spanish"], data[,"last year in english"])

##################################################
##################################################
##################################################

for(i in courses){
print(mean( data[data$course == i, ]$year, na.rm = T ))
}

for(i in courses){
  print( sum(!(is.na( data[data$course == i, ]$year))) )
}

for(i in levels(unlist(data[,"course"]))){
  hist( data[data$course == i, ]$year, breaks = 20, main = i )
}

par(mfrow = c(3,3))
for(i in courses){
  qqnorm( data[data$course == i, ]$year, breaks = 20, main = i )
  qqline( data[data$course == i, ]$year, breaks = 20, main = i )
}

for(i in courses){
  print( wilcox.test( data[data$course == i, ]$`last year in spanish`,
               data[data$course == i, ]$`last year in english` )$p.value )
}
