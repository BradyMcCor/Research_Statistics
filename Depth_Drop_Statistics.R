###################################
### Depth Drop Study Statistics ###
###################################

library(mvnormtest)
library(psych)
library(multcomp)
library(purrr)
library(stringr)
library(tibble)
library(reshape2)
library(M2SMF)
library(reshape2)
library(vctrs)
library(data.table)
library(rio)
library(dplyr)
library(plyr)
library(tidyverse)
library(irr)

# Setting the working directory
setwd("C:/Users/bsmea/Documents/USC MS/Research/Depth Jump Study/")

# Converting the xlsx files to csv and deleting the xlsx files
xls = dir(pattern = "xlsx")
created = mapply(convert, xls, gsub("xlsx", "csv", xls))
# unlink(xls) # delete xlsx files

# Pulling all the files in and putting them into a list
Data = list.files(path = "C:/Users/bsmea/Documents/USC MS/Research/Depth Jump Study/", pattern = ".csv")

# ptype dataframe
Ideal = data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24, 
                   row.names = NULL)
colnames(Ideal) = c("X", "Variable", "X2", "30cm_1", "30cm_2",
             "30cm_3", "40cm_1", "40cm_2", "40cm_3", "50cm_1",
             "50cm_2", "50cm_3", "60cm_1", "60cm_2", "60cm_3",
             "70cm_1", "70cm_2", "70cm_3", "X3", "X4", "X5",
             "X6", "X7")
Ideal = Ideal[-c(1),]
Ideal = as.matrix(Ideal)


# Putting all the data from a list to a csv file and adding the file name to the data frame
data = data.frame(filename = Data) %>% 
  mutate(file_contents = map(filename, ~ read_csv(skip = 2, 
  file.path("C:/Users/bsmea/Documents/USC MS/Research/Depth Jump Study/", .))))  
All_Data = unnest(data, cols = c(file_contents), ptype = Ideal)

##########################
### Wrangling the data ###
##########################

# Getting rid of the other empty columns
All_Data = All_Data[,-c(2,4,20:24)]

# Renaming column name to usable name
colnames(All_Data)[2] = "Variable"

# Reorganizing the data to have the force plate variables as column titles
ND = dcast(All_Data, filename ~ Variable, value.var = "")
# ND = reshape2::dcast(All_Data, Variable)
ND = tidyr::spread(All_Data, Variable, )

# Seperating out the file name to seperate columns
ND = ND %>% separate(filename, c("Name", "Type", "Height", "Trial"),sep = "_")

# Cleaning up the new column names
ND$Trial = gsub(".csv", "", ND$Trial)
ND$Trial = gsub("Trial", "", ND$Trial)
ND$Height = gsub("cm", "", ND$Height)

##################
### Statistics ###
##################
EMF_to_CMF = as.data.frame(ND$`Eccentric Mean Force [N]`/ND$`Concentric Mean Force [N]`)
CMF_to_EMF = as.data.frame(ND$`Concentric Mean Force [N]`/ND$`Eccentric Mean Force [N]`)
ND = cbind(ND,EMF_to_CMF, CMF_to_EMF)
colnames(ND)[43] = "EMF_to_CMF"
colnames(ND)[44] = "CMF_to_EMF"

shapiro.test(ND$`RSI (Flight Time/Contact Time)`)
mod4 = wilcox.test(ND$`RSI (Flight Time/Contact Time)` ~ ND$Type)
mod4

shapiro.test(ND$`Peak Power [W]`)
mod5 = wilcox.test(ND$`Peak Power [W]` ~ ND$Type)
mod5

shapiro.test(ND$`Peak Power / BM [W/kg]`)
mod6 = wilcox.test(ND$`Peak Power / BM [W/kg]` ~ ND$Type)
mod6

shapiro.test(ND$`RSI (Flight Time/Contact Time)`)
mod7 = kruskal.test(ND$`RSI (Flight Time/Contact Time)` ~ ND$Height)
mod7

shapiro.test(ND$`Contact Time [s]`)
mod8 = wilcox.test(ND$`Contact Time [s]` ~ ND$Type)
mod8

shapiro.test(ND$`Concentric Mean Force [N]`)
mod9 = wilcox.test(ND$`Concentric Mean Force [N]` ~ ND$Type)
mod9

shapiro.test(ND$`Eccentric Mean Force [N]`)
mod10 = wilcox.test(ND$`Eccentric Mean Force [N]` ~ ND$Type)
mod10

shapiro.test(ND$`Jump Height (Flight Time) [cm]`)
mod11 = wilcox.test(ND$`Jump Height (Flight Time) [cm]` ~ ND$Type)
mod11

shapiro.test(ND$EMF_to_CMF)
mod12 = wilcox.test(ND$EMF_to_CMF ~ ND$Type)
mod12

shapiro.test(ND$CMF_to_EMF)
mod13 = wilcox.test(ND$CMF_to_EMF ~ ND$Type)
mod13

shapiro.test(ND$`RSI (Flight Time/Contact Time)`)
mod13 = wilcox.test(ND$`RSI (Flight Time/Contact Time)` ~ ND$Type)
mod13


Stats_Drop = dplyr::filter(ND, Type == 'Drop')
Stats_Depth = dplyr::filter(ND, Type == 'Depth')

library(data.table)
Peaks_Depth = setDT(Stats_Depth)[, .SD[which.max(`RSI (Flight Time/Contact Time)`)], Name]
Peaks_Drop = setDT(Stats_Drop)[, .SD[which.max(`RSI (Flight Time/Contact Time)`)], Name]


mean(Stats_Depth$`Contact Time [s]`, na.rm = TRUE)
mean(Stats_Drop$`Contact Time [s]`, na.rm = TRUE)
sd(Stats_Depth$`Contact Time [s]`, na.rm = TRUE)
sd(Stats_Drop$`Contact Time [s]`, na.rm = TRUE)

mean(Stats_Depth$`Jump Height (Flight Time) [cm]`, na.rm = TRUE)
mean(Stats_Drop$`Jump Height (Flight Time) [cm]`, na.rm = TRUE)
sd(Stats_Depth$`Jump Height (Flight Time) [cm]`, na.rm = TRUE)
sd(Stats_Drop$`Jump Height (Flight Time) [cm]`, na.rm = TRUE)

mean(Stats_Depth$`Concentric Mean Force [N]`, na.rm = TRUE)
mean(Stats_Drop$`Concentric Mean Force [N]`, na.rm = TRUE)
sd(Stats_Depth$`Concentric Mean Force [N]`, na.rm = TRUE)
sd(Stats_Drop$`Concentric Mean Force [N]`, na.rm = TRUE)

mean(Stats_Depth$`Eccentric Mean Force [N]`, na.rm = TRUE)
mean(Stats_Drop$`Eccentric Mean Force [N]`, na.rm = TRUE)
sd(Stats_Depth$`Eccentric Mean Force [N]`, na.rm = TRUE)
sd(Stats_Drop$`Eccentric Mean Force [N]`, na.rm = TRUE)

mean(Stats_Depth$`RSI (Flight Time/Contact Time)`, na.rm = TRUE)
mean(Stats_Drop$`RSI (Flight Time/Contact Time)`, na.rm = TRUE)
sd(Stats_Depth$`RSI (Flight Time/Contact Time)`, na.rm = TRUE)
sd(Stats_Drop$`RSI (Flight Time/Contact Time)`, na.rm = TRUE)


# Correlation Tables

C1 = cor(ND[5:42], use = "complete.obs")
C1 = as.data.frame(C1)

ND2 = ND[ which(ND$Type=='Drop'), ]
C2 = cor(ND2[5:42], use = "complete.obs")
C2 = as.data.frame(C2)

ND3 = ND[ which(ND$Type=='Depth'), ]
C3 = cor(ND3[5:42], use = "complete.obs")
C3 = as.data.frame(C3)

write.csv(C1,"C:/Users/bsmea/Documents/USC MS/Research/Depth Jump Study/Combined Cor Table.csv", row.names = TRUE)
write.csv(C2,"C:/Users/bsmea/Documents/USC MS/Research/Depth Jump Study/Drop Jump Cor Table.csv", row.names = TRUE)
write.csv(C3,"C:/Users/bsmea/Documents/USC MS/Research/Depth Jump Study/Depth Jump Cor Table.csv", row.names = TRUE)









