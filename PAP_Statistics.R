#################
### PAP Study ###
#################
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
library(easycsv)
library(dplyr)

setwd("C:/Users/bsmea/Documents/USC MS/Research/PAP Study/Results/")

# Converting the xlsx files to csv and deleting the xlsx files
xls = dir(pattern = "xlsx")
created = mapply(convert, xls, gsub("xlsx", "csv", xls))
# unlink(xls) # delete xlsx files


# Pulling all the CMJ files into one data frame
files = list.files(path = "C:/Users/bsmea/Documents/USC MS/Research/PAP Study/Results/",
        pattern = 'CMJ', full.names = TRUE)

CMJ_Data = do.call(rbind, lapply(files, function(x) 
  transform(read.csv(x), File = basename(x))))

# Getting rid of the empty rows and useless rows
CMJ_Data = CMJ_Data[CMJ_Data$...2 != "", ]
CMJ_Data = CMJ_Data[CMJ_Data$...2 != "Name",]

# Separating out the file name
CMJ_Data = CMJ_Data %>% separate(File, c("Name", "Location", "Study", "Test", "Condition"),sep = "_")

# Getting rid of columns
CMJ_Data = CMJ_Data[,-c(1,3,10:14, 16,17)]

# Cleaning the condition
CMJ_Data$Condition = gsub("Cond", "", CMJ_Data$Condition)
CMJ_Data$Condition = gsub(".csv", "", CMJ_Data$Condition)

# Setting the column names
colnames(CMJ_Data) = c("Variable", "Baseline 1", "Baseline 2", "Trial 1", "Trial 2",
                       "Trial 3", "Trial 4", "Name", "Type", "Condition")

# Reshaping the data frame
CMJ_Data = CMJ_Data %>% 
  pivot_longer(cols = c(`Baseline 1`, `Baseline 2`, `Trial 1`, `Trial 2`,
                 `Trial 3`, `Trial 4`), 
               names_to = "Test", values_to = "cases")

CMJ_Data = dcast(CMJ_Data, Name + Condition + Type + Test ~ Variable, value.var = "cases")

CMJ_Data1 = (CMJ_Data[,5:206])
CMJ_Data1 = CMJ_Data[,5:206] %>% mutate_if(is.character,as.numeric)
CMJ_Data1 = as.data.frame(c(CMJ_Data[,1:4], CMJ_Data1))

##################
### Statistics ###
##################

# Creating subsets #
# Creating separation by condition
Cond1 = CMJ_Data[CMJ_Data$Condition == "1",]
Cond2 = CMJ_Data[CMJ_Data$Condition == "2",]
Cond3 = CMJ_Data[CMJ_Data$Condition == "3",]

# Testing for the normality across all variables #
Norm_var1 <- colnames(CMJ_Data[,5:206])
Norm1 = data.frame(Test = Norm_var1, P_Value = 0)
for(i in 1:length(Norm_var1)) 
{
  CMJ_Data[,i+4] = as.numeric(CMJ_Data[,i+4])
  a = shapiro.test(CMJ_Data[,i+4])
  Norm1[i,2] = a$p.value
}



# Testing for the difference between Tests for all variables and all Conditions #
CMJ_var1 <- colnames(CMJ_Data[,5:206])
CMJ1 = data.frame(Test = CMJ_var1, P_Value = 0)
for(i in 1:length(CMJ_var1)) 
{
  if(Norm1[i,2] < 0.05)
  {
    a = kruskal.test(CMJ_Data[,i+4] ~ CMJ_Data$Test, data = CMJ_Data)
    CMJ1[i,2] = a$p.value
  }
  else
  {
    b = summary.aov(aov(CMJ_Data[,i+4] ~ CMJ_Data$Test, data = CMJ_Data))
    CMJ1[i,2] = b[[1]][1,5]
  }
}


# Testing for the normality across all variables in Cond1 #
Norm_var2 <- colnames(Cond1[,5:206])
Norm2 = data.frame(Test = Norm_var2, P_Value = 0)
for(i in 1:length(Norm_var2)) 
{
  Cond1[,i+4] = as.numeric(Cond1[,i+4])
  a = shapiro.test(Cond1[,i+4])
  Norm2[i,2] = a$p.value
}


# Testing for the difference between Tests for all variables in condition 1 #
CMJ_var2 <- colnames(Cond1[,5:206])
CMJ2 = data.frame(Test = CMJ_var2, P_Value = 0)
for(i in 1:length(CMJ_var2)) 
{
  if(Norm2[i,2] < 0.05)
  {
    a = kruskal.test(Cond1[,i+4] ~ Cond1$Test, data = Cond1)
    CMJ2[i,2] = a$p.value
  }
  else
  {
    b = summary.aov(aov(Cond1[,i+4] ~ Cond1$Test, data = Cond1))
    CMJ2[i,2] = b[[1]][1,5]
  }
}


# Testing for the normality across all variables in Cond2 #
Norm_var3 <- colnames(Cond2[,5:206])
Norm3 = data.frame(Test = Norm_var3, P_Value = 0)
for(i in 1:length(Norm_var3)) 
{
  Cond2[,i+4] = as.numeric(Cond2[,i+4])
  a = shapiro.test(Cond2[,i+4])
  Norm3[i,2] = a$p.value
}

# Testing for the difference between Tests for all variables in condition 2 #
CMJ_var3 <- colnames(Cond2[,5:206])
CMJ3 = data.frame(Test = CMJ_var3, P_Value = 0)
for(i in 1:length(CMJ_var3)) 
{
  if(Norm3[i,2] < 0.05)
  {
    a = kruskal.test(Cond1[,i+4] ~ Cond2$Test, data = Cond2)
    CMJ3[i,2] = a$p.value
  }
  else
  {
    b = summary.aov(aov(Cond1[,i+4] ~ Cond2$Test, data = Cond2))
    CMJ3[i,2] = b[[1]][1,5]
  }
}


# Testing for the normality across all variables in Cond3 #
Norm_var4 <- colnames(Cond3[,5:206])
Norm4 = data.frame(Test = Norm_var4, P_Value = 0)
for(i in 1:length(Norm_var4)) 
{
  Cond3[,i+4] = as.numeric(Cond3[,i+4])
  a = shapiro.test(Cond3[,i+4])
  Norm4[i,2] = a$p.value
}

# Testing for the difference between Tests for all variables in condition 3 #
CMJ_var4 <- colnames(Cond3[,5:206])
CMJ4 = data.frame(Test = CMJ_var4, P_Value = 0)
for(i in 1:length(CMJ_var4)) 
{
  a = kruskal.test(Cond3[,i+4] ~ Cond3$Test, data = Cond3)
  CMJ4[i,2] = b$p.value
}


# Testing for the difference between Tests for all variables in condition 3 #
CMJ_var4 <- colnames(Cond3[,5:206])
CMJ4 = data.frame(Test = CMJ_var4, P_Value = 0)
for(i in 1:length(CMJ_var4)) 
{
  a = kruskal.test(Cond3[,i+4] ~ Cond3$Test, data = Cond3)
  CMJ4[i,2] = b$p.value
}

# Testing for the difference between Tests for all variables in condition 3 #
CMJ_var4 <- colnames(Cond3[,5:206])
CMJ4 = data.frame(Test = CMJ_var4, P_Value = 0)
for(i in 1:length(CMJ_var4)) 
{
  a = kruskal.test(Cond3[,i+4] ~ Cond3$Test, data = Cond3)
  CMJ4[i,2] = b$p.value
}





