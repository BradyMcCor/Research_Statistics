#################################
### Nordbord Study Statistics ###
#################################

## Bringing in the data ##
N_Data = read.csv(file.choose())
cor(N_Data$LSI, N_Data$Ham.ECC)
## Testing for normality ##

# Normally Distributed
shapiro.test(N_Data$Hop.R)

# Normally Distributed
shapiro.test(N_Data$Hop.L)

# Normally Distributed
shapiro.test(N_Data$LSI)

# Normally Distributed
shapiro.test(N_Data$Conc.F)

# Normally Distributed
shapiro.test(N_Data$Ecc.F)

# Normally Distributed
shapiro.test(N_Data$Ham.R)

# Normally Distributed
shapiro.test(N_Data$Ham.L)

# Normally Distributed
shapiro.test(N_Data$Ham.ECC)

N_Data = N_Data[,-c(1,6,11)]
## Testing for correlations ##
Cor_Table = as.data.frame(cor(N_Data[1:8]))
