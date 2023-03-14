####################################
# Power analysis for Africa Bd paper
# Hasan Sulaeman
####################################
# All analysis assumes 0.11 endemic Bd prevalence from Lips et al. (2009)

# Dependencies ----
data = read.csv("Africa_all.csv", header = TRUE)

# Power analysis by country ----
af = aggregate(BdNeg ~ CountryName, data, FUN = sum) 
ri = aggregate(BdDetected ~ CountryName, data, FUN = sum)
ca = aggregate(TotalTested ~ CountryName, data, FUN = sum)
af[,3] = ri[,2]
af[,4] = ca[,2]
names(af)[c(2,3,4)] = c('neg','pos','tot')
af$prob =  dbinom(0, af$tot, .11)
write.csv(af, "PowerByCountry.csv")

# Power analysis by time period ----
af = aggregate(BdNeg ~ TimePeriod1, data = data, FUN = sum)
ri = aggregate(BdDetected ~ TimePeriod1, data = data, FUN = sum)
ca = aggregate(TotalTested ~ TimePeriod1, data = data, FUN = sum)
af[,3] = ri[,2]
af[,4] = ca[,2]
names(af)[c(2, 3, 4)] = c('neg','pos','tot')
af$prob =  dbinom(0, af$tot, .11)
write.csv(af, 'PowerByTime.csv')