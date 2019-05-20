# Power Analysis for Africa, by Countries
setwd("/Users/hasansulaeman/Dropbox/Africa Bd Project/Africa manuscript/Summary Analyses")
data = read.csv("Africa_all.csv", header=TRUE)
af=aggregate(BdNeg~CountryName, data=data, FUN=sum)
ri=aggregate(BdDetected~CountryName, data=data, FUN=sum)
ca=aggregate(TotalTested~CountryName, data=data, FUN=sum)
af[,3]=ri[,2]
af[,4]=ca[,2]
names(af)[c(2,3,4)] = c('neg','pos','tot')
af$prob =  dbinom(0,af$tot,.11)
write.csv(af,'PowerByCountry.csv')

# Power Analysis for Africa, by Decades
af=aggregate(BdNeg~TimePeriod1, data=data, FUN=sum)
ri=aggregate(BdDetected~TimePeriod1, data=data, FUN=sum)
ca=aggregate(TotalTested~TimePeriod1, data=data, FUN=sum)
af[,3]=ri[,2]
af[,4]=ca[,2]
names(af)[c(2,3,4)] = c('neg','pos','tot')
af$prob =  dbinom(0,af$tot,.11)
write.csv(af,'PowerByTime.csv')

# Piovia-Scott of All of Africa
af=aggregate(BdNeg~YearCollected, data=data, FUN=sum)
ri=aggregate(BdDetected~YearCollected, data=data, FUN=sum)
ca=aggregate(TotalTested~YearCollected, data=data, FUN=sum)
af[,3]=ri[,2]
af[,4]=ca[,2]
names(af)[c(2,3,4)] = c('neg','pos','tot')

