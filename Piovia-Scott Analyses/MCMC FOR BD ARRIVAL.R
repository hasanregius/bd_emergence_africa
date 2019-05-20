# Library Setup 
library(reshape2)
library(runjags)
library(rjags)
mu=read.csv("/Users/hasansulaeman/Dropbox/Africa Bd Project/Africa manuscript/Piovia-Scott Analyses/All of Africa/Africa.csv", header=TRUE)

mu.yr = dcast(data=mu,Year~Disease.Status,value.var='Decade',fun.aggregate=length)
names(mu.yr)[c(2,3)] = c('neg','pos')

mu.yr$tot = mu.yr$neg+mu.yr$pos
write.csv(mu.yr,'YearTable.csv')

# Estimate time of arrival
# use jags and rjags

library(runjags)
library(rjags)

mu$Year = mu$Year-min(mu$Year)

# establish data
inf = mu$pos
n = mu$n
time = mu$Year
nrows = nrow(mu)

# create model

model_string = 'model{ for(ii in 1:nrows){ inf[ii]~dbin(p[ii],
                n[ii]) p[ii]<-ppres[ii]*mu ppres[ii]<-step(tst[ii]) 
                tst[ii]<-time[ii]-arr.time} arr.time~dunif(-48,48) mu~dbeta(1,1)}'

arr.time = jags.model(textConnection(model_string), data = list('inf' = inf,
                      'n' = n, 'time' = time,  'nrows'=nrows), n.chains = 3, 
                      n.adapt = 10000)

update(arr.time,10000)

arr.time_samples = coda.samples(arr.time,c('arr.time','mu'),n.iter=20000)

plot(arr.time_samples)

summary(arr.time_samples)
coef(arr.time_samples)


########################################