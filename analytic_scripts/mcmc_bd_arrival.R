#######################################################
# Pathogen Arrival MCMC Bayesian Analysis: Bd in Africa
# Hasan Sulaeman
#######################################################

# Dependencies ----
#  Packages ----
library(reshape2)
library(runjags)
library(rjags)

#  Model string ----
model_string = 'model{
    for (ii in 1:nrows) {
        inf[ii]~dbin(p[ii],
        n[ii]) p[ii] <- ppres[ii]*mu 
        ppres[ii] <- step(tst[ii])
        tst[ii]<-time[ii]-arr.time
        } 
    arr.time~dunif(-48,48) mu~dbeta(1,1)
}'

#  Function ----
bd_arrival = function(inf, n, time, nrows, n_iterations, plot_title) {
    # For if plot title isn't given
    if (missing(plot_title)) {
        plot_title = ""
    }

    # For if number of iterations isn't provided, default to 40k total
    if (missing(n_iterations)) {
    n_iterations = 20000
    }

    # Run the model, 40k iterations
    arr.time = jags.model(textConnection(model_string), 
                          data = list('inf' = inf, 'n' = n, 'time' = time, 'nrows' = nrows), 
                          n.chains = 3, n.adapt = 20000)
    arr.time_samples = coda.samples(arr.time, c('arr.time', 'mu'), n.iter = n_iterations)

    # Plot the iterations 
    plot(arr.time_samples, main = plot_title)

    # Print out summary statistics
    summary(arr.time_samples)
    coef(arr.time_samples)

    # Return the results
    return(arr.time_samples)
}

# Arrival in continental Africa ----
mu = read.csv("/Users/hasansulaeman/Dropbox/Africa Bd Project/Africa manuscript/Piovia-Scott Analyses/All of Africa/Africa.csv", header=TRUE)
mu.yr = dcast(data=mu,Year~Disease.Status,value.var='Decade',fun.aggregate=length)
names(mu.yr)[c(2,3)] = c('neg','pos')
names(mu.yr) = tolower(names(mu.yr))
mu.yr$tot = mu.yr$neg+mu.yr$pos
mu$Year = mu$year - min(mu$year)

# Run the model
arrival_africa = bd_arrival(inf = mu$pos, n = mu$n, time = mu$year, nrows = nrow(mu), 
                            plot_title = "Bd arrival: continental Africa")

# Arrival in Cameroon, a country of interest ----
mu.yr = read.csv("/Users/hasansulaeman/Dropbox/Africa Bd Project/Africa manuscript/Piovia-Scott Analyses/ Cameroon/Cameroon.PV.Anal.csv", header=TRUE)
mu.yr = dcast(data = mu, Year ~ Disease.Status, value.var='Decade', fun.aggregate = length)
names(mu.yr)[c(2,3)] = c('neg','pos')
names(mu.yr) = tolower(names(mu.yr))
mu.yr$tot = mu.yr$neg + mu.yr$pos

# Run the model
arrival_cameroon = bd_arrival(inf = mu$pos, n = mu$n, time = mu$year, nrows = nrow(mu), 
                              plot_title = "Bd arrival: Cameroon")

# Arrival in Kenya, a country of interest ----
mu.yr = read.csv("/Users/hasansulaeman/Dropbox/Africa Bd Project/Combined Data Files/Kenya Graph R-script & Files/Kenya.PV.Anal.csv", header=TRUE)
mu.yr = dcast(data = mu, Year ~ Disease.Status, value.var = 'Decade', fun.aggregate = length)
names(mu.yr)[c(2,3)] = c('neg','pos')
names(mu.yr) = tolower(names(mu.yr))
mu.yr$tot = mu.yr$neg + mu.yr$pos
mu.yr$yrc = mu.yr$year - 1931

# Run the model
arrival_kenya = bd_arrival(inf = mu$pos, n = mu$n, time = mu$year, nrows = nrow(mu), 
                           plot_title = "Bd arrival: Kenya")