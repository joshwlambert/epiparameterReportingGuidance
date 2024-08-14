### upper quantile of incubation period

library(tidyverse)

# notes from spreadsheet
# characterise upper quantile e.g. 95% of incubation period to estimate the duration of contact tracing follow up
# Can get a central estimate from the central estimate of mean and central estimate of sd but cannot get uncertainty around that"

## Table S2
## given a mean and SD with uncertainty around it
## overall: mean 10.3 (9.9 - 10.7) SD 8.2 (7.8 - 8.6)

## use method of moments to transform mean and SD into useable format
## shape (alpha)
alpha1 <- (10.3^2)/(8.2^2)

## rate (beta)
beta1 <- 10.3/(8.2^2)

# estimate 95% quantile
qgamma(p=0.95,shape = alpha1, rate = beta1) ## 26.38

## alternative method: sample from gamma distribution and take upper quantile

# sample from gamma distribution
samples1 <- rgamma(n=10000, shape = alpha1, rate = beta1)

# check that things behaving as we would expect
mean(samples1)

quantile(samples1,0.95) # 25.84

## essentially both telling us 26 days but no uncertainty

## second example
alpha2 <- (11.8^2)/(9.5^2)

## rate (beta)
beta2 <- 11.8/(9.5^2)

# estimate 95% quantile
qgamma(p=0.95,shape = alpha2, rate = beta2) ## 26.38

# plot
samples2 <- rgamma(n=10000, shape = alpha2, rate = beta2)


plot(density(samples1))
lines(density(samples2))
plot(pgamma(samples1,shape=alpha1,rate=beta1))

plot(ecdf(samples1),xlab="Time",ylab="Cumulative density",main="",col="blue")
lines(ecdf(samples2),col="red")
legend(60, 0.2, legend=c("Period 1", "Period 2"),
       col=c("blue", "red"), lty=1:1, cex=0.8)
