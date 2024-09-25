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





#### write up
#Example use case: estimating the upper quantile of the incubation period distribution
#The incubation period, defined as the time between infection and the development of symptoms, is an important epidemiological quantity for establishing guidance on the duration of contact tracing of identified cases, as well as the duration of  isolation or quarantine for subsequently identified contacts. It is critical that the duration of isolation mitigates the risk of onward transmission, but does not extend any longer than necessary due to the costs, both financial and social, associated with isolation. 
#The incubation period is typically presented as a probability distribution, summarised by a mean and estimate of the variability (most commonly standard deviation), rather than the full distribution. However, this can make estimation of the upper quantile of this distribution, the quantity that is used to guide the aforementioned contact tracing and isolation policies, difficult.
#Consider the following example:
#
#                     Period 1               Period 2
#Mean (Mean, 95% CI) 10.3 (9.9 - 10.7) 11.8 (11.0 - 12.7)
#SD (Mean, 95% CI) 8.2 (7.8 - 8.6) 9.5 (8.7 - 10.5)
#
#Using the method of moments for the gamma distribution, the shape (alpha) and rate (beta) parameters for period 1 are calculated as 1.58 and 0.15, respectively. Parameterising a gamma distribution according to these parameters corresponds to a 95% quantile of 26 days. However, it is not possible to obtain any uncertainty around this estimate without the authors having provided the full set of samples used to generate these estimates.
#Under the same method, the upper 95% quantile for period 2 is 30 days (shape = 1.54; rate = 0.13). 
#Don’t think this would impact on being able to test for statistical significance between the two? Which is what I was hoping to do with the 2 different periods




