
library(tidyverse)
library(EpiNow2)

### put your data in this format where time to outcome is counted from the time of observation to either time of death, recovery or censoring
# data <- data.frame(time_to_outcome = c(3, 5, 8, 2, 6, 8),
#                    outcome = c("alive", "dead", "censored", "alive", "dead", "censored"))

# generate some data in the format above
n <- 100
data <- data.frame(time_to_outcome = round(runif(n,1,15)),
                   outcome = sample(x = c("alive", "dead"),
                                    size = n,
                                    replace = TRUE,
                                    prob = c(0.8,0.2))) %>%
  mutate(outcome = case_when(
    time_to_outcome > 10 ~ "censored",
    .default = outcome
  ))


## utils in separate file
source("R/cfr_utils.R")

###
# in this example 2 alive, 2 dead and 2 unknown both censored at same time
# so CFR estimate should always be between 2/6 and 4/6

# if time to both outcome is the same then we would infer that 1 of the 2 censored
# individuals is dead the other is alive, i.e. CFR is 50% (3 deaths, 3 alive)

# is time to death is much longer than we would expect a CFR closer to the upper bound of 4/6

# ### change the parameters of these to be what you want
# mean_O2R <- 5
# sd_O2R <- 2
# f_O2R <- function(x) dlnorm(x, mean = mean_O2R, sd = sd_O2R)
# F_O2R <- function(x) plnorm(x, mean = mean_O2R, sd = mean_O2R)

# mean_O2D <- 2 #changing this changes massively the estimate in line with what we expect (see above)
# sd_O2D <- 2
# f_O2D <- function(x, mean_O2D, sd_O2D) dlnorm(x, mean = mean_O2D, sd = sd_O2D)
# F_O2D <- function(x, mean_O2D, sd_O2D) plnorm(x, mean = mean_O2D, sd = mean_O2D)

## define the likelihood function as in Ghani et al.

# likelihood <- function(data, CFR, mean_O2D, sd_O2D) {
#   ## ideally add something to check that data is in correct format
#
#   ## exclude unreasonable CFR values
#   if(CFR < 0 || CFR > 1)
#   {
#     res <- -Inf
#   } else {
#
#     ## create placeholder for individual contributions to likelihood
#     data$like <- NA
#
#     ## calculate individual likelihoods depending on outcomes
#
#     # likelihood contribution from those who die
#     t <- data$time_to_outcome[data$outcome == "dead"]
#     data$like[data$outcome == "dead"] <- CFR * f_O2D(t, mean = mean_O2D,
#                                                      sd = sd_O2D)
#
#     # likelihood contribution from those who survive
#     t <- data$time_to_outcome[data$outcome == "alive"]
#     data$like[data$outcome == "alive"] <- (1-CFR) * f_O2R(t)
#
#     # likelihood contribution from those who are censored
#     t <- data$time_to_outcome[data$outcome == "censored"]
#     data$like[data$outcome == "censored"] <- CFR * (1 - F_O2D(t,mean = mean_O2D,sd = sd_O2D)) + (1-CFR) * (1 - F_O2R(t))
#
#     ## total likelihood is product of individual ones
#     res <- prod(data$like)
#   }
#
#   res
# }

# ## Find the maximum likelihood
#
# ## For visualisation
# cfr <- seq(0, 1, 0.01)
# like <- sapply(cfr, function(c) likelihood(data,c,mean_O2D = 1,sd_O2D = 1))
# #plot(cfr, like, type = "l")
#
# ## MLE
# cfr_ml <- optimize(likelihood, mean = mean_O2D, sd = sd_O2D, data = data, interval = c(0, 1), maximum = TRUE)$maximum
#points(cfr_ml, likelihood(data, cfr_ml), col = "red")

## repeat the calculation under different estimates of O2D distribution




mean_O2R <- convert_to_logmean(5,2)
sd_O2R <- convert_to_logsd(5,2)

get_cfr(data = data, mean_O2D = 5, sd_O2D = 2)
get_cfr(data = data, mean_O2D = log(5), sd_O2D = log(2))

get_cfr(data = data,
        mean_O2D = convert_to_logmean(5,1),
        sd_O2D = convert_to_logsd(5,1))
#get_cfr(data = data, mean_O2D = log(5), sd_O2D = log(2))
get_cfr(data = data, mean_O2D = 5, sd_O2D = 1)





get_cfr(data = data, mean_O2D = 1, sd_O2D = 1)
get_cfr(data = data, mean_O2D = 2, sd_O2D = 2)

get_cfr(data = data, mean_O2D = 5, sd_O2D = 2)
get_cfr(data = data, mean_O2D = 36, sd_O2D = 2)





## Find the 95% CIs by using likelihood ratio test

likelihood_ratio <- function(data, CFR, mle_loglik) {
  logL_cfr <- log(likelihood(data, CFR))
  -2 * (logL_cfr - mle_loglik)
}
mle_loglik <- log(likelihood(data, cfr_ml))
critical_value <- qchisq(0.95, df = 1)

lower_bound <- uniroot(function(CFR) likelihood_ratio(data, CFR, mle_loglik) - critical_value,
                       lower = 0, upper = cfr_ml)$root

upper_bound <- uniroot(function(CFR) likelihood_ratio(data, CFR, mle_loglik) - critical_value,
                       lower = cfr_ml, upper = 1)$root

abline(v = lower_bound, col = "red", lty = 2)
abline(v = upper_bound, col = "red", lty = 2)

cat("CFR estimates with 95% CI:", cfr_ml, "(", lower_bound, "-", upper_bound, ")\n")

