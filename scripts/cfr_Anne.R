
### put your data in this format where time to outcome is counted from the time of observation to either time of death, recovery or censoring
data <- data.frame(time_to_outcome = c(3, 5, 8, 2, 6, 8),
                   outcome = c("alive", "dead", "censored", "alive", "dead", "censored"))

###
# in this example 2 alive, 2 dead and 2 unknown both censored at same time
# so CFR estimate should always be between 2/6 and 4/6

# if time to both outcome is the same then we would infer that 1 of the 2 censored
# individuals is dead the other is alive, i.e. CFR is 50% (3 deaths, 3 alive)

# is time to death is much longer than we would expect a CFR closer to the upper bound of 4/6

### change the parameters of these to be what you want
shape_O2R <- 1
scale_O2R <- 1
f_O2R <- function(x) dgamma(x, shape = shape_O2R, scale = scale_O2R)
F_O2R <- function(x) pgamma(x, shape = shape_O2R, scale = scale_O2R)

shape_O2D <- 1 #changing this changes massively the estimate in line with what we expect (see above)
scale_O2D <- 1
f_O2D <- function(x) dgamma(x, shape = shape_O2D, scale = scale_O2D)
F_O2D <- function(x) pgamma(x, shape = shape_O2D, scale = scale_O2D)

## define the likelihood function as in Ghani et al.

likelihood <- function(data, CFR) {
  ## ideally add something to check that data is in correct format

  ## exclude unreasonable CFR values
  if(CFR < 0 || CFR > 1)
  {
    res <- -Inf
  } else {

    ## create placeholder for individual contributions to likelihood
    data$like <- NA

    ## calculate individual likelihoods depending on outcomes

    # likelihood contribution from those who die
    t <- data$time_to_outcome[data$outcome == "dead"]
    data$like[data$outcome == "dead"] <- CFR * f_O2D(t)

    # likelihood contribution from those who survive
    t <- data$time_to_outcome[data$outcome == "alive"]
    data$like[data$outcome == "alive"] <- (1-CFR) * f_O2R(t)

    # likelihood contribution from those who are censored
    t <- data$time_to_outcome[data$outcome == "censored"]
    data$like[data$outcome == "censored"] <- CFR * (1 - F_O2D(t)) + (1-CFR) * (1 - F_O2R(t))

    ## total likelihood is product of individual ones
    res <- prod(data$like)
  }

  res
}

## Find the maximum likelihood

## For visualisation
cfr <- seq(0, 1, 0.01)
like <- sapply(cfr, function(c) likelihood(data,c))
plot(cfr, like, type = "l")

## MLE
cfr_ml <- optimize(likelihood, data = data, interval = c(0, 1), maximum = TRUE)$maximum
points(cfr_ml, likelihood(data, cfr_ml), col = "red")

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
