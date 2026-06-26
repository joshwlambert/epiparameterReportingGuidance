## cfr_utils

f_O2R <- function(x) dlnorm(x, mean = mean_O2R, sd = sd_O2R)
F_O2R <- function(x) plnorm(x, mean = mean_O2R, sd = mean_O2R)

f_O2D <- function(x, mean_O2D, sd_O2D) dlnorm(x, mean = mean_O2D, sd = sd_O2D)
F_O2D <- function(x, mean_O2D, sd_O2D) plnorm(x, mean = mean_O2D, sd = mean_O2D)

likelihood <- function(data, CFR, mean_O2D, sd_O2D) {
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
    data$like[data$outcome == "dead"] <- CFR * f_O2D(t, mean_O2D, sd_O2D)

    # likelihood contribution from those who survive
    t <- data$time_to_outcome[data$outcome == "alive"]
    data$like[data$outcome == "alive"] <- (1-CFR) * f_O2R(t)

    # likelihood contribution from those who are censored
    t <- data$time_to_outcome[data$outcome == "censored"]
    data$like[data$outcome == "censored"] <- CFR * (1 - F_O2D(t, mean_O2D, sd_O2D)) + (1-CFR) * (1 - F_O2R(t))

    ## total likelihood is product of individual ones
    res <- prod(data$like)
  }

  res
}

get_cfr <- function(data,mean_O2D,sd_O2D){

  cfr <- seq(0, 1, 0.01)
  like <- sapply(cfr, function(c) likelihood(data,c, mean_O2D, sd_O2D))
  #plot(cfr, like, type = "l")

  ## MLE
  cfr_ml <- optimize(likelihood, mean = mean_O2D, sd = sd_O2D,
                     data = data, interval = c(0, 1), maximum = TRUE)$maximum

  return(cfr_ml)
}








