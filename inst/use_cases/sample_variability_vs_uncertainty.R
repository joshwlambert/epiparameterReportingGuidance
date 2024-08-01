## Issue: A measure of uncertainty is provided, but it is not clear whether this
## uncertainty is due to sample variability, or is an uncertainty around the 
## estimator.
## Implication: Erronoeus attribution of uncertainty to sample variability or
## vice versa can bias the downstream analysis. This is particularly true when
## the underlying data are not shared.
## Illustration: We illustrate this issue using simulated data from a normal 
## distribution D. We first simulate some data, get the observed mean and sd,
## and the uncertainty around these estimates. We then compare the observed 
## data with that simulated with the erroneous interpretation of the uncertainty.

library(ggplot2)
library(purrr)

### Setting up the data simulation
set.seed(1)
mean_delay <- 15
sd_delay <- 5

sample_size <- list(small = 10, large = 1000)
samples <- map(
  sample_size, ~rnorm(.x, mean = mean_delay, sd = sd_delay)
)

obs_mu <- map_dbl(samples, mean)
obs_sd <- map_dbl(samples, sd)

## Precision of estimates; assume normality so that we can use the exact formula
## precision of the mean: standard error i.e. sd / sqrt(n)
## See following useful reference for SE of the SD
## Standard errors:
## A review and evaluation of standard error estimators using Monte Carlo simulations
## Bradley Harding, Christophe Tremblay, Denis Cousineau
## The Quantitative Methods for Psychology 2014, Vol 10, No 2, First page 107
precision_mu <- map2_dbl(obs_sd, sample_size, ~.x / sqrt(.y))
precision_sd <- map2_dbl(obs_sd, sample_size, ~ .x / sqrt(2 * (.y - 1)))

## Treat precision_mu as sample SD; in downstream analysis, sample size will
## probably be large.
wrong_sample <- rnorm(sample_size$large, mean = mean(.x), sd = .y)

wrong_sample_means <- map_dbl(wrong_samples, mean)
