#' Metropolis-Hastings MCMC for fitting a gamma distribution
#'
#' @description
#' A minimal set of functions implementing a random-walk Metropolis-Hastings
#' MCMC sampler to estimate the shape and scale parameters of a gamma
#' distribution from observed data. The collection provides the building blocks
#' of the sampler: the likelihood, the priors, the (unnormalised) posterior, and
#' the parameter update ("move") steps.
#'
#' @details
#' The data in `dat` are assumed to be independent draws from a gamma
#' distribution with unknown `shape` and `scale`. Each parameter is given an
#' independent, weakly informative exponential prior. New parameter values are
#' proposed on the log scale using a lognormal proposal distribution and are
#' accepted or rejected with the standard Metropolis-Hastings acceptance
#' probability; because the lognormal proposal is asymmetric, a Hastings
#' correction term is included. All densities are computed and combined on the
#' log scale for numerical stability.
#'
#' Everywhere we work on the log scale.
#'
#' @param i Integer index of a single observation in `dat`, selecting the
#'   element whose log-likelihood contribution is computed.
#' @param dat Numeric vector of observed data assumed to follow a gamma
#'   distribution.
#' @param shape Positive numeric shape parameter of the gamma distribution.
#' @param scale Positive numeric scale parameter of the gamma distribution.
#' @param param Numeric parameter value at which the exponential prior density
#'   is evaluated.
#' @param mean_prior Numeric mean of the exponential prior distribution.
#'   Defaults to 1000, giving a near-flat, weakly informative prior.
#' @param mean_prior_shape Numeric mean of the exponential prior on the gamma
#'   shape parameter (default 1000).
#' @param mean_prior_scale Numeric mean of the exponential prior on the gamma
#'   scale parameter (default 1000).
#' @param curr_val_shape Numeric current value of the gamma shape parameter in
#'   the MCMC chain.
#' @param curr_val_scale Numeric current value of the gamma scale parameter in
#'   the MCMC chain.
#' @param sdlog Numeric standard deviation, on the log scale, of the lognormal
#'   proposal distribution used to draw a new parameter value.
#'
#' @return
#' `log_likelihood_i()` and `log_likelihood()` return the numeric log-likelihood
#' of a single observation and of the full data set, respectively.
#' `log_prior_flat_exp()`, `log_prior()` and `log_posterior()` return a numeric
#' log prior or log (unnormalised) posterior density. `move_shape()` and
#' `move_scale()` return a length-2 numeric vector: the updated parameter value
#' followed by an acceptance indicator (1 if the proposal was accepted, 0 if
#' rejected).
#'
#' @name MCMC
#'
#' @author Anne Cori & Joshua Lambert
#'
#' @keywords internal
#' @importFrom stats dexp dgamma dlnorm optimize plnorm rlnorm runif
#' @importFrom utils globalVariables
NULL

# Likelihood function -----------------------------------------------------

#' @name MCMC
log_likelihood_i <- function(i, dat, shape, scale) {
  return(dgamma(dat[i], shape = shape, scale = scale, log = TRUE))
}

### total function ###
#' @name MCMC
log_likelihood <- function(dat, shape, scale) {
  return(sum(dgamma(dat, shape = shape, scale = scale, log = TRUE)))
}

# Priors ------------------------------------------------------------------

# flat exponential prior
#' @name MCMC
log_prior_flat_exp <- function(param, mean_prior = 1000) {
  return(dexp(param, rate = 1 / mean_prior, log = TRUE))
}

### total prior function ###
#' @name MCMC
log_prior <- function(shape,
                      scale,
                      mean_prior_shape = 1000,
                      mean_prior_scale = 1000) {
  return(
    log_prior_flat_exp(shape, mean_prior_shape) +
      log_prior_flat_exp(scale, mean_prior_scale)
  )
}

# Posterior ---------------------------------------------------------------

### total function ###
#' @name MCMC
log_posterior <- function(dat,
                          shape,
                          scale,
                          mean_prior_shape = 1000,
                          mean_prior_scale = 1000) {
  return(
    log_likelihood(dat, shape, scale) +
      log_prior(shape, scale, mean_prior_shape, mean_prior_scale)
  )
}

# Moves -------------------------------------------------------------------

### move shape with a lognormal proposal ###
#' @name MCMC
move_shape <- function(curr_val_shape,
                       curr_val_scale,
                       sdlog,
                       dat,
                       mean_prior_shape = 1000,
                       mean_prior_scale = 1000) {
  # draw proposed value
  new_val_shape <- rlnorm(1, meanlog = log(curr_val_shape), sdlog = sdlog)

  # calculates probability of acceptance
  ratio_post <- log_posterior(
    dat,
    new_val_shape,
    curr_val_scale,
    mean_prior_shape,
    mean_prior_scale
  ) - log_posterior(
    dat,
    curr_val_shape,
    curr_val_scale,
    mean_prior_shape,
    mean_prior_scale
  )
  # correction for lognormal distribution
  correction <- log(curr_val_shape) - log(new_val_shape)
  # things are additive here as on log scale
  p_accept <- ratio_post + correction
  if (p_accept > 0) p_accept <- 0

  # accept/reject step
  tmp <- log(runif(1))
  # accepting with a certain probability
  if (tmp < p_accept) {
    updated_val_shape <- new_val_shape
    accept <- 1
  } else {
    # reject
    updated_val_shape <- curr_val_shape
    accept <- 0
  }

  # return a vector of size 2 where
  #		the first value is the new value in the chain
  #		the second value is 1 if the proposed value was accepted, 0 otherwise
  return(c(updated_val_shape, accept))
}

### move scale with a lognormal proposal ###
#' @name MCMC
move_scale <- function(curr_val_shape,
                       curr_val_scale,
                       sdlog,
                       dat,
                       mean_prior_shape = 1000,
                       mean_prior_scale = 1000) {
  # draw proposed value
  new_val_scale <- rlnorm(1, meanlog = log(curr_val_scale), sdlog = sdlog)

  # calculates probability of acceptance
  ratio_post <- log_posterior(dat, curr_val_shape, new_val_scale, mean_prior_shape, mean_prior_scale) - log_posterior(dat, curr_val_shape, curr_val_scale, mean_prior_shape, mean_prior_scale)
  # correction for lognormal distribution
  correction <- log(curr_val_scale) - log(new_val_scale)
  # things are additive here as on log scale
  p_accept <- ratio_post + correction
  if (p_accept > 0) {
    p_accept <- 0
  }

  # accept/reject step
  tmp <- log(runif(1))
  # accepting with a certain probability
  if (tmp < p_accept) {
    updated_val_scale <- new_val_scale
    accept <- 1
  } else {
    # reject
    updated_val_scale <- curr_val_scale
    accept <- 0
  }

  # return a vector of size 2 where
  #		the first value is the new value in the chain
  #		the second value is 1 if the proposed value was accepted, 0 otherwise
  return(c(updated_val_scale, accept))
}
