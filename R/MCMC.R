#' Functions to run an MCMC to estimate the parameters of a gamma distribution
#'
#' @description These function enable us to fit a gamma distribution to data
#'
#' @details NOTE EVERYWHERE WE WORK ON THE LOG SCALE
#'
#' @param i stub
#' @param dat stub
#' @param shape stub
#' @param scale stub
#' @param dat stub
#' @param shape stub
#' @param scale stub
#' @param param stub
#' @param mean_prior stub
#' @param shape stub
#' @param scale stub
#' @param mean_prior_shape stub
#' @param mean_prior_scale stub
#' @param dat stub
#' @param shape stub
#' @param scale stub
#' @param mean_prior_shape stub
#' @param mean_prior_scale stub
#' @param curr_val_shape stub
#' @param curr_val_scale stub
#' @param sdlog stub
#' @param dat stub
#' @param mean_prior_shape stub
#' @param mean_prior_scale stub
#' @param curr_val_shape stub
#' @param curr_val_scale stub
#' @param sdlog stub
#' @param dat stub
#' @param mean_prior_shape stub
#' @param mean_prior_scale stub
#'
#' @name MCMC
#'
#' @author Anne Cori & Joshua Lambert
#'
#' @return stub
#' @keywords internal
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
  correction <- log(curr_val_shape) - log(new_val_shape) # correction for lognormal distribution
  p_accept <- ratio_post + correction # things are additive here as on log scale
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
  return(c(updated_val_shape,accept))
}

### move scale with a lognormal proposal ###
#' @name MCMC
move_scale <- function(curr_val_shape,
                       curr_val_scale,
                       sdlog,
                       dat,
                       mean_prior_shape=1000,
                       mean_prior_scale=1000) {
  # draw proposed value
  new_val_scale <- rlnorm(1, meanlog = log(curr_val_scale), sdlog = sdlog)

  # calculates probability of acceptance
  ratio_post <- log_posterior(dat, curr_val_shape, new_val_scale, mean_prior_shape, mean_prior_scale) - log_posterior(dat, curr_val_shape, curr_val_scale, mean_prior_shape, mean_prior_scale)
  correction <- log(curr_val_scale) - log(new_val_scale) # correction for lognormal distribution
  p_accept <- ratio_post + correction # things are additive here as on log scale
  if(p_accept>0) {p_accept <- 0}

  # accept/reject step
  tmp <- log(runif(1))
  # accepting with a certain probability
  if (tmp<p_accept) {
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
  return(c(updated_val_scale,accept))
}
