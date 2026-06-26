# script to plot the MCMC estimates for the shape and scale parameters of the
# gamma distribution and show the bias in calculating the distribution mean
# from correlated parameters for use case on convert summary statistics to
# parameters

library(ggplot2)
set.seed(910)
source("R/MCMC.R")

# simulate gamma distributed data
true_shape <- 15
true_scale <- 0.8
dat <- rgamma(n = 30, shape = true_shape, scale = true_scale)
n <- length(dat)


# running the MCMC
shape_init <- 20 # starting from arbitrary value
scale_init <- 5 # starting from arbitrary value
sd_prop_shape <- 0.1 # standard deviation of the proposal distribution for shape
sd_prop_scale <- 0.15 # standard deviation of the proposal distribution for scale
n_iter <- 10000 # run for n_iter iterations
accept_prop_shape <- 0 # will store the proportion of accepted values amongst those proposed for shape
accept_prop_scale <- 0 # will store the proportion of accepted values amongst those proposed for scale
shape_chain <- vector() # where we will store the values of the chain for shape
shape_chain[1] <- shape_init
scale_chain <- vector() # where we will store the values of the chain for scale
scale_chain[1] <- scale_init
loglike_chain <- vector() # where we will store the values of the loglikelihood
loglike_chain[1] <- log_likelihood(dat, shape_chain[1], scale_chain[1])

for (k in 1:(n_iter-1)) {
  print(k)
  # move shape
  tmp <- move_shape(shape_chain[k], scale_chain[k], sd_prop_shape, dat)
  shape_chain[k+1] <- tmp[1]
  accept_prop_shape <- accept_prop_shape + tmp[2]
  # move scale
  tmp <- move_scale(shape_chain[k+1], scale_chain[k], sd_prop_scale, dat)
  scale_chain[k+1] <- tmp[1]
  accept_prop_scale <- accept_prop_scale + tmp[2]
  # recording the likelihood after all moves
  loglike_chain[k+1] <- log_likelihood(dat, shape_chain[k+1], scale_chain[k+1])
}

# turning number of accepted values into proportion
accept_prop_shape <- accept_prop_shape/n_iter
accept_prop_scale <- accept_prop_scale/n_iter

# examining the proportion of acceptance
accept_prop_shape
accept_prop_scale

# plotting the loglikelihood
plot(loglike_chain, type = "l", ylab = "loglikelihood", xlab = "Iterations")

# plotting the output
par(mfrow = c(2, 1))
plot(shape_chain, type = "l", ylab = "shape", xlab = "Iterations")
plot(scale_chain, type = "l", ylab = "scale", xlab = "Iterations")

# how many iterations should be considered as burnin?
burnin <- 1000

# plotting without burnin
shape_chain_no_burnin <- shape_chain[-(1:burnin)]
scale_chain_no_burnin <- scale_chain[-(1:burnin)]
par(mfrow = c(2, 1))
plot(shape_chain_no_burnin,type="l",ylab="shape",xlab="Iterations")
plot(scale_chain_no_burnin,type="l",ylab="scale",xlab="Iterations")

# autocorrelation
acf(shape_chain_no_burnin)
acf(scale_chain_no_burnin)

# correlation
cor <- cor(shape_chain_no_burnin, scale_chain_no_burnin)

gamma_param_samples <- data.frame(
  shape = shape_chain_no_burnin,
  scale = scale_chain_no_burnin
)

gamma_param_cor_plot <- ggplot2::ggplot(data = gamma_param_samples) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = shape, y = scale),
    alpha = 0.5
  ) +
  ggplot2::scale_x_continuous(name = "Shape parameter") +
  ggplot2::scale_y_continuous(name = "Scale parameter") +
  ggplot2::labs(title = paste("Correlation =",signif(cor,2))) +
  ggplot2::theme_bw()

# get mean from posteriors
mean(shape_chain_no_burnin)
mean(scale_chain_no_burnin)

# Calculate plug-in estimate: mean(alpha) * mean(beta)
E_alpha <- mean(shape_chain_no_burnin)
E_beta <- mean(scale_chain_no_burnin)
transform_mean <- E_alpha * E_beta

true_mean_post <- mean(shape_chain_no_burnin * scale_chain_no_burnin)

cat("E[alpha] = ", round(E_alpha, 3), "\n")
cat("E[beta] = ", round(E_beta, 3), "\n")
cat("Transform estimate E[alpha]*E[beta] = ", round(transform_mean, 3), "\n")
cat("Transformed posterior mean E[alpha * beta] = ", round(true_mean_post, 3), "\n")
cat("Bias = ", round(transform_mean - true_mean_post, 3), "\n")

# Assuming shape_chain_no_burnin and scale_chain_no_burnin are already simulated as before
# Compute alpha * beta for each posterior sample
mean_samples <- shape_chain_no_burnin * scale_chain_no_burnin

# Compute summary statistics
posterior_mean <- mean(mean_samples)
transform_estimate <- mean(shape_chain_no_burnin) * mean(scale_chain_no_burnin)

# Create data frame for ggplot
df <- data.frame(gamma_mean = mean_samples)

# Plot histogram and vertical lines
gamma_dist_bias_mean_plot <- ggplot2::ggplot(data = df) +
  ggplot2::geom_histogram(
    ggplot2::aes(x = gamma_mean),
    bins = 60,
    fill = "lightblue",
    col = "white"
  ) +
  ggplot2::geom_vline(
    xintercept = posterior_mean,
    color = "darkgreen",
    linewidth = 1.2
  ) +
  ggplot2::geom_vline(
    xintercept = transform_estimate,
    color = "brown1",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  ggplot2::scale_x_continuous(name = "Mean generation time (days)") +
  ggplot2::scale_y_continuous(name = "Count") +
  ggplot2::labs(
    title = paste(
      "Bias in Estimated Gamma Mean =",
      signif(transform_mean - true_mean_post, 2)
    )
  ) +
  ggplot2::theme_bw()

gamma_dist_bias_mean_plot <- cowplot::plot_grid(
  gamma_param_cor_plot,
  gamma_dist_bias_mean_plot,
  labels = c('A', 'B')
)

ggplot2::ggsave(
  file.path("inst", "plots", "gamma_dist_bias_mean.png"),
  plot = gamma_dist_bias_mean_plot,
  device = "png",
  width = 200,
  height = 100,
  units = "mm",
  dpi = 300
)


## translate into R0
calc_R <- function(r,shape,scale){
  (1 + (r/scale))^shape
}

# growth rate taken from COVID-19 peak in UK Autumn 2020
r <- 0.04

calc_R(r,true_shape,true_scale)
calc_R(r,E_alpha,E_beta)




