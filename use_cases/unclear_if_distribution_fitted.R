## Issue: Authors report summary statistics without making it clear whether
## these are empirical estimates or have been estimated from a fitted 
## distribution.
## Downstream implication: If the summary statistics are not from the fitted
## distribution, using them as if they were can lead to incorrect conclusions.
## Illustration: We simulate data from a gamma distribution and fit gamma,
## log-normal, and Weibull distributions to the data. We then calculate the
## 99th percentile the fitted distributions and illustrate that even where the 
## mean and standard deviation of the fitted distributions are close to the
## simulated data, the 99th percentile can be quite different. 

library(ggplot2)
library(fitdistrplus)

# Set the mean and standard deviation for the simulation
mean_val <- 12.91
sd_val <- 7

# Number of data points to simulate
n <- 200

# Calculate Gamma distribution parameters from mean and sd
shape_gamma <- (mean_val / sd_val)^2
rate_gamma <- mean_val / (sd_val^2)

# Simulate data from a gamma distribution
set.seed(123)
gamma_data <- rgamma(n, shape = shape_gamma, rate = rate_gamma)

# Fit Gamma, Log-normal, and Weibull distributions to the simulated data
fit_gamma <- fitdist(gamma_data, "gamma")
fit_lognorm <- fitdist(gamma_data, "lnorm")
fit_weibull <- fitdist(gamma_data, "weibull")

# Define a range for x values (the range of the simulated data)
x_vals <- seq(min(gamma_data), max(gamma_data), length.out = 1000)

# Plot histogram and fitted densities
ggplot(data.frame(x = gamma_data), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.5, boundary = 0) +
  stat_function(fun = dgamma, args = list(shape = fit_gamma$estimate[1], 
   rate = fit_gamma$estimate[2]), 
                col = "blue", lwd = 1) +
  stat_function(fun = dlnorm, 
    args = list(meanlog = fit_lognorm$estimate[1], sdlog = fit_lognorm$estimate[2]), 
                col = "green", lwd = 1) +
  stat_function(fun = dweibull, 
    args = list(shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]), 
                col = "red", lwd = 1) +
  ggtitle("Gamma vs Log-normal vs Weibull Distribution Fit") +
  theme_minimal() +
  xlab("Simulated Data") +
  ylab("Density") +
  xlim(min(x_vals), max(x_vals))  # Define limits for x based on data range

qgamma(0.99, shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2])
qlnorm(0.99, meanlog = fit_lognorm$estimate[1], sdlog = fit_lognorm$estimate[2])    
qweibull(0.99, shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2])

mean(rgamma(1000, shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]))
mean(rlnorm(1000, meanlog = fit_lognorm$estimate[1], sdlog = fit_lognorm$estimate[2]))
mean(rweibull(1000, shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]))

