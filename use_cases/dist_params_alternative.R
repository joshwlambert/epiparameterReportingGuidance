### update to link to incubation period


x <- seq(0, 30, length.out = 100)
shape_scale <- stats::dgamma(x = x, shape = 8, scale = 1.5)
shape_rate <- stats::dgamma(x = x, shape = 8, rate = 1.5)
plot(x, shape_rate, type = "l", col = "blue", lwd = 2, ylab = "Density",
     xlab = "Incubation period (days)", main = "Gamma Distribution")
lines(x, shape_scale, col = "red", lwd = 2)
abline(v=8/1.5,lty=2,lwd=2,col="blue")
abline(v=8*1.5,lty=2,lwd=2,col="red")

# meanlog_sdlog <- stats::dlnorm(x = x, meanlog = 0.5, sdlog = 0.5)
# mean_sd <- stats::dlnorm(x = x, meanlog = 1.87, sdlog = 1)
# plot(x, meanlog_sdlog, type = "l", col = "coral", lwd = 2, ylab = "Density",
#      xlab = "x", main = "Lognormal Distribution")
# # Add the Gamma distribution with scale = 2
# lines(x, mean_sd, col = "green", lwd = 2)
