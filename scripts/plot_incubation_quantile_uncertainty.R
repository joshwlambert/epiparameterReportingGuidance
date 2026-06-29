# ### upper quantile of incubation period

library(ggplot2)
library(matrixStats)
library(epiparameter)
library(tidyr)

mean_c <- 12.6
sd_c <- 13.1

# bounds which we'll use in our simulation to generate the "true" distribution
mean_l <- 9.1
mean_u <- 17.1
sd_l <- 9.6
sd_u <- 19.6

n_samples <- 1000
n_iter <- 100

samples <- matrix(NA, nrow = n_samples, ncol = n_iter)
for (i in 1:n_iter) {

  ## sample a mean & SD
  mean_iter <- rnorm(n = 1, mean = mean_c, sd = (mean_u - mean_l) / 3.92)
  sd_iter <- rnorm(n = 1, mean = sd_c, sd = (sd_u - sd_l) / 3.92)

  # generate parameters accordingly
  param_iter <- convert_summary_stats_to_params(
    x = "gamma", mean = mean_iter, sd = sd_iter
  )

  samples[, i] <- rgamma(
    n = n_samples, shape = param_iter$shape, scale = param_iter$scale
  )
}

## for plotting
samples_longer <- pivot_longer(
  data.frame(samples),
  cols = paste0("X", seq(1:n_iter))
)

q95 <- mean(colQuantiles(samples, probs = 0.95))
l95 <- quantile(colQuantiles(samples, probs = 0.95), 0.025)
u95 <- quantile(colQuantiles(samples, probs = 0.95), 0.975)

incubation_quantile_uncertainty_plot <- ggplot(
  samples_longer,
  aes(x = value, group = name)
) +
  stat_ecdf(geom = "step", col = "grey", alpha = 0.4) +
  geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
  geom_linerange(
    aes(y = 0.95, xmin = l95, xmax = u95),
    col = "#E15759", lwd = 1
  ) +
  geom_point(aes(x = q95, y = 0.95), col = "#4E79A7", size = 3) +
  theme_bw() +
  labs(
    x = "Day",
    y = "Cumulative probability distribution\nof incubation period"
  ) +
  xlim(c(0, 80)) +
  ylim(c(0, 1))

ggplot2::ggsave(
  file.path("plots", "incubation_quantile_uncertainty.png"),
  plot = incubation_quantile_uncertainty_plot,
  device = "png",
  width = 150,
  height = 100,
  units = "mm",
  dpi = 300
)
