# script to plot distributions for use case on ambiguously reporting
# distribution type

library(epiparameter)
library(tidyr)
library(ggplot2)
library(cowplot)

mean <- 10
sd <- 2.5
eval <- seq(0, 30, by = 0.1)

gamma_params <- epiparameter::convert_summary_stats_to_params(
  "gamma",
  mean = mean,
  sd = sd
)
lnorm_params <- epiparameter::convert_summary_stats_to_params(
  "lnorm",
  mean = mean,
  sd = sd
)
weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull",
  mean = mean,
  sd = sd
)

gamma_dens <- dgamma(
  eval,
  shape = gamma_params$shape,
  scale = gamma_params$scale
)
lnorm_dens <- dlnorm(
  eval,
  meanlog = lnorm_params$meanlog,
  sdlog = lnorm_params$sdlog
)
weibull_dens <- dweibull(
  eval,
  shape = weibull_params$shape,
  scale = weibull_params$scale
)

dens_df <- data.frame(
  x = eval,
  gamma = gamma_dens,
  lnorm = lnorm_dens,
  weibull = weibull_dens
)

dens_df <- tidyr::pivot_longer(
  data = dens_df,
  cols = c("gamma", "lnorm", "weibull")
)

high_gamma_95q <- qgamma(
  0.95,
  shape = gamma_params$shape,
  scale = gamma_params$scale
)
high_lnorm_95q <- qlnorm(
  0.95,
  meanlog = lnorm_params$meanlog,
  sdlog = lnorm_params$sdlog
)
high_weibull_95q <- qweibull(
  0.95,
  shape = weibull_params$shape,
  scale = weibull_params$scale
)

high_gamma_99q <- qgamma(
  0.99,
  shape = gamma_params$shape,
  scale = gamma_params$scale
)
high_lnorm_99q <- qlnorm(
  0.99,
  meanlog = lnorm_params$meanlog,
  sdlog = lnorm_params$sdlog
)
high_weibull_99q <- qweibull(
  0.99,
  shape = weibull_params$shape,
  scale = weibull_params$scale
)

# plot(gamma_dens, type = "l")
# lines(lnorm_dens, col = "red")
# lines(weibull_dens, col = "blue")
# abline(v = gamma_95q)
# abline(v = lnorm_95q, col = "red")
# abline(v = gamma_95q, col = "blue")

high_mean_sd_ratio <- ggplot2::ggplot(data = dens_df) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = value, col = name)) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = high_gamma_95q),
    linetype = 2,
    col = "#3B9AB2"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = high_lnorm_95q),
    linetype = 2,
    col = "#EBCC2A"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = high_weibull_95q),
    linetype = 2,
    col = "#F21A00"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = high_gamma_99q),
    linetype = 3,
    col = "#3B9AB2"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = high_lnorm_99q),
    linetype = 3,
    col = "#EBCC2A"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = high_weibull_99q),
    linetype = 3,
    col = "#F21A00"
  ) +
  ggplot2::scale_x_continuous(name = "Serial interval (days)") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::scale_color_manual(
    name = "Distribution",
    labels = c("Gamma", "Lognormal", "Weibull"),
    values = c("#3B9AB2", "#EBCC2A", "#F21A00")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

mean <- 5
sd <- 4
eval <- seq(0, 30, by = 0.1)

gamma_params <- epiparameter::convert_summary_stats_to_params(
  "gamma",
  mean = mean,
  sd = sd
)
lnorm_params <- epiparameter::convert_summary_stats_to_params(
  "lnorm",
  mean = mean,
  sd = sd
)
weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull",
  mean = mean,
  sd = sd
)

gamma_dens <- dgamma(
  eval,
  shape = gamma_params$shape,
  scale = gamma_params$scale)
lnorm_dens <- dlnorm(
  eval,
  meanlog = lnorm_params$meanlog,
  sdlog = lnorm_params$sdlog
)
weibull_dens <- dweibull(
  eval,
  shape = weibull_params$shape,
  scale = weibull_params$scale
)

dens_df <- data.frame(
  x = eval,
  gamma = gamma_dens,
  lnorm = lnorm_dens,
  weibull = weibull_dens
)

dens_df <- tidyr::pivot_longer(
  data = dens_df,
  cols = c("gamma", "lnorm", "weibull")
)

low_gamma_95q <- qgamma(
  0.95,
  shape = gamma_params$shape,
  scale = gamma_params$scale
)
low_lnorm_95q <- qlnorm(
  0.95,
  meanlog = lnorm_params$meanlog,
  sdlog = lnorm_params$sdlog
)
low_weibull_95q <- qweibull(
  0.95,
  shape = weibull_params$shape,
  scale = weibull_params$scale
)

low_gamma_99q <- qgamma(
  0.99,
  shape = gamma_params$shape,
  scale = gamma_params$scale
)
low_lnorm_99q <- qlnorm(
  0.99,
  meanlog = lnorm_params$meanlog,
  sdlog = lnorm_params$sdlog
)
low_weibull_99q <- qweibull(
  0.99,
  shape = weibull_params$shape,
  scale = weibull_params$scale
)

low_mean_sd_ratio <- ggplot2::ggplot(data = dens_df) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = value, col = name)) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = low_gamma_95q),
    linetype = 2,
    col = "#3B9AB2"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = low_lnorm_95q),
    linetype = 2,
    col = "#EBCC2A"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = low_weibull_95q),
    linetype = 2,
    col = "#F21A00"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = low_gamma_99q),
    linetype = 3,
    col = "#3B9AB2"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = low_lnorm_99q),
    linetype = 3,
    col = "#EBCC2A"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = low_weibull_99q),
    linetype = 3,
    col = "#F21A00"
  ) +
  ggplot2::scale_x_continuous(name = "Serial interval (days)") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::scale_color_manual(values = c("#3B9AB2", "#EBCC2A", "#F21A00")) +
  ggplot2::theme_bw()

ambiguous_dist_plot <- cowplot::plot_grid(
  high_mean_sd_ratio,
  low_mean_sd_ratio + ggplot2::theme(legend.position="none"),
  nrow = 2,
  labels = c('A', 'B')
)

ggplot2::ggsave(
  file.path("inst", "plots", "ambiguous_dist.png"),
  plot = ambiguous_dist_plot,
  device = "png",
  width = 200,
  height = 150,
  units = "mm",
  dpi = 300
)
