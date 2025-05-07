# script to plot distributions for use case on ambiguously reported
# distribution parameters

library(epiparameter)
library(tidyr)
library(ggplot2)

shape <- 8
scale <- 1.5
rate <- 1.5
eval <- seq(0, 30, by = 0.1)

shape_scale_dens <- dgamma(eval, shape = shape, scale = scale)
shape_rate_dens <- dgamma(eval, shape = shape, rate = rate)

shape_scale_mean <- epiparameter::convert_params_to_summary_stats(
  "gamma",
  shape = shape,
  scale = scale
)$mean
shape_rate_mean <- epiparameter::convert_params_to_summary_stats(
  "gamma",
  shape = shape,
  scale = (1 / scale)
)$mean

dens_df <- data.frame(
  x = eval,
  shape_scale = shape_scale_dens,
  shape_rate = shape_rate_dens
)

dens_df <- tidyr::pivot_longer(
  data = dens_df,
  cols = c("shape_scale", "shape_rate")
)

ambiguous_dist_params_plot <- ggplot2::ggplot(data = dens_df) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = value, col = name)) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = shape_rate_mean),
    linetype = 2,
    col = "#F21A00"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(
      xintercept = shape_scale_mean),
    linetype = 2,
    col = "#3B9AB2"
  ) +
  ggplot2::scale_x_continuous(name = "Incubation period (days)") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::labs(col = "Parameterisation") +
  ggplot2::scale_color_manual(
    values = c("#F21A00", "#3B9AB2"),
    labels = c("Shape + Rate", "Shape + Scale")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

ggplot2::ggsave(
  file.path("inst", "plots", "ambiguous_dist_params.png"),
  plot = ambiguous_dist_params_plot,
  device = "png",
  width = 200,
  height = 150,
  units = "mm",
  dpi = 300
)
