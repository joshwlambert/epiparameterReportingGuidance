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
  ggplot2::scale_x_continuous(name = "Onset-to-death (days)") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::labs(col = "Parameterisation") +
  ggplot2::scale_color_manual(
    values = c("#F21A00", "#3B9AB2"),
    labels = c("Shape + Rate", "Shape + Scale")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")


# CFR estimate ------------------------------------------------------------

# script for CFR with misinterpreted onset-to-death

# original params shape = 4, scale = 2.5

library(simulist)
library(tidyr)
library(dplyr)
library(incidence2)
library(cfr)
library(ggplot2)

set.seed(1)
linelist <- simulist::sim_linelist(
  onset_to_death = \(x) rgamma(n = x, shape = 8, scale = 1.5),
  hosp_death_risk = 0.5,
  non_hosp_death_risk = 0.5,
  outbreak_size = c(1000, 5000)
)
dim(linelist)

wide_linelist <- linelist |>
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) |>
  dplyr::rename(
    date_death = died,
    date_recovery = recovered
  )
head(wide_linelist)

daily <- incidence2::incidence(
  wide_linelist,
  date_index = c(
    onset = "date_onset",
    hospitalisation = "date_admission",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)
plot(daily)

linelist_list <- list()
date_range <- as.Date(as.Date("2023-03-01"):as.Date("2024-06-01"))

for (i in seq_along(date_range)) {
  linelist_list[[i]] <- simulist::truncate_linelist(
    linelist = linelist,
    truncation_day = date_range[i]
  )
}

daily_incidence_list <- lapply(linelist_list, function(x) {
  x <- x |>
    tidyr::pivot_wider(
      names_from = outcome,
      values_from = date_outcome
    ) |>
    dplyr::rename(
      date_death = died,
      date_recovery = recovered
    )
  daily <- incidence2::incidence(
    x,
    date_index = c(
      onset = "date_onset",
      death = "date_death"
    ),
    interval = "daily",
    complete_dates = TRUE
  )
  daily
})

cfr_data_list <- lapply(daily_incidence_list, function(x) {
  cfr::prepare_data(x, cases_variable = "onset", deaths_variable = "death")
})

cfr <- lapply(cfr_data_list, function(x) {
  cfr::cfr_static(data = x, delay_density = \(y) dgamma(y, shape = 8, scale = 1.5))
})

head(cfr)

cfr <- do.call(rbind, cfr)
cfr$date_range <- date_range
cfr$type <- "true"


# cfr_central <- sapply(cfr, `[[`, "severity_estimate")
# cfr_lower <- sapply(cfr, `[[`, "severity_low")
# cfr_upper <- sapply(cfr, `[[`, "severity_high")
# plot(cfr_central, type = "l", ylim = c(0.2, 0.65))
# polygon(
#   x = c(1:length(cfr), rev(1:length(cfr))),
#   y = c(cfr_lower, rev(cfr_upper)),
#   col = rgb(0, 0, 1, 0.2),
#   border = NA
# )


# next repeat with incorrect distribution ---------------------------------

cfr_bias_O2D <- lapply(cfr_data_list, function(x) {
  cfr::cfr_static(data = x, delay_density = \(y) dgamma(y, shape = 8, rate = 1.5))
})

cfr_bias_O2D <- do.call(rbind, cfr_bias_O2D)
cfr_bias_O2D$date_range <- date_range
cfr_bias_O2D$type <- "bias"

cfr <- rbind(cfr, cfr_bias_O2D)

# number of days since the index case (outbreak_start_date default in
# simulist::sim_linelist() is 2023-01-01)
cfr$days_since_index <- as.numeric(cfr$date_range - as.Date("2023-01-01"))

# cfr_bias_central <- sapply(cfr_bias_O2D, `[[`, "severity_estimate")
# cfr_bias_lower <- sapply(cfr_bias_O2D, `[[`, "severity_low")
# cfr_bias_upper <- sapply(cfr_bias_O2D, `[[`, "severity_high")
# lines(cfr_bias_central, type = "l")
# polygon(
#   x = c(1:length(cfr), rev(1:length(cfr))),
#   y = c(cfr_bias_lower, rev(cfr_bias_upper)),
#   col = rgb(1, 0, 0, 0.2),
#   border = NA
# )

cfr$type <- factor(cfr$type, levels = c("true", "bias"))

# day with the largest absolute difference between the two CFR estimates
max_diff_day <- cfr |>
  dplyr::select(days_since_index, type, severity_estimate) |>
  tidyr::pivot_wider(names_from = type, values_from = severity_estimate) |>
  dplyr::mutate(diff = abs(true - bias)) |>
  dplyr::slice_max(diff, n = 1) |>
  dplyr::pull(days_since_index)

ambiguous_onset_to_death_cfr_plot <- ggplot2::ggplot(data = cfr) +
  ggplot2::geom_vline(
    xintercept = max_diff_day,
    linetype = 2
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = days_since_index,
      y = severity_estimate,
      colour = type
    )
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(
      ymin = severity_low,
      ymax = severity_high,
      x = days_since_index,
      fill = type
    ),
    alpha = 0.2
  ) +
  ggplot2::scale_x_continuous(name = "Number of days since index case") +
  ggplot2::scale_y_continuous(
    name = "Real-time CFR estimate"
  ) +
  ggplot2::scale_colour_manual(
    labels = c("Correct", "Incorrect"),
    values = c("#3B9AB2", "#F21A00")
  ) +
  ggplot2::scale_fill_manual(
    labels = c("Correct", "Incorrect"),
    values = c("#3B9AB2", "#F21A00")
  ) +
  ggplot2::labs(
    colour = "Onset-to-death parameterisation",
    fill = "Onset-to-death parameterisation"
  ) +
  ggplot2::theme_bw() +
  # ggplot2::theme(legend.position = "top")
  ggplot2::theme(legend.position = "none")


# plot(cfr_bias_central - 0.25, type = "l", col = "red")
# lines(cfr_central - 0.25, type = "l", col = "blue")

library(patchwork)

ambiguous_params_plot <- ambiguous_dist_params_plot + ambiguous_onset_to_death_cfr_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(
      legend.position = "bottom"
    ),
    tag_levels = "A"
  )


ggplot2::ggsave(
  file.path("inst", "plots", "ambiguous_dist_params.png"),
  plot = ambiguous_params_plot,
  device = "png",
  width = 250,
  height = 150,
  units = "mm",
  dpi = 300
)
