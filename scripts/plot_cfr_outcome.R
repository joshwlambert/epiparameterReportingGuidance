# script to plot the impact of onset-to-death distribution on case fatality
# ratio for use case on ambiguously reported onset-to-death distribution
# parameters

library(ggplot2)

# Define initial parameters

# Total reported cases at time 0
initial_onsets <- 100
# True CFR
cfr <- 0.5

# Define time range for the plot
time_range <- 0:30

# Scenario (a): narrow delay distribution (correct interpretation)
meanlog_a <- 1.53
sdlog_a <- 0.38

# Scenario (b): wider delay distribution
meanlog_b <- 1.6 # log(5)
sdlog_b <- 2

# Calculate cumulative proportion with known outcomes for each scenario:
prop_known_a <- plnorm(time_range, meanlog_a, sdlog = sdlog_a)
prop_known_b <- plnorm(time_range, meanlog_b, sdlog = sdlog_b)

# Calculate expected number of known outcomes
known_outcomes_a <- initial_onsets * prop_known_a
known_outcomes_b <- initial_onsets * prop_known_b

# Calculate cumulative fatal outcomes
cumulative_deaths <- initial_onsets * cfr * prop_known_a

events <- data.frame(
  time_range = time_range,
  cumulative_deaths = cumulative_deaths,
  known_outcomes_a = known_outcomes_a,
  known_outcomes_b = known_outcomes_b
)

events <- tidyr::pivot_longer(
  data = events,
  cols = c("cumulative_deaths", "known_outcomes_a", "known_outcomes_b"),
  names_to = "event_type"
)

day <- 10
cfr_df <- data.frame(
  x = day,
  cfr_a = initial_onsets * cfr * plnorm(day, meanlog_a, sdlog = sdlog_a),
  cfr_b = initial_onsets * cfr * plnorm(day, meanlog_b, sdlog = sdlog_b)
)
cfr_df <- tidyr::pivot_longer(
  data = cfr_df,
  cols = c("cfr_a", "cfr_b"),
  names_to = "cfr_type"
)

cfr_outcome_plot <- ggplot2::ggplot(data = events) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = time_range, y = value, colour = event_type),
    linetype = 2
  ) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = 10), colour = "grey") +
  ggplot2::geom_point(
    data = cfr_df,
    mapping = ggplot2::aes(x = x, y = value, colour = cfr_type),
    size = 3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = day + 8,
    y = cfr_df$value[1] - 1, # minus 1 to avoid expected outcomes line
    label = "CFR 50% (onset-death mean = 5, sd = 2)",
    colour = "#4E79A7"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = day + 8,
    y = cfr_df$value[2],
    label = "CFR 30% (onset-death mean = 36, sd = 2)",
    colour = "#E15759"
  ) +
  ggplot2::scale_x_continuous(name = "Day") +
  ggplot2::scale_y_continuous(name = "Number of Events", limits = c(0, 100)) +
  ggplot2::scale_colour_manual(
    name = "Event type",
    labels = c(
      "CFR 50%",
      "CFR 30%",
      "Cumulative deaths",
      "Estimated fatal outcomes (mean = 5, SD = 2)",
      "Estimated fatal outcomes (mean = 36, SD = 2)"
    ),
    values = c("#4E79A7", "#E15759", "black", "#4E79A7", "#E15759")) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3, byrow = TRUE)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path("plots", "cfr_outcome.png"),
  plot = cfr_outcome_plot,
  device = "png",
  width = 200,
  height = 150,
  units = "mm",
  dpi = 300
)
