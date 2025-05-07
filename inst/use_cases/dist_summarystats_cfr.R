
###### Text for case study ######

# Issue: An estimate of delay from disease onset to death is reported in a scientific publication in an ambiguous
# way:

"… the average duration between symptom onset and death of the patients was estimated. A lognormal distribution was fitted to the data. The mean onset-to-death delay was 5 days, with a standard deviation of 2."

# In this case, authors actually mean to report the mean and sd (summary statistics) of a lognormal distribution.
# However, these values could be misinterpreted as distribution parameters for this distribution.
# Misinterpreting the mean as the meanlog of the distribution would yield a mean of 36 days, with a subsequent impact on the estimated delay-adjusted CFR.
# Figure 5 illustrates this issue: during a growing epidemic, using a longer onset-to-death delay severely underestimates the CFR, as cases are wrongly expected to take over 7 times longer to reach a fatal outcome, thus overinflating the denominator.
# With a mean onset-to-death delay of 5 days (correct interpretation), the adjusted CFR is 50%, versus 30% with the incorrect mean of 36 days.

###### R script for case study ######
# Loading required packages
library(cfr)
library(tidyverse)
library(incidence2)
library(data.table)

# Define initial parameters
initial_onsets <- 100    # Total reported cases at time 0
cfr <- 0.5               # True CFR

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

# Create a data frame for the point annotations at day 10
points_data <- tibble(
  day = 10,
  value = c(initial_onsets * cfr * plnorm(10, meanlog_a, sdlog_a),
            initial_onsets * cfr * plnorm(10, meanlog_b, sdlog_b)),
  type = c("Known outcomes - Scenario A (mean=5)",
           "Known outcomes - Scenario B (mean=36)"),
  label = c("CFR 50% (onset-death mean = 5, sd = 2)",
            "CFR 30% (onset-death mean = 36, sd = 2)")
)

# Plot using ggplot2
ggplot(plot_data, aes(x = day, y = value, color = type, linetype = type)) +
  geom_line(size = 0.5) +
  geom_vline(xintercept = 10, linetype = "dotted", color = "grey50") +
  geom_point(data = points_data, aes(x = day, y = value, color = type), size = 3) +
  geom_text(data = points_data, aes(x = day + 0.5, y = value - 0.7, label = label, color = type),
            hjust = 0, size = 4.5, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(
    "Known outcomes - Scenario A (mean=5)" = "blue",
    "Known outcomes - Scenario B (mean=36)" = "red",
    "Cumulative deaths (true CFR)" = "black"
  )) +
  scale_linetype_manual(values = c(
    "Known outcomes - Scenario A (mean=5)" = "dashed",
    "Known outcomes - Scenario B (mean=36)" = "dashed",
    "Cumulative deaths (true CFR)" = "dotted"
  )) +
  labs(
    title = "",
    x = "Day",
    y = "Number of Events",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


