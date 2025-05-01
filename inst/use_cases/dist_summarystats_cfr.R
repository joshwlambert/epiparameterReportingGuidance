
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

# Plot the expected fatal outcomes for scenario (a)
plot(time_range, cumulative_deaths, type = "l", col="black", lty = 2,
     ylim=c(0, 100), xlim=c(0, max(time_range)),
     xlab="Day", ylab="Number of Events",
     main="Impact of Delay Distribution on CFR Estimation",
     yaxt = "n")
axis(2, at = seq(0, 100, by = 10))

# Also add curves for expected fatal outcomes for both interpretations
lines(time_range, known_outcomes_a, col="blue", lty = 2)
lines(time_range, known_outcomes_b, col="red", lty = 2)

# Add a point at day 10 for both scenarios
day <- 10
abline(v = 10, col = "grey", cex = 0.5)
points(day, initial_onsets * cfr * plnorm(day, meanlog_a, sdlog = sdlog_a), col="blue", pch=19)
points(day, initial_onsets * cfr * plnorm(day, meanlog_b, sdlog = sdlog_b), col="red", pch=19)

# Add text labels
text(x = day + 0.5, y = initial_onsets * cfr * plnorm(day, meanlog_a, sdlog = sdlog_a) - 1,
     labels = "CFR 50% (onset-death mean = 5, sd = 2)", col="blue", adj=0)
text(x = day + 0.5, y = initial_onsets * cfr * plnorm(day, meanlog_b, sdlog = sdlog_b) -1,
     labels = "CFR 30% (onset-death mean = 36, sd = 2)", col="red", adj=0)


plot.new()  # start a new plot in the second panel

legend("center",
       legend=c("Estimated fatal outcomes (a): mean = 5 and sd = 2", "Estimated fatal outcomes (b): mean = 36 and sd = 2",
                "Cumulative deaths"),
       col=c("blue", "red", "black"),
       lty=c(3, 3, 3),
       bty="n")




