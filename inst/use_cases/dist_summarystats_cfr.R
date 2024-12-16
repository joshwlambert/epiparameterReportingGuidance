# Use case for reporting guidance paper #2- Mean of the sample reported
# ambiguously so it's confused with the mean of the lognormal distribution

library(simulist)
library(epiparameter)
library(tidyverse)
library(incidence2)
library(cfr)
library(data.table)


# Step 1: Simulating data for the use case with o-d lognormal and CFR of 30%

contact_distribution <- epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 3)
  )
)

ip_COVID <-
  epiparameter(
    disease = "COVID-19",
    epi_name = "infectious period",
    prob_distribution =
      create_prob_distribution(
        prob_distribution = "gamma",
        prob_distribution_params =
          c(shape = 2.6, scale = 1.2)
      )
  )

# True mean and sd- 5 and 1.5 -> meanlog = 1.53 and sdlog = 0.29
# Parameterisation with meanlog

onset_to_death <-
  epiparameter(
    disease = "unknown",
    epi_name = "onset to death",
    summary_stats =
      create_summary_stats(mean = 5, sd = 1.5),
    prob_distribution = "lnorm"
  )

set.seed(123)
linelist_covid <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = ip_COVID,
  prob_infect = 0.7,
  onset_to_hosp = NULL,
  hosp_risk = NULL,
  non_hosp_death_risk = 0.12,
  onset_to_death = onset_to_death,
  outbreak_size = c(3000, 6000)
)

# STEP 2: Aggregation

linelist_covid$date_death <- fifelse(linelist_covid$outcome == "died", linelist_covid$date_outcome, NA)
incidence_COVID <- incidence2::incidence(linelist_covid, date_index = c("date_onset", "date_death"), interval = 1L) %>% complete_dates()

incidence_COVID$date_index <- as.Date(incidence_COVID$date_index)
covid_inc_cfr <- cfr::prepare_data(incidence_COVID, cases_variable = "date_onset", deaths_variable = "date_death")

ggplot(covid_inc_cfr, aes(x = date)) + geom_point(aes(y = cases), colour = "blue") +
  geom_point(aes(y = deaths), colour = "red") + theme_bw() + scale_x_date(date_breaks = "7 days")

# STEP 3: Truncating data

real_time <- "2023-01-12"
incidence_rt_covid <- covid_inc_cfr[covid_inc_cfr$date <= real_time,]

# STEP 4: Creating objects with parameters and summary stats from distribution used to simulate data

true_dist_params <- get_parameters(onset_to_death)
true_dist_summary_stats <- convert_params_to_summary_stats("lnorm", meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]])

# STEP 5: Problem statement

# We want to estimate the delay-adjusted CFR on our own outbreak data, and for this
# we look at the available literature where we find a publication that reports the delay
# from disease onset to death as follows:
# "... the average duration between the time when symptoms first appeared and
# death of the patients was estimated. The mean onset-death delay was of 5 days,
# with a standard deviation of 1.5"

# 5 and 2 are really distribution summary stats

# TRUE CFR (calculated using lognormal parameters that were used to simulate the data)

true_cfr <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]]))
sum(covid_inc_cfr$deaths)/sum(covid_inc_cfr$cases)

# Estimate of known outcomes using true distribution

true_outcomes <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]]))
true_outcomes_n <- round(sum(true_outcomes$estimated_outcomes))

# a) We think they are meanlog and sdlog (calculated taking the provided mean and sd and using them directly as distribution parameters)

cfr_assumed_params <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = 1.53, sdlog = 1.5))
outcomes_assumed_params <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = 1.53, sdlog = 1.5))
assumed_params_outcomes_n <- round(sum(outcomes_assumed_params$estimated_outcomes))

# b) We think they are sample statistics

# b.1) Lognormal to randomly generate sample and lognormal fitted distribution

lnorm_parameters <- convert_summary_stats_to_params("lnorm", mean = 5, sd = 1.5)

lnorm_sample <- rlnorm(n = 500, meanlog = lnorm_parameters$meanlog, sdlog = lnorm_parameters$sdlog)

lnorm_fit <- fitdistrplus::fitdist(data = lnorm_sample, distr = "lnorm")

cfr_sample_lnorm <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = lnorm_fit$estimate[["meanlog"]], sdlog = lnorm_fit$estimate[["sdlog"]]))
outcomes_lnorm_sample <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = lnorm_fit$estimate[["meanlog"]], sdlog = lnorm_fit$estimate[["sdlog"]]))

# b.2) Gamma to randomly generate sample and gamma fitted distribution

gamma_parameters <- convert_summary_stats_to_params("gamma", mean = 5, sd = 1.5)

gamma_sample <- rgamma(n = 500, shape = gamma_parameters$shape, scale = gamma_parameters$scale)

gamma_fit <- fitdistrplus::fitdist(data = gamma_sample, distr = "gamma")

cfr_sample_gamma <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = gamma_fit$estimate[["shape"]], rate = gamma_fit$estimate[["rate"]]))
outcomes_gamma_sample <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = gamma_fit$estimate[["shape"]], rate = gamma_fit$estimate[["rate"]]))

# c) We think they are distribution summary statistics (correct interpretation) but unsure of which distribution

# c.1) Lognormal and fitted lognormal

params_lnorm <- convert_summary_stats_to_params("lnorm", mean = 5, sd = 1.5)
cfr_lnorm_fit <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = params_lnorm$meanlog, sdlog = params_lnorm$sdlog))
outcomes_lnorm_ss <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = params_lnorm$meanlog, sdlog = params_lnorm$sdlog))

# c.2) Lognormal and fitted gamma

params_gamma <- convert_summary_stats_to_params("gamma", mean = 5, sd = 1.5)
cfr_gamma_fit <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = params_gamma$shape, scale = params_gamma$scale))
outcomes_gamma_ss <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = params_gamma$shape, scale = params_gamma$scale))

# c.3) Lognormal and fitted weibull

params_weibull <- convert_summary_stats_to_params("weibull", mean = 5, sd = 1.5)
cfr_weibull_fit <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dweibull(x, shape = params_weibull$shape, scale = params_weibull$scale))
outcomes_weibull_ss <- estimate_outcomes(incidence_rt_covid, delay_density = function(x) dweibull(x, shape = params_weibull$shape, scale = params_weibull$scale))

### Text for case study ####

# Issue: An estimate of delay from disease onset to death is reported in a scientific publication in an ambiguous
# way:

"“… the average duration between the time when symptoms first appeared and death of the patients was estimated. The mean onset-to-death delay was of 14.5 days, with a standard deviation of 6.7.”"

# This statement could be (mis)interpreted in several ways; a) as the mean and standard deviation of the raw data, i.e., as sample statistics;
# b) the mean and standard deviation of a fitted distribution, i.e., as summary statistics; c) as the meanlog and sdlog of a fitted lognormal distribution, i.e., as distribution parameters.
# In this case study, the authors of the paper actually mean to report the summary statistics for a lognormal distribution they fitted to the data.


# Implication: In a scenario in which the case fatality risk (CFR) needs to be calculated for an ongoing, growing disease outbreak, an onset-to-death delay distribution is required
# to calculate an unbiased CFR estimate, due to some individuals being infected but their outcome (i.e recovery or death) is unknown Nishiura et al. (2009). If available data are insufficient to estimate
# the outbreak-specific onset-to-death delay, a previously inferred estimate from the existing literature will need to be used to calculate the CFR.
# If, as above, the reporting of the onset-to-death delay parameter is ambiguous and can be misinterpreted, this will result in a biased CFR estimate.


# Illustration: The CFR calculation for an unbiased estimate requires a parametric probability density/mass function.
# Given the ambiguity we demonstrate the correct interpretation and three misinterpretations of the reported onset-to-death and show how the CFR varies as a result.
# We use the {simulist} and {cfr} R packages to simulate line list data and calculate the CFR, respectively Lambert and Tamayo (2024) and Gupte, Kucharski, and Russell (2024).

# CFR table

CFR_table <- data.table(
  Interpretation = c(
    "True CFR",
    "Lognormal parameters",
    "Summary statistics and lognormal fit",
    "Summary statistics and gamma fit",
    "Summary statistics and weibull fit",
    "Sample statistics and lognormal fit",
    "Sample statistics and gamma fit"
  ),
  `CFR (%) [95% CI]` = c(
    paste0(true_cfr$severity_estimate * 100, " [",
           true_cfr$severity_low * 100, ", ", true_cfr$severity_high * 100, "]"),
    paste0(cfr_assumed_params$severity_estimate * 100, " [",
           cfr_assumed_params$severity_low * 100, ", ", cfr_assumed_params$severity_high * 100, "]"),
    paste0(cfr_lnorm_fit$severity_estimate * 100, " [",
           cfr_lnorm_fit$severity_low * 100, ", ", cfr_lnorm_fit$severity_high * 100, "]"),
    paste0(cfr_gamma_fit$severity_estimate * 100, " [",
           cfr_gamma_fit$severity_low * 100, ", ", cfr_gamma_fit$severity_high * 100, "]"),
    paste0(cfr_weibull_fit$severity_estimate * 100, " [",
           cfr_weibull_fit$severity_low * 100, ", ", cfr_weibull_fit$severity_high * 100, "]"),
    paste0(cfr_sample_lnorm$severity_estimate * 100, " [",
           cfr_sample_lnorm$severity_low * 100, ", ", cfr_sample_lnorm$severity_high * 100, "]"),
    paste0(cfr_sample_gamma$severity_estimate * 100, " [",
           cfr_sample_gamma$severity_low * 100, ", ", cfr_sample_gamma$severity_high * 100, "]")
  )
)

est_outcomes_table <- data.table(
  Interpretation = c(
    "True estimated outcomes",
    "Lognormal parameters",
    "Summary statistics and lognormal fit",
    "Summary statistics and gamma fit",
    "Summary statistics and weibull fit",
    "Sample statistics and lognormal fit",
    "Sample statistics and gamma fit"
  ),
  `Estimated known outcomes` = c(
    paste0(round(sum(true_outcomes$estimated_outcomes))),
    paste0(round(sum(outcomes_assumed_params$estimated_outcomes))),
    paste0(round(sum(outcomes_lnorm_ss$estimated_outcomes))),
    paste0(round(sum(outcomes_gamma_ss$estimated_outcomes))),
    paste0(round(sum(outcomes_weibull_ss$estimated_outcomes))),
    paste0(round(sum(outcomes_lnorm_sample$estimated_outcomes))),
    paste0(round(sum(outcomes_gamma_sample$estimated_outcomes)))
  )
)

# Misinterpreting 14.5 and 6.7 as the meanlog and sdlog of a lognormal distribution creates an extremely wide distribution, with an unrealistic mean of 1e16.
# This implies that most cases take an unreasonably long time to reach an outcome, reducing the number of cases expected to have an outcome during the outbreak.
# Consequently, the number of observed deaths exceeds the number of cases with an expected outcome, resulting in an invalid CFR estimate (e.g., over 100%, shown as NA).
# When interpreting these values as distribution summary statistics, the correct interpretation can analytically convert the mean and standard deviation to the lognormal distribution parameters
# (meanlog = 2.58,  sdlog = 0.43) and parameterise the onset-to-death, resulting in a CFR of 4.67%. Assuming the mean and standard deviation represent summary statistics for other parametric distributions,
# such as the gamma or Weibull distributions, results in a slight overestimation of the "true" CFR, with values of 4.75% and 4.84%, respectively.
# Incorrectly interpreting the mean and standard deviation as empirical values from the available sample, and then fitting a gamma and lognormal distribution, results in erroneous CFR estimates.
# If the reported estimates are assumed to be sample summary statistics, we test the assumption that the distribution is lognormal (correct) and gamma (incorrect).
# The assumed parametric form can be used to simulate a sample and fit the distribution to estimate the parameters.
# With a lognormal assumption, the CFR is estimated at 4.57%, while assuming a gamma distribution gives a CFR of 4.72%. Although the bias is not highly significant, it still requires additional effort from analysts and introduces distortion into the estimation.

# Overall, while most methods yield comparable results, misinterpreting the reported values as lognormal distribution parameters leads to unusable outputs, emphasising the importance of reporting epidemiological parameters.

write.table(CFR_table, quote = F, sep = "-", row.names = F)


###### Part 2- removing outbreak dynamics ######
# Simulate data
nn <- 100 # Number of cases to simulate
recovery <- 0.9 # CFR of 10%
set.seed(10) # Set seed for reproducibility

# Generate random case onset timings in Jan & Feb 2024
cases <- as.Date("2024-01-01") + sample.int(60, nn, replace = TRUE)
data <- as.data.frame(cases)

# Generate outcome dates
outcome <- data$cases + sample.int(60, nn, replace = TRUE)
data$outcome <- outcome
data$deaths <- data$outcome
data$deaths[sample(nrow(data), ceiling(recovery * nrow(data)))] <- NA
data$recoveries <- as.Date(ifelse(is.na(data$deaths) == TRUE, data$outcome, NA))

# Converting to incidence
data_incidence <- incidence2::incidence(data, date_index = c("cases", "deaths"))
data_for_cfr <- cfr::prepare_data(data = data_incidence, cases_variable = "cases", deaths_variable = "deaths")
plot(data_incidence)

# Setting cutoff date
cutoff <- "2024-02-17"
data_rt <- data_for_cfr
data_rt <- data_rt[data_rt$date <= cutoff,]

# Estimating outcomes using different distributions
outcomes_5 <- estimate_outcomes(data_rt, delay_density = function(x) dgamma(x, shape = 5, scale = 1))
outcomes_30 <- estimate_outcomes(data_rt, delay_density = function(x) dgamma(x, shape = 30, scale = 1))

total_outcomes_5 <- round(sum(outcomes_5$estimated_outcomes))
total_outcomes_30 <- round(sum(outcomes_30$estimated_outcomes))

# Comparing CFRs
total_deaths_rt <- sum(data_rt$deaths)
cfr_5 <- round((total_deaths_rt / total_outcomes_5 * 100), 1)
cfr_30 <- round((total_deaths_rt / total_outcomes_30 * 100), 1)

# Create a comparison table
comparison_table <- data.frame(
  Distribution = c("Mean = 5 days", "Mean = 30 days"),
  Estimated_Outcomes = c(total_outcomes_5, total_outcomes_30),
  Estimated_CFR = c(cfr_5, cfr_30)
)

# Display the table
print(comparison_table)

# Optional: Use knitr::kable for better formatting (e.g., in RMarkdown)
knitr::kable(comparison_table, digits = 4, caption = "Comparison of Estimated Outcomes and CFRs")


### Plotting
# Prepare individual-level data:
# For recoveries (NA in deaths), we impute an outcome date (e.g., 15 days after onset)
df <- data %>%
  mutate(outcome_type = if_else(is.na(deaths), "R", "D")) %>%
  arrange(cases) %>%
  mutate(id_ordered = row_number())

# For each distribution, mark the first N cases as "Observed"
df_5 <- df %>%
  mutate(obs_status = if_else(id_ordered <= total_outcomes_5, "Included", "Not included"),
         dist = "Mean = 5 days")
df_30 <- df %>%
  mutate(obs_status = if_else(id_ordered <= total_outcomes_30, "Included", "Not included"),
         dist = "Mean = 30 days")

# Combine into one facet-ready dataset
df_facet <- bind_rows(df_5, df_30)

# Prepare long-format data for plotting onset and outcome events
plot_data <- df_facet %>%
  pivot_longer(
    cols = c(cases, outcome),
    names_to = "event_type",
    values_to = "date"
  ) %>%
  mutate(event_label = if_else(event_type == "cases", "Onset", "Outcome"))

# --- Create the faceted plot ---
ggplot(df_facet, aes(x = cases, xend = outcome,
                     y = id_ordered, yend = id_ordered,
                     color = obs_status)) +
  # Horizontal segments linking onset to outcome
  geom_segment(size = 0.5) +
  # Plot points: circle for onset, triangle for outcome
  geom_point(data = plot_data, aes(x = date, y = id_ordered,
                                   shape = event_label, color = obs_status),
             size = 2, inherit.aes = FALSE) +
  # Real-time cutoff line (vertical dashed line)
  geom_vline(xintercept = as.Date(cutoff),
             linetype = "dashed",
             color = "black",
             size = 0.5) +
  # Facet by delay distribution, arranged vertically (i.e. horizontal panels)
  facet_wrap(~ dist, ncol = 2) +
  scale_color_manual(values = c("Included" = "maroon", "Not included" = "grey")) +
  scale_shape_manual(values = c("Onset" = 16, "Outcome" = 17)) +
  labs(title = "Onset and outcome dates with real-time cutoff",
       x = "",
       y = "Cases (ordered by onset date)",
       color = "Included in estimation",
       shape = "Event type") +
  theme_minimal()













