
# Use case for reporting guidance paper #2- Mean of the sample reported
# ambiguously so it's confused with the mean of the lognormal distribution

library(simulist)
library(epiparameter)
library(tidyverse)
library(incidence2)
library(cfr)


# Step 1: Simulating data for the use case with o-d lognormal and CFR of 30%

contact_distribution <- epidist(
  disease = "COVID-19",
  epi_dist = "contact distribution",
  prob_distribution = "pois",
  prob_distribution_params = c(mean = 3)
)

ip_COVID <- epidist(
  disease = "COVID-19",
  epi_dist = "infectious period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 3, scale = 3)
)

o_d_COVID <- epidist_db(
  disease = "covid-19",
  epi_dist = "onset to death",
  single_epidist = TRUE
)


set.seed(123)
linelist_covid <- sim_linelist(
  contact_distribution = contact_distribution,
  infect_period = ip_COVID,
  prob_infect = 0.7,
  onset_to_hosp = NA,
  hosp_risk = NA,
  non_hosp_death_risk = 0.3,
  onset_to_death = o_d_COVID,
  outbreak_size = c(1000, 2000)
)

# STEP 2: Aggregation

linelist_covid$date_death <- fifelse(linelist_covid$outcome == "died", linelist_covid$date_outcome, NA)
incidence_COVID <- incidence2::incidence(linelist_covid, date_index = c("date_onset", "date_death"), interval = 1L)

incidence_COVID$date_index <- as.Date(incidence_COVID$date_index)
covid_inc_cfr <- cfr::prepare_data(incidence_COVID, cases_variable = "date_onset", deaths_variable = "date_death")

ggplot(covid_inc_cfr, aes(x = date)) + geom_point(aes(y = cases), colour = "blue") +
  geom_point(aes(y = deaths), colour = "red") + theme_bw() + scale_x_date(date_breaks = "7 days")

# STEP 3: Truncating data

# Real-time point at 2023-01-09

real_time <- "2023-01-27"
incidence_rt_covid <- covid_inc_cfr[covid_inc_cfr$date <= real_time,]

# STEP 4: Converting parameters

true_dist_params <- get_parameters(o_d_COVID)
true_dist_summary_stats <- convert_params_to_summary_stats("lnorm", meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]])

# STEP 5: Problem statement

# We want to estimate the delay-adjusted CFR on our own outbreak data, and for this
# we look at the available literature where we find a publication that reports the delay
# from disease onset to death as follows:
# "... the average duration between the time when symptoms first appeared and
# death of the patients was estimated. The mean onset-death delay was of 14.5 days,
# with a standard deviation of 6.7"


# 14.5 and 6.7 are really distribution summary stats

# TRUE CFR

true_cfr <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]]))


# a) We think they are meanlog and sdlog

crf_assumed_params <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = true_dist_summary_stats[["mean"]], sdlog = true_dist_summary_stats[["sd"]]))

# b) We think they are sample statistics

# b.1) Lognormal to randomly generate sample and lognormal fitted distribution

lnorm_sample <- rlnorm(n = 500, meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]])

lnorm_fit <- fitdistrplus::fitdist(data = lnorm_sample, distr = "lnorm")

cfr_sample_lnorm <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = lnorm_fit$estimate[["meanlog"]], sdlog = lnorm_fit$estimate[["sdlog"]]))

# b.2) Gamma and gamma

gamma_parameters <- convert_summary_stats_to_params("gamma", mean = true_dist_summary_stats[["mean"]], sd = true_dist_summary_stats[["sd"]])

gamma_sample <- rgamma(n = 500, shape = gamma_parameters$shape, scale = gamma_parameters$scale)

gamma_fit <- fitdistrplus::fitdist(data = gamma_sample, distr = "gamma")

cfr_sample_gamma <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = gamma_fit$estimate[["shape"]], rate = gamma_fit$estimate[["rate"]]))
