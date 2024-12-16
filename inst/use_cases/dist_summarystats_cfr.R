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

onset_to_death <-
  epiparameter(
    disease = "unknown",
    epi_name = "onset to death",
    summary_stats =
      create_summary_stats(mean = 14.5, sd = 6.7),
    prob_distribution = "lnorm"
  )

set.seed(123)
linelist_covid <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = ip_COVID,
  prob_infect = 0.7,
  onset_to_hosp = NULL,
  hosp_risk = NULL,
  non_hosp_death_risk = 0.05,
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

real_time <- "2023-02-03"
incidence_rt_covid <- covid_inc_cfr[covid_inc_cfr$date <= real_time,]

# STEP 4: Creating objects with parameters and summary stats from distribution used to simulate data

true_dist_params <- get_parameters(onset_to_death)
true_dist_summary_stats <- convert_params_to_summary_stats("lnorm", meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]])

# STEP 5: Problem statement

# We want to estimate the delay-adjusted CFR on our own outbreak data, and for this
# we look at the available literature where we find a publication that reports the delay
# from disease onset to death as follows:
# "... the average duration between the time when symptoms first appeared and
# death of the patients was estimated. The mean onset-death delay was of 14.5 days,
# with a standard deviation of 6.7"

# 14.5 and 6.7 are really distribution summary stats

# TRUE CFR (calculated using lognormal parameters that were used to simulate the data)

true_cfr <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = true_dist_params[["meanlog"]], sdlog = true_dist_params[["sdlog"]]))

sum(covid_inc_cfr$deaths)/sum(covid_inc_cfr$cases)


# a) We think they are meanlog and sdlog (calculated taking the provided mean and sd and using them directly as distribution parameters)

cfr_assumed_params <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = 14.5, sdlog = 6.7))

# b) We think they are sample statistics

# b.1) Lognormal to randomly generate sample and lognormal fitted distribution

lnorm_parameters <- convert_summary_stats_to_params("lnorm", mean = 14.5, sd = 6.7)

lnorm_sample <- rlnorm(n = 500, meanlog = lnorm_parameters$meanlog, sdlog = lnorm_parameters$sdlog)

lnorm_fit <- fitdistrplus::fitdist(data = lnorm_sample, distr = "lnorm")

cfr_sample_lnorm <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = lnorm_fit$estimate[["meanlog"]], sdlog = lnorm_fit$estimate[["sdlog"]]))

# b.2) Gamma to randomly generate sample and gamma fitted distribution

gamma_parameters <- convert_summary_stats_to_params("gamma", mean = 14.5, sd = 6.7)

gamma_sample <- rgamma(n = 500, shape = gamma_parameters$shape, scale = gamma_parameters$scale)

gamma_fit <- fitdistrplus::fitdist(data = gamma_sample, distr = "gamma")

cfr_sample_gamma <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = gamma_fit$estimate[["shape"]], rate = gamma_fit$estimate[["rate"]]))

# c) We think they are distribution summary statistics (correct interpretation) but unsure of which distribution

# c.1) Lognormal and fitted lognormal

params_lnorm <- convert_summary_stats_to_params("lnorm", mean = 14.5, sd = 6.7)
cfr_lnorm_fit <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dlnorm(x, meanlog = params_lnorm$meanlog, sdlog = params_lnorm$sdlog))


# c.2) Lognormal and fitted gamma

params_gamma <- convert_summary_stats_to_params("gamma", mean = 14.5, sd = 6.7)
cfr_gamma_fit <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dgamma(x, shape = params_gamma$shape, scale = params_gamma$scale))


# c.3) Lognormal and fitted weibull

params_weibull <- convert_summary_stats_to_params("weibull", mean = 14.5, sd = 6.7)
cfr_weibull_fit <- cfr::cfr_static(incidence_rt_covid, delay_density = function(x) dweibull(x, shape = params_weibull$shape, scale = params_weibull$scale))

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
