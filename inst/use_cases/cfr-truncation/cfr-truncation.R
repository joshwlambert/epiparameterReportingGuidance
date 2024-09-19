## case study: CFR estimates not accounting for delays
library(cfr)
library(dplyr)
library(ggplot2)
library(EpiNow2)

# load data from the 1976 ebola epidemic --- would be good to find a dataset that is not in the vignette
data("ebola1976")
df_ebola_subset <- filter(ebola1976, date <= "1976-09-30")

# naive cfr
naive <- cfr::cfr_static(
  data = df_ebola_subset
)

# adjusting with onset to death distribution - Barry et al., 2018
adjusted <- cfr::cfr_static(
  df_ebola_subset,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

estimates <- rbind(naive, adjusted)
estimates$method <- c("naive", "adjusted")
estimates$method <- factor(estimates$method, levels =
                             c("naive", "adjusted"))

ggplot(estimates, aes(col = method)) + geom_point(aes(y = method, x = severity_estimate)) +
  geom_errorbar(aes(xmin = severity_low, xmax = severity_high, y = method)) + theme_bw() +
  xlim(c(0,1)) + labs(x = "CFR estimate (%)", y = "Method")

# work out the forecasted final size of a hypothetical outbreak and the
# estimate of total deaths associated





