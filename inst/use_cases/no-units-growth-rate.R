# Issue: Growth rate is often reported without the units of time. This renders the
## reported value uninterpretable. 
## Analysis: Growth rate is most often converted into doubling or halving time,
## which is a more interpretable measure. Here we plot the relationship between
## growth rate and doubling time for a range of growth rates and draw the readers'
## attention to the different values of doubling time that one would obtain if
## a weekly growth rate is interpreted as a daily growth rate.
library(ggplot2)
growth_rates <- seq(0.1, 1, 0.1)

doubling_times <- log(2)/growth_rates


ggplot() +
  geom_line(aes(x = growth_rates, y = doubling_times)) +
    geom_point(aes(x = growth_rates, y = doubling_times)) +
    ## if 0.5 were a weekly rate, then to get the daily rate we would divide by
    ## 7 i.e. multiply the doubling time by 7
    geom_point(aes(y = log(2) * 7 / 0.5, x = 0.5), col = "red") +
    theme_minimal() +
    labs(x = "Growth rate", y = "Doubling time") 
