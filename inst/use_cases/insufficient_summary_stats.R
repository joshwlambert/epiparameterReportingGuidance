## Issue: Summary statistics are provided but are not sufficient to fully 
## characterise the underlying distribution.
## Example: based on the best-fitting gamma distribution, 
## the median delay was estimated to be 12 days (95% CI 10-14)

library(epitrix)
library(ggplot2)
library(purrr)
## Mode of gamma distribution is given by (shape - 1) * scale
## Hence multiple combinations of shape and scale can give the same mode
shapes <- list(4, 5)
scales <- list(4, 3)
params <- map2(shapes, scales, ~ gamma_shapescale2mucv(.x, .y))
mus <- map(params, ~ .x$mu)
cvs <- map(params, ~ .x$cv)
sds <- map2(mus, cvs, ~ .x * .y)

samples <- map2(
  shapes, scales,
  ~ rgamma(10000, shape = .x, scale = .y)
)


p95 <- map(samples, ~ quantile(.x, 0.95))
ggplot() +
  geom_density(aes(x = samples[[1]]), fill = "red", alpha = 0.25, col = NA) +
  geom_density(aes(x = samples[[2]]), fill = "blue", alpha = 0.25, col = NA) +
  geom_vline(aes(xintercept = p95[[1]]), color = "red") +
  geom_vline(aes(xintercept = p95[[2]]), color = "blue") +
  theme_minimal()
