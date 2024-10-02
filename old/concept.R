
library(tidyverse)

# Constants -------------------------------------------------------------------

data_n    <- 200
beta0 <- -15
beta1 <- 2
sigma <- 10

# x predictor
x <- runif(n = data_n, min = 0, max = 20)

# Linear ----------------------------------------------------------------------

# Generate outcome
y <- beta0 + x * beta1 + rnorm(data_n, sd = sigma)

# fit model
mod_lm <- glm(y ~ x, family = 'gaussian')

# Plot
tibble(y = y, x = x) %>% 
  ggplot(., aes(x = x, y = y)) + 
    geom_point() + 
    geom_smooth(method = 'glm', method.args = list(family = 'gaussian')) 




# Poisson ---------------------------------------------------------------------

# compute mu's
mu <- exp(beta0 + x * beta1)

# generate outcome
y <- rpois(n = data_n, lambda = mu)

# fit model
mod_poisson <- glm(y ~ x, family = 'poisson')

# plot
tibble(y = y, x = x) %>% 
  ggplot(., aes(x = x, y = y)) + 
    geom_point() + 
    geom_smooth(method = 'glm', method.args = list(family = 'poisson'))




# Logistic --------------------------------------------------------------------

# Generate data
s  <- beta0 + beta1 * x + rnorm(data_n, sd = sigma)
y  <- rbinom(data_n, size = 1, prob = exp(s) / (1 + exp(s)))

# Fit model
mod_log <- glm(y ~ x, family = 'binomial')




# plot
tibble(y = y, x = x) %>% 
  ggplot(., aes(x = x, y = y)) + 
    geom_point() + 
    geom_smooth(method = 'glm', method.args = list(family = 'binomial')) 


