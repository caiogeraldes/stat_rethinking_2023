require(rethinking)
require(tidyverse)

# ## Exercice 1:
# Suppose the globe tossing data (Lecture 2, Chapter 2) had turned out to be 4
# water and 11 land. Construct the posterior distribution.

size <- 100
n_water <- 4
n_land <- 11
n_tosses <- n_water + n_land

p_grid <- seq(from = 0, to = 1, length.out = size)

prior <- rep(1, size)

likelihood <- dbinom(n_water, size = n_tosses, prob = p_grid)

unstd_post <- likelihood * prior

posterior <- unstd_post / sum(unstd_post)

tibble(p = p_grid, post = posterior) %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_line() + geom_point() +
  xlab("probability of water") +
  ylab("posterior probability") +
  labs(subtitle = str_c(as.character(size), " examples"))


# ## Exercice 2: Using the posterior distribution from 1, compute the posterior
# predictive distribution for the next 5 tosses of the same globe. I recommend
# you use the sampling method.

n_simulations <- 1e4
sample_size <- 5

post_samples <- sample(
   p_grid,
   size = n_simulations,
   replace = TRUE,
   prob = posterior
)

w_pred <- rbinom(
  n_simulations,
  size = sample_size,
  prob = post_samples
)

tibble(w_pred) %>%
  ggplot(aes(x = w_pred)) +
  geom_histogram(bins = 18) +
  xlab("number of predicted water samples") +
  ylab("Frequency") +
  labs(title = "Predictive posterior distribution")

# 3. Use the posterior predictive distribution from 2 to calculate the probabil-
# ity of 3 or more water samples in the next 5 tosses.

sum(w_pred >= 3) / n_simulations
# [1] 0.1813


# 4-OPTIONAL. This problem is an optional challenge for people who are
# taking the course for a second or third time. Suppose you observe W = 5
# water points, but you forgot to write down how many times the globe was
# tossed, so you don’t know the number of land points L. Assume that p = 0.7
# and compute the posterior distribution of the number of tosses N. Hint: Use
# the binomial distribution.

size <- 100
n_water <- 5
p <- 0.7

n_grid <- seq(from = 5, to = 100, length.out = size)


likelihood <- dbinom(n_water, size = n_grid, prob = p)
#   [1] 1.68070e-01         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [11]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [21]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [31]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [41]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [51]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [61]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [71]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [81]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN
#  [91]         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN         NaN 2.68369e-43
