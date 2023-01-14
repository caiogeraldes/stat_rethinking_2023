require(rethinking)
require(tidyverse)

# ## Exercice 1:
# Suppose the globe tossing data (Lecture 2, Chapter 2) had turned out to be 4
# water and 11 land. Construct the posterior distribution.

grid_approximation_pW <- function(W, L, size) {
    N <- W + L
    p_grid <- seq(from = 0, to = 1, length.out = size)
    prior <- rep(1, size)
    likelihood <- dbinom(W, size = N, prob = p_grid)
    unstd_post <- likelihood * prior
    posterior <- unstd_post / sum(unstd_post)
    tibble(p = p_grid, post = posterior)
}

size <- 100
W <- 4
L <- 11

model <- grid_approximation_pW(W, L, size)
model

model %>%
  ggplot(aes(x = p, y = post)) +
  geom_line() + geom_point() +
  xlab("probability of water") +
  ylab("posterior probability") +
  labs(subtitle = str_c(as.character(size), " examples"))

# ## Exercice 2: Using the posterior distribution from 1, compute the posterior
# predictive distribution for the next 5 tosses of the same globe. I recommend
# you use the sampling method.

n_simulations <- 1e4
sample_size <- 5
p_grid <- seq(from = 0, to = 1, length.out = size)

post_samples <- sample(
   p_grid,
   size = n_simulations,
   replace = TRUE,
   prob = model$post
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

make_n_grid <- function(W, N_max) {
  seq(from = W, to = N_max, length.out = (N_max - W + 1))
}

grid_approximation_tosses <- function(W, p_W, N_max) {
  n_grid <- make_n_grid(W, N_max)
  likelihood_w_given_n <- dbinom(W, n_grid, p_W)
  unstd_post <- likelihood_w_given_n
  posterior <- unstd_post / sum(unstd_post)
  tibble(n = n_grid, post = round(posterior,3))
}


N_max <- 30
W <- 5
p_W <- 0.7

model <- grid_approximation_tosses(W, p_W, N_max)
model


model %>%
  ggplot(aes(x = n, y = post)) +
  geom_line() + geom_point() +
  xlab("N tosses") +
  ylab("posterior probability")

n_simulations <- 1e4
n_grid <- make_n_grid(W, N_max)
post_samples <- sample(
   n_grid,
   size = n_simulations,
   replace = TRUE,
    prob = model$post
)
hpdi <- HPDI(post_samples)
hpdi



tibble(post_samples) %>%
  ggplot(aes(x = post_samples)) +
  geom_bar(aes(fill=sapply(post_samples, function(x) hpdi[1] <= x && x <= hpdi[2]))) +
  xlab("number of predicted tosses") +
  ylab("Frequency") +
  labs(title = "Predictive posterior distribution of N tosses given W=5, pW = 0.7", subtitle="89% HPDI in blue") +
  theme(legend.position="none")


### Testing
# To test the model, we will assume a longer tossing run, with $W=50$ and a $N_{max} = 100$:

N_max <- 100
W <- 50
p_W <- 0.7

model <- grid_approximation_tosses(W, p_W, N_max)
model

model %>%
  ggplot(aes(x = n, y = post)) +
  geom_line() + geom_point() +
  xlab("N tosses") +
  ylab("posterior probability")

n_simulations <- 1e4
n_grid <- make_n_grid(W, N_max)
post_samples <- sample(
   n_grid,
   size = n_simulations,
   replace = TRUE,
    prob = model$post
)

hpdi <- HPDI(post_samples)
hpdi

tibble(post_samples) %>%
  ggplot(aes(x = post_samples)) +
  geom_bar(aes(fill=sapply(post_samples, function(x) hpdi[1] <= x && x <= hpdi[2]))) +
  xlab("number of predicted tosses") +
  ylab("Frequency") +
  labs(title = "Predictive posterior distribution of N tosses given W=50, pW = 0.7", subtitle="89% HPDI in blue") +
  theme(legend.position="none")


# By changing $p_W$ for a value lower than $0.5$, say $p_W = 0.3$, the likelihood of longer tossing runs increases:

N_max <- 100
W <- 50
p_W <- 0.3

model <- grid_approximation_tosses(W, p_W, N_max)
model

model %>%
  ggplot(aes(x = n, y = post)) +
  geom_line() + geom_point() +
  xlab("N tosses") +
  ylab("posterior probability")

n_simulations <- 1e4
n_grid <- make_n_grid(W, N_max)
post_samples <- sample(
   n_grid,
   size = n_simulations,
   replace = TRUE,
    prob = model$post
)
hpdi <- HPDI(post_samples)
hpdi

tibble(post_samples) %>%
  ggplot(aes(x = post_samples)) +
  geom_bar(aes(fill=sapply(post_samples, function(x) hpdi[1] <= x && x <= hpdi[2]))) +
  xlab("number of predicted tosses") +
  ylab("Frequency") +
  labs(title = "Predictive posterior distribution of N tosses given W=50, pW = 0.3", subtitle="89% HPDI in blue") +
  theme(legend.position="none")
