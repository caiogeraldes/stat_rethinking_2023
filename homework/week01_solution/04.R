# 4-OPTIONAL. This problem is an optional challenge for people who are
# taking the course for a second or third time. Suppose you observe W = 5
# water points, but you forgot to write down how many times the globe was
# tossed, so you don’t know the number of land points L. Assume that p = 0.7
# and compute the posterior distribution of the number of tosses N. Hint: Use
# the binomial distribution.

# $N = W + L = W + \bar{W}$
# $W \sim \text{Binomial}(N, p_W)$


# ### Simulation

require(rethinking)
require(tidyverse)

size <- 20
n_water <- 5
p <- 0.7
n_grid <- seq(from = 1, to = size, length.out = size)
prior <- c(rep(0, (n_water-1)), rep(1, size-(n_water-1))) # Flat if N < W)
likelihood_w_given_n <- dbinom(n_water, n_grid, p) # P(5|N, 0.7)
unstd_post <- likelihood_w_given_n * prior
posterior <- unstd_post / sum(unstd_post)

tibble(n = n_grid, post = posterior) %>%
  ggplot(aes(x = n, y = post)) +
  geom_line() + geom_point() +
  xlab("N tosses") +
  ylab("posterior probability")


n_simulations <- 1e4
post_samples <- sample(
   n_grid,
   size = n_simulations,
   replace = TRUE,
   prob = posterior
)
tibble(post_samples) %>%
  ggplot(aes(x = post_samples)) +
  geom_bar() +
  xlab("number of predicted tosses") +
  ylab("Frequency") +
  labs(title = "Predictive posterior distribution of N tosses given W=5")



############

sim_Tn_rounds_of_tosses <- function(p=0.7, Tn=1e3, size=30) {
    N <- sample(1:size, size=Tn, replace=TRUE)
    W <- sapply(N, function(x) sum(rbinom(x, 1, p)))
    tibble(N,W)
}

size = 30
sim_data <- sim_Tn_rounds_of_tosses(size=size)
d.model <- list(
    W = sim_data$W,
    N = sim_data$N,
    S = size
)
f.model <- alist(
    W ~ dbinom(N,p_W),
    p_W ~ dunif(0,1),
    N ~ dbinom(S, p_N),
    p_N ~ dunif(0,1)
)

model <- ulam(
    flist = f.model,
    data = d.model
)

precis(model)



max_t <- 200
sim_data <- sim_Tn_rounds_of_tosses(size=max_t)
n_water <- sim_data$W
p_grid <- seq(0,1,length.out = 1e3)

n_grid <- seq(from = 1, to = max_t, length.out = max_t)
prior <- rep(1, 1e3)
sample_prob <- sapply(n_water, function(x) dbinom(x, n_grid, p))
unstd_post <- sample_prob * prior
posterior <- unstd_post / sum(unstd_post)

# SAMPLE FROM THE POSTERIOR
sampler <- function(data, posterior, size=1e4, max_t=30) {
   x <- sample(1:nrow(data),1) 
   n_grid <- seq(from = 1, to = max_t, length.out = max_t)
   xintercept <- data$N[x]
   samples <- sample(n_grid, size=size, replace=TRUE, prob=posterior[,x])
   hpdi <- HPDI(samples)
   report <- str_c(
       "Actual N: ",
       as.character(xintercept),
       "\nHPDI:\n",
       names(hpdi[1]), "\t\t", names(hpdi[2]),
       "\n",
       as.character(round(hpdi[1],3)),
       "\t\t\t",
       as.character(round(hpdi[2],3))
       )
   tibble(samples) %>%
       ggplot(aes(x=samples)) +
       geom_density() +
       xlim(0, max_t+1) +
       labs(subtitle = report) +
       geom_vline(xintercept = xintercept, color="red") +
       geom_vline(xintercept = hpdi[1], color="purple", alpha = 0.5) +
       geom_vline(xintercept = hpdi[2], color="purple", alpha = 0.5)
}

sampler(sim_data, posterior, 1e5, max_t = max_t)


sampler(tibble(N=c(1), W=c(5)), posterior, 1e5, max_t = 200)
dens(samples)
HPDI(samples)

 tibble(n = n_grid, post = posterior[,x]) %>%
    ggplot(aes(x = n, y = post)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = sim_data$N[x]) +
    xlab("N tosses") +
    ylab("posterior probability") 
