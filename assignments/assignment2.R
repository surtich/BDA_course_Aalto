library(markmyassignment)
library(tidyverse)
library(ggplot2)

assignment_path <-
  paste("https://github.com/avehtari/BDA_course_Aalto/",
    "blob/master/assignments/tests/assignment2.yml",
    sep = ""
  )
set_assignment(assignment_path)
# To check your code/functions, just run
mark_my_assignment()

library(aaltobda)
data("algae")

algae_test <- c(0, 1, 1, 0, 0, 0)

# 1.b)
beta_point_est <- function(prior_alpha = 2, prior_beta = 10, data = algae_test) {
  y <- sum(data)
  n <- length(data)

  posterior_alpha <- prior_alpha + y
  posterior_beta <- prior_beta + n - y

  posterior_alpha / (posterior_alpha + posterior_beta)
}

beta_point_est(prior_alpha = 2, prior_beta = 10, data = algae_test)

beta_interval <- function(prior_alpha = 2, prior_beta = 10, data = algae_test, prob = 0.9) {
  y <- sum(data)
  n <- length(data)

  posterior_alpha <- prior_alpha + y
  posterior_beta <- prior_beta + n - y

  qbeta(c((1-prob)/2, prob + (1-prob)/2), posterior_alpha, posterior_beta)
}

beta_interval(prior_alpha = 2, prior_beta = 10, data = algae_test, prob = 0.9)


# 1.c)
beta_low <- function(prior_alpha = 2, prior_beta = 10, data = algae_test, pi_0 = 0.2) {
  y <- sum(data)
  n <- length(data)

  posterior_alpha <- prior_alpha + y
  posterior_beta <- prior_beta + n - y

  pbeta(pi_0, posterior_alpha, posterior_beta)
}

beta_low(prior_alpha = 2, prior_beta = 10, data = algae_test, pi_0 = 0.2)

# 1.d)
