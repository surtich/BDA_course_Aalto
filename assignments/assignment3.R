library(markmyassignment)
library(tidyverse)
library(ggplot2)

assignment_path <-
  paste("https://github.com/avehtari/BDA_course_Aalto/",
    "blob/master/assignments/tests/assignment3.yml",
    sep = ""
  )
set_assignment(assignment_path)
# To check your code/functions, just run
mark_my_assignment()

#1

library(aaltobda)
data("windshieldy1")
head(windshieldy1)

windshieldy_test <- c(13.357, 14.928, 14.896, 14.820)


#1.a)

mu_point_est <- function (data = windshieldy_test) {
  mean(data)
}

mu_point_est(data = windshieldy_test)

mu_interval <- function(data = windshieldy_test, prob = 0.95) {
  n <- length(data)
  q <- qt(c((1-prob)/2, prob + (1-prob)/2), n - 1)

  mean(data) + q * sd(data) / sqrt(n)
}


mu_interval(data = windshieldy_test, prob = 0.95)

# 1.b

mu_pred_point_est <- function(data = windshieldy_test) {
  mean(data)
}

mu_pred_point_est(data = windshieldy_test)

mu_pred_interval <- function(data = windshieldy_test, prob = 0.95) {
  n <- length(data)
  q <- qt(c((1-prob)/2, prob + (1-prob)/2), n - 1)

  mean(data) + q * sd(data) * sqrt(1 + 1/n)  
}


mu_pred_interval(data = windshieldy_test, prob = 0.95)

# 2

set.seed(4711)
p0 <- rbeta(100000, 5, 95)
p1 <- rbeta(100000, 10, 90)

posterior_odds_ratio_point_est(p0 = p0, p1 = p1)
## [1] 2.676
posterior_odds_ratio_interval(p0 = p0, p1 = p1, prob = 0.9)
## [1] 0.875 6.059