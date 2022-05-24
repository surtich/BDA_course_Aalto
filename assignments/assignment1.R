library(markmyassignment)
library(tidyverse)
library(ggplot2)

assignment_path <-
  paste("https://github.com/avehtari/BDA_course_Aalto/",
    "blob/master/assignments/tests/assignment1.yml",
    sep = ""
  )
set_assignment(assignment_path)
# To check your code/functions, just run
mark_my_assignment()

# 2

mu <- 0.2
sigma <- 0.01

alpha <- mu * (mu * (1 - mu) / sigma^2 - 1)
beta <- alpha * (1 - mu) / mu

theta <- seq(0, 1, 0.001)
d <- dbeta(theta, alpha, beta)

data.frame(theta = theta, d = d) %>% ggplot() +
  geom_line(aes(x = theta, y = d))


theta2 <- rbeta(1000, alpha, beta)

data.frame(theta = theta, d = d) %>% ggplot() +
  geom_line(aes(x = theta, y = d)) +
  geom_histogram(data = data.frame(theta2), stat = "density", color = "blue", aes(x = theta2))

mean(theta2)
var(theta2)

quantile(theta2, c(0.025, 0.975))


# 2

# P(+|E)
sensitivity <- 0.98

# P(-|NE)
specificity <- 0.96

# P(E)
prevalence <- 0.001

# P(E|-) = P(-|E)*P(E)/P(-)
# P(-) = P(-|E)*P(E) + P(-|NE)*(1-P(E))
p_false_negative <- (1 - sensitivity) * prevalence / ((1 - sensitivity) * prevalence + specificity * (1 - prevalence))
p_false_negative


# P(NE|+) = P(+|NE)*P(NE)/P(+)
p_false_positive <- (1 - specificity) * (1 - prevalence) / ((1 - specificity) * (1 - prevalence) + sensitivity * prevalence)
p_false_positive

# P(+|NE)
1 - specificity

# P(+, E) = P(+|E)*P(E)
sensitivity * prevalence


# 4

boxes <- matrix(c(2, 2, 1, 5, 5, 1),
  ncol = 2,
  dimnames = list(c("A", "B", "C"), c("red", "white"))
)

boxes

p_boxes <- list("A" = 0.4, "B" = 0.1, "C" = 0.5)

p_red <- function(boxes) {
  p <- 0
  for (r in row.names(boxes)) {
    p <- p + p_boxes[[r]] * boxes[r, "red"] / sum(boxes[r, ])
  }
  p
}

p_red(boxes)


p_box <- function(boxes) {

  # P(A|R)  = P(R|A)*P(A) / P(R)

  p <- c()
  for (r in row.names(boxes)) {
    p <- c(p, p_boxes[[r]] * boxes[r, "red"] / sum(boxes[r, ]))
  }
  p / sum(p)
}


p_box(boxes)

# 5

p_identical_twin <- function(fraternal_prob = 1 / 125, identical_prob = 1 / 300) {
  identical_prob * .5 / (identical_prob * .5 + fraternal_prob * .5 * .5)
}

p_identical_twin()

p_identical_twin(fraternal_prob = 1 / 100, identical_prob = 1 / 500)