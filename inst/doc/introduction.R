## ----setup, include=FALSE-----------------------------------------------------
library(Rcpp)
library(RcppNumerical)
knitr::opts_chunk$set(message = FALSE, warning = FALSE,
                      comment = "#", collapse = TRUE)

## -----------------------------------------------------------------------------
integrate_1d_test()

## -----------------------------------------------------------------------------
library(mvtnorm)
trueval = pmvnorm(c(-1, -1), c(1, 1), sigma = matrix(c(1, 0.5, 0.5, 1), 2))
integrate_md_test()
as.numeric(trueval) - integrate_md_test()$approximate

## -----------------------------------------------------------------------------
# integrate() in R
integrate(function(x) x^2 * dnorm(x), 0.5, Inf)
integrate_1d_inf_test(0.5, Inf)

## -----------------------------------------------------------------------------
integrate_md_inf_test()

## -----------------------------------------------------------------------------
optim_test()

## -----------------------------------------------------------------------------
optim_box_test()

## -----------------------------------------------------------------------------
set.seed(123)
n = 5000
p = 100
x = matrix(rnorm(n * p), n)
beta = runif(p)
xb = c(x %*% beta)
p = exp(xb) / (1 + exp(xb))
y = rbinom(n, 1, p)

system.time(res1 <- glm.fit(x, y, family = binomial())$coefficients)
system.time(res2 <- logistic_reg(x, y))
max(abs(res1 - res2))

## -----------------------------------------------------------------------------
system.time(res3 <- fastLR(x, y)$coefficients)
max(abs(res1 - res3))

