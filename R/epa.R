# Epanechnikov kernel function
#
# Kernel function for grid windows
epaker <- function(x) {
  (3/4)*(1-x^2)*(abs(x)<=1)
}

qepa <- function(p, mu, r) {
  2 * sin(asin(2 * p - 1)/3) * r + mu
}

repa <- function(n, mu, r) {
  qepa(runif(n, min = 0, max = 1), mu, r)
}