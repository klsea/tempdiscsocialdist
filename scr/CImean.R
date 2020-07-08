CImean <- function(a, s, n) {
  # a = sample mean
  # s = standard deviation of sample
  # n = sample size
  error <- qnorm(0.975)*s/sqrt(n)
  left <- a-error
  right <- a+error
  ci <- c(left, right)
  return(ci)
}
