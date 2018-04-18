
generate_data <- function(n, p) {
  cov <- matrix(rnorm(n * p), nrow = n, ncol = p)
  vec <- rnorm(n)
  return(list(covariates = cov,
              responses = vec))
}