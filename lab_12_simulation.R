
generate_data <- function(n, p) {
  cov <- matrix(rnorm(n * p), nrow = n, ncol = p)
  vec <- rnorm(n)
  return(list(covariates = cov,
              responses = vec))
}

model_select <- function(covariates, responses, cutoff) {
  reg1 <- lm(responses ~ covariates)
  sum1 <- summary(reg1)
  pvals <- sum1$coefficients[-1,4]
  if (sum(pvals <= cutoff) == 0) {
    return(vector())
  }
  reg2 <- lm(responses ~ covariates[,(pvals <= cutoff)])
  sum2 <- summary(reg2)
  return(sum2$coefficients[-1,4])
}

