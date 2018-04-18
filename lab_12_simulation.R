
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

run_simulations <- function(n_trials, n, p, cutoff) {
  pvals <- numeric(0)
  for (trials in 1:n_trials) {
    data <- generate_data(n,p)
    pvals <- c(pvals, model_select(data$covariates, data$responses, cutoff))
  }
  
  hist(x = pvals, main = paste("Histogram of PVals at ", n_trials, " trials, n = ", n, " p = ", p, sep = ''))
}

for (n in c(100, 1000, 10000)) {
  for (p in c(10, 20, 50)) {
    run_simulations(1000, n, p, 0.05)
  }
}
