

#DGP function
generate_dgp_data <- function(n, fixed_A = NULL, seed=12345) {
  # Ensure reproducibility
  set.seed(seed)  
  
  # Generate covariates W1 and W2
  W1 <- rnorm(n, mean = 0, sd = 1)
  W2 <- rnorm(n, mean = 0, sd = 1)
  
  # Determine treatment A
  if (is.null(fixed_A)) {
    # Define A based on W1 and W2 if fixed_A is NULL
    A <- rbinom(n, 1, prob = plogis(W1 - 0.5 * W2))
  } else {
    # Set A to fixed_A if it is not NULL
    A <- rep(fixed_A, n)
  }
  
  # Generate the outcome Y with a protective effect from A
  logit_p_Y <- 0.5 * W1 - 0.25 * W2 - 0.5 * A * W1 - 0.2 * (W2^2) - 2
  prob_Y <- plogis(logit_p_Y)
  Y <- rbinom(n, 1, prob = prob_Y)
  
  # Create and return a data frame
  return(data.frame(Y = Y, A = A, W1 = W1, W2 = W2))
}
