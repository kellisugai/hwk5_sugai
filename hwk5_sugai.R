# ============================================================================
# Name: Kelli Sugai 
# Date: 5/07/2025
# PHB 228: Statistical Computing
# Homework 5: Bootstrap Methods  
# ============================================================================

# Load required packages
library(ggplot2)
library(MASS)  # For mvrnorm function
# ----------------------------------------------------------------------------
# PROBLEM 1: BOOTSTRAP BIAS ESTIMATION
# ----------------------------------------------------------------------------

# The bootstrap can be used to estimate bias in statistical estimators. For
# a sample statistic T, the bias is defined as E[T] - ￿, where ￿ is the true
# parameter value.

# a) Implement a function bootstrap_bias() that estimates the bias of
# the correlation coefficient between two variables using the bootstrap.
# Your function should have parameters:
  # • x and y: Two numeric vectors
  # • n_bootstrap: Number of bootstrap samples (default 2000)
  # • return_distribution: Logical indicating whether to return the
  #   bootstrap distribution (default TRUE)

# Function for bootstrap bias estimation. 
bootstrap_bias <- function(x, y, n_bootstrap = 2000, return_distribution = TRUE) 
  {
  # Find Bias = E[T'] - T,
  # Where T = the true correlation, and T' = bootstrapped correlation, 
  # and E[T'] = the average of all the bootstrapped correlations. 
  
  # Find T, the correlation between x and y. 
  realT <- cor(x,y)
  
  # Bootstrap T', a series of correlations, n_bootstrap times. 
  dist <- numeric(n_bootstrap)   # Prepare an object to hold the estimates. 
  for(i in 1:n_bootstrap){
    # Generate random indices. 
    indices <- sample(1:length(x), size = length(x), replace = TRUE)
    
    # Get correlation of x and y at these indices. 
    dist[i] <- cor(x[indices], y[indices])
  }
  
  # Calculate E[T'], the mean of our bootstrapped correlations. 
  distMean <- mean(dist)
  
  # Calculate the bias, E[T'] - T
  bias <- distMean - realT 
  
  # Use bias-corrected estimator, T - 'Bias' to find corrected bias. 
  bias_corrected <- realT - bias
  
  # Get results ready to return. 
  finals <- list(
    original_estimate = realT,
    bootstrap_mean = distMean,
    bias = bias,
    bias_corrected = bias_corrected
  )
  
  # Check if we want to return bootstrap_dist. 
  if(return_distribution == TRUE){
    finals$bootstrap_distribution <- dist
  }
  
  return(finals)
}

# Function implementation. 
set.seed(456) # For reproducibility.
x <- rnorm(30)
y <- rnorm(30)
bootstrap_bias(x, y, return_distribution = FALSE)
# $original_estimate
# [1] -0.2706364

# $bootstrap_mean
# [1] -0.2610255

# $bias
# [1] 0.009610889

# $bias_corrected
# [1] -0.2802473

# b) Generate bivariate data (n = 30) where the true correlation is 0.7, but
# with an outlier that affects the correlation. Apply your function to
# estimate the bias.

set.seed(456) # For reproducibility. 
n <- 30
rho <- 0.7 # True correlation. 

# Generate correlated data. 
sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
xy <- mvrnorm(n-1, mu = c(0, 0), Sigma = sigma) 
  # Simulates 1 sample from the multivariate normal dist, n = 29.  

# Add outlier. 
xy <- rbind(xy, c(3, -3)) # Adds outlier to the n = 30 spot. 
x <- xy[,1] # x = column 1 of simulated bivariate data. 
y <- xy[,2] # y = column 2 of simulated bivariate data.

# Calculate sample correlation. 
sample_cor <- cor(x, y)
cat("Sample correlation:", sample_cor, "\n")
cat("True correlation:", rho, "\n")

# Estimate bias. 
bootstrap_bias(x,y, return_distribution =  FALSE)
# $original_estimate
# [1] 0.4548485

# $bootstrap_mean
# [1] 0.4969968

# $bias
# [1] 0.04214826

# $bias_corrected
# [1] 0.4127003

