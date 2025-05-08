# ============================================================================
# Name: Kelli Sugai 
# Date: 5/07/2025
# PHB 228: Statistical Computing
# Homework 5: Bootstrap Methods  
# ============================================================================

# Load required packages
library(ggplot2)
library(MASS)  # For mvrnorm function
library(knitr)
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
bootstrapped <- bootstrap_bias(x,y, return_distribution =  TRUE)
# $original_estimate
# [1] 0.4548485

# $bootstrap_mean
# [1] 0.4969968

# $bias
# [1] 0.04214826

# $bias_corrected
# [1] 0.4127003

# c) Create a histogram of the bootstrap distribution of the correlation
# coefficient. Mark the original sample correlation and the bias-corrected
# estimate.

# Make data frame of bootstrapped estimates so we can use ggplot. 
df <- data.frame(
  correlation = bootstrapped$bootstrap_distribution
)

# Visualization. 
ggplot(data = df, 
       mapping = aes(x = correlation)) + 
  geom_histogram() + 
  geom_vline(xintercept = bootstrapped$original_estimate, color = "red") + 
  geom_vline(xintercept = bootstrapped$bias_corrected, color = "blue") + 
  labs(title = "Bootstrapped Correlation Coefficients",  
       x = "Correlation Coefficient", 
       y = "Frequency") + 
  annotate(geom = "text", x = bootstrapped$original_estimate + .2, y = 300, 
           label = "Original Estimate", color = "red") + 
  annotate(geom = "text", x = bootstrapped$original_estimate - .3, y = 250,
           label = "Bias-Corrected Estimate", color = "blue") + 
  theme_light()

# d) Discuss the results. Does the bootstrap effectively estimate the bias in
# this case? How does the bias-corrected estimate compare to the true
# correlation?

# Make a table for interpretation. 
interp <- data.frame(
  Measure = c("Real Correlation", "Sample Correlation", "Original Estimate", 
              "Bias-Corrected Estimate"), 
  Estimates = c(rho, sample_cor, bootstrapped$original_estimate, 
                bootstrapped$bias_corrected)
)
kable(interp, 
      caption = "Bootstrap Estimates Compared to Actual Correlation")

# In this case, the bootstrapping severely underestimated the actual correlation. 
# The real correlation = 0.70, but bootstrapping estimated it to be 0.45. 
# The bias-corrected estimate does even worse, estimating 0.42. 
# It did not effectively estimate the bias, as it assumed the estimate was 
# underestimating. The sample correlation was not a good reflection of the true 
# correlation, so bootstrapping compounded this inaccuracy. 

# ----------------------------------------------------------------------------
# PROBLEM 2: BOOTSTRAP CONFIDENCE INTERVALS
# ----------------------------------------------------------------------------

# The file returns.csv contains annual returns data for a portfolio over 50
# years. I don't have the data, so will generate similar. 

set.seed(42)
returns <- rnorm(50, mean = 0.08, sd = 0.12)
returns <- returns + rexp(50, rate = 20) - 0.05

# a) Implement a function bootstrap_mean() that uses the nonparametric
# bootstrap to estimate the standard error and 95% confidence interval
# for the mean annual return. Your function should have parameters:
  # • data: A vector of data values
  # • n_bootstrap: Number of bootstrap samples (default 2000)
  # • ci_type: Type of confidence interval to construct (“percentile”,
  #   “basic”, or “normal”)

# Function for bootstrap confidence intervals. 
bootstrap_mean <- function(data, n_bootstrap = 2000, ci_type = "percentile") {
  # Find original mean. 
  original_mean <- mean(data)
  
  # Make object to hold the bootstrapped means. 
  boot_means = numeric(n_bootstrap)
  
  # Get bootstrapped means, n_bootstrap times. 
  for(i in 1:n_bootstrap){
    sampled <- sample(data, size = length(data), replace = TRUE)
    boot_means[i] <- mean(sampled)
  }
  
  # Get bootstrapped standard error. 
  boot_se <- sd(boot_means)
  
  # Get confidence intervals. 
  alphaLevel <- as.double(0.05)
  ci <- c(NA,NA) 
  
  # Check the input for CI type. 
  if(ci_type == "percentile"){
    ci <- quantile(boot_means, probs = c(alphaLevel/2, 1 - alphaLevel/2))
  }
  
  else if(ci_type == "basic"){
    lowerCI <- 2*original_mean - quantile(boot_means, probs = 1-alphaLevel/2)
    upperCI <- 2*original_mean - quantile(boot_means, probs = alphaLevel/2)
    ci <- c(lowerCI, upperCI)
  }
  
  else if(ci_type == "normal"){
    z <- 1.96
    lowerCI <- original_mean - z*boot_se
    upperCI <- original_mean + z*boot_se
    ci <- c(lowerCI, upperCI)
  }
  
  else{
    return("Error: Please choose one of the following CI types: percentile, 
             basic, or normal.")
    stop()
  }
  
  return(list(
    original_mean = original_mean,
    boot_se = boot_se,
    ci = ci,
    boot_means = boot_means
  ))
}

# b) Apply your function to the returns data using all three CI methods.
# Report and compare the results.

percentileReturn <- bootstrap_mean(returns, ci_type = "percentile")
basicReturn <- bootstrap_mean(returns, ci_type = "basic")
normalReturn <- bootstrap_mean(returns, ci_type = "normal")

# Make comparison table. 
compTable <- data.frame(
  Method = c("Percentile", "Basic", "Normal"),
  Mean = c(percentileReturn$original_mean, basicReturn$original_mean, 
           normalReturn$original_mean), 
  Error = c(percentileReturn$boot_se, basicReturn$boot_se, normalReturn$boot_se),
  Lower = c(percentileReturn$ci[1], basicReturn$ci[1], normalReturn$ci[1]),
  Upper = c(percentileReturn$ci[2], basicReturn$ci[2], normalReturn$ci[2])
)
kable(compTable, 
      caption = "Comparison of CI Bootstrapping Methods")

# c) Create a histogram of the bootstrap distribution of the mean. Add
# vertical lines indicating the three different confidence intervals. 

# Put into a data frame so we can use ggplot. 
bootmeans <- data.frame(means = percentileReturn$boot_means)

# Visualization. 
ggplot(data = bootmeans, 
       mapping = aes(x = means)) + 
  geom_histogram() + 
  geom_vline(xintercept = percentileReturn$ci, color = "red") + 
  geom_vline(xintercept = basicReturn$ci, color = "darkgreen") + 
  geom_vline(xintercept = normalReturn$ci, color = "blue") + 
  labs(title = "Bootstrap Distribution of the Mean", 
       x = "Means", y = "Frequency") + 
  annotate("text", x = 0.15, y = 155, label = "Percentile", color = "red") + 
  annotate("text", x = 0.15, y = 149, label = "Basic", color = "darkgreen") +
  annotate("text", x = 0.15, y = 143, label = "Normal", color = "blue") + 
  annotate("text", x = 0.15, y = 163, label = "CI Method:", color = "black") +
  theme_minimal()

# d) Discuss which confidence interval method you believe is most appropriate 
# for this data and why. 

# For bootstrapping, I believe we should use the percentile method. Both the 
# basic and normal CI methods relied on using the original sample mean in their 
# construction. The percentile method only relied on the bootstrapped means.
# The other two methods both make assumptions about the distribution of the 
# data, which limits their generalization. 

# ----------------------------------------------------------------------------
# PROBLEM 3: REJECTION SAMPLING 
# ----------------------------------------------------------------------------

# a) Implement a rejection sampling algorithm to generate samples from a
# Beta(3,2) distribution using a Uniform(0,1) proposal distribution. 

# Function for rejection sampling from Beta(3,2).
rejection_sample_beta <- function(n, a = 3, b = 2) {
  samples <- numeric(0) # Prepare variable to hold samples. 
  totals <- 0 # To hold total number of tries to make acceptance rate later. 
  
  # Beta(3,2) is f(x), the target distribution 
  fx <- function(x){
    dbeta(x, a, b)
  }
  
  # Uniform(0,1) is g(x), the proposal distribution 
  gx <- function(x){
    dunif(x, 0, 1)
  }
  
  # Calculate maximum. 
  m <- optimize(fx, interval = c(0,1), maximum = TRUE)
  m <- m$objective 
  
  # Iterate until we have accepted n samples. 
  while(length(samples) < n){
    x <- runif(1)
    u <- runif(1)
    totals <- totals + 1 
    
    # Accept x if u <= f(x)/m. 
    if(u <= fx(x)/m){
      samples <- c(samples,x)
    }
  }
  
  # Compute acceptance rate. 
  acceptance_rate <- n/totals 
  
    return(list(
    samples = samples,
    acceptance_rate = acceptance_rate
    ))
}

# b) Generate 5000 samples using your function and create a histogram of
# the results. Overlay the true Beta(3,2) density curve for comparison.

# Run algorithm. 
set.seed(456)
rejSamp <- rejection_sample_beta(5000)

# Visualize. 
hist(rejSamp$samples, 
     probability = TRUE, 
     xlab = "Samples", 
     main = "Rejection Samping from Beta(3,2)", 
     col = "darksalmon")
curve(dbeta(x, shape1 = 3, shape2 = 2),
      add = TRUE,
      col = "deeppink1", 
      lwd = 3)

# c) Calculate the acceptance rate of your sampler. Discuss whether this
# is an efficient approach and what could be done to improve it.

paste(round(rejSamp$acceptance_rate*100,3), "%") # "56.6 %"

# The acceptance rate of my rejection sampler is ~56.6%. This means that it 
# rejects about 44% of samples and is not efficient. A more efficient 
# approach would be changing the proposal distribution, as the uniform 
# and the beta distribution are not very similar. Choosing a distribution 
# that is closer to Beta(3,2) would likely have a higher acceptance rate. 

# d) Compare the mean and variance of your generated samples with the
# theoretical mean and variance of the Beta(3,2) distribution. 

# Calculate the mean and variance of the generated samples. 
meanSamp <- mean(rejSamp$samples)
varSamp <- var(rejSamp$samples)

# Calculate the mean and variance of Beta(3,2). 
# Mean = a / (a + b)
realMean <- 3/(3+2)
# Variance = a*b / [(a+b)^2*(a+b+1)] 
realVar <- (3*2) / (((3+2)^2)*(3+2+1))

# Display results. 
compData <- matrix(c(meanSamp, varSamp, realMean, realVar), ncol = 2,
                      nrow = 2, byrow = TRUE)
rownames(compData) <- c("Generated", "Real")
colnames(compData) <- c("Mean", "Variance")
print(round(compData,5))

# The means and variances of our generated sample are nearly equivalent to the 
# real distribution of Beta(3,2), and are accurate to 3 decimal places. 

# ----------------------------------------------------------------------------
# PROBLEM 4: BOOTSTRAP HYPOTHESIS TESTING
# ----------------------------------------------------------------------------
# The file treatment.csv contains data from an experiment with two groups:
# treatment and control.

# I don't have the data file, so will generate similar data. 
set.seed(123)
control <- rnorm(40, mean = 10, sd = 2)
treatment <- rnorm(40, mean = 11.5, sd = 2)
treatment_data <- data.frame(
  group = c(rep("control", 40), rep("treatment", 40)),
  value = c(control, treatment)
)

# Extract groups
treatment_group <- treatment_data$value[treatment_data$group == "treatment"]
control_group <- treatment_data$value[treatment_data$group == "control"]

# a) Implement a function bootstrap_ttest() that performs a bootstrap
# hypothesis test for the difference in means between two groups. The
# null hypothesis is that there is no difference between the groups. Your
# function should:
    # 1. Take vectors of data from two groups, and number of bootstrap samples
    # 2. Return the observed difference, p-value, and bootstrap null distribution

# Function for bootstrap hypothesis test
bootstrap_ttest <- function(x, y, n_bootstrap = 2000){
  # Test the null hypothesis: E(X) = E(Y)
  n <- n_bootstrap # Number of bootstrap samples. 
  observed_diff <- mean(x) - mean(y)
  
  # Null distribution holding variable. 
  null_distribution <- numeric(n)
  
  pooledGroup <- c(x,y)
  
  # Sample loop.
  for(i in 1:n){
    # Regenerate pooled group order randomly. 
    booted <- sample(pooledGroup, size = length(x) + length(y), 
                     replace = TRUE) 
    
    # Split booted into x and y groups. 
    newX <- booted[1:length(x)]
    newY <- booted[(length(x) + 1):length(booted)]
    
    # Find the difference in the resampled means.
    null_distribution[i] <- mean(newX) - mean(newY)
  }
  
  # Compute the p-value, comparing our null distribution to the real difference. 
  p_value <- mean(abs(null_distribution) >= abs(observed_diff))
  
  return(list(
    observed_diff = observed_diff,
    p_value = p_value,
    null_distribution = null_distribution
  ))
}

# b) Apply your function to test whether there is a significant difference
# between the treatment and control groups. Use 2000 bootstrap samples.
set.seed(789)
sigDiff <- bootstrap_ttest(treatment_group, control_group, n_bootstrap = 2000)
sigDiff$p_value # 5e-04
# There is a significant difference between the means of the two groups. 


