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

