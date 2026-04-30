################################################################################
# Continuous-Time Intensity Matrix Computation
# Purpose: Convert discrete-time transition matrices to continuous-time intensities
# Method: Following Galindo et al. (2024) - Lambda = log(Pi) / delta_t
# Output: Lambda intensity matrices for 2-state, 4-state, and 6-state systems
################################################################################

library(expm)   # For matrix logarithm and exponential
library(dplyr)

data_dir <- "C:/Users/HP/OneDrive/Documentos/UChicago/4. Thesis/versions/1. argentinian tax/1. data/output_data"
setwd(data_dir)

# Quarterly observation interval
delta_t <- 0.25  # 3 months = 0.25 years


#########################
# FUNCTION: Convert discrete transition matrix to continuous intensities
#########################

compute_lambda_matrix <- function(Pi_empirical, delta_t, state_name) {
  
  cat("\n========================================\n")
  cat("Processing:", state_name, "\n")
  cat("========================================\n")
  
  # 0. DIAGNOSTICS: Check matrix validity
  cat("\nInput Transition Matrix:\n")
  print(round(Pi_empirical, 6))
  
  cat("\nRow sums (should all be 1):\n")
  print(round(rowSums(Pi_empirical), 6))
  
  cat("\nDiagonal elements (self-persistence):\n")
  print(round(diag(Pi_empirical), 6))
  
  # Check for problematic rows
  row_sums <- rowSums(Pi_empirical)
  zero_rows <- which(abs(row_sums) < 1e-10)
  
  if (length(zero_rows) > 0) {
    cat("\nERROR: Found rows that sum to zero (no transitions observed):\n")
    print(rownames(Pi_empirical)[zero_rows])
    cat("\nCannot compute matrix logarithm. These states need more data.\n")
    return(NULL)
  }
  
  # Check if row sums are close to 1
  if (max(abs(row_sums - 1)) > 1e-6) {
    cat("\nWARNING: Row sums are not exactly 1. Renormalizing...\n")
    Pi_empirical <- Pi_empirical / rowSums(Pi_empirical)
  }
  
  # Check for zero diagonals (states with no persistence)
  zero_diag <- which(diag(Pi_empirical) < 1e-10)
  if (length(zero_diag) > 0) {
    cat("\nWARNING: States with near-zero diagonal (no persistence):\n")
    print(rownames(Pi_empirical)[zero_diag])
    cat("These states may cause numerical issues.\n")
  }
  
  # 1. Compute generator matrix: Lambda = log(Pi) / delta_t
  Lambda_matrix <- tryCatch({
    logm(Pi_empirical) / delta_t
  }, error = function(e) {
    cat("\nERROR: Cannot compute matrix logarithm.\n")
    cat("Error message:", e$message, "\n")
    cat("\nThis usually means:\n")
    cat("- Some states are not reachable from others (non-irreducible)\n")
    cat("- Matrix is not a valid transition matrix\n")
    cat("- Some states have too few observations\n")
    return(NULL)
  })
  
  if (is.null(Lambda_matrix)) {
    return(NULL)
  }
  
  # Check for NaN/Inf values
  if (any(is.na(Lambda_matrix)) || any(is.infinite(Lambda_matrix))) {
    cat("\nERROR: Matrix logarithm contains NaN or Inf values.\n")
    cat("This matrix is likely not log-valid (non-irreducible or has eigenvalue issues).\n")
    return(NULL)
  }
  
  # 2. Check for negative off-diagonals
  off_diag_negative <- which(Lambda_matrix < -1e-10 & row(Lambda_matrix) != col(Lambda_matrix), arr.ind = TRUE)
  
  if (nrow(off_diag_negative) > 0) {
    cat("\nWARNING: Found", nrow(off_diag_negative), "negative off-diagonal elements\n")
    print(off_diag_negative)
    cat("Setting them to zero...\n")
    
    # Set problematic entries to zero
    for (i in 1:nrow(off_diag_negative)) {
      Lambda_matrix[off_diag_negative[i, 1], off_diag_negative[i, 2]] <- 0.0
    }
  }
  
  # 3. Recalculate diagonals to ensure row sums = 0
  diag(Lambda_matrix) <- 0
  diag(Lambda_matrix) <- -rowSums(Lambda_matrix)
  
  # 4. Verify row sums
  row_sums <- rowSums(Lambda_matrix)
  if (any(is.na(row_sums)) || max(abs(row_sums)) > 1e-10) {
    if (any(is.na(row_sums))) {
      cat("\nERROR: Row sums contain NA values\n")
    } else {
      cat("\nWARNING: Row sums not exactly zero (max deviation:", max(abs(row_sums)), ")\n")
    }
  }
  
  # 5. Display intensity matrix
  cat("\nIntensity Matrix (Lambda):\n")
  print(round(Lambda_matrix, 6))
  
  cat("\nRow sums (should be ~0):\n")
  print(round(row_sums, 10))
  
  # 6. Compute expected duration in each state
  cat("\nExpected Duration in Each State (years):\n")
  expected_duration <- -1 / diag(Lambda_matrix)
  
  # Make sure we have row names before creating the data frame
  if (is.null(rownames(Lambda_matrix)) || length(rownames(Lambda_matrix)) == 0) {
    state_names <- paste0("State_", 1:nrow(Lambda_matrix))
  } else {
    state_names <- rownames(Lambda_matrix)
  }
  
  duration_df <- data.frame(
    State = state_names,
    Duration_Years = expected_duration,
    Duration_Months = expected_duration * 12
  )
  print(duration_df)
  
  # 7. Compute stationary distribution
  eigen_values <- eigen(t(Lambda_matrix))
  stationary_idx <- which.min(abs(eigen_values$values))
  stationary_dist <- Re(eigen_values$vectors[, stationary_idx])
  stationary_dist <- stationary_dist / sum(stationary_dist)
  
  cat("\nStationary Distribution:\n")
  stat_df <- data.frame(
    State = state_names,
    Proportion = stationary_dist
  )
  print(stat_df)
  
  # 8. Verify we can recover original transition matrix
  Lambda_quarterly <- Lambda_matrix * delta_t
  P_recovered <- expm(Lambda_quarterly)
  
  cat("\nRecovered Quarterly Transition Matrix (should match input):\n")
  print(round(P_recovered, 6))
  
  cat("\nRow sums of recovered matrix (should be 1):\n")
  print(round(rowSums(P_recovered), 10))
  
  # 9. Compute annual transition matrix
  Lambda_annual <- Lambda_matrix * 1.0
  P_annual <- expm(Lambda_annual)
  
  cat("\nAnnual Transition Matrix:\n")
  print(round(P_annual, 6))
  
  return(Lambda_matrix)
}


#########################
# PROCESS 2-STATE SYSTEM
#########################

Pi_2state <- read.csv("transition_matrix_2state.csv")

# Check column names and adjust accordingly
state_col <- names(Pi_2state)[1]  # Get the first column name (state identifier)

# Convert to matrix
Pi_2 <- Pi_2state %>%
  select(-1) %>%  # Remove first column (state names)
  as.matrix()

# Set row and column names
rownames(Pi_2) <- Pi_2state[[state_col]]
colnames(Pi_2) <- names(Pi_2state)[-1]

Lambda_2 <- compute_lambda_matrix(Pi_2, delta_t, "2-STATE (F/I)")


#########################
# PROCESS 4-STATE SYSTEM
#########################

Pi_4state <- read.csv("transition_matrix_4state.csv")

# Check column names and adjust accordingly
state_col <- names(Pi_4state)[1]

# Convert to matrix
Pi_4 <- Pi_4state %>%
  select(-1) %>%
  as.matrix()

# Set row and column names
rownames(Pi_4) <- Pi_4state[[state_col]]
colnames(Pi_4) <- names(Pi_4state)[-1]

Lambda_4 <- compute_lambda_matrix(Pi_4, delta_t, "4-STATE (FH/FL/IH/IL)")


#########################
# PROCESS 6-STATE SYSTEM
#########################

#### NOT USED FOR NOW

# Pi_6state <- read.csv("transition_matrix_6state.csv")
# 
# # Check column names and adjust accordingly
# state_col <- names(Pi_6state)[1]
# 
# # Convert to matrix
# Pi_6 <- Pi_6state %>%
#   select(-1) %>%
#   as.matrix()
# 
# # Set row and column names
# rownames(Pi_6) <- Pi_6state[[state_col]]
# colnames(Pi_6) <- names(Pi_6state)[-1]
# 
# Lambda_6 <- compute_lambda_matrix(Pi_6, delta_t, "6-STATE (FT_H/FT_L/MS_H/MS_L/I_H/I_L)")


#########################
# SAVE LAMBDA MATRICES
#########################

# Save with row names for clarity
write.csv(Lambda_2, file = "lambda_matrix_2state.csv", row.names = TRUE)
write.csv(Lambda_4, file = "lambda_matrix_4state.csv", row.names = TRUE)
#write.csv(Lambda_6, file = "lambda_matrix_6state.csv", row.names = TRUE)

cat("\n========================================\n")
cat("Lambda matrices saved:\n")
cat("- lambda_matrix_2state.csv\n")
cat("- lambda_matrix_4state.csv\n")
cat("========================================\n")


#########################
# INTERPRETATION GUIDE
#########################

cat("\n========================================\n")
cat("INTERPRETATION OF LAMBDA INTENSITIES\n")
cat("========================================\n")

cat("\n
Lambda (λ) represents the instantaneous rate of transition between states:
- λ_ij = intensity of transitioning from state i to state j (i ≠ j)
- λ_ii = -(sum of all exit rates from state i)

Expected duration in state i = -1 / λ_ii years

Higher |λ_ii| → shorter duration → more fluid/unstable state
Lower |λ_ii| → longer duration → more stable/persistent state

The diagonal elements ensure that: Σ_j λ_ij = 0 for all rows i
(Conservation of probability)
\n")

cat("\n========================================\n")
cat("TYPICAL PATTERNS IN LABOR MARKETS:\n")
cat("========================================\n")

cat("\n
- Formal-High (FH): Usually most stable → lowest |λ_ii|
- Informal-High (IH): Often most fluid → highest |λ_ii|
- Formal-Low (FL) and Informal-Low (IL): Intermediate stability

Transitions from Informal → Formal typically have lower intensity than
Formal → Informal in countries with high informality.
\n")

