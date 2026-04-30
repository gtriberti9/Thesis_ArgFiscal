################################################################################
# Achdou et al. (2021) Estimation Method for Lambda Intensities
# Purpose: Estimate lambda intensities to match observed informality rate
# Method: Use steady-state relationship between intensities and informality
################################################################################

library(expm)
library(dplyr)

data_dir <- "C:/Users/HP/OneDrive/Documentos/UChicago/4. Thesis/versions/1. argentinian tax/1. data/output_data"
setwd(data_dir)

#########################
# 1. LOAD EMPIRICAL TRANSITION MATRIX
#########################

Pi_2state <- read.csv("transition_matrix_2state.csv")

# Convert to matrix
state_col <- names(Pi_2state)[1]
Pi_2 <- Pi_2state %>%
  select(-1) %>%
  as.matrix()

rownames(Pi_2) <- Pi_2state[[state_col]]
colnames(Pi_2) <- names(Pi_2state)[-1]

# Normalize labels to F/I and enforce matrix order [F, I]
normalize_state_label <- function(x) {
  lx <- tolower(trimws(x))
  if (grepl("^(f|formal)$", lx)) return("F")
  if (grepl("^(i|informal)$", lx)) return("I")
  stop(paste("Unrecognized 2-state label:", x, "(expected Formal/Informal or F/I)."))
}

row_labels_raw <- rownames(Pi_2)
col_labels_raw <- colnames(Pi_2)
row_labels_norm <- vapply(row_labels_raw, normalize_state_label, character(1))
col_labels_norm <- vapply(col_labels_raw, normalize_state_label, character(1))

if (!setequal(row_labels_norm, c("F", "I")) || !setequal(col_labels_norm, c("F", "I"))) {
  stop("Pi_2 must contain exactly one formal and one informal state in both rows and columns.")
}

rownames(Pi_2) <- row_labels_norm
colnames(Pi_2) <- col_labels_norm
Pi_2 <- Pi_2[c("F", "I"), c("F", "I")]

cat("========================================\n")
cat("EMPIRICAL TRANSITION MATRIX (Quarterly)\n")
cat("========================================\n")
print(round(Pi_2, 6))

# Quarterly observation interval
delta_t <- 0.25


#########################
# EMBEDDABILITY CHECK (2-state)
#########################

check_embeddability_2state <- function(P, delta_t, tol = 1e-10) {
  if (!all(dim(P) == c(2, 2))) stop("This check is for 2x2 matrices only.")

  # Basic probability checks
  row_err <- max(abs(rowSums(P) - 1))
  if (row_err > 1e-8) {
    cat("\nWARNING: Row sums are not 1 (max error =", row_err, ").\n")
  }
  if (any(P < -tol) || any(P > 1 + tol)) {
    cat("\nWARNING: Matrix has entries outside [0,1].\n")
  }

  # Key 2-state embeddability condition:
  # second eigenvalue = p11 + p22 - 1 must be strictly positive
  rho <- P[1,1] + P[2,2] - 1

  cat("\n=== 2-STATE EMBEDDABILITY CHECK ===\n")
  cat("rho = p11 + p22 - 1 =", round(rho, 10), "\n")

  if (rho <= tol) {
    cat("Result: NOT embeddable in a homogeneous CTMC (rho <= 0).\n")
    return(invisible(list(embeddable = FALSE, rho = rho)))
  }

  # Implied total hazard and directional hazards
  s <- -log(rho) / delta_t     #turnover speed
  lambda12 <- P[1,2] * s / (1 - rho)   # state1 -> state2
  lambda21 <- P[2,1] * s / (1 - rho)   # state2 -> state1

  cat("Result: Embeddable (rho > 0).\n")
  cat("Implied total hazard s =", round(s, 10), "\n")
  cat("Implied lambda_12 =", round(lambda12, 10), "\n")
  cat("Implied lambda_21 =", round(lambda21, 10), "\n")

  # Reconstruct generator and recover P for a numerical check
  Q <- matrix(c(-lambda12, lambda12,
                lambda21, -lambda21), 2, 2, byrow = TRUE)
  rownames(Q) <- rownames(P)
  colnames(Q) <- colnames(P)

  P_recovered <- expm::expm(Q * delta_t)
  rec_err <- max(abs(P_recovered - P))
  cat("Recovery error max|exp(Q*dt)-P| =", signif(rec_err, 6), "\n")

  invisible(list(
    embeddable = TRUE,
    rho = rho,
    s = s,
    lambda12 = lambda12,
    lambda21 = lambda21,
    Q = Q,
    recovery_error = rec_err
  ))
}

emb_check <- check_embeddability_2state(Pi_2, delta_t)



#########################
# 2. COMPUTE INITIAL LAMBDA FROM DATA
#########################

cat("\n========================================\n")
cat("STEP 1: Initial Lambda from Matrix Logarithm\n")
cat("========================================\n")

Lambda_initial <- logm(Pi_2) / delta_t

cat("\nInitial Lambda Matrix:\n")
print(round(Lambda_initial, 6))

# Extract off-diagonal intensities
lambda_FI_initial <- Lambda_initial[1, 2]  # Formal → Informal
lambda_IF_initial <- Lambda_initial[2, 1]  # Informal → Formal

cat("\nInitial Intensities (from data):\n")
cat("λ_FI (Formal → Informal):", round(lambda_FI_initial, 6), "\n")
cat("λ_IF (Informal → Formal):", round(lambda_IF_initial, 6), "\n")

# Stationary distribution from initial Lambda
eigen_vals <- eigen(t(Lambda_initial))
stat_idx <- which.min(abs(eigen_vals$values))
stat_dist_initial <- Re(eigen_vals$vectors[, stat_idx])
stat_dist_initial <- stat_dist_initial / sum(stat_dist_initial)

eta_initial <- stat_dist_initial[2]  # Proportion informal (assuming I is second)

cat("\nImplied Stationary Distribution:\n")
cat("Formal:", round(stat_dist_initial[1], 4), "\n")
cat("Informal (η):", round(stat_dist_initial[2], 4), "\n")

#########################
# 3. DATA-DRIVEN OUTPUTS (NO CALIBRATION)
#########################

# Use the empirical generator directly
Lambda_data <- Re(Lambda_initial)
rownames(Lambda_data) <- c("F", "I")
colnames(Lambda_data) <- c("F", "I")

cat("\nData-driven Lambda Matrix:\n")
print(round(Lambda_data, 6))

cat("\nRow sums (should be ~0):\n")
print(round(rowSums(Lambda_data), 10))

# Expected durations from data-driven hazards
duration_formal <- -1 / Lambda_data[1, 1]
duration_informal <- -1 / Lambda_data[2, 2]

cat("\nExpected duration in Formal:", round(duration_formal, 2), "years =",
    round(duration_formal * 12, 1), "months\n")
cat("Expected duration in Informal:", round(duration_informal, 2), "years =",
    round(duration_informal * 12, 1), "months\n")

# Implied transition matrices from data-driven generator
P_quarterly_data <- expm(Lambda_data * delta_t)
P_annual_data <- expm(Lambda_data * 1.0)

cat("\nQuarterly Transition Matrix (from data-driven Lambda):\n")
print(round(P_quarterly_data, 6))

cat("\nAnnual Transition Matrix (from data-driven Lambda):\n")
print(round(P_annual_data, 6))

#########################
# 4. SAVE CALIBRATED MATRIX
#########################

write.csv(Lambda_data, file = "lambda_matrix_2state_datadriven.csv", row.names = TRUE)


