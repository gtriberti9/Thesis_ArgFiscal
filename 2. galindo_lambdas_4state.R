################################################################################
# Achdou et al. (2021) Estimation Method for Lambda Intensities for 4-state
# Purpose: Estimate lambda intensities to match observed informality rate
# States: FH (Formal-High), FL (Formal-Low), IH (Informal-High), IL (Informal-Low)
################################################################################

library(expm)
library(dplyr)

install.packages("nloptr")
library(nloptr)  # For optimization

data_dir <- "C:/Users/HP/OneDrive/Documentos/UChicago/4. Thesis/versions/1. argentinian tax/1. data/output_data"
setwd(data_dir)

#########################
# 1. LOAD EMPIRICAL DATA
#########################

Pi_4state <- read.csv("transition_matrix_4state.csv")

# Convert to matrix
state_col <- names(Pi_4state)[1]
Pi_4 <- Pi_4state %>%
  select(-1) %>%
  as.matrix()

rownames(Pi_4) <- Pi_4state[[state_col]]
colnames(Pi_4) <- names(Pi_4state)[-1]

cat("========================================\n")
cat("EMPIRICAL TRANSITION MATRIX (Quarterly)\n")
cat("========================================\n")
print(round(Pi_4, 4))

delta_t <- 0.25

#########################
# 2. COMPUTE LAMBDA FROM DATA VIA MATRIX LOGARITHM
#########################

cat("\n========================================\n")
cat("LAMBDA MATRIX (Data-Driven)\n")
cat("==========================================\n")

Lambda_initial <- logm(Pi_4) / delta_t

state_names <- rownames(Pi_4)
rownames(Lambda_initial) <- state_names
colnames(Lambda_initial) <- state_names

#########################
# 2A. EMBEDDABILITY CHECK
#########################

cat("\n========================================\n")
cat("EMBEDDABILITY DIAGNOSTIC\n")
cat("========================================\n")

# Check eigenvalues of empirical P
P_evals <- eigen(Pi_4)$values
cat("\nEigenvalues of empirical P (should be real, in [0,1]):\n")
print(round(P_evals, 4))

# Count negative off-diagonals (indicator of embeddability issues)
neg_offdiag <- sum(Lambda_initial[col(Lambda_initial) != row(Lambda_initial)] < 0)
cat("\nNumber of negative off-diagonals in raw Lambda:", neg_offdiag, "\n")

if (neg_offdiag > 0) {
  cat("Note: Negative off-diagonals detected. Will set to zero (embeddability adjustment).\n")
}

# Compute recovery error after cleaning (will show later)
cat("\n")

# Clean up small negative off-diagonals (embeddability handling)
Lambda_data <- Lambda_initial
for (i in 1:nrow(Lambda_data)) {
  for (j in 1:ncol(Lambda_data)) {
    if (i != j && Lambda_data[i, j] < 0) {
      Lambda_data[i, j] <- 0
    }
  }
}

rownames(Lambda_data) <- state_names
colnames(Lambda_data) <- state_names

# Recalculate diagonals to maintain row-sum-zero constraint
diag(Lambda_data) <- 0
diag(Lambda_data) <- -rowSums(Lambda_data)

cat("Lambda Matrix (from data, cleaned):\n")
print(round(Lambda_data, 4))

cat("\nRow sums (should be ~0):\n")
print(round(rowSums(Lambda_data), 10))

# Recovery error check
P_recovered <- expm(Lambda_data * delta_t)
recovery_error <- max(abs(P_recovered - Pi_4))
cat("\nRecovery error (max absolute difference):", format(recovery_error, scientific = TRUE), "\n")
if (recovery_error < 1e-2) {
  cat("✓ Embeddability check PASSED: Q represents P accurately.\n")
} else {
  cat("⚠ Embeddability check WARNING: Recovery error is", recovery_error, "\n")
}

#########################
# 3. STATIONARY DISTRIBUTION
#########################

cat("\n========================================\n")
cat("Implied Stationary Distribution (from data)\n")
cat("========================================\n")

eigen_vals <- eigen(t(Lambda_data))
stat_idx <- which.min(abs(eigen_vals$values))
stat_dist_data <- Re(eigen_vals$vectors[, stat_idx])
stat_dist_data <- stat_dist_data / sum(stat_dist_data)
names(stat_dist_data) <- rownames(Pi_4)

print(round(stat_dist_data, 4))

# Aggregate
formal_share <- sum(stat_dist_data[grepl("^F", names(stat_dist_data))])
informal_share <- sum(stat_dist_data[grepl("^I", names(stat_dist_data))])

cat("\nAggregated:\n")
cat("Total Formal:", round(formal_share, 4), "\n")
cat("Total Informal:", round(informal_share, 4), "\n")

#########################
# 4. EXPECTED STATE DURATIONS
#########################

cat("\n========================================\n")
cat("Expected State Durations\n")
cat("========================================\n")

durations_years <- -1 / diag(Lambda_data)
durations_months <- durations_years * 12

duration_df <- data.frame(
  State = rownames(Pi_4),
  Duration_Years = durations_years,
  Duration_Months = durations_months
)
rownames(duration_df) <- NULL

print(duration_df)

#########################
# 5. IMPLIED TRANSITION MATRICES
#########################

cat("\n========================================\n")
cat("Quarterly Transition Matrix (Recovered from Lambda)\n")
cat("========================================\n")

P_quarterly_data <- expm(Lambda_data * delta_t)
print(round(P_quarterly_data, 4))

cat("\nRecovery Error (should be ~0):\n")
print(max(abs(P_quarterly_data - Pi_4)))

cat("\n========================================\n")
cat("Annual Transition Matrix\n")
cat("========================================\n")

P_annual_data <- expm(Lambda_data * 1.0)
print(round(P_annual_data, 4))

#########################
# 6. SAVE OUTPUT
#########################

write.csv(Lambda_data, file = "lambda_matrix_4state_datadriven.csv", row.names = TRUE)
write.csv(P_quarterly_data, file = "transition_matrix_4state_quarterly_datadriven.csv", row.names = TRUE)
write.csv(P_annual_data, file = "transition_matrix_4state_annual_datadriven.csv", row.names = TRUE)

cat("\n========================================\n")
cat("Output saved.\n")
cat("========================================\n")



# ----- CHECK 1: Recover observed quarterly transition matrix -----
P_hat <- expm(Lambda_data * delta_t)
rec_err <- max(abs(P_hat - Pi_4))
cat("\nMax abs recovery error |exp(Q*dt)-P|:", rec_err, "\n")

# ----- CHECK 2: Informality rate recovery -----
formal_states <- c("FH", "FL")
informal_states <- c("IH", "IL")

# Observed current distribution from clean_data (weighted shares)
v_obs <- c(FH = 0.408, FL = 0.179, IH = 0.0759, IL = 0.337)
v_obs <- v_obs / sum(v_obs)

inf_obs <- sum(v_obs[informal_states])
cat("\nObserved informality (current cross-section):", round(inf_obs, 4), "\n")

# (a) One-quarter-ahead implied informality from Q
v_next <- as.numeric(v_obs %*% P_hat)
names(v_next) <- names(v_obs)
inf_next <- sum(v_next[informal_states])
cat("Implied informality next quarter from Q:", round(inf_next, 4), "\n")

# (b) Stationary (long-run) informality implied by Q
ev <- eigen(t(Lambda_data))
idx <- which.min(abs(ev$values))
pi_star <- Re(ev$vectors[, idx])
pi_star <- pi_star / sum(pi_star)
names(pi_star) <- rownames(Pi_4)
inf_star <- sum(pi_star[informal_states])
cat("Implied informality in stationary distribution:", round(inf_star, 4), "\n")


#######################
# CALIBRATION OF THE FORMAL ECONOMY BENCHMARK
#######################

# While the Lambda matrix for Argentina is data-driven. The matrix for the formal
# economy benchmar with aroun 20% of informality needs to be recomputed. 

cat("\n========================================\n")
cat("BENCHMARK CALIBRATION: FORMAL 0.80 / INFORMAL 0.20\n")
cat("========================================\n")

# Uses Lambda_data (estimated from Argentina transitions) as the baseline structure.

compute_stationary <- function(Lambda) {
  ev <- eigen(t(Lambda))
  idx <- which.min(abs(ev$values))
  pi <- Re(ev$vectors[, idx])
  pi <- pi / sum(pi)
  names(pi) <- rownames(Lambda)
  pi
}

# Build target by changing only aggregate formal/informal shares,
# preserving within-formal and within-informal composition from v_obs.
build_target_dist <- function(v_base, formal_target = 0.80, informal_target = 0.20) {
  formal_states <- names(v_base)[grepl("^F", names(v_base))]
  informal_states <- names(v_base)[grepl("^I", names(v_base))]

  wF <- v_base[formal_states] / sum(v_base[formal_states])
  wI <- v_base[informal_states] / sum(v_base[informal_states])

  target <- numeric(length(v_base))
  names(target) <- names(v_base)
  target[formal_states] <- formal_target * wF
  target[informal_states] <- informal_target * wI
  target / sum(target)
}

# Choose composition source (current observed cross-section)
v_base <- v_obs
target_dist_bench <- build_target_dist(v_base, formal_target = 0.80, informal_target = 0.20)

cat("\nTarget benchmark distribution:\n")
print(round(target_dist_bench, 4))
cat("Target informal share:", round(sum(target_dist_bench[grepl("^I", names(target_dist_bench))]), 4), "\n")

# Scale map:
# s1: FH->FL
# s2: FH->I*
# s3: FL->FH
# s4: FL->I*
# s5: I*->FH
# s6: I*->FL
apply_scales <- function(Lambda_base, s) {
  L <- Lambda_base

  # F->F
  L["FH", "FL"] <- Lambda_base["FH", "FL"] * s[1]
  L["FL", "FH"] <- Lambda_base["FL", "FH"] * s[3]

  # F->I
  L["FH", "IH"] <- Lambda_base["FH", "IH"] * s[2]
  L["FH", "IL"] <- Lambda_base["FH", "IL"] * s[2]
  L["FL", "IH"] <- Lambda_base["FL", "IH"] * s[4]
  L["FL", "IL"] <- Lambda_base["FL", "IL"] * s[4]

  # I->F
  L["IH", "FH"] <- Lambda_base["IH", "FH"] * s[5]
  L["IL", "FH"] <- Lambda_base["IL", "FH"] * s[5]
  L["IH", "FL"] <- Lambda_base["IH", "FL"] * s[6]
  L["IL", "FL"] <- Lambda_base["IL", "FL"] * s[6]

  # Keep I->I transitions unchanged
  L["IH", "IL"] <- Lambda_base["IH", "IL"]
  L["IL", "IH"] <- Lambda_base["IL", "IH"]

  # Enforce generator structure
  diag(L) <- 0
  diag(L) <- -rowSums(L)
  L
}

objective_bench <- function(s, Lambda_base, target_dist) {
  L <- apply_scales(Lambda_base, s)
  pi_hat <- compute_stationary(L)
  sum((pi_hat - target_dist)^2)
}

# Ensure names are exactly FH, FL, IH, IL
if (!all(c("FH", "FL", "IH", "IL") %in% rownames(Lambda_data))) {
  stop("Lambda_data row names must include FH, FL, IH, IL.")
}

# Optimize scale parameters
x0 <- rep(1, 6)
opt <- nloptr(
  x0 = x0,
  eval_f = function(x) objective_bench(x, Lambda_data, target_dist_bench),
  lb = rep(0.1, 6),
  ub = rep(10, 6),
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    xtol_rel = 1e-10,
    maxeval = 20000
  )
)

scales_bench <- opt$solution
Lambda_benchmark <- apply_scales(Lambda_data, scales_bench)
pi_benchmark <- compute_stationary(Lambda_benchmark)

cat("\nOptimal scale parameters:\n")
print(round(scales_bench, 4))

cat("\nBenchmark stationary distribution:\n")
print(round(pi_benchmark, 4))
cat("Benchmark informal share:", round(sum(pi_benchmark[grepl("^I", names(pi_benchmark))]), 4), "\n")

cat("\nSSE vs target:", signif(sum((pi_benchmark - target_dist_bench)^2), 6), "\n")
cat("Row sums (benchmark Lambda, should be 0):\n")
print(round(rowSums(Lambda_benchmark), 12))

# Implied transition matrices
P_quarterly_benchmark <- expm(Lambda_benchmark * delta_t)
P_annual_benchmark <- expm(Lambda_benchmark * 1.0)

cat("\nQuarterly benchmark transition matrix:\n")
print(round(P_quarterly_benchmark, 4))

cat("\nAnnual benchmark transition matrix:\n")
print(round(P_annual_benchmark, 4))

# Save benchmark outputs
write.csv(Lambda_benchmark, "lambda_matrix_4state_benchmark_eta20.csv", row.names = TRUE)
write.csv(P_quarterly_benchmark, "transition_matrix_4state_quarterly_benchmark_eta20.csv", row.names = TRUE)
write.csv(P_annual_benchmark, "transition_matrix_4state_annual_benchmark_eta20.csv", row.names = TRUE)

cat("\nBenchmark outputs saved.\n")

#######################
# CALIBRATION OF ARGENTINA TARGET (FORMAL 0.58 / INFORMAL 0.42)
#######################

cat("\n========================================\n")
cat("ARGENTINA TARGET CALIBRATION: FORMAL 0.58 / INFORMAL 0.42\n")
cat("========================================\n")

target_dist_arg <- build_target_dist(v_base, formal_target = 0.58, informal_target = 0.42)

cat("\nTarget Argentina distribution:\n")
print(round(target_dist_arg, 4))
cat("Target informal share:", round(sum(target_dist_arg[grepl("^I", names(target_dist_arg))]), 4), "\n")

opt_arg <- nloptr(
  x0 = x0,
  eval_f = function(x) objective_bench(x, Lambda_data, target_dist_arg),
  lb = rep(0.1, 6),
  ub = rep(10, 6),
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    xtol_rel = 1e-10,
    maxeval = 20000
  )
)

scales_arg <- opt_arg$solution
Lambda_arg_target <- apply_scales(Lambda_data, scales_arg)
pi_arg_target <- compute_stationary(Lambda_arg_target)

cat("\nOptimal scale parameters (Argentina target):\n")
print(round(scales_arg, 4))

cat("\nArgentina-target stationary distribution:\n")
print(round(pi_arg_target, 4))
cat("Argentina-target informal share:", round(sum(pi_arg_target[grepl("^I", names(pi_arg_target))]), 4), "\n")

cat("\nSSE vs target:", signif(sum((pi_arg_target - target_dist_arg)^2), 6), "\n")
cat("Row sums (Argentina-target Lambda, should be 0):\n")
print(round(rowSums(Lambda_arg_target), 12))

P_quarterly_arg_target <- expm(Lambda_arg_target * delta_t)
P_annual_arg_target <- expm(Lambda_arg_target * 1.0)

cat("\nQuarterly Argentina-target transition matrix:\n")
print(round(P_quarterly_arg_target, 4))

cat("\nAnnual Argentina-target transition matrix:\n")
print(round(P_annual_arg_target, 4))

write.csv(Lambda_arg_target, "lambda_matrix_4state_datadriven.csv", row.names = TRUE)
write.csv(P_quarterly_arg_target, "transition_matrix_4state_quarterly_datadriven.csv", row.names = TRUE)
write.csv(P_annual_arg_target, "transition_matrix_4state_annual_datadriven.csv", row.names = TRUE)

cat("\nArgentina-target outputs saved with data-driven filenames.\n")





#########################
# EMBEDDABILITY ROBUSTNESS CHECKS (JUSTIFICATION BLOCK)
#########################

# Assumes available objects:
# Pi_4, Lambda_initial, delta_t, state_names
# Pi_4, Lambda_initial, delta_t, state_names
# packages: expm, dplyr (already in your script)

fro_norm <- function(M) sqrt(sum(M^2))

make_generator_from_raw <- function(Lraw, neg_tol = 0) {
  L <- Re(Lraw)
  n <- nrow(L)

  # Clip only off-diagonal negatives below tolerance
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && L[i, j] < neg_tol) L[i, j] <- 0
    }
  }

  # Re-impose generator diagonals
  diag(L) <- 0
  diag(L) <- -rowSums(L)

  rownames(L) <- rownames(Lraw)
  colnames(L) <- colnames(Lraw)
  L
}

embeddability_report <- function(P, Lraw, dt, neg_tol = 0, label = "clip<0") {
  Lcorr <- make_generator_from_raw(Lraw, neg_tol = neg_tol)

  # Diagnostics on raw logm
  off_idx <- row(Lraw) != col(Lraw)
  raw_off <- Re(Lraw[off_idx])

  neg_vals <- raw_off[raw_off < 0]
  pos_vals <- raw_off[raw_off > 0]

  n_neg <- length(neg_vals)
  min_neg <- if (n_neg > 0) min(neg_vals) else 0
  sum_abs_neg <- if (n_neg > 0) sum(abs(neg_vals)) else 0
  median_pos <- if (length(pos_vals) > 0) median(pos_vals) else NA_real_
  ratio_neg_to_pos <- if (!is.na(median_pos) && median_pos > 0) sum_abs_neg / sum(pos_vals) else NA_real_

  # Fit checks
  P_hat_raw <- expm::expm(Re(Lraw) * dt)
  P_hat_corr <- expm::expm(Lcorr * dt)

  err_raw_max <- max(abs(P_hat_raw - P))
  err_corr_max <- max(abs(P_hat_corr - P))
  err_raw_fro <- fro_norm(P_hat_raw - P)
  err_corr_fro <- fro_norm(P_hat_corr - P)

  # Stationary distribution comparison
  stat_from_generator <- function(L) {
    ev <- eigen(t(L))
    k <- which.min(abs(ev$values))
    v <- Re(ev$vectors[, k])
    v <- v / sum(v)
    v
  }

  pi_raw <- stat_from_generator(Re(Lraw))
  pi_corr <- stat_from_generator(Lcorr)
  pi_gap_l1 <- sum(abs(pi_corr - pi_raw))
  names(pi_raw) <- rownames(P)
  names(pi_corr) <- rownames(P)

  # Generator validity checks for corrected matrix
  corr_off <- Lcorr[off_idx]
  valid_off_nonneg <- all(corr_off >= -1e-14)
  valid_row_sum <- max(abs(rowSums(Lcorr))) <= 1e-12
  valid_real <- all(Im(Lcorr) == 0)

  list(
    label = label,
    n_neg_offdiag_raw = n_neg,
    min_neg_offdiag_raw = min_neg,
    sum_abs_neg_raw = sum_abs_neg,
    ratio_neg_to_total_positive_mass = ratio_neg_to_pos,
    err_raw_max = err_raw_max,
    err_corr_max = err_corr_max,
    err_raw_fro = err_raw_fro,
    err_corr_fro = err_corr_fro,
    stationary_L1_gap_corr_vs_raw = pi_gap_l1,
    corrected_generator_valid = (valid_off_nonneg && valid_row_sum && valid_real),
    pi_raw = pi_raw,
    pi_corr = pi_corr,
    Lambda_corr = Lcorr
  )
}

cat("\n========================================\n")
cat("EMBEDDABILITY JUSTIFICATION CHECKS\n")
cat("========================================\n")

# Main correction used in your paper code
rep0 <- embeddability_report(Pi_4, Lambda_initial, delta_t, neg_tol = 0, label = "clip<0")

# Optional stricter variant: clip tiny negatives below -1e-10 only
rep1 <- embeddability_report(Pi_4, Lambda_initial, delta_t, neg_tol = -1e-10, label = "clip<-1e-10")

summary_tbl <- data.frame(
  Correction = c(rep0$label, rep1$label),
  Raw_Neg_OffDiag_Count = c(rep0$n_neg_offdiag_raw, rep1$n_neg_offdiag_raw),
  Raw_Min_Neg_OffDiag = c(rep0$min_neg_offdiag_raw, rep1$min_neg_offdiag_raw),
  Raw_SumAbs_Neg = c(rep0$sum_abs_neg_raw, rep1$sum_abs_neg_raw),
  RawNeg_to_PosMass = c(rep0$ratio_neg_to_total_positive_mass, rep1$ratio_neg_to_total_positive_mass),
  MaxErr_exp_raw_vs_P = c(rep0$err_raw_max, rep1$err_raw_max),
  MaxErr_exp_corr_vs_P = c(rep0$err_corr_max, rep1$err_corr_max),
  FroErr_exp_raw_vs_P = c(rep0$err_raw_fro, rep1$err_raw_fro),
  FroErr_exp_corr_vs_P = c(rep0$err_corr_fro, rep1$err_corr_fro),
  StatDist_L1_gap = c(rep0$stationary_L1_gap_corr_vs_raw, rep1$stationary_L1_gap_corr_vs_raw),
  Corr_Generator_Valid = c(rep0$corrected_generator_valid, rep1$corrected_generator_valid)
)

print(summary_tbl)

cat("\nStationary distribution comparison (raw logm generator vs corrected):\n")
st_cmp <- data.frame(
  State = rownames(Pi_4),
  pi_raw = as.numeric(rep0$pi_raw),
  pi_corrected = as.numeric(rep0$pi_corr),
  abs_diff = abs(as.numeric(rep0$pi_corr) - as.numeric(rep0$pi_raw))
)
print(st_cmp)

cat("\nDecision guide:\n")
cat("1) If Raw_SumAbs_Neg is tiny and RawNeg_to_PosMass is tiny, negatives are likely numerical noise.\n")
cat("2) If MaxErr_exp_corr_vs_P remains very small, correction does not distort observed transitions.\n")
cat("3) If StatDist_L1_gap is very small, long-run implications are unchanged.\n")
cat("4) If all true, clipping is a minor regularization, not hard-coding.\n")





