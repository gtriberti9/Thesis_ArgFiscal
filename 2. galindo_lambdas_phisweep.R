################################################################################
# PHI SWEEP: Generate Lambda matrices for different IH compositions
# Fix eta = 0.42, vary phi (share of IH among informals)
# Only within-informal composition changes (IH↔IL rates)
# eta restored via uniform scaling of informal→formal exit rates
################################################################################

library(expm)
library(dplyr)

#########################
# 1. SETUP
#########################

data_dir <- "C:/Users/HP/OneDrive/Documentos/UChicago/4. Thesis/versions/1. argentinian tax/1. data/output_data"
setwd(data_dir)

# Load the baseline Lambda matrix (Argentina calibration, eta=0.42)
Lambda_raw  <- read.csv("lambda_matrix_4state_datadriven.csv", row.names = 1)
Lambda_data <- as.matrix(Lambda_raw)

cat("Baseline Lambda matrix loaded:\n")
print(round(Lambda_data, 4))
cat("Row sums:", round(rowSums(Lambda_data), 10), "\n")

#########################
# 2. PARAMETERS
#########################

eta_target <- 0.42

# Phi grid
phi_min  <- 0.10
phi_max  <- 0.60
phi_step <- 0.05
phi_grid <- seq(phi_min, phi_max, by = phi_step)

cat(sprintf("\nPhi grid: %.3f to %.3f in steps of %.3f (%d points)\n",
            phi_min, phi_max, phi_step, length(phi_grid)))

# Baseline within-informal speed (held fixed throughout)
s_within_base <- Lambda_data["IH", "IL"] + Lambda_data["IL", "IH"]
cat(sprintf("Baseline s_within = %.6f\n", s_within_base))

# Baseline phi
ev_base   <- eigen(t(Lambda_data))
idx_base  <- which.min(abs(ev_base$values))
pi_base   <- Re(ev_base$vectors[, idx_base])
pi_base   <- pi_base / sum(pi_base)
names(pi_base) <- rownames(Lambda_data)
eta_base  <- sum(pi_base[c("IH", "IL")])
phi_base  <- pi_base["IH"] / eta_base
cat(sprintf("Baseline eta = %.4f,  phi = %.4f\n", eta_base, phi_base))

#########################
# 3. HELPER FUNCTIONS
#########################

# Compute stationary distribution of a generator matrix
compute_stationary <- function(Lambda) {
  ev  <- eigen(t(Lambda))
  idx <- which.min(abs(ev$values))
  pi  <- Re(ev$vectors[, idx])
  pi  <- pi / sum(pi)
  names(pi) <- rownames(Lambda)
  pi
}

# Main rescaling function:
# Step 1 — set IH↔IL rates to achieve phi_target (s_within fixed)
# Step 2 — bisect on uniform scale of ALL informal→formal exit rates
#           to restore eta = eta_target
# Everything else (within-formal FH↔FL, formal→informal) unchanged.
apply_scales_composition <- function(Lambda_base, phi_target, eta_target,
                                     bisect_lo = 0.001, bisect_hi = 50.0,
                                     bisect_tol = 1e-12, bisect_maxit = 300) {

  s_within <- Lambda_base["IH", "IL"] + Lambda_base["IL", "IH"]

  # ── Step 1: set within-informal rates ──────────────────────────────────────
  L <- Lambda_base
  L["IH", "IL"] <- (1.0 - phi_target) * s_within   # IH → IL
  L["IL", "IH"] <- phi_target * s_within             # IL → IH

  # Recompute diagonals after within-informal change
  diag(L) <- 0
  diag(L) <- -rowSums(L)

  # ── Step 2: bisect on uniform informal→formal exit scale ───────────────────
  # f(s) = eta(s) - eta_target  (should cross zero)
  f <- function(s) {
    Ls <- L
    for (i in c("IH", "IL")) {
      for (j in c("FH", "FL")) {
        Ls[i, j] <- L[i, j] * s
      }
    }
    diag(Ls) <- 0
    diag(Ls) <- -rowSums(Ls)
    pi <- compute_stationary(Ls)
    as.numeric(pi["IH"] + pi["IL"]) - eta_target
  }

  # Check that root is bracketed
  f_lo <- f(bisect_lo)
  f_hi <- f(bisect_hi)

  if (sign(f_lo) == sign(f_hi)) {
    cat(sprintf("  WARNING: root not bracketed (f_lo=%.4f, f_hi=%.4f). ",
                f_lo, f_hi))
    # Try wider bounds
    bisect_lo <- 1e-6; bisect_hi <- 200.0
    f_lo <- f(bisect_lo); f_hi <- f(bisect_hi)
    if (sign(f_lo) == sign(f_hi)) {
      cat("Wider bounds also failed. Using s=1 (no eta correction).\n")
      s_star <- 1.0
    } else {
      cat("Wider bounds OK.\n")
      s_star <- NULL
    }
  } else {
    s_star <- NULL
  }

  if (is.null(s_star)) {
    for (iter in 1:bisect_maxit) {
      s_mid <- 0.5 * (bisect_lo + bisect_hi)
      f_mid <- f(s_mid)
      if (abs(f_mid) < bisect_tol) { s_star <- s_mid; break }
      if (sign(f_mid) == sign(f_lo)) {
        bisect_lo <- s_mid; f_lo <- f_mid
      } else {
        bisect_hi <- s_mid; f_hi <- f_mid
      }
    }
    if (is.null(s_star)) s_star <- 0.5 * (bisect_lo + bisect_hi)
  }

  # ── Build final Lambda ──────────────────────────────────────────────────────
  L_final <- L
  for (i in c("IH", "IL")) {
    for (j in c("FH", "FL")) {
      L_final[i, j] <- L[i, j] * s_star
    }
  }
  diag(L_final) <- 0
  diag(L_final) <- -rowSums(L_final)

  list(Lambda  = L_final,
       s_within = s_within,
       s_exit   = s_star)
}

#########################
# 4. PHI SWEEP
#########################

cat("\n========================================\n")
cat("RUNNING PHI SWEEP (composition only)\n")
cat("========================================\n")

summary_rows <- list()

for (phi_val in phi_grid) {

  cat(sprintf("\n--- phi_target = %.3f ---\n", phi_val))

  result <- apply_scales_composition(Lambda_data, phi_val, eta_target)
  L_phi  <- result$Lambda
  pi_phi <- compute_stationary(L_phi)

  eta_achieved <- as.numeric(sum(pi_phi[c("IH", "IL")]))
  phi_achieved <- as.numeric(pi_phi["IH"] / eta_achieved)
  eta_err      <- abs(eta_achieved - eta_target)
  phi_err      <- abs(phi_achieved - phi_val)

  cat(sprintf("  s_within = %.6f  s_exit = %.6f\n",
              result$s_within, result$s_exit))
  cat(sprintf("  eta = %.6f (err=%.2e)   phi = %.6f (err=%.2e)\n",
              eta_achieved, eta_err, phi_achieved, phi_err))
  cat("  Row sums:", round(rowSums(L_phi), 10), "\n")

  # ── Verification: within-informal rates ────────────────────────────────────
  cat(sprintf("  IH->IL: base=%.6f  new=%.6f  (should be %.6f)\n",
              Lambda_data["IH", "IL"],
              L_phi["IH", "IL"],
              (1.0 - phi_val) * result$s_within))
  cat(sprintf("  IL->IH: base=%.6f  new=%.6f  (should be %.6f)\n",
              Lambda_data["IL", "IH"],
              L_phi["IL", "IH"],
              phi_val * result$s_within))

  # ── Verification: unchanged rates ──────────────────────────────────────────
  cat(sprintf("  FH->FL: base=%.6f  new=%.6f  (should be unchanged)\n",
              Lambda_data["FH", "FL"], L_phi["FH", "FL"]))
  cat(sprintf("  FH->IH: base=%.6f  new=%.6f  (should be unchanged)\n",
              Lambda_data["FH", "IH"], L_phi["FH", "IH"]))

  if (eta_err > 1e-6) {
    cat(sprintf("  WARNING: eta error = %.2e > 1e-6\n", eta_err))
  }
  if (phi_err > 1e-6) {
    cat(sprintf("  WARNING: phi error = %.2e > 1e-6\n", phi_err))
  }

  # ── Save Lambda matrix ──────────────────────────────────────────────────────
  phi_label <- sprintf("%03.0f", phi_val * 1000)
  fname     <- sprintf("lambda_matrix_4state_phi%s.csv", phi_label)
  write.csv(L_phi, file = fname, row.names = TRUE)

  # ── Store summary ───────────────────────────────────────────────────────────
  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    phi_target   = phi_val,
    phi_achieved = phi_achieved,
    eta_achieved = eta_achieved,
    phi_err      = phi_err,
    eta_err      = eta_err,
    s_within     = result$s_within,
    s_exit       = result$s_exit,
    fname        = fname,
    stringsAsFactors = FALSE
  )
}

#########################
# 5. SUMMARY TABLE
#########################

summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL

cat("\n========================================\n")
cat("PHI SWEEP SUMMARY\n")
cat("========================================\n")
print(round(summary_df[, c("phi_target", "phi_achieved", "eta_achieved",
                            "phi_err", "eta_err",
                            "s_within", "s_exit")], 6))

write.csv(summary_df, "phi_sweep_summary.csv", row.names = FALSE)

cat("\nAll Lambda matrices saved.\n")
cat(sprintf("Summary saved to: phi_sweep_summary.csv\n"))
cat(sprintf("Lambda files: lambda_matrix_4state_phi100.csv ... phi225.csv\n"))