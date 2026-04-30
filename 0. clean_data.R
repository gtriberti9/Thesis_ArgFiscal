################################################################################
# EPH Data Cleaning and Transition Matrix Construction
# Purpose: Clean EPH panel data and construct weighted transition matrices
# Output: 2-state, 4-state, and 6-state transition probability matrices
################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(arrow)
library(readr)

years <- c("2016","2017","2018","2019","2020","2021","2022","2023","2024","2025")
trim  <- c("1","2","3","4")

# Base directory
data_dir <- "C:/Users/HP/OneDrive/Documentos/UChicago/4. Thesis/versions/1. argentinian tax/1. data"

# Where ZIP files are located
data_dir_input <- file.path(data_dir, "input_data")

# Where data will be stored
output_dir <- file.path(data_dir, "output_data")

#########################
# 1. EXTRACT DATA FROM ZIP FILES
#########################

# IDs + interview flag + WEIGHTS
id_vars <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "H15", "PONDERA")

# Labor variables
labor_vars <- c("PP05I", "PP07H", "PP07G4", "PP07G_59", "P21", "CAT_OCUP", "ESTADO", "EMPLEO")

vars <- c(id_vars, labor_vars)

out_dir <- file.path(data_dir_input, "parquet_individual_ids")
unlink(out_dir, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

to_numeric_strict <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("NA", "", "NaN", "NULL", ".", " ")] <- NA_character_
  suppressWarnings(readr::parse_number(x_chr))
}

ensure_vars_exist <- function(df, vars) {
  missing <- setdiff(vars, names(df))
  if (length(missing) > 0) df[missing] <- NA
  df
}


# Extracting and reading each zip file from all years
process_one_quarter <- function(y, t, data_dir_input, vars, out_dir) {
  zip_name <- paste0("EPH_usu_", t, "_Trim_", y, "_xls.zip")
  zip_path <- file.path(data_dir_input, zip_name)
  key      <- paste0(y, "_", t)
  out_file <- file.path(out_dir, paste0("individual_", key, ".parquet"))
  
  if (!file.exists(zip_path)) {
    message("Skipping ZIP (not found): ", zip_name)
    return(invisible(NULL))
  }
  
  zip_list <- tryCatch(unzip(zip_path, list = TRUE),
                       error = function(e) { message("Cannot read ZIP: ", zip_name); NULL })
  if (is.null(zip_list)) return(invisible(NULL))
  
  target <- zip_list$Name[
    grepl("individual", zip_list$Name, ignore.case = TRUE) &
      grepl("\\.xlsx?$", zip_list$Name, ignore.case = TRUE)
  ]
  if (length(target) == 0) {
    message("No 'individual' Excel file in: ", zip_name)
    return(invisible(NULL))
  }
  target <- target[1]
  
  tmp_dir <- file.path(tempdir(), paste0("eph_", key))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch({ unzip(zip_path, files = target, exdir = tmp_dir); TRUE },
                 error = function(e) { message("Extraction failed: ", zip_name); FALSE })
  if (!ok) return(invisible(NULL))
  
  excel_path <- file.path(tmp_dir, target)
  
  df <- tryCatch(read_excel(excel_path),
                 error = function(e) { message("Read error: ", zip_name); NULL })
  if (is.null(df)) return(invisible(NULL))
  
  df <- df %>%
    select(any_of(vars)) %>%
    ensure_vars_exist(vars) %>%
    select(all_of(vars)) %>%
    mutate(year = as.integer(y), trim = as.integer(t), .before = 1)
  
  # H15 == 1 : individual interview completed
  df <- df %>% filter(is.na(H15) | H15 == 1)
  
  # IDs: CODUSU is character
  df <- df %>% mutate(CODUSU = as.character(CODUSU))
  
  # Everything else numeric
  numeric_cols <- setdiff(names(df), c("CODUSU"))
  df <- df %>% mutate(across(all_of(numeric_cols), to_numeric_strict))
  
  write_parquet(df, out_file)
  message("Wrote: ", basename(out_file), " | rows=", nrow(df))
  invisible(out_file)
}

grid <- tidyr::crossing(year = years, trim = trim)
walk2(grid$year, grid$trim, ~ process_one_quarter(.x, .y, data_dir_input, vars, out_dir))


#########################
# 2. LOAD AND PREPARE DATA
#########################

ds <- open_dataset(out_dir, format = "parquet")
df <- ds %>% collect()

df <- df %>%
  mutate(
    id = paste0(CODUSU, "_", NRO_HOGAR, "_", COMPONENTE),
    time = year * 10L + trim
  )

# Filter to EMPLOYED only (ESTADO == 1) with positive income (P21)
df <- df %>% filter(ESTADO == 1, !is.na(P21), P21 > 0)

# Weighted median function. x = Income (P21) and w = Weights (PONDERA)
weighted.median <- function(x, w, na.rm = FALSE) {
  
  # We are only interested in rows were we have both values 
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }

  # Then we sort income in increasing order with their respective weights
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  # Find the first position in which the cumulative share is at least 0.5
  # We get the income at which half of the population is below and half above
  cumw <- cumsum(w) / sum(w)
  x[which(cumw >= 0.5)[1]]
}

# Quarter-specific income threshold (using weighted median)
# Here we classify by high and lows for each year-trimester. Since
# Argentina has very high inflation cycles, comparing incomes from different years
# would be missleading
df <- df %>%
  group_by(year, trim) %>%
  mutate(
    p21_th = weighted.median(P21, w = PONDERA, na.rm = TRUE),
    y_high = if_else(P21 > p21_th, 1, 0)
  ) %>%
  ungroup()


#########################
# 3. DEFINE FORMALITY STATUS
#########################

df <- df %>%
  mutate(
    # Directly from EPH
    formal = case_when(
      (EMPLEO == 1) ~ 1,
      (EMPLEO == 2) ~ 0,
      TRUE ~ NA_real_
    )
  )


#########################
# 4. CREATE STATE CLASSIFICATIONS
#########################

# 2-STATE: Formal (F) vs Informal (I)
df <- df %>%
  mutate(
    state2 = case_when(
      formal == 1 ~ "F",
      formal == 0 ~ "I",
      TRUE ~ NA_character_
    )
  )

# 4-STATE: Formal-High, Formal-Low, Informal-High, Informal-Low
df <- df %>%
  mutate(
    state4 = case_when(
      formal == 1 & y_high == 1 ~ "FH",
      formal == 1 & y_high == 0 ~ "FL",
      formal == 0 & y_high == 1 ~ "IH",
      formal == 0 & y_high == 0 ~ "IL",
      TRUE ~ NA_character_
    )
  )


#########################
# 5. COMPUTE WEIGHTED TRANSITION MATRICES
#########################

compute_transition_matrix <- function(df, state_var) {
  transitions_all <- df %>%
    arrange(id, year, trim) %>%
    group_by(id) %>%
    mutate(
      state_t = !!sym(state_var), 
      state_next = lead(!!sym(state_var)),
      year_next  = lead(year),
      trim_next = lead(trim),
      w_t = PONDERA 
    ) %>%
    ungroup() %>%
    mutate(
      is_consecutive = (year_next == year & trim_next == trim + 1L) |
      (year_next == year + 1L & trim == 4L & trim_next == 1L)
      )
  
  # Average weights from t and t+1
  transitions <- transitions_all %>%
    filter(!is.na(state_t),
    !is.na(state_next),
    is_consecutive,
    !is.na(w_t),
    w_t > 0
    ) %>%
    mutate(weight = w_t)
  
  # Compute weighted transition probabilities
  P_long <- transitions %>%
    group_by(state_t, state_next) %>%
    summarize(weighted_n = sum(weight), .groups = "drop") %>%
    group_by(state_t) %>%
    mutate(p = weighted_n / sum(weighted_n)) %>%
    ungroup()
  
  # Pivot to matrix form
  P_matrix <- P_long %>%
    select(state_t, state_next, p) %>%
    pivot_wider(names_from = state_next, values_from = p, values_fill = 0) %>%
    arrange(state_t)
  

  ################
  # SANITY CHECKS
  ################
  row_check <- P_matrix %>%
  mutate(row_sum = rowSums(across(-state_t)))

  origin_counts <- transitions %>%
  group_by(state_t) %>%
  summarize(
    n_pairs = n(),
    weighted_pairs = sum(weight),
    .groups = "drop"
    ) %>%
    arrange(state_t)

  drop_check <- transitions_all %>%
  summarize(
    total_candidate_pairs = n(),
    missing_origin_state = sum(is.na(state_t)),
    missing_destination_state = sum(is.na(state_next)),
    non_consecutive_pairs = sum(!is_consecutive, na.rm = TRUE),
    missing_or_nonpositive_weight = sum(is.na(w_t) | w_t <= 0),
    used_pairs = nrow(transitions)
    )

  qc <- transitions_all %>%
  mutate(
    reason = case_when(
      is.na(state_t) ~ "drop_origin_missing",
      is.na(state_next) ~ "drop_destination_missing",
      !is_consecutive ~ "drop_non_consecutive",
      is.na(w_t) | w_t <= 0 ~ "drop_bad_weight",
      TRUE ~ "kept"
    )
  ) %>%
  count(reason, sort = TRUE) %>%
  mutate(share = n / sum(n))

  dest_qc <- transitions_all %>%
  mutate(
    dest_reason = case_when(
      is.na(year_next) ~ "last_observed_row_per_id",
      !is.na(year_next) & is.na(state_next) ~ "next_row_exists_but_state_missing",
      TRUE ~ "has_destination_state"
    )
  ) %>%
  count(dest_reason, sort = TRUE) %>%
  mutate(share = n / sum(n))

  list(
    matrix = P_matrix,
    transitions = transitions,
    sanity = list(
      row_check = row_check,
      origin_counts = origin_counts,
      drop_check = drop_check,
      qc = qc,
      dest_qc = dest_qc
      ))
  }
########################

# Compute all three matrices
result2 <- compute_transition_matrix(df, "state2")
result4 <- compute_transition_matrix(df, "state4")

P2_matrix <- result2$matrix
P4_matrix <- result4$matrix


cat("\n=== SANITY CHECKS: 2-STATE ===\n")
print(result2$sanity$row_check)
print(result2$sanity$origin_counts)
print(result2$sanity$drop_check)

cat("\n=== SANITY CHECKS: 4-STATE ===\n")
print(result4$sanity$row_check)
print(result4$sanity$origin_counts)
print(result4$sanity$drop_check)


cat("\n=== SANITY CHECKS: ATTRITION ===\n")
print(result2$sanity$qc)
print(result2$sanity$dest_qc)

#########################
# 6. DIAGNOSTICS: Cross-sectional distributions
#########################

cat("\n========================================\n")
cat("CROSS-SECTIONAL STATE DISTRIBUTIONS\n")
cat("========================================\n")

cat("\n=== 2-STATE (should show <40% informal since we don't cound TSFR) ===\n")
df %>%
  filter(!is.na(state2)) %>%
  group_by(state2) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA)
  ) %>%
  mutate(
    pct_unweighted = n / sum(n) * 100,
    pct_weighted = weighted_n / sum(weighted_n) * 100
  ) %>%
  print()

cat("\n=== 4-STATE DISTRIBUTION ===\n")
df %>%
  filter(!is.na(state4)) %>%
  group_by(state4) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA)
  ) %>%
  mutate(
    pct_unweighted = n / sum(n) * 100,
    pct_weighted = weighted_n / sum(weighted_n) * 100
  ) %>%
  print()


#########################
# ADDITIONAL: INCOME COMPARISON: INFORMAL VS FORMAL
#########################

cat("\n========================================\n")
cat("INCOME RATIO: INFORMAL VS FORMAL\n")
cat("========================================\n")

# Compute weighted mean income by formality status
income_comparison <- df %>%
  filter(!is.na(formal), !is.na(P21), P21 > 0) %>%
  group_by(formal) %>%
  summarize(
    n = n(),
    mean_income_unweighted = mean(P21),
    mean_income_weighted = weighted.mean(P21, w = PONDERA),
    median_income_weighted = weighted.median(P21, w = PONDERA),
    .groups = "drop"
  ) %>%
  mutate(
    status = if_else(formal == 1, "Formal", "Informal")
  ) %>%
  select(status, everything(), -formal)

print(income_comparison)

# Calculate ratio
informal_mean <- income_comparison$mean_income_weighted[income_comparison$status == "Informal"]
formal_mean <- income_comparison$mean_income_weighted[income_comparison$status == "Formal"]
income_ratio <- informal_mean / formal_mean

cat("\n=== INCOME RATIO ===\n")
cat("Informal mean income / Formal mean income:", round(income_ratio, 4), "\n")
cat("Interpretation: Informal workers earn", round(income_ratio * 100, 1), 
    "% of what formal workers earn on average\n")

# By income group (High vs Low)
cat("\n=== INCOME BY FORMALITY AND INCOME GROUP ===\n")
income_by_group <- df %>%
  filter(!is.na(state4), !is.na(P21), P21 > 0) %>%
  group_by(state4) %>%
  summarize(
    n = n(),
    mean_income_weighted = weighted.mean(P21, w = PONDERA),
    median_income_weighted = weighted.median(P21, w = PONDERA),
    .groups = "drop"
  ) %>%
  arrange(state4)

print(income_by_group)

# Ratios within income groups
FH_mean <- income_by_group$mean_income_weighted[income_by_group$state4 == "FH"]
FL_mean <- income_by_group$mean_income_weighted[income_by_group$state4 == "FL"]
IH_mean <- income_by_group$mean_income_weighted[income_by_group$state4 == "IH"]
IL_mean <- income_by_group$mean_income_weighted[income_by_group$state4 == "IL"]

cat("\n=== INCOME RATIOS WITHIN GROUPS ===\n")
cat("High income: IH/FH =", round(IH_mean / FH_mean, 4), "\n")
cat("Low income: IL/FL =", round(IL_mean / FL_mean, 4), "\n")
cat("Formal: FL/FH =", round(FL_mean / FH_mean, 4), "\n")
cat("Informal: IL/IH =", round(IL_mean / IH_mean, 4), "\n")

# Save income comparison
income_summary <- data.frame(
  Category = c("Overall", "Overall", 
               "By_State", "By_State", "By_State", "By_State",
               "Ratios", "Ratios", "Ratios", "Ratios", "Ratios"),
  Group = c("Formal", "Informal",
            "FH", "FL", "IH", "IL",
            "Informal/Formal", "IH/FH", "IL/FL", "FL/FH", "IL/IH"),
  Mean_Income = c(formal_mean, informal_mean,
                  FH_mean, FL_mean, IH_mean, IL_mean,
                  income_ratio, IH_mean/FH_mean, IL_mean/FL_mean, 
                  FL_mean/FH_mean, IL_mean/IH_mean),
  Observations = c(
    income_comparison$n[income_comparison$status == "Formal"],
    income_comparison$n[income_comparison$status == "Informal"],
    income_by_group$n,
    rep(NA, 5)
  )
)

write.csv(income_summary, file = file.path(output_dir, "income_comparison.csv"), row.names = FALSE)


# Income evaluation vs full population. Restricting to latest year (closer to current report levels)
df %>%
  filter(year == 2024, !is.na(formal), !is.na(P21), P21 > 0) %>%
  group_by(formal) %>%
  summarize(
    n = n(),
    mean_w = weighted.mean(P21, PONDERA),
    median_w = weighted.median(P21, PONDERA, na.rm = TRUE),
    .groups = "drop"
  )



#########################
# 7. PRINT TRANSITION MATRICES
#########################

cat("\n========================================\n")
cat("TRANSITION MATRICES\n")
cat("========================================\n")

cat("\n=== 2-STATE TRANSITION MATRIX ===\n")
print(P2_matrix)

cat("\n=== 4-STATE TRANSITION MATRIX ===\n")
print(P4_matrix)


cat("\n=== Sample Sizes ===\n")
cat("2-state transitions:", nrow(result2$transitions), "\n")
cat("4-state transitions:", nrow(result4$transitions), "\n")



#########################
# 8. SAVE TRANSITION MATRICES
#########################

output_dir <- file.path(data_dir, "output_data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(P2_matrix, file = file.path(output_dir, "transition_matrix_2state.csv"), row.names = FALSE)
write.csv(P4_matrix, file = file.path(output_dir, "transition_matrix_4state.csv"), row.names = FALSE)

cat("\n========================================\n")
cat("Files saved to:", output_dir, "\n")
cat("- transition_matrix_2state.csv\n")
cat("- transition_matrix_4state.csv\n")
cat("========================================\n")


