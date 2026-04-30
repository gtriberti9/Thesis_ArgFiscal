################################################################################
# EPH Income Heterogeneity Analysis: Formal vs Informal Sectors
# Purpose: Visualize productivity/income distributions across sectors
# Input: Existing parquet files from previous extraction
# Output: Multiple graphs showing income heterogeneity
################################################################################

library(dplyr)
library(tidyr)
library(arrow)
library(readr)
library(ggplot2)
library(scales)

# Base directory
data_dir <- "C:/Users/HP/OneDrive/Documentos/UChicago/4. Thesis/versions/1. argentinian tax/1. data"

# Where parquet files are located (already extracted)
parquet_dir <- file.path(data_dir, "input_data", "parquet_individual_ids")

#########################
# 1. LOAD DATA DIRECTLY FROM PARQUET
#########################

# Check if directory exists
if (!dir.exists(parquet_dir)) {
  stop("Parquet directory not found: ", parquet_dir, 
       "\nPlease verify the path or run the extraction script first.")
}

# Load all parquet files at once using arrow dataset
ds <- open_dataset(parquet_dir, format = "parquet")
df <- ds %>% collect()

cat("Loaded", nrow(df), "total observations\n")
cat("Years:", paste(sort(unique(df$year)), collapse = ", "), "\n")
cat("Quarters:", paste(sort(unique(df$trim)), collapse = ", "), "\n")

#########################
# 2. PREPARE DATA
#########################

df <- df %>%
  mutate(
    id = paste0(CODUSU, "_", NRO_HOGAR, "_", COMPONENTE),
    time = year * 10L + trim
  )

# Filter to employed only (ESTADO == 1) with positive income
df <- df %>% filter(ESTADO == 1, !is.na(P21), P21 > 0)

cat("\nAfter filtering to employed with positive income:", nrow(df), "observations\n")

# Weighted median function
weighted.median <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which(cumw >= 0.5)[1]]
}

# Calculate weighted quantiles function
weighted.quantile <- function(x, w, probs = c(0.25, 0.5, 0.75), na.rm = FALSE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  
  sapply(probs, function(p) {
    x[which(cumw >= p)[1]]
  })
}


#########################
# 3. DEFINE FORMALITY STATUS
#########################

cat("\nDefining formality status...\n")

df <- df %>%
  mutate(
    is_salaried = CAT_OCUP == 3,
    is_patron   = CAT_OCUP == 1,
    is_self_emp = CAT_OCUP == 2,
    
    # SALARIED: Formal if has retirement (PP07H==1) OR health insurance (PP07G4==1)
    formal_salaried = case_when(
      is_salaried & (PP07H == 1 | PP07G4 == 1) ~ 1,
      is_salaried & (PP07H == 2 & PP07G4 == 2) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # INDEPENDENT: Formal if registered in any monotributo/autónomo scheme
    # PP05I: 1=Autónomo, 2=Monotributo Social, 3=Monotributo Regular, 4=Not registered
    formal_indep = case_when(
      (is_patron | is_self_emp) & PP05I %in% c(1, 2, 3) ~ 1,
      (is_patron | is_self_emp) & PP05I == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Combined formality
    formal = case_when(
      is_salaried ~ formal_salaried,
      (is_patron | is_self_emp) ~ formal_indep,
      TRUE ~ NA_real_
    )
  )

# Quarter-specific income threshold (using weighted median)
df <- df %>%
  group_by(year, trim) %>%
  mutate(
    p21_th = weighted.median(P21, w = PONDERA, na.rm = TRUE),
    y_high = if_else(P21 > p21_th, 1, 0)
  ) %>%
  ungroup()

# Create sector labels
df <- df %>%
  mutate(
    sector = case_when(
      formal == 1 ~ "Formal",
      formal == 0 ~ "Informal",
      TRUE ~ NA_character_
    ),
    # 4-STATE classification
    state4 = case_when(
      formal == 1 & y_high == 1 ~ "FH",
      formal == 1 & y_high == 0 ~ "FL",
      formal == 0 & y_high == 1 ~ "IH",
      formal == 0 & y_high == 0 ~ "IL",
      TRUE ~ NA_character_
    )
  )

# Filter to observations with defined formality status
df_analysis <- df %>% filter(!is.na(sector))

cat("Observations with defined formality status:", nrow(df_analysis), "\n")

# Quick check
sector_counts <- df_analysis %>%
  group_by(sector) %>%
  summarize(n = n(), weighted_n = sum(PONDERA)) %>%
  mutate(pct = weighted_n / sum(weighted_n) * 100)

cat("\nSector distribution:\n")
print(sector_counts)

# 4-state distribution
cat("\n4-State distribution:\n")
state4_counts <- df_analysis %>%
  filter(!is.na(state4)) %>%
  group_by(state4) %>%
  summarize(n = n(), weighted_n = sum(PONDERA)) %>%
  mutate(pct = weighted_n / sum(weighted_n) * 100)
print(state4_counts)


#########################
# 4. CREATE OUTPUT DIRECTORY
#########################

output_dir <- file.path(data_dir, "output_data", "income_heterogeneity")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n========================================\n")
cat("INCOME HETEROGENEITY ANALYSIS\n")
cat("========================================\n")


#########################
# 5. SUMMARY STATISTICS
#########################

# Overall statistics by sector
summary_stats <- df_analysis %>%
  group_by(sector) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA),
    mean_income = weighted.mean(P21, w = PONDERA),
    median_income = weighted.median(P21, w = PONDERA),
    q25 = weighted.quantile(P21, w = PONDERA, probs = 0.25),
    q75 = weighted.quantile(P21, w = PONDERA, probs = 0.75),
    q10 = weighted.quantile(P21, w = PONDERA, probs = 0.10),
    q90 = weighted.quantile(P21, w = PONDERA, probs = 0.90),
    .groups = "drop"
  ) %>%
  mutate(
    pct_workers = weighted_n / sum(weighted_n) * 100,
    iqr = q75 - q25,
    p90_p10_ratio = q90 / q10
  )

cat("\n=== SUMMARY STATISTICS BY SECTOR ===\n")
print(summary_stats)

# Calculate income ratios
formal_mean <- summary_stats$mean_income[summary_stats$sector == "Formal"]
informal_mean <- summary_stats$mean_income[summary_stats$sector == "Informal"]
income_ratio <- informal_mean / formal_mean

formal_median <- summary_stats$median_income[summary_stats$sector == "Formal"]
informal_median <- summary_stats$median_income[summary_stats$sector == "Informal"]
median_ratio <- informal_median / formal_median

cat("\n=== INCOME GAP ===\n")
cat("Informal/Formal MEAN income ratio:", round(income_ratio, 4), "\n")
cat("Informal workers earn", round(income_ratio * 100, 1), 
    "% of formal workers on average (mean)\n")
cat("Formal sector wage premium (mean):", round((1/income_ratio - 1) * 100, 1), "%\n\n")

cat("Informal/Formal MEDIAN income ratio:", round(median_ratio, 4), "\n")
cat("Informal workers earn", round(median_ratio * 100, 1), 
    "% of formal workers on average (median)\n")
cat("Formal sector wage premium (median):", round((1/median_ratio - 1) * 100, 1), "%\n")

write.csv(summary_stats, 
          file.path(output_dir, "summary_statistics.csv"), 
          row.names = FALSE)


#########################
# 5a. 4-STATE INCOME ANALYSIS (FH, FL, IH, IL)
#########################

cat("\n========================================\n")
cat("4-STATE INCOME ANALYSIS\n")
cat("========================================\n")

# Calculate statistics by 4-state classification
state4_income_stats <- df_analysis %>%
  filter(!is.na(state4)) %>%
  group_by(state4) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA),
    mean_income = weighted.mean(P21, w = PONDERA),
    median_income = weighted.median(P21, w = PONDERA),
    q25 = weighted.quantile(P21, w = PONDERA, probs = 0.25),
    q75 = weighted.quantile(P21, w = PONDERA, probs = 0.75),
    .groups = "drop"
  ) %>%
  mutate(
    pct_workers = weighted_n / sum(weighted_n) * 100
  )

cat("\n=== Income Statistics by 4-State Classification ===\n")
print(state4_income_stats)

# Calculate key ratios
FH_mean <- state4_income_stats$mean_income[state4_income_stats$state4 == "FH"]
FL_mean <- state4_income_stats$mean_income[state4_income_stats$state4 == "FL"]
IH_mean <- state4_income_stats$mean_income[state4_income_stats$state4 == "IH"]
IL_mean <- state4_income_stats$mean_income[state4_income_stats$state4 == "IL"]

cat("\n=== KEY FINDING: High-Income Group Comparison ===\n")
cat("IH (Informal-High) mean income:", round(IH_mean, 2), "\n")
cat("FH (Formal-High) mean income:", round(FH_mean, 2), "\n")
cat("Ratio IH/FH:", round(IH_mean / FH_mean, 4), "\n")
cat("\nInterpretation: High-income informal workers earn", 
    round((IH_mean / FH_mean) * 100, 1), 
    "% of high-income formal workers\n")

if (IH_mean > FH_mean) {
  cat("\n*** IMPORTANT: High-income informals earn MORE than high-income formals! ***\n")
  cat("This suggests that the upper tail of informal sector includes\n")
  cat("high-productivity workers (e.g., professionals, business owners)\n")
  cat("who choose informality for tax avoidance or regulatory flexibility.\n")
}

cat("\n=== Other Income Ratios ===\n")
cat("IL/FL (Low-income comparison):", round(IL_mean / FL_mean, 4), "\n")
cat("FL/FH (Formal income gap):", round(FL_mean / FH_mean, 4), "\n")
cat("IL/IH (Informal income gap):", round(IL_mean / IH_mean, 4), "\n")

# Create summary table matching your format
income_comparison_4state <- data.frame(
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
    summary_stats$n[summary_stats$sector == "Formal"],
    summary_stats$n[summary_stats$sector == "Informal"],
    state4_income_stats$n,
    rep(NA, 5)
  )
)

write.csv(income_comparison_4state, 
          file.path(output_dir, "income_comparison_4state.csv"), 
          row.names = FALSE)

cat("\nSaved: income_comparison_4state.csv\n")


#########################
# 6. INCOME PERCENTILES ANALYSIS
#########################

cat("\nCalculating income percentiles...\n")

# Calculate weighted percentiles with unique breaks
percentile_breaks <- weighted.quantile(
  df_analysis$P21, 
  w = df_analysis$PONDERA, 
  probs = seq(0, 1, by = 0.01),
  na.rm = TRUE
)

# Make breaks unique by adding small increments where needed
percentile_breaks_unique <- percentile_breaks
if (any(duplicated(percentile_breaks))) {
  cat("Note: Some income values are identical, adjusting breaks...\n")
  for (i in 2:length(percentile_breaks_unique)) {
    if (percentile_breaks_unique[i] <= percentile_breaks_unique[i-1]) {
      percentile_breaks_unique[i] <- percentile_breaks_unique[i-1] + 0.01
    }
  }
}

df_analysis <- df_analysis %>%
  mutate(
    income_percentile = cut(P21, 
                            breaks = c(-Inf, percentile_breaks_unique[-1]), 
                            labels = 1:100,
                            include.lowest = TRUE) %>% as.numeric()
  )

# Composition by percentile
percentile_composition <- df_analysis %>%
  group_by(income_percentile, sector) %>%
  summarize(weighted_n = sum(PONDERA), .groups = "drop") %>%
  group_by(income_percentile) %>%
  mutate(
    total = sum(weighted_n),
    pct = weighted_n / total * 100
  ) %>%
  ungroup()

write.csv(percentile_composition, 
          file.path(output_dir, "percentile_composition.csv"), 
          row.names = FALSE)


#########################
# 7. CREATE VISUALIZATIONS
#########################

cat("\nGenerating visualizations...\n")

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) +
            theme(
              plot.title = element_text(face = "bold", size = 14),
              plot.subtitle = element_text(size = 11),
              legend.position = "bottom"
            ))

## GRAPH 1: Kernel Density Plot of Income Distributions
cat("  - Graph 1: Income Density Distributions\n")

# Calculate weighted densities
calc_weighted_density <- function(data, sector_name, n_points = 512) {
  sector_data <- data %>% filter(sector == sector_name)
  
  # Use log income for better visualization
  log_income <- log(sector_data$P21)
  weights <- sector_data$PONDERA
  
  # Weighted density estimation
  dens <- density(log_income, weights = weights/sum(weights), n = n_points)
  
  data.frame(
    log_income = dens$x,
    income = exp(dens$x),
    density = dens$y,
    sector = sector_name
  )
}

density_data <- df_analysis %>%
  select(sector, P21, PONDERA) %>%
  filter(!is.na(P21), !is.na(sector))

density_formal <- calc_weighted_density(density_data, "Formal")
density_informal <- calc_weighted_density(density_data, "Informal")
density_combined <- bind_rows(density_formal, density_informal)

p1 <- ggplot(density_combined, aes(x = income, y = density, color = sector, fill = sector)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = density), alpha = 0.2) +
  scale_x_log10(labels = comma) +
  scale_color_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  scale_fill_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  labs(
    title = "Income Distribution: Formal vs Informal Sectors",
    subtitle = "Weighted kernel density estimates (log scale)",
    x = "Monthly Income (log scale)",
    y = "Density",
    color = "Sector",
    fill = "Sector"
  )

ggsave(file.path(output_dir, "graph1_income_densities.png"), 
       p1, width = 10, height = 6, dpi = 300)


## GRAPH 2: Cumulative Distribution Functions (CDFs)
cat("  - Graph 2: Cumulative Distribution Functions\n")

cdf_data <- df_analysis %>%
  group_by(sector) %>%
  arrange(P21) %>%
  mutate(
    cumulative_weight = cumsum(PONDERA) / sum(PONDERA)
  ) %>%
  ungroup()

p2 <- ggplot(cdf_data, aes(x = P21, y = cumulative_weight * 100, color = sector)) +
  geom_line(linewidth = 1.2) +
  scale_x_log10(labels = comma) +
  scale_color_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  labs(
    title = "Cumulative Income Distribution: Formal vs Informal",
    subtitle = "Percentage of workers earning below each income level",
    x = "Monthly Income (log scale)",
    y = "Cumulative Percentage (%)",
    color = "Sector"
  ) +
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5)

ggsave(file.path(output_dir, "graph2_income_cdf.png"), 
       p2, width = 10, height = 6, dpi = 300)


## GRAPH 3: Composition by Income Percentile (Stacked Bar)
cat("  - Graph 3: Sector Composition by Income Percentile\n")

# Group percentiles into bins for cleaner visualization
percentile_bins <- percentile_composition %>%
  mutate(
    percentile_bin = cut(income_percentile, 
                         breaks = seq(0, 100, by = 10),
                         labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                                    "50-60", "60-70", "70-80", "80-90", "90-100"),
                         include.lowest = TRUE)
  ) %>%
  group_by(percentile_bin, sector) %>%
  summarize(weighted_n = sum(weighted_n), .groups = "drop") %>%
  group_by(percentile_bin) %>%
  mutate(pct = weighted_n / sum(weighted_n) * 100) %>%
  ungroup()

p3 <- ggplot(percentile_bins, aes(x = percentile_bin, y = pct, fill = sector)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  labs(
    title = "Sector Composition Across Income Percentiles",
    subtitle = "Percentage of formal vs informal workers in each income decile",
    x = "Income Percentile",
    y = "Percentage of Workers (%)",
    fill = "Sector"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "graph3_percentile_composition.png"), 
       p3, width = 10, height = 6, dpi = 300)


## GRAPH 4: Box Plots by Sector
cat("  - Graph 4: Income Box Plots by Sector\n")

p4 <- ggplot(df_analysis, aes(x = sector, y = P21, fill = sector)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  labs(
    title = "Income Distribution by Sector",
    subtitle = "Box plots with violin distributions (log scale)",
    x = "Sector",
    y = "Monthly Income (log scale)",
    fill = "Sector"
  ) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black")

ggsave(file.path(output_dir, "graph4_boxplots.png"), 
       p4, width = 8, height = 6, dpi = 300)


## GRAPH 5: Share of Each Sector by Income Percentile
cat("  - Graph 5: Sector Shares by Percentile\n")

p5 <- ggplot(percentile_bins, aes(x = percentile_bin, y = pct, 
                                  color = sector, group = sector)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  labs(
    title = "Sector Representation Across Income Distribution",
    subtitle = "Percentage of each sector in income deciles",
    x = "Income Percentile",
    y = "Percentage (%)",
    color = "Sector"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5)

ggsave(file.path(output_dir, "graph5_sector_shares.png"), 
       p5, width = 10, height = 6, dpi = 300)


## GRAPH 6: Income Ratios Across Percentiles
cat("  - Graph 6: Informal/Formal Income Ratio by Percentile\n")

# Calculate median income by sector and percentile bin
income_by_percentile <- df_analysis %>%
  mutate(
    percentile_bin = cut(income_percentile, 
                         breaks = seq(0, 100, by = 10),
                         labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                                    "50-60", "60-70", "70-80", "80-90", "90-100"),
                         include.lowest = TRUE)
  ) %>%
  group_by(percentile_bin, sector) %>%
  summarize(
    median_income = weighted.median(P21, PONDERA),
    mean_income = weighted.mean(P21, PONDERA),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = c(median_income, mean_income)
  ) %>%
  mutate(
    median_ratio = median_income_Informal / median_income_Formal,
    mean_ratio = mean_income_Informal / mean_income_Formal
  )

p6 <- ggplot(income_by_percentile, aes(x = percentile_bin)) +
  geom_col(aes(y = median_ratio * 100), fill = "#A23B72", alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = "Income Gap Across the Distribution",
    subtitle = "Informal income as percentage of formal income by percentile",
    x = "Income Percentile",
    y = "Informal/Formal Income Ratio (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "graph6_income_ratios.png"), 
       p6, width = 10, height = 6, dpi = 300)


## GRAPH 7: Overlapping Histograms
cat("  - Graph 7: Overlapping Income Histograms\n")

p7 <- ggplot(df_analysis, aes(x = P21, fill = sector, weight = PONDERA)) +
  geom_histogram(aes(y = after_stat(density)), position = "identity", 
                 alpha = 0.5, bins = 50) +
  scale_x_log10(labels = comma) +
  scale_fill_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  labs(
    title = "Overlapping Income Distributions",
    subtitle = "Weighted histograms showing distribution overlap",
    x = "Monthly Income (log scale)",
    y = "Density",
    fill = "Sector"
  ) +
  facet_wrap(~sector, ncol = 1)

ggsave(file.path(output_dir, "graph7_overlapping_histograms.png"), 
       p7, width = 10, height = 8, dpi = 300)


## GRAPH 8: 4-State Income Comparison (Bar Chart)
cat("  - Graph 8: 4-State Mean Income Comparison\n")

state4_plot_data <- state4_income_stats %>%
  mutate(
    formality = ifelse(state4 %in% c("FH", "FL"), "Formal", "Informal"),
    income_level = ifelse(state4 %in% c("FH", "IH"), "High", "Low"),
    state4_label = factor(state4, 
                          levels = c("FH", "IH", "FL", "IL"),
                          labels = c("Formal-High", "Informal-High", 
                                     "Formal-Low", "Informal-Low"))
  )

p8 <- ggplot(state4_plot_data, aes(x = state4_label, y = mean_income, fill = formality)) +
  geom_col() +
  geom_text(aes(label = comma(round(mean_income, 0))), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Mean Income by Formality and Income Level",
    subtitle = "High-income informals earn more than high-income formals",
    x = "Worker Category",
    y = "Mean Monthly Income",
    fill = "Sector"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "graph8_4state_comparison.png"), 
       p8, width = 10, height = 6, dpi = 300)


## GRAPH 8.1: 4-State Median Income Comparison
cat("  - Graph 8.1: 4-State Median Income Comparison\n")

state4_median_data <- df_analysis %>%
  filter(!is.na(state4)) %>%
  group_by(state4) %>%
  summarize(
    median_income = weighted.median(P21, w = PONDERA),
    .groups = "drop"
  ) %>%
  mutate(
    formality = ifelse(state4 %in% c("FH", "FL"), "Formal", "Informal"),
    income_level = ifelse(state4 %in% c("FH", "IH"), "High", "Low"),
    state4_label = factor(state4, 
                          levels = c("FH", "IH", "FL", "IL"),
                          labels = c("Formal-High", "Informal-High", 
                                     "Formal-Low", "Informal-Low"))
  )

p8_1 <- ggplot(state4_median_data, aes(x = state4_label, y = median_income, fill = formality)) +
  geom_col() +
  geom_text(aes(label = comma(round(median_income, 0))), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Median Income by Formality and Income Level",
    subtitle = "Comparing median (instead of mean) income across groups",
    x = "Worker Category",
    y = "Median Monthly Income",
    fill = "Sector"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "graph8_1_4state_median_comparison.png"), 
       p8_1, width = 10, height = 6, dpi = 300)

# Calculate median ratio
FH_median <- state4_median_data$median_income[state4_median_data$state4 == "FH"]
IH_median <- state4_median_data$median_income[state4_median_data$state4 == "IH"]

cat("    Median IH/FH ratio:", round(IH_median / FH_median, 4), "\n")


## GRAPH 8.2: 4-State Income by Occupation Type
cat("  - Graph 8.2: 4-State Income by Occupation Type\n")

# Create occupation type variable
state4_occupation_data <- df_analysis %>%
  filter(!is.na(state4)) %>%
  mutate(
    occupation_type = case_when(
      is_salaried ~ "Salaried",
      is_patron ~ "Employer/Patron",
      is_self_emp ~ "Self-Employed",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(state4, occupation_type) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA),
    mean_income = weighted.mean(P21, w = PONDERA),
    median_income = weighted.median(P21, w = PONDERA),
    .groups = "drop"
  ) %>%
  mutate(
    formality = ifelse(state4 %in% c("FH", "FL"), "Formal", "Informal"),
    state4_label = factor(state4, 
                          levels = c("FH", "IH", "FL", "IL"),
                          labels = c("Formal-High", "Informal-High", 
                                     "Formal-Low", "Informal-Low"))
  )

p8_2 <- ggplot(state4_occupation_data, aes(x = state4_label, y = mean_income, 
                                           fill = occupation_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Salaried" = "#2E86AB", 
                               "Self-Employed" = "#A23B72",
                               "Employer/Patron" = "#F18F01",
                               "Other" = "#999999")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Mean Income by Formality, Income Level, and Occupation Type",
    subtitle = "Breaking down the 4-state classification by type of work",
    x = "Worker Category",
    y = "Mean Monthly Income",
    fill = "Occupation Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "graph8_2_4state_by_occupation.png"), 
       p8_2, width = 12, height = 6, dpi = 300)

# Print occupation composition in high-income groups
cat("\n    Occupation composition in high-income groups:\n")
high_income_occupation <- state4_occupation_data %>%
  filter(state4 %in% c("FH", "IH")) %>%
  group_by(state4) %>%
  mutate(pct_of_state = weighted_n / sum(weighted_n) * 100) %>%
  select(state4_label, occupation_type, n, weighted_n, pct_of_state, mean_income)

print(high_income_occupation)

# Save occupation breakdown
write.csv(state4_occupation_data, 
          file.path(output_dir, "state4_by_occupation.csv"), 
          row.names = FALSE)


## GRAPH 9: Income Distributions for High-Income Groups Only
cat("  - Graph 9: High-Income Group Distributions (FH vs IH)\n")

high_income_data <- df_analysis %>%
  filter(state4 %in% c("FH", "IH")) %>%
  mutate(group = ifelse(state4 == "FH", "Formal-High", "Informal-High"))

p9 <- ggplot(high_income_data, aes(x = P21, fill = group, weight = PONDERA)) +
  geom_density(alpha = 0.5) +
  scale_x_log10(labels = comma) +
  scale_fill_manual(values = c("Formal-High" = "#2E86AB", "Informal-High" = "#A23B72")) +
  geom_vline(xintercept = FH_mean, color = "#2E86AB", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = IH_mean, color = "#A23B72", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Income Distribution: High-Income Workers Only",
    subtitle = sprintf("Mean: Formal-High = %s, Informal-High = %s (%.1f%% higher)", 
                       comma(round(FH_mean, 0)), 
                       comma(round(IH_mean, 0)),
                       (IH_mean/FH_mean - 1) * 100),
    x = "Monthly Income (log scale)",
    y = "Density",
    fill = "Group"
  )

ggsave(file.path(output_dir, "graph9_high_income_comparison.png"), 
       p9, width = 10, height = 6, dpi = 300)


## GRAPH 10: All 4 States - Density Comparison
cat("  - Graph 10: Income Densities for All 4 States\n")

state4_density_data <- df_analysis %>%
  filter(!is.na(state4)) %>%
  mutate(
    state4_label = factor(state4, 
                          levels = c("FH", "IH", "FL", "IL"),
                          labels = c("Formal-High", "Informal-High", 
                                     "Formal-Low", "Informal-Low"))
  )

p10 <- ggplot(state4_density_data, aes(x = P21, color = state4_label, weight = PONDERA)) +
  geom_density(linewidth = 1.2) +
  scale_x_log10(labels = comma) +
  scale_color_manual(values = c(
    "Formal-High" = "#2E86AB", 
    "Informal-High" = "#A23B72",
    "Formal-Low" = "#87CEEB",
    "Informal-Low" = "#FFB6C1"
  )) +
  labs(
    title = "Income Distribution by Formality and Income Level",
    subtitle = "Four distinct worker groups show different income patterns",
    x = "Monthly Income (log scale)",
    y = "Density",
    color = "Worker Category"
  )

ggsave(file.path(output_dir, "graph10_4state_densities.png"), 
       p10, width = 10, height = 6, dpi = 300)


#########################
# 8. LAST YEAR ANALYSIS
#########################

cat("\n=== LAST YEAR FORMALITY RATES ===\n")

# Get the most recent year in the data
last_year <- max(df_analysis$year, na.rm = TRUE)
cat("Most recent year in data:", last_year, "\n\n")

# Calculate formality rates for last year only
last_year_stats <- df_analysis %>%
  filter(year == last_year) %>%
  group_by(sector) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA),
    .groups = "drop"
  ) %>%
  mutate(
    pct_unweighted = n / sum(n) * 100,
    pct_weighted = weighted_n / sum(weighted_n) * 100
  )

cat("Formality Status in", last_year, ":\n")
print(last_year_stats)

# Extract key percentages
formal_pct <- last_year_stats %>% 
  filter(sector == "Formal") %>% 
  pull(pct_weighted)

informal_pct <- last_year_stats %>% 
  filter(sector == "Informal") %>% 
  pull(pct_weighted)

cat("\nSummary for", last_year, ":\n")
cat("  Formal workers:", round(formal_pct, 2), "%\n")
cat("  Informal workers:", round(informal_pct, 2), "%\n")

# Save last year statistics
write.csv(last_year_stats, 
          file.path(output_dir, "last_year_formality_rates.csv"), 
          row.names = FALSE)


#########################
# 9. HIGH-INCOME INFORMALS ANALYSIS
#########################

cat("\n=== HIGH-INCOME INFORMALS ANALYSIS ===\n")

# Define "high income" as top 25% of overall distribution
high_income_threshold <- weighted.quantile(df_analysis$P21, 
                                           df_analysis$PONDERA, 
                                           0.75)

high_income_analysis <- df_analysis %>%
  mutate(high_income = P21 >= high_income_threshold) %>%
  group_by(sector, high_income) %>%
  summarize(
    n = n(),
    weighted_n = sum(PONDERA),
    .groups = "drop"
  ) %>%
  group_by(sector) %>%
  mutate(pct_within_sector = weighted_n / sum(weighted_n) * 100) %>%
  ungroup() %>%
  group_by(high_income) %>%
  mutate(pct_of_high_income = weighted_n / sum(weighted_n) * 100) %>%
  ungroup()

print(high_income_analysis)

cat("\nHigh-income threshold (top 25%):", round(high_income_threshold, 2), "\n")

# What % of informal workers are in top 25%?
pct_informal_high <- high_income_analysis %>%
  filter(sector == "Informal", high_income == TRUE) %>%
  pull(pct_within_sector)

cat("Percentage of informal workers in top 25%:", round(pct_informal_high, 2), "%\n")

# What % of top 25% are informal?
pct_high_informal <- high_income_analysis %>%
  filter(sector == "Informal", high_income == TRUE) %>%
  pull(pct_of_high_income)

cat("Percentage of top 25% who are informal:", round(pct_high_informal, 2), "%\n")

write.csv(high_income_analysis, 
          file.path(output_dir, "high_income_analysis.csv"), 
          row.names = FALSE)


#########################
# 10. TIME TRENDS
#########################

cat("\n  - Graph 11: Income Gap Over Time\n")

time_trends <- df_analysis %>%
  group_by(year, sector) %>%
  summarize(
    mean_income = weighted.mean(P21, PONDERA),
    median_income = weighted.median(P21, PONDERA),
    .groups = "drop"
  )

p11 <- ggplot(time_trends, aes(x = year, y = mean_income, color = sector)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Formal" = "#2E86AB", "Informal" = "#A23B72")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Average Income Trends: Formal vs Informal Sectors",
    subtitle = "Weighted mean income by year",
    x = "Year",
    y = "Mean Monthly Income",
    color = "Sector"
  )

ggsave(file.path(output_dir, "graph11_time_trends.png"), 
       p11, width = 10, height = 6, dpi = 300)

# Calculate ratio over time
time_ratio <- time_trends %>%
  select(year, sector, mean_income) %>%
  pivot_wider(names_from = sector, values_from = mean_income) %>%
  mutate(ratio = Informal / Formal)

p12 <- ggplot(time_ratio, aes(x = year, y = ratio * 100)) +
  geom_line(linewidth = 1.2, color = "#A23B72") +
  geom_point(size = 3, color = "#A23B72") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = "Evolution of Income Gap",
    subtitle = "Informal income as percentage of formal income",
    x = "Year",
    y = "Informal/Formal Income Ratio (%)"
  )

ggsave(file.path(output_dir, "graph12_ratio_trends.png"), 
       p12, width = 10, height = 6, dpi = 300)

write.csv(time_trends, 
          file.path(output_dir, "time_trends.csv"), 
          row.names = FALSE)


#########################
# 10. SUMMARY REPORT
#########################

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")
cat("\nAll outputs saved to:", output_dir, "\n\n")

cat("Generated Files:\n")
cat("CSV Files:\n")
cat("  - summary_statistics.csv\n")
cat("  - income_comparison_4state.csv\n")
cat("  - state4_by_occupation.csv (NEW)\n")
cat("  - percentile_composition.csv\n")
cat("  - high_income_analysis.csv\n")
cat("  - time_trends.csv\n")
cat("  - last_year_formality_rates.csv\n\n")

cat("Graphs:\n")
cat("  - graph1_income_densities.png\n")
cat("  - graph2_income_cdf.png\n")
cat("  - graph3_percentile_composition.png\n")
cat("  - graph4_boxplots.png\n")
cat("  - graph5_sector_shares.png\n")
cat("  - graph6_income_ratios.png\n")
cat("  - graph7_overlapping_histograms.png\n")
cat("  - graph8_4state_comparison.png (mean income)\n")
cat("  - graph8_1_4state_median_comparison.png (NEW - median income)\n")
cat("  - graph8_2_4state_by_occupation.png (NEW - by occupation type)\n")
cat("  - graph9_high_income_comparison.png (FH vs IH densities)\n")
cat("  - graph10_4state_densities.png (all 4 states)\n")
cat("  - graph11_time_trends.png\n")
cat("  - graph12_ratio_trends.png\n")

cat("\n========================================\n")
cat("KEY FINDINGS:\n")
cat("========================================\n")
cat("Total employed workers analyzed:", nrow(df_analysis), "\n")
cat("Formal workers (all years):", 
    round(summary_stats$pct_workers[summary_stats$sector == "Formal"], 1), "%\n")
cat("Informal workers (all years):", 
    round(summary_stats$pct_workers[summary_stats$sector == "Informal"], 1), "%\n")
cat("\nLast year (", last_year, ") formality rates:\n", sep = "")
cat("  Formal workers:", round(formal_pct, 1), "%\n")
cat("  Informal workers:", round(informal_pct, 1), "%\n")
cat("\nIncome gap (Informal/Formal mean):", round(income_ratio * 100, 1), "%\n")
cat("Formal sector wage premium:", round((1/income_ratio - 1) * 100, 1), "%\n")
cat("\n*** IMPORTANT FINDING: High-Income Comparison ***\n")
cat("High-income informal workers earn", round((IH_mean/FH_mean) * 100, 1), 
    "% of high-income formal workers\n")
cat("Ratio IH/FH:", round(IH_mean/FH_mean, 3), "\n")
if (IH_mean > FH_mean) {
  cat("→ High-income INFORMALS earn MORE than high-income FORMALS!\n")
  cat("  This suggests productive workers choosing informality (tax avoidance, flexibility)\n")
}
cat("\nHigh-income informals (top 25%):", round(pct_informal_high, 1), 
    "% of informal workers\n")
cat("Informal share of top earners:", round(pct_high_informal, 1), "%\n")
cat("========================================\n")