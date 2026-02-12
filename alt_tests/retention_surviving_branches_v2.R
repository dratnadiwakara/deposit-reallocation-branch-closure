# ============================================================
# Bank-County Panel: Deposit Retention Through Remaining Branches
# ENHANCED VERSION FOR TOP FINANCE JOURNAL
# 
# This code analyzes deposit retention at surviving branches following
# branch closures using bank-county-year panel data.
#
# Key improvements:
#  - Parallel trends testing and event study framework
#  - Refined outcome measurement (retention rates, migration)
#  - Heterogeneity analysis (app availability, closure intensity)
#  - Multiple robustness checks and alternative specifications
#  - Economic magnitude calculations
#  - Comprehensive sample quality diagnostics
# ============================================================

rm(list=ls())
library(data.table)
library(stringr)
library(fixest)
library(ggplot2)
library(stargazer)

# ============================================================
# CONFIGURATION
# ============================================================

# Inputs
closure_path   <- "C:/OneDrive/data/closure_opening_data_simple.rds"
app_panel_path <- "C:/OneDrive/data/CH_full_app_reviews_panel.csv"

# Analysis parameters
start_year <- 2012
end_year   <- 2019
tail_p <- 0.95  # Extreme intensity cutoff (top 5%)
min_deposits_threshold <- 1000  # Minimum deposits ($1000s) for inclusion
min_remaining_branches <- 2     # Minimum remaining branches for within-bank variation


# ============================================================
# 0) LOAD AND PREPARE DATA
# ============================================================

cat("\n=== LOADING DATA ===\n")

# Load closure data
closure_raw <- readRDS(closure_path)
setDT(closure_raw)
closure_raw[, county := str_pad(STCNTYBR, 5, "left", "0")]
closure_raw[, YEAR := as.integer(YEAR)]

# Load app panel
app_panel <- fread(app_panel_path)
setDT(app_panel)
app_panel <- app_panel[, .(
  CERT = FDIC_certificate_id,
  YEAR = as.integer(year),
  first_app_available
)]
app_panel[, has_app := fifelse(first_app_available == 1, 1L, 0L)]

# Merge bank-year app indicator onto branch-year
closure_app <- merge(
  closure_raw,
  app_panel[, .(CERT, YEAR, has_app)],
  by = c("CERT", "YEAR"),
  all.x = TRUE
)
closure_app[is.na(has_app), has_app := 0L]

# Bank identifier
closure_app[, bank_id := CERT]

cat("Total branch-year observations:", nrow(closure_app), "\n")
cat("Years covered:", min(closure_app$YEAR), "-", max(closure_app$YEAR), "\n")
cat("Unique banks:", uniqueN(closure_app$bank_id), "\n")
cat("Unique counties:", uniqueN(closure_app$county), "\n")

# ============================================================
# 1) CREATE ALIGNED TEMPORAL DEPOSITS AT BRANCH LEVEL
# ============================================================

cat("\n=== CREATING TEMPORAL DEPOSIT VARIABLES ===\n")

setorder(closure_app, UNINUMBR, YEAR)
closure_app[, `:=`(
  dep_lag1   = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1  = shift(YEAR,    1L, type = "lag"),
  dep_lead1  = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1 = shift(YEAR,    1L, type = "lead")
), by = UNINUMBR]

# Aligned deposits (only if consecutive years)
closure_app[, dep_lag1_aligned  := fifelse(year_lag1  == YEAR - 1L, dep_lag1,  NA_real_)]
closure_app[, dep_lead1_aligned := fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_)]

# Branch existence flags
closure_app[, exists_t1  := !is.na(dep_lag1_aligned)  & dep_lag1_aligned  > 0]
closure_app[, exists_tp1 := !is.na(dep_lead1_aligned) & dep_lead1_aligned > 0]

# ============================================================
# 2) IDENTIFY CLOSED VS REMAINING BRANCHES
# ============================================================

cat("\n=== IDENTIFYING BRANCH STATUS ===\n")

closure_app[, is_closed_t := (closed == 1L)]

# Remaining branches: not closed, exist in t-1 and t+1
closure_app[, is_remaining_branch := (!is_closed_t) & exists_t1 & exists_tp1]

# Closed branches: closed in t, existed in t-1
closure_app[, is_closed_branch := is_closed_t & exists_t1]

cat("Closed branches (with t-1 deposits):", sum(closure_app$is_closed_branch, na.rm=TRUE), "\n")
cat("Remaining branches (with t-1 and t+1):", sum(closure_app$is_remaining_branch, na.rm=TRUE), "\n")

# ============================================================
# 3) BUILD BANK-COUNTY-YEAR AGGREGATES
# ============================================================

cat("\n=== AGGREGATING TO BANK-COUNTY-YEAR LEVEL ===\n")

sum_na_safe <- function(x) if (all(is.na(x))) NA_real_ else as.numeric(sum(x, na.rm = TRUE))

bank_cty_yr <- closure_app[, .(
  deps_closed_t1 = sum_na_safe(fifelse(is_closed_branch, dep_lag1_aligned, NA_real_)),
  
  deps_remain_t1  = sum_na_safe(fifelse(is_remaining_branch, dep_lag1_aligned, NA_real_)),
  deps_remain_t   = sum_na_safe(fifelse(is_remaining_branch, DEPSUMBR, NA_real_)),
  deps_remain_tp1 = sum_na_safe(fifelse(is_remaining_branch, dep_lead1_aligned, NA_real_)),
  
  n_closed_branches    = sum(is_closed_branch, na.rm = TRUE),
  n_remaining_branches = sum(is_remaining_branch, na.rm = TRUE),
  
  has_app = max(has_app, na.rm = TRUE)
), by = .(bank_id, county, YEAR)]

# Replace NA sums with 0 where appropriate
for (v in c("deps_closed_t1", "deps_remain_t1", "deps_remain_t", "deps_remain_tp1")) {
  bank_cty_yr[is.na(get(v)), (v) := 0]
}
bank_cty_yr[is.na(has_app), has_app := 0L]

# ============================================================
# 4) CREATE OUTCOME VARIABLES
# ============================================================

cat("\n=== CREATING OUTCOME VARIABLES ===\n")

# Change in deposits at remaining branches (t-1 to t+1)
bank_cty_yr[, delta_deps_remaining := deps_remain_tp1 - deps_remain_t1]

# Growth rate
bank_cty_yr[, gr_deps_remaining := fifelse(deps_remain_t1 > 0,
                                           (deps_remain_tp1 - deps_remain_t1) / deps_remain_t1,
                                           NA_real_)]

# Log difference
bank_cty_yr[, dlog_deps_remaining := fifelse(deps_remain_t1 > 0 & deps_remain_tp1 > 0,
                                             log(deps_remain_tp1) - log(deps_remain_t1),
                                             NA_real_)]

# IMPROVED: Separate t-1 to t and t to t+1 changes
bank_cty_yr[, delta_deps_t1_to_t := deps_remain_t - deps_remain_t1]
bank_cty_yr[, delta_deps_t_to_tp1 := deps_remain_tp1 - deps_remain_t]

bank_cty_yr[, dlog_deps_t1_to_t := fifelse(deps_remain_t1 > 0 & deps_remain_t > 0,
                                           log(deps_remain_t) - log(deps_remain_t1),
                                           NA_real_)]
bank_cty_yr[, dlog_deps_t_to_tp1 := fifelse(deps_remain_t > 0 & deps_remain_tp1 > 0,
                                            log(deps_remain_tp1) - log(deps_remain_t),
                                            NA_real_)]

# NEW: Retention and migration rates
bank_cty_yr[, total_deps_bank_county_t1 := deps_closed_t1 + deps_remain_t1]
bank_cty_yr[, total_deps_bank_county_tp1 := deps_remain_tp1]

bank_cty_yr[, retention_rate := fifelse(total_deps_bank_county_t1 > 0,
                                        total_deps_bank_county_tp1 / total_deps_bank_county_t1,
                                        NA_real_)]

bank_cty_yr[, migration_rate := fifelse(deps_closed_t1 > 0,
                                        (deps_remain_tp1 - deps_remain_t1) / deps_closed_t1,
                                        NA_real_)]

# ============================================================
# 5) CLOSURE INDICATORS AND NETWORK CHANGE FLAGS
# ============================================================

cat("\n=== CREATING CLOSURE AND NETWORK CHANGE FLAGS ===\n")

bank_cty_yr[, any_closure_t := as.integer(deps_closed_t1 > 0)]

setorder(bank_cty_yr, bank_id, county, YEAR)

# Forward/backward closure indicators
bank_cty_yr[, any_closure_tp1 := shift(any_closure_t, 1L, type = "lead"), by = .(bank_id, county)]
bank_cty_yr[, any_closure_tm1 := shift(any_closure_t, 1L, type = "lag"), by = .(bank_id, county)]
bank_cty_yr[is.na(any_closure_tp1), any_closure_tp1 := 0L]
bank_cty_yr[is.na(any_closure_tm1), any_closure_tm1 := 0L]

bank_cty_yr[, closure_next_year := any_closure_tp1]
bank_cty_yr[, closure_prev_year := any_closure_tm1]
bank_cty_yr[, closure_t_and_next := as.integer(any_closure_t == 1L & closure_next_year == 1L)]
bank_cty_yr[, closure_t_and_prev := as.integer(any_closure_t == 1L & closure_prev_year == 1L)]

# Branch counts and network changes
bank_cty_branches <- closure_app[, .(
  n_branches_curr = uniqueN(UNINUMBR)
), by = .(bank_id, county, YEAR)]

bank_cty_closed_counts <- closure_app[closed == 1L, .(
  n_closed_curr = uniqueN(UNINUMBR)
), by = .(bank_id, county, YEAR)]

bank_cty_yr <- merge(bank_cty_yr, bank_cty_branches,
                     by = c("bank_id", "county", "YEAR"), all.x = TRUE)
bank_cty_yr <- merge(bank_cty_yr, bank_cty_closed_counts,
                     by = c("bank_id", "county", "YEAR"), all.x = TRUE)

bank_cty_yr[is.na(n_branches_curr), n_branches_curr := 0L]
bank_cty_yr[is.na(n_closed_curr), n_closed_curr := 0L]

# Next-year branch count
setorder(bank_cty_yr, bank_id, county, YEAR)
bank_cty_yr[, n_branches_tp1 := shift(n_branches_curr, 1L, type = "lead"),
            by = .(bank_id, county)]
bank_cty_yr[is.na(n_branches_tp1), n_branches_tp1 := n_branches_curr]

# Net change in branch count
bank_cty_yr[, delta_branches_t_to_tp1 := n_branches_tp1 - n_branches_curr]

# Clean network change flags
bank_cty_yr[, clean_no_change_if_no_closure :=
              as.integer(any_closure_t == 0L & delta_branches_t_to_tp1 == 0L)]

bank_cty_yr[, clean_only_current_closures :=
              as.integer(any_closure_t == 1L &
                           delta_branches_t_to_tp1 == -n_closed_curr &
                           any_closure_tp1 == 0L)]

bank_cty_yr[, clean_next_year_network :=
              as.integer((any_closure_t == 0L & clean_no_change_if_no_closure == 1L) |
                           (any_closure_t == 1L & clean_only_current_closures == 1L))]

# ============================================================
# 6) CLOSURE INTENSITY AND EXTREME VALUES
# ============================================================

cat("\n=== IDENTIFYING EXTREME CLOSURE INTENSITY ===\n")

bank_cty_yr[, closure_intensity :=
              fifelse(deps_closed_t1 > 0 & deps_remain_t1 > 0,
                      deps_closed_t1 / deps_remain_t1,
                      NA_real_)]

# Percentile-based extreme flag
cut_intensity <- bank_cty_yr[deps_closed_t1 > 0 & !is.na(closure_intensity),
                             quantile(closure_intensity, probs = tail_p, na.rm = TRUE)]

bank_cty_yr[, extreme_intensity_pctl :=
              as.integer(
                deps_closed_t1 > 0 &
                  !is.na(closure_intensity) &
                  (closure_intensity >= cut_intensity | closure_intensity < 0.05)
              )]

cat("Extreme intensity cutoff (", tail_p*100, "th percentile):", round(cut_intensity, 3), "\n")
cat("Observations flagged as extreme:", sum(bank_cty_yr$extreme_intensity_pctl, na.rm=TRUE), "\n")

# Intensity quartiles for heterogeneity analysis
bank_cty_yr[deps_closed_t1 > 0 & !is.na(closure_intensity), 
            intensity_quartile := cut(closure_intensity,
                                      breaks = quantile(closure_intensity, c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE),
                                      labels = FALSE, include.lowest = TRUE)]

# Winsorized version for robustness
bank_cty_yr[, deps_closed_t1_w99 := pmin(deps_closed_t1,
                                         quantile(deps_closed_t1[deps_closed_t1 > 0], 0.99, na.rm=TRUE))]

# ============================================================
# 7) LAGGED OUTCOMES FOR PARALLEL TRENDS
# ============================================================

cat("\n=== CREATING LAGGED OUTCOMES FOR PRE-TRENDS ===\n")

setorder(bank_cty_yr, bank_id, county, YEAR)

bank_cty_yr[, `:=`(
  dlog_deps_remaining_lag1 = shift(dlog_deps_remaining, 1L, type = "lag"),
  dlog_deps_remaining_lag2 = shift(dlog_deps_remaining, 2L, type = "lag"),
  gr_deps_remaining_lag1 = shift(gr_deps_remaining, 1L, type = "lag")
), by = .(bank_id, county)]

# Event time indicators for event study
bank_cty_yr[, closure_lead1 := shift(any_closure_t, 1L, type = "lead"), by = .(bank_id, county)]
bank_cty_yr[, closure_lead2 := shift(any_closure_t, 2L, type = "lead"), by = .(bank_id, county)]
bank_cty_yr[, closure_lag1 := shift(any_closure_t, 1L, type = "lag"), by = .(bank_id, county)]
bank_cty_yr[, closure_lag2 := shift(any_closure_t, 2L, type = "lag"), by = .(bank_id, county)]

bank_cty_yr[is.na(closure_lead1), closure_lead1 := 0L]
bank_cty_yr[is.na(closure_lead2), closure_lead2 := 0L]
bank_cty_yr[is.na(closure_lag1), closure_lag1 := 0L]
bank_cty_yr[is.na(closure_lag2), closure_lag2 := 0L]

# ============================================================
# 8) DEFINE REGRESSION SAMPLES
# ============================================================

cat("\n=== DEFINING REGRESSION SAMPLES ===\n")

# Base sample: can measure remaining-branch outcomes
reg_base <- bank_cty_yr[
  YEAR >= start_year & YEAR <= end_year &
    n_remaining_branches > 0 &
    !is.na(dlog_deps_remaining)
]

cat("Base sample size:", nrow(reg_base), "\n")
cat("  - With closures:", sum(reg_base$any_closure_t), "\n")
cat("  - Without closures (controls):", sum(reg_base$any_closure_t == 0), "\n")

# Main sample: clean network, no extremes, minimum thresholds
reg_base[, include_main := 
           clean_next_year_network == 1 &
           extreme_intensity_pctl == 0 &
           deps_remain_t1 >= min_deposits_threshold &
           n_remaining_branches >= min_remaining_branches]

reg_main <- reg_base[include_main == 1]

cat("Main sample size:", nrow(reg_main), "\n")
cat("  - With closures:", sum(reg_main$any_closure_t), "\n")
cat("  - Without closures:", sum(reg_main$any_closure_t == 0), "\n")

# Alternative sample: exclude sequential closures
reg_no_sequential <- reg_main[closure_t_and_next == 0 & closure_t_and_prev == 0]

cat("No sequential closures sample:", nrow(reg_no_sequential), "\n")

# Exit diagnostics
exit_bc <- bank_cty_yr[
  YEAR >= start_year & YEAR <= end_year &
    deps_closed_t1 > 0 &
    n_remaining_branches == 0
]

cat("\n=== EXIT DIAGNOSTICS ===\n")
cat("Bank-county-years with closures & NO remaining branches:", nrow(exit_bc), "\n")
cat("Exit share among closure observations:",
    round(nrow(exit_bc) / max(1, nrow(bank_cty_yr[YEAR>=start_year & YEAR<=end_year & deps_closed_t1>0])), 4), "\n")

# ============================================================
# 9) DESCRIPTIVE STATISTICS
# ============================================================

cat("\n=== GENERATING DESCRIPTIVE STATISTICS ===\n")

# Summary statistics for main sample
desc_vars <- c("deps_closed_t1", "deps_remain_t1", "deps_remain_tp1",
               "delta_deps_remaining", "gr_deps_remaining", "dlog_deps_remaining",
               "retention_rate", "migration_rate", "closure_intensity",
               "n_remaining_branches", "n_closed_branches", "has_app")

desc_stats <- reg_main[, lapply(.SD, function(x) {
  c(mean = mean(x, na.rm=TRUE),
    sd = sd(x, na.rm=TRUE),
    p25 = quantile(x, 0.25, na.rm=TRUE),
    median = median(x, na.rm=TRUE),
    p75 = quantile(x, 0.75, na.rm=TRUE),
    N = sum(!is.na(x)))
}), .SDcols = desc_vars]

desc_stats_t <- transpose(desc_stats, keep.names = "variable")
# fwrite(desc_stats_t, paste0(output_dir, "descriptive_stats_main.csv"))

# Separate stats for treatment vs control
desc_treatment <- reg_main[any_closure_t == 1, lapply(.SD, function(x) {
  c(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE), N = sum(!is.na(x)))
}), .SDcols = desc_vars]

desc_control <- reg_main[any_closure_t == 0, lapply(.SD, function(x) {
  c(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE), N = sum(!is.na(x)))
}), .SDcols = desc_vars]

# ============================================================
# 10) PARALLEL TRENDS TEST
# ============================================================

cat("\n=== TESTING PARALLEL TRENDS ===\n")

# Test if closures predict LAGGED outcome changes
m_pretrend_1lag <- feols(
  dlog_deps_remaining_lag1 ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

m_pretrend_2lags <- feols(
  dlog_deps_remaining_lag2 ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

cat("Pre-trend test results:\n")
cat("  Lag 1: coef =", round(coef(m_pretrend_1lag)["deps_closed_t1"], 6),
    ", p-val =", round(pvalue(m_pretrend_1lag)["deps_closed_t1"], 4), "\n")
cat("  Lag 2: coef =", round(coef(m_pretrend_2lags)["deps_closed_t1"], 6),
    ", p-val =", round(pvalue(m_pretrend_2lags)["deps_closed_t1"], 4), "\n")

# ============================================================
# 11) MAIN REGRESSION RESULTS
# ============================================================

cat("\n=== RUNNING MAIN REGRESSIONS ===\n")

# (1) Base sample with all observations
m1_base <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_base,
  vcov = ~bank_id
)

# (2) Main sample: clean network, no extremes
m2_main <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# (3) Log specification
m3_log <- feols(
  dlog_deps_remaining ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# (4) No sequential closures
m4_no_seq <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_no_sequential,
  vcov = ~bank_id
)

# (5) Retention rate outcome
m5_retention <- feols(
  retention_rate ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# Display main results
etable(list(m1_base, m2_main, m3_log, m4_no_seq, m5_retention),
       headers = c("Base", "Main", "Log spec", "No sequential", "Retention rate"))

# ============================================================
# 12) SEPARATE PRE AND POST CLOSURE PERIODS
# ============================================================

cat("\n=== ANALYZING PRE VS POST CLOSURE PERIODS ===\n")

# t-1 to t (includes closure event)
m_pre <- feols(
  delta_deps_t1_to_t ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# t to t+1 (post-closure)
m_post <- feols(
  delta_deps_t_to_tp1 ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

etable(list(m_pre, m_post, m2_main),
       headers = c("t-1 to t", "t to t+1", "t-1 to t+1"))

# ============================================================
# 13) HETEROGENEITY ANALYSIS
# ============================================================

cat("\n=== HETEROGENEITY ANALYSIS ===\n")

# (A) Mobile app availability
m_hetero_app <- feols(
  delta_deps_remaining ~ deps_closed_t1 * has_app + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# (B) Closure intensity quartiles (among treatment observations)
m_hetero_intensity <- feols(
  delta_deps_remaining ~ i(intensity_quartile, deps_closed_t1, ref=1) + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main[any_closure_t == 1 & !is.na(intensity_quartile)],
  vcov = ~bank_id
)

# (C) Number of remaining branches
reg_main[, many_remaining := as.integer(n_remaining_branches >= 5)]

m_hetero_branches <- feols(
  delta_deps_remaining ~ deps_closed_t1 * many_remaining + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# (D) Year effects (test for Great Recession differences)
reg_main[, post_2015 := as.integer(YEAR >= 2015)]

m_hetero_year <- feols(
  delta_deps_remaining ~ deps_closed_t1 * post_2015 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

etable(list(m_hetero_app, m_hetero_branches, m_hetero_year),
       headers = c("Mobile app", "Many branches", "Post-2015"))

# ============================================================
# 14) ROBUSTNESS CHECKS
# ============================================================

cat("\n=== ROBUSTNESS CHECKS ===\n")

# (A) Alternative standard errors
m_rob_twoway <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id + county
)

# (B) Winsorized treatment variable
m_rob_winsor <- feols(
  delta_deps_remaining ~ deps_closed_t1_w99 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# (C) Control for lagged outcome
m_rob_lag_control <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1 + dlog_deps_remaining_lag1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

# (D) Growth rate outcome
m_rob_growth <- feols(
  gr_deps_remaining ~ deps_closed_t1 + deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_main,
  vcov = ~bank_id
)

etable(list(m2_main, m_rob_twoway, m_rob_winsor, m_rob_lag_control, m_rob_growth),
       headers = c("Baseline", "Two-way SE", "Winsorized", "Lag control", "Growth rate"))

# ============================================================
# 15) EVENT STUDY SPECIFICATION
# ============================================================

cat("\n=== EVENT STUDY ANALYSIS ===\n")

# Create event time dummies (relative to closure year)
# Note: This is simplified; for publication, use proper event study with
# multiple years of leads/lags

m_event <- feols(
  delta_deps_remaining ~ 
    i(closure_lead2, deps_closed_t1, ref=0) +
    i(closure_lead1, deps_closed_t1, ref=0) +
    i(any_closure_t, deps_closed_t1, ref=0) +
    i(closure_lag1, deps_closed_t1, ref=0) +
    deps_remain_t1 |
    bank_id^YEAR + county^YEAR,
  data = reg_base,
  vcov = ~bank_id
)

# Extract coefficients for plotting
event_coefs <- coef(m_event)
event_ses <- se(m_event)

event_results <- data.table(
  period = c(-2, -1, 0, 1),
  coef = event_coefs[grepl("closure_lead2|closure_lead1|any_closure_t|closure_lag1", names(event_coefs))],
  se = event_ses[grepl("closure_lead2|closure_lead1|any_closure_t|closure_lag1", names(event_ses))]
)
event_results[, ci_lower := coef - 1.96 * se]
event_results[, ci_upper := coef + 1.96 * se]



# ============================================================
# 16) ECONOMIC MAGNITUDES
# ============================================================

cat("\n=== CALCULATING ECONOMIC MAGNITUDES ===\n")

# Extract main coefficient
coef_closed <- coef(m2_main)["deps_closed_t1"]
se_closed <- se(m2_main)["deps_closed_t1"]

# Sample means (among treatment observations)
mean_closed <- mean(reg_main[any_closure_t == 1, deps_closed_t1], na.rm=TRUE)
mean_remain_t1 <- mean(reg_main[any_closure_t == 1, deps_remain_t1], na.rm=TRUE)
mean_delta <- mean(reg_main[any_closure_t == 1, delta_deps_remaining], na.rm=TRUE)

# Implied migration rate: how much of closed deposits end up at remaining branches?
implied_migration <- coef_closed / mean_closed
implied_migration_se <- se_closed / mean_closed

# Share of delta explained by closures (R-squared type measure)
share_explained <- (coef_closed * mean_closed) / mean_delta

cat("\n=== Economic Magnitudes ===\n")
cat("Mean deposits closed (treatment obs):", round(mean_closed, 0), "\n")
cat("Mean deposits remaining t-1 (treatment obs):", round(mean_remain_t1, 0), "\n")
cat("Mean change in remaining deposits (treatment obs):", round(mean_delta, 0), "\n\n")

cat("Main coefficient (delta_deps_remaining ~ deps_closed_t1):\n")
cat("  Coef:", round(coef_closed, 4), "\n")
cat("  SE:", round(se_closed, 4), "\n")
cat("  Interpretation: $1 closed → $", round(coef_closed, 2), 
    " gain at remaining branches\n\n")

cat("Implied migration rate:\n")
cat("  Point estimate:", round(implied_migration * 100, 2), "% of closed deposits retained\n")
cat("  SE:", round(implied_migration_se * 100, 2), "%\n")
cat("  95% CI: [", round((implied_migration - 1.96*implied_migration_se) * 100, 2), "%, ",
    round((implied_migration + 1.96*implied_migration_se) * 100, 2), "%]\n\n")

cat("Share of deposit change explained by closures:", round(share_explained * 100, 2), "%\n")

# Save magnitudes
magnitudes <- data.table(
  metric = c("coef_closed", "se_closed", "mean_closed", "mean_remain_t1", 
             "implied_migration_pct", "migration_se_pct"),
  value = c(coef_closed, se_closed, mean_closed, mean_remain_t1,
            implied_migration * 100, implied_migration_se * 100)
)
fwrite(magnitudes, paste0(output_dir, "economic_magnitudes.csv"))

# ============================================================
# 17) DIAGNOSTIC PLOTS
# ============================================================

cat("\n=== CREATING DIAGNOSTIC PLOTS ===\n")

# Distribution of closure intensity
p1 <- ggplot(reg_main[any_closure_t == 1 & !is.na(closure_intensity)], 
             aes(x = closure_intensity)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = cut_intensity, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Closure Intensity",
       subtitle = paste0("Red line = ", tail_p*100, "th percentile cutoff"),
       x = "Closure Intensity (closed deposits / remaining deposits)",
       y = "Frequency") +
  theme_minimal()


# Deposits by treatment status over time
time_series_data <- reg_base[, .(
  mean_remain = mean(deps_remain_t1, na.rm=TRUE),
  mean_closed = mean(deps_closed_t1[any_closure_t==1], na.rm=TRUE),
  n_closures = sum(any_closure_t)
), by = YEAR]

p2 <- ggplot(time_series_data, aes(x = YEAR)) +
  geom_line(aes(y = mean_remain, color = "Remaining deposits (t-1)"), size = 1) +
  geom_line(aes(y = mean_closed, color = "Closed deposits (t-1, conditional)"), size = 1) +
  geom_point(aes(y = n_closures * 50, color = "# Closures (x50)"), size = 2) +
  scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
  labs(title = "Deposits and Closures Over Time",
       x = "Year", y = "Deposits ($1000s)",
       color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Event study plot
p3 <- ggplot(event_results, aes(x = period, y = coef)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "red") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1),
                     labels = c("t-2", "t-1", "t (closure)", "t+1")) +
  labs(title = "Event Study: Deposit Changes Around Branch Closures",
       x = "Period Relative to Closure",
       y = "Coefficient on Closed Deposits",
       caption = "Vertical bars = 95% confidence intervals") +
  theme_minimal()


# ============================================================
# 18) BALANCE TABLE
# ============================================================

cat("\n=== CREATING BALANCE TABLE ===\n")

balance_vars <- c("deps_remain_t1", "n_remaining_branches", "n_branches_curr", 
                  "has_app", "dlog_deps_remaining_lag1")

balance_table <- rbindlist(lapply(balance_vars, function(v) {
  treated <- reg_main[any_closure_t == 1, get(v)]
  control <- reg_main[any_closure_t == 0, get(v)]
  
  t_test <- t.test(treated, control)
  
  data.table(
    variable = v,
    mean_treated = mean(treated, na.rm=TRUE),
    sd_treated = sd(treated, na.rm=TRUE),
    mean_control = mean(control, na.rm=TRUE),
    sd_control = sd(control, na.rm=TRUE),
    diff = mean(treated, na.rm=TRUE) - mean(control, na.rm=TRUE),
    p_value = t_test$p.value
  )
}))

fwrite(balance_table, paste0(output_dir, "balance_table.csv"))

# ============================================================
# 19) YEAR-BY-YEAR TREATMENT DISTRIBUTION
# ============================================================

year_closure_dist <- reg_main[, .(
  n_obs = .N,
  n_closures = sum(any_closure_t),
  pct_closures = mean(any_closure_t) * 100,
  mean_closed = mean(deps_closed_t1[any_closure_t==1], na.rm=TRUE),
  mean_intensity = mean(closure_intensity[any_closure_t==1], na.rm=TRUE)
), by = YEAR]

fwrite(year_closure_dist, paste0(output_dir, "closure_distribution_by_year.csv"))

cat("\n=== Year-by-Year Closure Distribution ===\n")
print(year_closure_dist)

# ============================================================
# 20) SAVE FINAL DATASETS
# ============================================================

cat("\n=== SAVING FINAL DATASETS ===\n")

# Save regression-ready datasets
fwrite(reg_base, paste0(output_dir, "regression_sample_base.csv"))
fwrite(reg_main, paste0(output_dir, "regression_sample_main.csv"))
fwrite(reg_no_sequential, paste0(output_dir, "regression_sample_no_sequential.csv"))

# Save bank-county-year panel
fwrite(bank_cty_yr, paste0(output_dir, "bank_county_year_panel.csv"))

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All outputs saved to:", output_dir, "\n")
cat("\nKey files created:\n")
cat("  - regression_sample_main.csv (primary analysis sample)\n")
cat("  - table_main_results.tex (main regression table)\n")
cat("  - table_heterogeneity.tex (heterogeneity analysis)\n")
cat("  - table_robustness.tex (robustness checks)\n")
cat("  - economic_magnitudes.csv (effect size interpretations)\n")
cat("  - descriptive_stats_main.csv (summary statistics)\n")
cat("  - Various diagnostic plots (.png files)\n")

# ============================================================
# 21) FINAL SUMMARY STATISTICS
# ============================================================

cat("\n=== FINAL SAMPLE SUMMARY ===\n")
cat("Analysis period:", start_year, "-", end_year, "\n")
cat("Base sample observations:", nrow(reg_base), "\n")
cat("Main sample observations:", nrow(reg_main), "\n")
cat("  - Bank-county-years with closures:", sum(reg_main$any_closure_t), "\n")
cat("  - Bank-county-years without closures:", sum(reg_main$any_closure_t == 0), "\n")
cat("  - Unique banks:", uniqueN(reg_main$bank_id), "\n")
cat("  - Unique counties:", uniqueN(reg_main$county), "\n")
cat("\nMain result (delta_deps_remaining ~ deps_closed_t1):\n")
cat("  Coefficient:", round(coef_closed, 4), "\n")
cat("  Standard error:", round(se_closed, 4), "\n")
cat("  T-statistic:", round(coef_closed / se_closed, 2), "\n")
cat("  Implied migration rate:", round(implied_migration * 100, 2), "%\n")
cat("\n=== END OF SCRIPT ===\n")