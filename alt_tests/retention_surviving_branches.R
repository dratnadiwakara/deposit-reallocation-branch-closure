# ============================================================
# Bank–County Panel: "Retention through remaining branches"
# UPDATED per request:
#  1) Regression sample now INCLUDES bank–county–years with deps_closed_t1 == 0
#     (controls), as long as there are remaining branches to measure outcomes.
#  2) Adds:
#       - any_closure_t      = 1{deps_closed_t1 > 0}
#       - closure_next_year  = 1{any_closure_{t+1} == 1}   (within same bank_id,county)
#       - closure_t_and_next = 1{any_closure_t==1 & closure_next_year==1}
#     so you can filter out “back-to-back closure years” if desired.
#  3) Keeps exit diagnostics (closures but no remaining branches) as before.
# ============================================================

rm(list=ls())
library(data.table)
library(stringr)
library(fixest)

# Inputs
closure_path   <- "C:/OneDrive/data/closure_opening_data_simple.rds"
app_panel_path <- "C:/OneDrive/data/CH_full_app_reviews_panel.csv"

start_year <- 2012
end_year   <- 2019

# -----------------------------
# 0) Load data
# -----------------------------
closure_raw <- readRDS(closure_path); setDT(closure_raw)
closure_raw[, county := str_pad(STCNTYBR, 5, "left", "0")]
closure_raw[, YEAR := as.integer(YEAR)]

app_panel <- fread(app_panel_path); setDT(app_panel)
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
  by = c("CERT","YEAR"),
  all.x = TRUE
)
closure_app[is.na(has_app), has_app := 0L]

# Bank identifier for bank–county panel
closure_app[, bank_id := CERT]  # swap to RSSDID if desired

# -----------------------------
# 1) Create aligned t-1 and t+1 deposits at branch level
# -----------------------------
setorder(closure_app, UNINUMBR, YEAR)
closure_app[, `:=`(
  dep_lag1   = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1  = shift(YEAR,    1L, type = "lag"),
  dep_lead1  = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1 = shift(YEAR,    1L, type = "lead")
), by = UNINUMBR]

closure_app[, dep_lag1_aligned  := fifelse(year_lag1  == YEAR - 1L, dep_lag1,  NA_real_)]
closure_app[, dep_lead1_aligned := fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_)]

closure_app[, exists_t1  := !is.na(dep_lag1_aligned)  & dep_lag1_aligned  > 0]
closure_app[, exists_tp1 := !is.na(dep_lead1_aligned) & dep_lead1_aligned > 0]

# -----------------------------
# 2) Identify "closed branches" vs "remaining branches"
# -----------------------------
closure_app[, is_closed_t := (closed == 1L)]

closure_app[, is_remaining_branch :=
              (!is_closed_t) & exists_t1 & exists_tp1]

closure_app[, is_closed_branch :=
              is_closed_t & exists_t1]

# -----------------------------
# 3) Build bank–county–year aggregates
# -----------------------------
sum_na_safe <- function(x) if (all(is.na(x))) NA_real_ else as.numeric(sum(x, na.rm = TRUE))

bank_cty_yr_ret <- closure_app[, .(
  deps_closed_t1 = sum_na_safe(fifelse(is_closed_branch, dep_lag1_aligned, NA_real_)),
  
  deps_remain_t1  = sum_na_safe(fifelse(is_remaining_branch, dep_lag1_aligned, NA_real_)),
  deps_remain_tp1 = sum_na_safe(fifelse(is_remaining_branch, dep_lead1_aligned, NA_real_)),
  
  n_closed_branches    = sum(is_closed_branch, na.rm = TRUE),
  n_remaining_branches = sum(is_remaining_branch, na.rm = TRUE),
  
  has_app = max(has_app, na.rm = TRUE)
), by = .(bank_id, county, YEAR)]

# Replace NA sums with 0 where appropriate
for (v in c("deps_closed_t1","deps_remain_t1","deps_remain_tp1")) {
  bank_cty_yr_ret[is.na(get(v)), (v) := 0]
}
bank_cty_yr_ret[is.na(has_app), has_app := 0L]

# Outcomes
bank_cty_yr_ret[, delta_deps_remaining := deps_remain_tp1 - deps_remain_t1]
bank_cty_yr_ret[, gr_deps_remaining := fifelse(deps_remain_t1 > 0,
                                               (deps_remain_tp1 - deps_remain_t1) / deps_remain_t1,
                                               NA_real_)]
bank_cty_yr_ret[, dlog_deps_remaining := fifelse(deps_remain_t1 > 0 & deps_remain_tp1 > 0,
                                                 log(deps_remain_tp1) - log(deps_remain_t1),
                                                 NA_real_)]

# -----------------------------
# 3b) NEW: closure indicators + "closure next year" flag
# -----------------------------
bank_cty_yr_ret[, any_closure_t := as.integer(deps_closed_t1 > 0)]

setorder(bank_cty_yr_ret, bank_id, county, YEAR)

# Any closure next year (t+1) for same bank×county
bank_cty_yr_ret[, any_closure_tp1 := shift(any_closure_t, 1L, type = "lead"), by = .(bank_id, county)]
bank_cty_yr_ret[is.na(any_closure_tp1), any_closure_tp1 := 0L]

# Helper flags you can filter on
bank_cty_yr_ret[, closure_next_year  := any_closure_tp1]
bank_cty_yr_ret[, closure_t_and_next := as.integer(any_closure_t == 1L & closure_next_year == 1L)]


# -----------------------------
# 3c) Build bank–county–year branch counts and next-year changes
# -----------------------------
# Total observed branches in the bank×county×year (SoD presence that year)
bank_cty_branches <- closure_app[, .(
  n_branches_curr = uniqueN(UNINUMBR)
), by = .(bank_id, county, YEAR)]

# How many branches closed in year t within bank×county
# (count of unique branch IDs flagged closed==1 in year t)
bank_cty_closed_counts <- closure_app[closed == 1L, .(
  n_closed_curr = uniqueN(UNINUMBR)
), by = .(bank_id, county, YEAR)]

# Merge into bank_cty_yr_ret
bank_cty_yr_ret <- merge(bank_cty_yr_ret, bank_cty_branches,
                         by = c("bank_id","county","YEAR"), all.x = TRUE)
bank_cty_yr_ret <- merge(bank_cty_yr_ret, bank_cty_closed_counts,
                         by = c("bank_id","county","YEAR"), all.x = TRUE)

bank_cty_yr_ret[is.na(n_branches_curr), n_branches_curr := 0L]
bank_cty_yr_ret[is.na(n_closed_curr),   n_closed_curr   := 0L]

# Next-year branch count (t+1) within same bank×county
setorder(bank_cty_yr_ret, bank_id, county, YEAR)
bank_cty_yr_ret[, n_branches_tp1 := shift(n_branches_curr, 1L, type = "lead"),
                by = .(bank_id, county)]
bank_cty_yr_ret[is.na(n_branches_tp1), n_branches_tp1 := n_branches_curr]  # conservative

# Net change in branch count from t to t+1
bank_cty_yr_ret[, delta_branches_t_to_tp1 := n_branches_tp1 - n_branches_curr]

# -----------------------------
# 3d) Define "clean change" dummies
# -----------------------------
# Case A (no closures in year t): want NO network change between t and t+1
bank_cty_yr_ret[, clean_no_change_if_no_closure :=
                  as.integer(any_closure_t == 0L & delta_branches_t_to_tp1 == 0L)]

# Case B (closures in year t): want the ONLY change between t and t+1 to be those closures
# That implies:
#   - branch count should fall by exactly n_closed_curr (t's closures)
#   - and there should be NO closures in t+1 (we already have closure_next_year / any_closure_tp1)
# Notes:
#   - This rules out openings (which would offset the drop).
#   - It also rules out additional closures in t+1.
bank_cty_yr_ret[, clean_only_current_closures :=
                  as.integer(any_closure_t == 1L &
                               delta_branches_t_to_tp1 == -n_closed_curr &
                               any_closure_tp1 == 0L)]

# Unified flag: passes whichever condition applies
bank_cty_yr_ret[, clean_next_year_network :=
                  as.integer((any_closure_t == 0L & clean_no_change_if_no_closure == 1L) |
                               (any_closure_t == 1L & clean_only_current_closures == 1L))]



bank_cty_yr_ret[, closure_intensity :=
                  fifelse(deps_closed_t1 > 0 & deps_remain_t1 > 0,
                          deps_closed_t1 / deps_remain_t1,
                          NA_real_)]

# -----------------------------
# A) Percentile-based extreme flag (e.g., top 1% or top 0.5%)
# -----------------------------
# Choose tail cutoff here
tail_p <- 0.95  # 0.99 = top 1%; use 0.995 for top 0.5%

cut_intensity <- bank_cty_yr_ret[deps_closed_t1 > 0 & !is.na(closure_intensity),
                                 quantile(closure_intensity, probs = tail_p, na.rm = TRUE)]

bank_cty_yr_ret[, extreme_intensity_pctl :=
                  as.integer(
                    deps_closed_t1 > 0 &
                      !is.na(closure_intensity) &
                      (closure_intensity >= cut_intensity | closure_intensity < 0.05)
                  )
]
# -----------------------------
# 4) UPDATED: Regression sample includes controls (deps_closed_t1 == 0)
# -----------------------------
# Keep bank–county–years where we can measure remaining-branch outcomes.
# - Must have remaining branches (so we observe deposits through branches)
# - Must have defined outcome (optional: choose dlog or delta/gr)
# - Within year window
reg_bc <- bank_cty_yr_ret[
  YEAR >= start_year & YEAR <= end_year &
    n_remaining_branches > 0 &
    !is.na(dlog_deps_remaining)
]

# Example: if you want to drop back-to-back closure years later:
# reg_bc_no_seq <- reg_bc[closure_t_and_next == 0]

# -----------------------------
# 5) Exit diagnostics (unchanged idea)
# -----------------------------
# Bank exits county in year t in the sense:
#  - there are closures (deps_closed_t1>0) but no remaining branches we can track
exit_bc <- bank_cty_yr_ret[
  YEAR >= start_year & YEAR <= end_year &
    deps_closed_t1 > 0 &
    n_remaining_branches == 0
]

cat("\n=== Exit diagnostics ===\n")
cat("Bank–County–Years with closures & NO remaining branches:", nrow(exit_bc), "\n")
cat("Exit share among closure bank–county–years:",
    round(nrow(exit_bc) / max(1, nrow(bank_cty_yr_ret[YEAR>=start_year & YEAR<=end_year & deps_closed_t1>0])), 4), "\n")

# -----------------------------
# (Optional) Example regression using expanded sample with controls
# -----------------------------
# FE suggestion: bank×year + county×year as before
m_ret_controls <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1  |
    bank_id^YEAR + county^YEAR,
  data = reg_bc,
  vcov = ~ bank_id
)

# If you want to exclude sequential-closure cases:
m_ret_controls_no_seq <- feols(
  delta_deps_remaining ~ deps_closed_t1 + deps_remain_t1  |
    bank_id^YEAR + county^YEAR,
  data = reg_bc[clean_next_year_network  == 1 & extreme_intensity_pctl==0],
  vcov = ~ bank_id
)

etable(list(m_ret_controls, m_ret_controls_no_seq),
       headers = c("All (incl. controls)", "Drop closure next-year"))


