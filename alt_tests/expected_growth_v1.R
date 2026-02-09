rm(list=ls())
library(data.table)
library(stringr)
library(fixest)
library(DescTools)
library(ggplot2)

# Load summary stats function
source('https://raw.githubusercontent.com/dratnadiwakara/r-utilities/refs/heads/main/summary_stat_tables.R')

# Inputs (adjust paths)
closure_path   <- "C:/OneDrive/data/closure_opening_data_simple.rds"
app_panel_path <- "C:/OneDrive/data/CH_full_app_reviews_panel.csv"
fdic_path      <- "C:/OneDrive/data/fdic_sod_2000_2025_simple.rds"

start_year <- 2012
end_year   <- 2019

#-----------------------------
# 0) Load data
#-----------------------------
closure_raw <- readRDS(closure_path); setDT(closure_raw)
closure_raw[, county := str_pad(STCNTYBR, 5, "left", "0")]
closure_raw[, YEAR := as.integer(YEAR)]

app_panel <- fread(app_panel_path); setDT(app_panel)
app_panel <- app_panel[, .(
  CERT = FDIC_certificate_id,
  YEAR = as.integer(year),
  first_app_available,    
  reviews_available,
  yearly_rating
)]

app_panel[, has_app := fifelse(first_app_available == 1, 1L, 0L)]

#-----------------------------
# 1) Branch-level lags/leads for growth
#-----------------------------
setorder(closure_raw, UNINUMBR, YEAR)

closure_raw[, `:=`(
  dep_lag1  = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1 = shift(YEAR,    1L, type = "lag"),
  dep_lead3  = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3 = shift(YEAR,    3L, type = "lead")
), by = UNINUMBR]

closure_raw[, dep_lag1_aligned  := fifelse(year_lag1  == YEAR - 1L, dep_lag1,  NA_real_)]
closure_raw[, dep_lead3_aligned := fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_)]

closure_raw[, gr_branch := fifelse(!is.na(dep_lag1_aligned) & dep_lag1_aligned > 0,
                                   (dep_lead3_aligned - dep_lag1_aligned) / dep_lag1_aligned,
                                   NA_real_)]

#-----------------------------
# 2) Merge bank-year app indicator onto branch-year
#-----------------------------
closure_app <- merge(
  closure_raw,
  app_panel[, .(CERT, YEAR, has_app)],
  by = c("CERT", "YEAR"),
  all.x = TRUE
)
closure_app[is.na(has_app), has_app := 0L]
closure_app[, own_bank_has_app := has_app]

#-----------------------------
# 3) County-year closure shares and ABSOLUTE AMOUNTS
#-----------------------------
sum_na_safe <- function(x) if (all(is.na(x))) NA_real_ else as.numeric(sum(x, na.rm = TRUE))

county_totals <- closure_app[, .(
  total_deps_county_lag1 = sum_na_safe(dep_lag1_aligned)
), by = .(county, YEAR)]

closed_by_app <- closure_app[closed == 1L, .(
  cl_closed_app   = sum_na_safe(fifelse(has_app == 1L, dep_lag1_aligned, NA_real_)),
  cl_closed_noapp = sum_na_safe(fifelse(has_app == 0L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR)]

county_closure_shares <- merge(county_totals, closed_by_app, by = c("county","YEAR"), all.x = TRUE)
county_closure_shares[is.na(cl_closed_app),   cl_closed_app   := 0]
county_closure_shares[is.na(cl_closed_noapp), cl_closed_noapp := 0]

county_closure_shares[, share_deps_closed_app :=
                        fifelse(total_deps_county_lag1 > 0, cl_closed_app / total_deps_county_lag1, 0)
]
county_closure_shares[, share_deps_closed_noapp :=
                        fifelse(total_deps_county_lag1 > 0, cl_closed_noapp / total_deps_county_lag1, 0)
]
county_closure_shares[, share_deps_closed := share_deps_closed_app + share_deps_closed_noapp]

#-----------------------------
# 4) County-year total deposits at t-1 and t+3
#-----------------------------

# Get county totals at each year (for all branches, including those that will close)
county_deps_all_years <- closure_app[, .(
  total_county_deps = sum_na_safe(DEPSUMBR)
), by = .(county, YEAR)]

# For market share calculation, we need:
# - Total at t-1 (includes all branches)
# - Total at t+3 (includes only survivors - branches that exist at t+3)

# Create helper: mark branches that exist at each time point
closure_app[, exists_t1 := !is.na(dep_lag1_aligned) & dep_lag1_aligned > 0]
closure_app[, exists_t3 := !is.na(dep_lead3_aligned) & dep_lead3_aligned > 0]

# County totals at t-1 (all branches that existed then)
county_deps_t1 <- closure_app[exists_t1 == TRUE, .(
  total_county_deps_t1 = sum_na_safe(dep_lag1_aligned)
), by = .(county, YEAR)]

# County totals at t+3 (only survivors)
county_deps_t3 <- closure_app[exists_t3 == TRUE, .(
  total_county_deps_t3 = sum_na_safe(dep_lead3_aligned)
), by = .(county, YEAR)]

#-----------------------------
# 5) Minimal county controls
#-----------------------------

# 5a) Build bank-county-year totals (to define "incumbent")
bank_cty_yr <- closure_app[, .(
  deps_curr = sum_na_safe(DEPSUMBR),
  deps_lag1 = sum_na_safe(dep_lag1_aligned),
  n_close   = sum(closed == 1L, na.rm = TRUE)
), by = .(RSSDID, county, YEAR)]

bank_cty_yr[, bank_type := fifelse(n_close > 0L, "CLOSER", "INCUMBENT")]

# 5b) County-year counts of banks (lagged)
county_bankcounts <- bank_cty_yr[, .(
  banks_county_curr = .N
), by = .(county, YEAR)]
setorder(county_bankcounts, county, YEAR)
county_bankcounts[, banks_county_lag1 := shift(banks_county_curr, 1L, type = "lag"), by = county]

# 5c) County deposit totals from FDIC SoD
fdic_all <- readRDS(fdic_path); setDT(fdic_all)
fdic_all[, county := str_pad(STCNTYBR, 5, "left", "0")]
fdic_all[, YEAR := as.integer(YEAR)]

market_totals <- fdic_all[, .(total_deps = sum(DEPSUMBR, na.rm = TRUE)), by = .(county, YEAR)]
complete_grid <- CJ(county = unique(market_totals$county),
                    YEAR   = min(market_totals$YEAR):max(market_totals$YEAR))
market_bal <- merge(complete_grid, market_totals, by = c("county","YEAR"), all.x = TRUE)
market_bal[is.na(total_deps), total_deps := 0]
setorder(market_bal, county, YEAR)

market_bal[, deps_lag1 := shift(total_deps, 1L, type = "lag"), by = county]
market_bal[, deps_lag4 := shift(total_deps, 4L, type = "lag"), by = county]
market_bal[, county_dep_growth_t4_t1 := fifelse(deps_lag4 > 0,
                                                (deps_lag1 - deps_lag4) / deps_lag4,
                                                NA_real_)]

# 5d) Combine controls into one county-year table
controls_cy <- merge(county_bankcounts[, .(county, YEAR, banks_county_lag1)],
                     market_bal[, .(county, YEAR, deps_lag1, county_dep_growth_t4_t1)],
                     by = c("county","YEAR"), all.x = TRUE)

controls_cy <- merge(controls_cy, county_totals[, .(county, YEAR, total_deps_county_lag1)],
                     by = c("county","YEAR"), all.x = TRUE)

#-----------------------------
# 6) Build regression branch panel
#-----------------------------
branch_panel <- closure_app[!is.na(gr_branch)]
branch_panel[, gr_branch := Winsorize(gr_branch, val = quantile(gr_branch, probs = c(0.025, 0.975), na.rm = FALSE))]
branch_panel[, state_yr := paste0(substr(county, 1, 2), YEAR)]
branch_panel[, bank_yr := paste0(RSSDID, YEAR)]

branch_panel <- merge(branch_panel,
                      county_closure_shares[, .(county, YEAR, share_deps_closed, 
                                                share_deps_closed_app, share_deps_closed_noapp,
                                                cl_closed_app, cl_closed_noapp)],
                      by = c("county","YEAR"),
                      all.x = TRUE)

branch_panel <- merge(branch_panel,
                      controls_cy[, .(county, YEAR, banks_county_lag1, county_dep_growth_t4_t1,
                                      deps_lag1)],
                      by = c("county","YEAR"),
                      all.x = TRUE)

# Merge county totals at t-1 and t+3
branch_panel <- merge(branch_panel,
                      county_deps_t1[, .(county, YEAR, total_county_deps_t1)],
                      by = c("county","YEAR"),
                      all.x = TRUE)

branch_panel <- merge(branch_panel,
                      county_deps_t3[, .(county, YEAR, total_county_deps_t3)],
                      by = c("county","YEAR"),
                      all.x = TRUE)

# Incumbent indicator at bank-county-year level
branch_panel <- merge(branch_panel,
                      bank_cty_yr[, .(RSSDID, county, YEAR, bank_type)],
                      by = c("RSSDID","county","YEAR"),
                      all.x = TRUE)
branch_panel[, incumbent_bank := fifelse(bank_type == "INCUMBENT", 1L, 0L)]

# Top 4 banks
top4_cert <- c(628L, 3510L, 3511L, 7213L)
branch_panel[, top4_bank := fifelse(CERT %in% top4_cert, 1L, 0L)]

#-----------------------------
# 7) NEW: Calculate Market Share Changes
#-----------------------------

# Only keep branches that existed at BOTH t-1 and t+3 (survivors)
branch_panel <- branch_panel[
  !is.na(dep_lag1_aligned) & dep_lag1_aligned > 0 &
    !is.na(dep_lead3_aligned) & dep_lead3_aligned > 0
]

# Market share at t-1 (among all branches that existed at t-1)
branch_panel[, mkt_share_t1 := fifelse(total_county_deps_t1 > 0,
                                       dep_lag1_aligned / total_county_deps_t1,
                                       NA_real_)]

# Market share at t+3 (among only survivors)
branch_panel[, mkt_share_t3 := fifelse(total_county_deps_t3 > 0,
                                       dep_lead3_aligned / total_county_deps_t3,
                                       NA_real_)]

# Change in market share (percentage points)
branch_panel[, delta_mkt_share := mkt_share_t3 - mkt_share_t1]

# Also calculate in basis points for easier interpretation
branch_panel[, delta_mkt_share_bps := delta_mkt_share * 10000]

# Winsorize
branch_panel[, delta_mkt_share_bps := Winsorize(delta_mkt_share_bps, 
                                                val = quantile(delta_mkt_share_bps, 
                                                               probs = c(0.025, 0.975), 
                                                               na.rm = TRUE))]

branch_panel[, delta_mkt_share_win := Winsorize(delta_mkt_share, 
                                                val = quantile(delta_mkt_share, 
                                                               probs = c(0.01, 0.99), 
                                                               na.rm = TRUE))]

#-----------------------------
# 8) Final regression sample
#-----------------------------
reg_dt <- branch_panel[
  YEAR >= start_year & YEAR <= end_year &
    banks_county_lag1 >= 3 &
    incumbent_bank == 1
]

#-----------------------------
# 9) Summary Statistics
#-----------------------------

cat("\n=== SUMMARY STATISTICS ===\n\n")

# Histogram of market share change
print(ggplot(reg_dt, aes(x = delta_mkt_share_bps)) + 
        geom_histogram(bins = 50) +
        labs(title = "Distribution of Market Share Change (basis points)",
             x = "Change in Market Share (bps)",
             y = "Count") +
        geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
        theme_minimal())

# Summary stats
vars_mktshare <- c(
  "gr_branch",
  "delta_mkt_share",
  "delta_mkt_share_bps",
  "mkt_share_t1",
  "mkt_share_t3",
  "share_deps_closed_app",
  "share_deps_closed_noapp",
  "share_deps_closed",
  "own_bank_has_app",
  "top4_bank",
  "banks_county_lag1",
  "county_dep_growth_t4_t1",
  "dep_lag1_aligned"
)

vars_mktshare <- intersect(vars_mktshare, names(reg_dt))

cat("\n--- Summary Stats: All Years ---\n")
temp <- summary_stats_function(reg_dt, vars_mktshare)
md_table(temp)

cat("\n--- Summary Stats: Counties with Closures Only ---\n")
temp <- summary_stats_function(reg_dt[share_deps_closed > 0], vars_mktshare)
md_table(temp)

cat("\n--- Summary Stats: 2019 Only ---\n")
temp <- summary_stats_function(reg_dt[YEAR == 2019], vars_mktshare)
md_table(temp)

# Check: average market share change by closure exposure
cat("\n--- Average Market Share Change by Closure Quartiles ---\n")
reg_dt[share_deps_closed > 0, closure_quartile := cut(share_deps_closed, 
                                                      breaks = quantile(share_deps_closed, 
                                                                        probs = c(0, 0.25, 0.5, 0.75, 1)),
                                                      labels = c("Q1", "Q2", "Q3", "Q4"),
                                                      include.lowest = TRUE)]

temp <- reg_dt[!is.na(closure_quartile), .(
  mean_delta_mkt_share_bps = mean(delta_mkt_share_bps, na.rm = TRUE),
  median_delta_mkt_share_bps = median(delta_mkt_share_bps, na.rm = TRUE),
  N = .N
), by = closure_quartile]
print(temp)

#-----------------------------
# 10) Regressions
#-----------------------------

setFixest_fml(
  ..fixef_branch = ~ UNINUMBR + state_yr + bank_yr,
  ..controls = ~ log1p(banks_county_lag1) +
    county_dep_growth_t4_t1 +
    log1p(deps_lag1)
)

cat("\n\n=== ORIGINAL REGRESSIONS (Growth Rate) ===\n\n")

r_original <- list()
r_original[[1]] <- feols(gr_branch ~ share_deps_closed + ..controls | ..fixef_branch,
                         data = reg_dt)

r_original[[2]] <- feols(gr_branch ~ share_deps_closed * top4_bank + ..controls | ..fixef_branch,
                         data = reg_dt)

r_original[[3]] <- feols(gr_branch ~ share_deps_closed_app + share_deps_closed_noapp + ..controls | ..fixef_branch,
                         data = reg_dt)

r_original[[4]] <- feols(gr_branch ~ share_deps_closed_app * own_bank_has_app +
                           share_deps_closed_noapp * own_bank_has_app + ..controls |
                           ..fixef_branch,
                         data = reg_dt)

r_original[[5]] <- feols(gr_branch ~ share_deps_closed_app * top4_bank +
                           share_deps_closed_noapp * top4_bank + ..controls |
                           ..fixef_branch,
                         data = reg_dt)

etable(r_original, headers = c("Total","+ Top 4 Interaction", "CB App/NoApp", "+ Own App", "+ Top4 Bank"))


cat("\n\n=== NEW REGRESSIONS (Market Share Change in Percentage Points) ===\n\n")

r_mktshare <- list()
r_mktshare[[1]] <- feols(delta_mkt_share_win ~ share_deps_closed + ..controls | ..fixef_branch,
                         data = reg_dt)

r_mktshare[[2]] <- feols(delta_mkt_share_win ~ share_deps_closed * top4_bank + ..controls | ..fixef_branch,
                         data = reg_dt)

r_mktshare[[3]] <- feols(delta_mkt_share_win ~ share_deps_closed_app + share_deps_closed_noapp + ..controls | ..fixef_branch,
                         data = reg_dt)

r_mktshare[[4]] <- feols(delta_mkt_share_win ~ share_deps_closed_app * own_bank_has_app +
                           share_deps_closed_noapp * own_bank_has_app + ..controls |
                           ..fixef_branch,
                         data = reg_dt)

r_mktshare[[5]] <- feols(delta_mkt_share_win ~ share_deps_closed_app * top4_bank +
                           share_deps_closed_noapp * top4_bank + ..controls |
                           ..fixef_branch,
                         data = reg_dt)

etable(r_mktshare, headers = c("Total","+ Top 4 Interaction", "CB App/NoApp", "+ Own App", "+ Top4 Bank"))


cat("\n\n=== NEW REGRESSIONS (Market Share Change in Basis Points) ===\n\n")

r_mktshare_bps <- list()
r_mktshare_bps[[1]] <- feols(delta_mkt_share_bps ~ share_deps_closed + ..controls | ..fixef_branch,
                             data = reg_dt)

r_mktshare_bps[[2]] <- feols(delta_mkt_share_bps ~ share_deps_closed * top4_bank + ..controls | ..fixef_branch,
                             data = reg_dt)

r_mktshare_bps[[3]] <- feols(delta_mkt_share_bps ~ share_deps_closed_app + share_deps_closed_noapp + ..controls | ..fixef_branch,
                             data = reg_dt)

r_mktshare_bps[[4]] <- feols(delta_mkt_share_bps ~ share_deps_closed_app * own_bank_has_app +
                               share_deps_closed_noapp * own_bank_has_app + ..controls |
                               ..fixef_branch,
                             data = reg_dt)

r_mktshare_bps[[5]] <- feols(delta_mkt_share_bps ~ share_deps_closed_app * top4_bank +
                               share_deps_closed_noapp * top4_bank + ..controls |
                               ..fixef_branch,
                             data = reg_dt)

etable(r_mktshare_bps, headers = c("Total","+ Top 4 Interaction", "CB App/NoApp", "+ Own App", "+ Top4 Bank"))


cat("\n\n=== INTERPRETATION GUIDE ===\n")
cat("
Market Share Change Interpretation:
- Positive coefficient = branch GAINS market share when closures occur
- Coefficient magnitude = percentage point (or bps) change in market share
  per 1 percentage point increase in closure share

Example (basis points version):
- Coefficient of 50 on share_deps_closed means:
  If 10% of county deposits close (share_deps_closed = 0.10),
  average survivor gains 5 basis points (0.10 × 50) of market share

Key Questions:
1. Do closures lead to market share gains? (positive coefficients)
2. Are gains larger from non-app vs app closures?
3. Do Top 4 banks gain more/less market share?
4. Does having your own app help you gain market share?

Note: This specification only includes branches that survive from t-1 to t+3,
so market shares are calculated among survivors only.
")