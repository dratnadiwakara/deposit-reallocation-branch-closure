# =============================================================================
# File: build_minimal_county_panel.R
# =============================================================================
rm(list=ls())
library(data.table)
library(stringr)


# --- Setup ------------------------------------------------------------------
# Utility: safe sum that returns NA if every value is missing, otherwise sums non-missing entries.
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(sum(x, na.rm = TRUE))
}

# Source paths match the originals so you can run this as a drop-in simplification.
panel_path <- "C:/OneDrive/data/closure_opening_data_simple.rds"
demo_path <- "C:/OneDrive/data/county_demo_data.rds"
fdic_path <- "C:/OneDrive/data/fdic_sod_2000_2025_simple.rds"
output_path <- "C:/OneDrive/data/county_coexistence_panel_slim.rds"


closure_raw <- readRDS(panel_path)
setDT(closure_raw)
closure_raw[, county := str_pad(STCNTYBR, 5, "left", "0")]

# Remove RSSDID-year combinations with no records in the following year.
# We keep duplicates within a year, but drop every row for a bank-year
# if the same bank has no data in YEAR+1 so that fully exiting banks are removed.
year_presence <- unique(closure_raw[, .(RSSDID, YEAR)])
setorder(year_presence, RSSDID, YEAR)
year_presence[, has_next_year := shift(YEAR, type = "lead") == YEAR + 1L, by = RSSDID]
closure_raw <- closure_raw[year_presence, on = .(RSSDID, YEAR), nomatch = 0][has_next_year == TRUE]
closure_raw[, has_next_year := NULL]

# Create aligned lag/lead deposits (needed for growth and shares)
closure_raw[, `:=`(
  dep_lead1 = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  dep_lead3 = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  dep_lag1 = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1 = shift(YEAR, 1L, type = "lag")
), by = UNINUMBR]
closure_raw[, `:=`(
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_),
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lag1_aligned = fifelse(year_lag1 == YEAR - 1L, dep_lag1, NA_real_)
)]


# Aggregate each bank-county-year
bank_year_summary <- closure_raw[, .(
  deps_curr = sum_na_safe(DEPSUMBR),
  deps_lag1 = sum_na_safe(dep_lag1_aligned),
  deps_lead1 = sum_na_safe(dep_lead1_aligned),
  deps_lead3 = sum_na_safe(dep_lead3_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_surviving = sum(closed == 0L, na.rm = TRUE)
), by = .(RSSDID, county, YEAR)]

# Label banks as closers vs incumbents based on branch closures.
bank_year_summary[, bank_type := fifelse(n_closed > 0L, "CLOSER", "INCUMBENT")]

# Summaries at county-year level
county_market_summary <- bank_year_summary[, .(
  branches_county_curr = .N,
  total_deps_county_lag1 = sum_na_safe(deps_lag1)
), by = .(county, YEAR)]
setorder(county_market_summary, county, YEAR)
county_market_summary[, branches_county_lag1 := shift(branches_county_curr, 1L, type = "lag"), by = county]

# Incumbent aggregates
incumbent_only <- bank_year_summary[bank_type == "INCUMBENT"]
incumbent_deposits <- incumbent_only[, .(
  inc_deps_curr = sum_na_safe(deps_curr),
  inc_deps_lag1 = sum_na_safe(deps_lag1),
  inc_deps_lead3 = sum_na_safe(deps_lead3)
), by = .(county, YEAR)]

# Closed deposit aggregates (all closures regardless of size)
closer_only <- bank_year_summary[bank_type == "CLOSER"]

# Only sum deposit lag amounts for branches that actually closed.
closed_branches <- closure_raw[closed == 1L]
closed_branches <- closed_branches[closer_only, on = .(RSSDID, county, YEAR), nomatch = 0]
closed_deposit_totals <- closed_branches[, .(
  cl_closed_lag1 = sum_na_safe(dep_lag1_aligned)
), by = .(county, YEAR)]


# --- Merge panels and compute target variables -----------------------------
# Merge summaries together and compute the growth/shock ratios we need downstream.
coexistence_panel <- merge(county_market_summary, incumbent_deposits, by = c("county", "YEAR"), all.x = TRUE)
coexistence_panel <- merge(coexistence_panel, closed_deposit_totals, by = c("county", "YEAR"), all.x = TRUE)

cols_to_zero <- c(
  "cl_closed_lag1","inc_deps_curr","inc_deps_lag1","inc_deps_lead3"
) 
for(j in cols_to_zero){
  if (j %in% names(coexistence_panel)) {
    set(coexistence_panel, i = which(is.na(coexistence_panel[[j]])), j = j, value = 0)
  }
}

coexistence_panel <- coexistence_panel[total_deps_county_lag1 > 0]
coexistence_panel[, gr_3y_cd_all := (inc_deps_lead3 - inc_deps_curr) / total_deps_county_lag1]
coexistence_panel[, share_deps_closed_all := cl_closed_lag1 / total_deps_county_lag1]
coexistence_panel[, incumbent_mkt_share_lag1_all := inc_deps_lag1 / total_deps_county_lag1]

# --- Market controls from FDIC totals ---------------------------------------
fdic_all <- readRDS(fdic_path)
setDT(fdic_all)

fdic_all[, county := str_pad(STCNTYBR, 5, "left", "0")]
market_totals <- fdic_all[, .(total_deps = sum(DEPSUMBR, na.rm = TRUE)), by = .(county, YEAR)]

complete_grid <- CJ(county = unique(market_totals$county), YEAR = min(market_totals$YEAR):max(market_totals$YEAR))
market_balanced <- merge(complete_grid, market_totals, by = c("county", "YEAR"), all.x = TRUE)
market_balanced[is.na(total_deps), total_deps := 0]
setorder(market_balanced, county, YEAR)
market_balanced[, deps_lag1 := shift(total_deps, 1L, type = "lag"), by = county]
market_balanced[, deps_lag4 := shift(total_deps, 4L, type = "lag"), by = county]
market_balanced[, county_dep_growth_t4_t1 := fifelse(deps_lag4 > 0,
                                                      (deps_lag1 - deps_lag4) / deps_lag4,
                                                      NA_real_)]

# Merge the computed market controls for the panel.
coexistence_panel <- merge(coexistence_panel,
                           market_balanced[, .(county, YEAR, deps_lag1, county_dep_growth_t4_t1)],
                           by = c("county", "YEAR"),
                           all.x = TRUE)

# --- Demographics -----------------------------------------------------------
# Add the demographic flags needed for the regression interactions.
demo <- readRDS(demo_path)
setDT(demo)
demo <- unique(demo[!is.na(county_fips), .(county_fips, yr, sophisticated, above_median_age, above_median_income)])
setnames(demo, c("county_fips", "yr"), c("county", "YEAR"))
coexistence_panel <- merge(coexistence_panel, demo, by = c("county", "YEAR"), all.x = TRUE)

# Keep only rows with demographic flags (matching the downstream QMD expectations).
coexistence_panel <- coexistence_panel[!is.na(sophisticated)]

# --- Final trimming ---------------------------------------------------------
coexistence_panel[, state_yr := paste0(substr(county, 1, 2), YEAR)]

# Final column set matches the regression chunk that consumes this file.
keep_cols <- c(
  "county", "YEAR", "state_yr",
  "sophisticated", "above_median_age", "above_median_income",
  "gr_3y_cd_all", "share_deps_closed_all", "incumbent_mkt_share_lag1_all",
  "branches_county_lag1", "county_dep_growth_t4_t1", "deps_lag1"
)

minimal_coexistence_panel <- coexistence_panel[, intersect(keep_cols, names(coexistence_panel)), with = FALSE]

# --- Save trimmed panel -----------------------------------------------------
saveRDS(minimal_coexistence_panel, output_path)
