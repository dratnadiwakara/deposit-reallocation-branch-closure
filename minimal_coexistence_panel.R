# =============================================================================
# File: minimal_coexistence_panel.R
# Purpose: Build a trimmed version of the Coexistence panel that only keeps
#          the variables required by the regression block that starts in
#          `county_year_incumbents_01202026_v1.qmd` (the chunk beginning on
#          line 244 of that document).
# =============================================================================

library(data.table)
library(stringr)

# Paths mirror the locations referenced in the QMD and original script.
panel_path <- "C:/data/county_coexistence_panel_Dec19_v6.rds"
demo_path <- "C:/OneDrive/data/county_demo_data.rds"
fdic_path <- "C:/OneDrive/data/fdic_sod_2000_2025_simple.rds"
output_path <- "C:/data/county_coexistence_panel_minimal.rds"

# Read the full panel produced by `create_county_competition_panel_COEXISTENCE_v6.R`
panel <- readRDS(panel_path)
setDT(panel)

# Create the shorter name used in the QMD chunk
panel[, gr_3y_cd_all := incumbent_dep_gr_3yr_cd_all]

# Merge the county demographic data so the `..interactions_all` formula can
# resolve `sophisticated`, `above_median_age`, and `above_median_income`.
demo <- readRDS(demo_path)
setDT(demo)
demo <- unique(demo[!is.na(county_fips),
                    .(county_fips, yr, sophisticated, above_median_age, above_median_income)])
setnames(demo, c("county_fips", "yr"), c("county", "YEAR"))
panel <- merge(panel, demo, by = c("county", "YEAR"), all.x = TRUE)

# Drop records that still miss the demographic flags (matching the QMD).
panel <- panel[!is.na(sophisticated)]

# Build `state_yr`, which is used in the fixed-effects formula.
panel[, state_yr := paste0(substr(county, 1, 2), YEAR)]

# Reconstruct the county-level market controls that appear in `..controls`.
fdic <- readRDS(fdic_path)
setDT(fdic)
fdic[, county := str_pad(STCNTYBR, 5, "left", "0")]
county_totals <- fdic[, .(total_deps = sum(DEPSUMBR, na.rm = TRUE)), by = .(county, YEAR)]

all_counties <- unique(county_totals$county)
year_range <- min(county_totals$YEAR):max(county_totals$YEAR)
full_grid <- CJ(county = all_counties, YEAR = year_range)
county_balanced <- merge(full_grid, county_totals, by = c("county", "YEAR"), all.x = TRUE)
county_balanced[is.na(total_deps), total_deps := 0]
setorder(county_balanced, county, YEAR)

county_balanced[, deps_lag1 := shift(total_deps, 1L, type = "lag"), by = county]
county_balanced[, deps_lag4 := shift(total_deps, 4L, type = "lag"), by = county]
county_balanced[, county_dep_growth_t4_t1 := fifelse(deps_lag4 > 0,
                                                   (deps_lag1 - deps_lag4) / deps_lag4,
                                                   NA_real_)]

# Join the market controls back onto the panel.
panel <- merge(panel,
               county_balanced[, .(county, YEAR, deps_lag1, county_dep_growth_t4_t1)],
               by = c("county", "YEAR"),
               all.x = TRUE)

# Keep only the columns referenced in the regression chunk.
keep_cols <- c(
  "county", "YEAR", "state_yr", "sophisticated", "above_median_age",
  "above_median_income", "gr_3y_cd_all", "share_deps_closed_all",
  "incumbent_mkt_share_lag1_all", "branches_county_lag1",
  "county_dep_growth_t4_t1", "deps_lag1"
)

minimal_panel <- panel[, intersect(keep_cols, names(panel)), with = FALSE]

# Save the trimmed dataset for downstream analysis.
saveRDS(minimal_panel, output_path)
