# =============================================================================
# File: build_incumbent_branch_panel.R
# Purpose: Build the branch-year panel that mirrors the minimal county-year
#          panel produced by `build_minimal_county_panel.R`, keeping every
#          branch (`UNINUMBR`) with flags for incumbent vs closer status.
# =============================================================================
rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)

reviews <- fread("C:/OneDrive/data/CH_app_reviews_data.csv")
reviews <- reviews[,.(mean_app_rating=mean(rating,na.rm=T)),by=.(FDIC_certificate_id,review_year)]
reviews[,app_rating_bin:=ntile(mean_app_rating,4),by=review_year]
setnames(reviews, c("FDIC_certificate_id", "review_year"), c("CERT", "YEAR"))

# --- Setup ------------------------------------------------------------------
panel_path <- "C:/OneDrive/data/closure_opening_data_simple.rds"
output_path <- "C:/OneDrive/data/deposit_reallocatoin_branch_panel.rds"
county_panel_path <- "C:/OneDrive/data/county_coexistence_panel_slim.rds"


# --- Load closure data and normalize county codes ---------------------------
closure_raw <- readRDS(panel_path)
setDT(closure_raw)
closure_raw[, county := str_pad(STCNTYBR, 5, "left", "0")]

# Merge app ratings onto closure data. Banks not in reviews get app_rating_bin = 0.
closure_raw <- merge(closure_raw, reviews[, .(CERT, YEAR, mean_app_rating, app_rating_bin)],
                     by = c("CERT", "YEAR"), all.x = TRUE)
closure_raw[is.na(app_rating_bin), app_rating_bin := 0L]
closure_raw[is.na(mean_app_rating), mean_app_rating := -1]

# Remove RSSDID-year combinations that have no observation in the following year.
year_presence <- unique(closure_raw[, .(RSSDID, YEAR)])
setorder(year_presence, RSSDID, YEAR)
year_presence[, has_next_year := shift(YEAR, type = "lead") == YEAR + 1L, by = RSSDID]
closure_raw <- closure_raw[year_presence, on = .(RSSDID, YEAR), nomatch = 0][has_next_year == TRUE]
closure_raw[, has_next_year := NULL]


# --- Branch-level lag/lead alignment ---------------------------------------
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


# --- Identify incumbent vs closer banks (same criteria used in county panel) -
bank_county_year_summary <- closure_raw[, .(
  deps_curr = sum(DEPSUMBR, na.rm = TRUE),
  deps_lag1 = sum(dep_lag1_aligned, na.rm = TRUE),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_surviving = sum(closed == 0L, na.rm = TRUE)
), by = .(RSSDID, county, YEAR)]

# A bank is a CLOSER in a county-year if it closed at least one branch there.
bank_county_year_summary[, bank_type := fifelse(n_closed > 0L, "CLOSER", "INCUMBENT")]

# Flag: does the closer have any remaining (surviving) branches in the county?
bank_county_year_summary[, closer_has_remaining := fifelse(bank_type == "CLOSER" & n_surviving > 0L, 1L, 0L)]


# --- Calculate county-level closure shares by app rating bin ---------------
# Need to compute closed deposits split by app_rating_bin (0, 1, 2, 3, 4)
# and total deposits for denominators.

# Helper function for safe sum
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(sum(x, na.rm = TRUE))
}

# Get total market deposits at county-year level (lag1 basis for denominator)
county_totals <- closure_raw[, .(
  total_deps_county_lag1 = sum_na_safe(dep_lag1_aligned)
), by = .(county, YEAR)]

# Identify closed branches and their app_rating_bin
closed_branches_all <- closure_raw[closed == 1L]

# For each app_rating_bin (0, 1, 2, 3, 4), sum closed deposits at county-year level
closed_by_rating <- closed_branches_all[, .(
  cl_closed_bin0 = sum_na_safe(fifelse(app_rating_bin == 0L, dep_lag1_aligned, NA_real_)),
  cl_closed_bin1 = sum_na_safe(fifelse(app_rating_bin == 1L, dep_lag1_aligned, NA_real_)),
  cl_closed_bin2 = sum_na_safe(fifelse(app_rating_bin == 2L, dep_lag1_aligned, NA_real_)),
  cl_closed_bin3 = sum_na_safe(fifelse(app_rating_bin == 3L, dep_lag1_aligned, NA_real_)),
  cl_closed_bin4 = sum_na_safe(fifelse(app_rating_bin == 4L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR)]

# Merge totals and compute shares
county_closure_shares <- merge(county_totals, closed_by_rating, by = c("county", "YEAR"), all.x = TRUE)

# Fill NAs with 0
for (col in c("cl_closed_bin0", "cl_closed_bin1", "cl_closed_bin2", "cl_closed_bin3", "cl_closed_bin4")) {
  set(county_closure_shares, i = which(is.na(county_closure_shares[[col]])), j = col, value = 0)
}

# Compute share variables
county_closure_shares[, `:=`(
  share_deps_closed_bin0 = fifelse(total_deps_county_lag1 > 0, cl_closed_bin0 / total_deps_county_lag1, 0),
  share_deps_closed_bin1 = fifelse(total_deps_county_lag1 > 0, cl_closed_bin1 / total_deps_county_lag1, 0),
  share_deps_closed_bin2 = fifelse(total_deps_county_lag1 > 0, cl_closed_bin2 / total_deps_county_lag1, 0),
  share_deps_closed_bin3 = fifelse(total_deps_county_lag1 > 0, cl_closed_bin3 / total_deps_county_lag1, 0),
  share_deps_closed_bin4 = fifelse(total_deps_county_lag1 > 0, cl_closed_bin4 / total_deps_county_lag1, 0)
)]


# --- Keep ALL branches (no filter) and add flags ----------------------------
branch_panel <- closure_raw

# Merge bank-county-year flags onto each branch row.
branch_panel <- merge(branch_panel,
                      bank_county_year_summary[, .(RSSDID, county, YEAR, bank_type, closer_has_remaining)],
                      by = c("RSSDID", "county", "YEAR"),
                      all.x = TRUE)

# Create incumbent_bank indicator (1 if the bank is an incumbent in that county-year).
branch_panel[, incumbent_bank := fifelse(bank_type == "INCUMBENT", 1L, 0L)]


# --- Compute branch-level growth indicators --------------------------------
# 3-year growth measured relative to the previous-year deposit level for the same branch.
branch_panel[, gr_branch := fifelse(!is.na(dep_lag1_aligned) & dep_lag1_aligned > 0,
                                   (dep_lead3_aligned - dep_lag1_aligned) / dep_lag1_aligned,
                                   NA_real_)]

branch_panel[, state_yr := paste0(substr(county, 1, 2), YEAR)]

branch_panel <- branch_panel[!is.na(gr_branch)]


# --- Final dataset ----------------------------------------------------------
keep_cols <- c(
  "RSSDID", "UNINUMBR", "YEAR", "county", "state_yr", "CERT",
  "DEPSUMBR", "dep_lag1_aligned", "dep_lead3_aligned", "gr_branch", "closed",
  "incumbent_bank", "closer_has_remaining", "app_rating_bin", "mean_app_rating"
)

branch_panel_trimmed <- branch_panel[, intersect(keep_cols, names(branch_panel)), with = FALSE]

# Merge the 5 closure share variables by app rating bin
branch_panel_trimmed <- merge(branch_panel_trimmed,
                              county_closure_shares[, .(county, YEAR, 
                                                        share_deps_closed_bin0,
                                                        share_deps_closed_bin1,
                                                        share_deps_closed_bin2,
                                                        share_deps_closed_bin3,
                                                        share_deps_closed_bin4)],
                              by = c("county", "YEAR"),
                              all.x = TRUE)

# Attach county-level controls that match `build_minimal_county_panel.R`.
if (file.exists(county_panel_path)) {
  county_panel <- readRDS(county_panel_path)
  setDT(county_panel)
  county_sel <- county_panel[, .(
    county, YEAR, sophisticated, above_median_age, above_median_income,
    share_deps_closed_all, incumbent_mkt_share_lag1_all,
    branches_county_lag1, county_dep_growth_t4_t1, deps_lag1
  )]

  branch_panel_trimmed <- merge(branch_panel_trimmed, county_sel,
                               by = c("county", "YEAR"),
                               all.x = TRUE)
}

saveRDS(branch_panel_trimmed, output_path)
