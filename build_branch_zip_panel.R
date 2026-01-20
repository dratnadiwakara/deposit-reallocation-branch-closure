# =============================================================================
# File: build_branch_zip_panel.R
# Purpose: Build minimal branch-year panel with ZIP-level competitor closure
#          deposit share for branch_zip_regression.qmd
# =============================================================================

rm(list = ls())
library(data.table)
library(stringr)
library(readxl)
library(dplyr)

# --- Load and process app reviews -------------------------------------------
reviews <- fread("C:/OneDrive/data/CH_app_reviews_data.csv")
reviews <- reviews[, .(mean_app_rating = mean(rating, na.rm = TRUE)), 
                   by = .(FDIC_certificate_id, review_year)]
reviews[, app_rating_bin := ntile(mean_app_rating, 4), by = review_year]
setnames(reviews, c("FDIC_certificate_id", "review_year"), c("CERT", "YEAR"))

# --- Setup ------------------------------------------------------------------
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(sum(x, na.rm = TRUE))
}

panel_path <- "C:/OneDrive/data/closure_opening_data_simple.rds"
output_path <- "C:/OneDrive/data/branch_zip_panel.rds"


# --- Load and prepare branch data -------------------------------------------
closure_raw <- readRDS(panel_path)
setDT(closure_raw)

# Keep only necessary columns
closure_raw <- closure_raw[, .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR, STCNTYBR,
                               closed, new_branch, CERT)]

closure_raw[, county := str_pad(STCNTYBR, 5, "left", "0")]
closure_raw[, zip := str_pad(ZIPBR, 5, "left", "0")]

# Merge app ratings onto closure data. Banks not in reviews get app_rating_bin = 0.
closure_raw <- merge(closure_raw, reviews[, .(CERT, YEAR, mean_app_rating, app_rating_bin)],
                     by = c("CERT", "YEAR"), all.x = TRUE)
closure_raw[is.na(app_rating_bin), app_rating_bin := 0L]
closure_raw[is.na(mean_app_rating), mean_app_rating := -1]


# --- Branch-level lag/lead for deposits and growth -------------------------
setorder(closure_raw, UNINUMBR, YEAR)

closure_raw[, `:=`(
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  dep_lead3 = shift(DEPSUMBR, 3L, type = "lead"),
  dep_lag1 = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1 = shift(YEAR, 1L, type = "lag")
), by = UNINUMBR]

# Aligned deposits (ensure consecutive years)
closure_raw[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lag1_aligned = fifelse(year_lag1 == YEAR - 1L, dep_lag1, NA_real_)
)]

# Branch-level 3-year growth (only variable used in regressions)
closure_raw[, dep_gr_3yr := fifelse(!is.na(dep_lead3_aligned) & DEPSUMBR > 0,
                                    (dep_lead3_aligned - DEPSUMBR) / DEPSUMBR,
                                    NA_real_)]


# --- Aggregate own bank activity by ZIP-year --------------------------------
# Count own closures/openings (needed for sample filtering only)
bank_zip_year <- closure_raw[, .(
  own_closures_zip = sum(closed == 1L, na.rm = TRUE),
  own_openings_zip = sum(new_branch == 1L, na.rm = TRUE)
), by = .(RSSDID, zip, YEAR)]


# --- Competitor closure deposit share (using t-1 deposits) -----------------
# Split by app rating bin (0, 1, 2, 3, 4)

# ZIP total dep_{t-1}
zip_dep_tminus1 <- closure_raw[, .(
  zip_dep_tminus1 = sum_na_safe(dep_lag1_aligned)
), by = .(zip, YEAR)]

# Own bank's closed branch deposits (t-1) by app rating bin
own_close_dep_by_bin <- closure_raw[closed == 1L, .(
  own_close_bin0 = sum_na_safe(fifelse(app_rating_bin == 0L, dep_lag1_aligned, NA_real_)),
  own_close_bin1 = sum_na_safe(fifelse(app_rating_bin == 1L, dep_lag1_aligned, NA_real_)),
  own_close_bin2 = sum_na_safe(fifelse(app_rating_bin == 2L, dep_lag1_aligned, NA_real_)),
  own_close_bin3 = sum_na_safe(fifelse(app_rating_bin == 3L, dep_lag1_aligned, NA_real_)),
  own_close_bin4 = sum_na_safe(fifelse(app_rating_bin == 4L, dep_lag1_aligned, NA_real_))
), by = .(RSSDID, zip, YEAR)]

# Total closed deposits in ZIP (all banks) by app rating bin
zip_close_dep_by_bin <- closure_raw[closed == 1L, .(
  zip_close_bin0 = sum_na_safe(fifelse(app_rating_bin == 0L, dep_lag1_aligned, NA_real_)),
  zip_close_bin1 = sum_na_safe(fifelse(app_rating_bin == 1L, dep_lag1_aligned, NA_real_)),
  zip_close_bin2 = sum_na_safe(fifelse(app_rating_bin == 2L, dep_lag1_aligned, NA_real_)),
  zip_close_bin3 = sum_na_safe(fifelse(app_rating_bin == 3L, dep_lag1_aligned, NA_real_)),
  zip_close_bin4 = sum_na_safe(fifelse(app_rating_bin == 4L, dep_lag1_aligned, NA_real_))
), by = .(zip, YEAR)]

# Merge into bank_zip_year
bank_zip_year <- merge(bank_zip_year, own_close_dep_by_bin,
                       by = c("RSSDID", "zip", "YEAR"), all.x = TRUE)

# Fill NAs with 0 for own closures
for (col in c("own_close_bin0", "own_close_bin1", "own_close_bin2", "own_close_bin3", "own_close_bin4")) {
  set(bank_zip_year, i = which(is.na(bank_zip_year[[col]])), j = col, value = 0)
}

bank_zip_year <- merge(bank_zip_year, zip_close_dep_by_bin,
                       by = c("zip", "YEAR"), all.x = TRUE)

# Fill NAs with 0 for ZIP closures
for (col in c("zip_close_bin0", "zip_close_bin1", "zip_close_bin2", "zip_close_bin3", "zip_close_bin4")) {
  set(bank_zip_year, i = which(is.na(bank_zip_year[[col]])), j = col, value = 0)
}

# Compute competitor closed deposits by bin (ZIP total - own)
bank_zip_year[, `:=`(
  competitor_close_bin0 = pmax(zip_close_bin0 - own_close_bin0, 0),
  competitor_close_bin1 = pmax(zip_close_bin1 - own_close_bin1, 0),
  competitor_close_bin2 = pmax(zip_close_bin2 - own_close_bin2, 0),
  competitor_close_bin3 = pmax(zip_close_bin3 - own_close_bin3, 0),
  competitor_close_bin4 = pmax(zip_close_bin4 - own_close_bin4, 0)
)]

bank_zip_year <- merge(bank_zip_year, zip_dep_tminus1,
                       by = c("zip", "YEAR"), all.x = TRUE)

# Competitor closure deposit shares by app rating bin
bank_zip_year[, `:=`(
  competitor_close_dep_share_bin0 = fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
                                            competitor_close_bin0 / zip_dep_tminus1, 0),
  competitor_close_dep_share_bin1 = fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
                                            competitor_close_bin1 / zip_dep_tminus1, 0),
  competitor_close_dep_share_bin2 = fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
                                            competitor_close_bin2 / zip_dep_tminus1, 0),
  competitor_close_dep_share_bin3 = fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
                                            competitor_close_bin3 / zip_dep_tminus1, 0),
  competitor_close_dep_share_bin4 = fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
                                            competitor_close_bin4 / zip_dep_tminus1, 0)
)]

# Total competitor closure share (sum of all bins)
bank_zip_year[, competitor_close_dep_share_prev := 
  competitor_close_dep_share_bin0 + 
  competitor_close_dep_share_bin1 + 
  competitor_close_dep_share_bin2 + 
  competitor_close_dep_share_bin3 + 
  competitor_close_dep_share_bin4]


# --- Merge back to branch level ---------------------------------------------
closure_raw <- merge(
  closure_raw,
  bank_zip_year[, .(RSSDID, zip, YEAR, own_closures_zip, own_openings_zip,
                    competitor_close_dep_share_prev,
                    competitor_close_dep_share_bin0,
                    competitor_close_dep_share_bin1,
                    competitor_close_dep_share_bin2,
                    competitor_close_dep_share_bin3,
                    competitor_close_dep_share_bin4)],
  by = c("RSSDID", "zip", "YEAR"),
  all.x = TRUE
)


# --- Create fixed effect IDs ------------------------------------------------
closure_raw[, county_yr := paste0(county, "_", YEAR)]
closure_raw[, bank_yr := paste0(RSSDID, "_", YEAR)]


# --- Add ZIP demographics ---------------------------------------------------
# Load pre-computed ZIP demographics (ACS + IRS data)
# See archive/results_competitor_close_open_v4.0.qmd lines 80-187 for full logic

# Load ACS data (tract level)
acs_data <- readRDS("C:/OneDrive/data/acs_data_2010_2022_tract.rds")

# Load tract-ZIP crosswalk
tract_zip_crosswalk <- read_excel('C:/OneDrive/data/ZIP_TRACT_122019.xlsx')
tract_zip_crosswalk <- data.table(tract_zip_crosswalk)
tract_zip_crosswalk[, zip := str_pad(ZIP, 5, "left", "0")]
tract_zip_crosswalk[, tract := as.character(TRACT)]
tract_zip_crosswalk[, tract := str_pad(tract, 11, "left", "0")]
tract_zip_crosswalk[, tot_ratio := TOT_RATIO]

# Aggregate ACS to ZIP level (weighted by tract-ZIP overlap)
acs_with_zip <- acs_data %>%
  left_join(tract_zip_crosswalk[, c("zip", "tract", "tot_ratio")], by = c("tract" = "tract"))

zip_aggregated_data <- acs_with_zip %>%
  group_by(zip, yr) %>%
  summarize(
    median_income = sum(median_income * tot_ratio, na.rm = TRUE),
    pct_college_educated = sum(pct_college_educated * tot_ratio, na.rm = TRUE),
    median_age = sum(median_age * tot_ratio, na.rm = TRUE),
    .groups = 'drop'
  )

zip_aggregated_data <- data.table(zip_aggregated_data)

# Load IRS data
irs_data <- readRDS("C:/OneDrive/data/irs_data_2010_2022_zip.rds")
irs_data <- irs_data[, c("zipcode", "yr", "dividend_frac", "capital_gain_frac")]
irs_data[, zipcode := str_pad(zipcode, 5, "left", "0")]

# Merge ACS and IRS
zip_demo_data <- merge(zip_aggregated_data, irs_data, 
                       by.x = c("zip", "yr"), 
                       by.y = c("zipcode", "yr"), 
                       all.x = TRUE)

# Create characteristic quintiles/bins
key_chars <- c("median_income", "median_age", "pct_college_educated", 
               "capital_gain_frac", "dividend_frac")

zip_demo_data[, paste0(key_chars, "_q") := lapply(.SD, function(x) ntile(x, 2)), 
              by = yr, .SDcols = key_chars]

# Sophisticated flag: high education + high dividend/capital gains
zip_demo_data[, sophisticated_ed_sm := ifelse(
  !is.na(dividend_frac) & pct_college_educated_q == 2 & 
    (dividend_frac_q == 2 | capital_gain_frac_q == 2), 1,
  ifelse(!is.na(dividend_frac), 0, NA)
)]

# Above median flags
zip_demo_data[, above_median_income := fifelse(median_income_q == 2, 1L, 0L)]
zip_demo_data[, above_median_age := fifelse(median_age_q == 2, 1L, 0L)]

# Keep only needed columns
zip_demo_data <- zip_demo_data[, .(zip, yr, sophisticated_ed_sm, 
                                   above_median_income, above_median_age,
                                   median_income, median_age)]

# Fill missing years by copying nearby years
temp1 <- zip_demo_data[yr == 2010]
for(y in 2000:2009) {
  temp <- copy(temp1)
  temp[, yr := y]
  zip_demo_data <- rbind(zip_demo_data, temp)
}

temp1 <- zip_demo_data[yr == 2022]
for(y in 2023:2024) {
  temp <- copy(temp1)
  temp[, yr := y]
  zip_demo_data <- rbind(zip_demo_data, temp)
}

temp1 <- zip_demo_data[yr == 2010]
for(y in 2011:2012) {
  temp <- copy(temp1)
  temp[, yr := y]
  zip_demo_data <- rbind(zip_demo_data, temp)
}

temp1 <- zip_demo_data[yr == 2013]
for(y in 2014:2015) {
  temp <- copy(temp1)
  temp[, yr := y]
  zip_demo_data <- rbind(zip_demo_data, temp)
}

temp1 <- zip_demo_data[yr == 2019]
for(y in 2020:2021) {
  temp <- copy(temp1)
  temp[, yr := y]
  zip_demo_data <- rbind(zip_demo_data, temp)
}

# Merge demographics to branch panel
closure_raw <- merge(
  closure_raw,
  zip_demo_data[!is.na(zip), .(zip, yr, sophisticated_ed_sm, above_median_income, 
                                above_median_age, median_income, median_age)],
  by.x = c("zip", "YEAR"),
  by.y = c("zip", "yr"),
  all.x = TRUE
)


# --- Keep only valid 3-year growth observations -----------------------------
# Filter to observations with demographics and valid growth
branch_panel <- closure_raw[!is.na(dep_gr_3yr) & !is.na(sophisticated_ed_sm)]

# Convert sophisticated to integer
branch_panel[, sophisticated := as.integer(sophisticated_ed_sm == 1L)]
branch_panel[, sophisticated_ed_sm := NULL]

# --- Save -------------------------------------------------------------------
saveRDS(branch_panel, output_path)

message("Branch ZIP panel saved to: ", output_path)
message("Observations: ", nrow(branch_panel))
message("Unique branches: ", uniqueN(branch_panel$UNINUMBR))
message("Years: ", min(branch_panel$YEAR), " to ", max(branch_panel$YEAR))
