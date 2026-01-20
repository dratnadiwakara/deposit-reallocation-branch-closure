# =====================================================================
# File: create_branch_competition_panel.R
# Purpose: Construct branch-year dataset with local competitive dynamics
#          (own vs competitor openings/closures), 3y-forward deposit growth,
#          and competitor closure deposit share (t-1).
# =====================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)

# ---------------------------------------------------------------------
# 1. Load the base branch-year data
# ---------------------------------------------------------------------
closure_opening_data <- readRDS("C:/data/closure_opening_data_simple.rds")

# Keep relevant columns
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR,STCNTY,
      closed, new_branch)
]

# ---------------------------------------------------------------------
# 2. Compute branch-level 3-year forward deposit growth (t -> t+3)
# ---------------------------------------------------------------------
setorder(closure_opening_data, UNINUMBR, YEAR)

closure_opening_data[
  , `:=`(
    year_lead3 = shift(YEAR, 3L, type = "lead"),
    year_lead1 = shift(YEAR, 1L, type = "lead"),
    dep_lead3  = shift(DEPSUMBR, 3L, type = "lead"),
    dep_lead1  = shift(DEPSUMBR, 1L, type = "lead")
  ),
  by = UNINUMBR
]

closure_opening_data[
  ,
  dep_gr_3yr := fifelse(
    !is.na(dep_lead3) & !is.na(DEPSUMBR) & DEPSUMBR > 0 &
      year_lead3 == YEAR + 3L,
    (dep_lead3 - DEPSUMBR) / DEPSUMBR,
    NA_real_
  )
]


closure_opening_data[
  ,
  dep_gr_1yr := fifelse(
    !is.na(dep_lead1) & !is.na(DEPSUMBR) & DEPSUMBR > 0 &
      year_lead1 == YEAR + 1L,
    (dep_lead1 - DEPSUMBR) / DEPSUMBR,
    NA_real_
  )
]


# Winsorize dep_gr_3yr at 1st/99th percentiles
closure_opening_data[
  , dep_gr_3yr := Winsorize(dep_gr_3yr, val = quantile(dep_gr_3yr, probs = c(0.01, 0.99), na.rm = T))
]

closure_opening_data[
  , dep_gr_1yr := Winsorize(dep_gr_1yr, val = quantile(dep_gr_1yr, probs = c(0.01, 0.99), na.rm = T))
]

# ---------------------------------------------------------------------
# 3. Summarize branch openings and closures by bank-ZIP-year
# ---------------------------------------------------------------------
bank_zip_year_activity <- closure_opening_data[
  , .(
    own_closures_zip = sum(closed == 1L, na.rm = TRUE),
    own_openings_zip = sum(new_branch == 1L, na.rm = TRUE)
  ),
  by = .(RSSDID, ZIPBR, YEAR)
]


# ---------------------------------------------------------------------
# 4. Summarize total openings/closures by ZIP-year (all banks)
# ---------------------------------------------------------------------
zip_year_totals <- closure_opening_data[
  , .(
    total_closures_zip = sum(closed == 1L, na.rm = TRUE),
    total_openings_zip = sum(new_branch == 1L, na.rm = TRUE)
  ),
  by = .(ZIPBR, YEAR)
]




# ---------------------------------------------------------------------
# 5. Merge and compute competitor activity (counts)
# ---------------------------------------------------------------------
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  zip_year_totals,
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

bank_zip_year_activity[
  ,
  `:=`(
    competitor_closures_zip = total_closures_zip - own_closures_zip,
    competitor_openings_zip = total_openings_zip - own_openings_zip
  )
]

# ---------------------------------------------------------------------
# 5B. Competitor closure deposit share using prior-year deposits
#      competitor_close_dep_share_prev =
#      (sum of dep_{t-1} for competitor branches that close in t in ZIP) /
#      (total ZIP dep_{t-1})
# ---------------------------------------------------------------------
# Branch-level prior-year deposits
closure_opening_data[
  , dep_lag1 := shift(DEPSUMBR, 1L, type = "lag"),
  by = UNINUMBR
]

# ZIP-year total dep_{t-1} (sum of dep_lag1 across branches present at t)
zip_dep_tminus1 <- closure_opening_data[
  , .(zip_dep_tminus1 = sum(dep_lag1, na.rm = TRUE)),
  by = .(ZIPBR, YEAR)
]

# For branches that close at t: sum dep_{t-1} by bank-ZIP-year
close_dep_tminus1_by_bank <- closure_opening_data[
  closed == 1L,
  .(own_close_dep_tminus1 = sum(dep_lag1, na.rm = TRUE)),
  by = .(RSSDID, ZIPBR, YEAR)
]

# ZIP totals of dep_{t-1} associated with closures at t (all banks)
zip_close_dep_tminus1_total <- close_dep_tminus1_by_bank[
  , .(zip_close_dep_tminus1_total = sum(own_close_dep_tminus1, na.rm = TRUE)),
  by = .(ZIPBR, YEAR)
]

# Attach own and ZIP totals to bank_zip_year_activity
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  close_dep_tminus1_by_bank,
  by = c("RSSDID", "ZIPBR", "YEAR"),
  all.x = TRUE
)

bank_zip_year_activity[
  is.na(own_close_dep_tminus1), own_close_dep_tminus1 := 0
]

bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  zip_close_dep_tminus1_total,
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

bank_zip_year_activity[
  is.na(zip_close_dep_tminus1_total), zip_close_dep_tminus1_total := 0
]

# Competitor portion of dep_{t-1} tied to closures at t in ZIP
bank_zip_year_activity[
  , competitor_close_dep_tminus1 := pmax(
    zip_close_dep_tminus1_total - own_close_dep_tminus1, 0
  )
]

# Denominator: total ZIP dep_{t-1}
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  zip_dep_tminus1,
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

# Fraction
bank_zip_year_activity[
  ,
  competitor_close_dep_share_prev :=
    fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
            competitor_close_dep_tminus1 / zip_dep_tminus1,
            NA_real_)
]

# ---------------------------------------------------------------------
# 6. Attach variables back to each branch-year observation
# ---------------------------------------------------------------------
closure_opening_data <- merge(
  closure_opening_data,
  bank_zip_year_activity[
    , .(RSSDID, ZIPBR, YEAR,
        own_closures_zip, own_openings_zip,
        competitor_closures_zip, competitor_openings_zip,
        competitor_close_dep_share_prev)
  ],
  by = c("RSSDID", "ZIPBR", "YEAR"),
  all.x = TRUE
)



# ---------------------------------------------------------------------
# 7. Normalizations (use previous year's branches in the ZIP)
# ---------------------------------------------------------------------
zip_year_branch_counts <- closure_opening_data[
  , .(branches_zip = uniqueN(UNINUMBR)),
  by = .(ZIPBR, YEAR)
]

setorder(zip_year_branch_counts, ZIPBR, YEAR)
zip_year_branch_counts[
  , branches_zip_lag := shift(branches_zip, 1L, type = "lag"),
  by = ZIPBR
]

closure_opening_data <- merge(
  closure_opening_data,
  zip_year_branch_counts[, .(ZIPBR, YEAR, branches_zip_lag)],
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

closure_opening_data[
  ,
  `:=`(
    competitor_closures_rate = fifelse(!is.na(branches_zip_lag) & branches_zip_lag > 0,
                                       competitor_closures_zip / branches_zip_lag, NA_real_),
    competitor_openings_rate = fifelse(!is.na(branches_zip_lag) & branches_zip_lag > 0,
                                       competitor_openings_zip / branches_zip_lag, NA_real_)
  )
]


# ---------------------------------------------------------------------
# 8. Save the final dataset
# ---------------------------------------------------------------------
saveRDS(closure_opening_data,
        "C:/data/branch_competition_panel.rds")

# =====================================================================
# End of file
# =====================================================================

