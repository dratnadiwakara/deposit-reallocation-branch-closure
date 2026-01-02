# =============================================================================
# File: create_county_competition_panel_COEXISTENCE_v5.R
# Purpose: Analyze impact of Closures on Incumbents (Large, Small, All).
#          Updates V5:
#          - Added 'share_deps_closed_sm' for horse-race regressions.
#          - Added 'Exit' vs 'Consolidation' split for Large Closers (Charlotte's request).
#          - Added variants of market share controls for All, Lg, Sm.
# =============================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)
library(zoo) 
library(stringr)

# --- Helper Function for NA-safe Summing ---
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  return(as.numeric(sum(x, na.rm = TRUE))) 
}

# --- Define Paths ---
data_path   <- "C:/data/closure_opening_data_simple.rds"
output_path <- "C:/data/county_coexistence_panel_Dec19_v6.rds"


# ---------------------------------------------------------------------
# 1. Load Banking Data & Define 'Large' vs 'Small'
# ---------------------------------------------------------------------
sod <- readRDS("C:/data/fdic_sod_2000_2025_simple.rds")

# Identify Small Banks: Assets < $10 Billion in 2019
sod_small_def <- unique(sod[YEAR == 2019 & ASSET < 10000000, .(RSSDID)])
sod_large_def <- unique(sod[YEAR == 2019 & ASSET >= 10000000, .(RSSDID)])

closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)
closure_opening_data[, county := str_pad(STCNTYBR, 5, "left", "0")]

# Flag Small vs Large
closure_opening_data[, is_small := ifelse(RSSDID %in% sod_small_def$RSSDID, 1, 0)]
closure_opening_data[, is_large := ifelse(RSSDID %in% sod_large_def$RSSDID, 1, 0)]

# Keep essential columns
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, county, YEAR, DEPSUMBR, closed, new_branch, CERT, is_large)
]

setorder(closure_opening_data, UNINUMBR, YEAR)

# Create Shifts
closure_opening_data[, `:=`(
  dep_lead3   = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3  = shift(YEAR, 3L, type = "lead"),
  dep_lead1   = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1  = shift(YEAR, 1L, type = "lead"),
  dep_lag1    = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1   = shift(YEAR, 1L, type = "lag")
), by = UNINUMBR]

# Align Shifts
closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_),
  dep_lag1_aligned  = fifelse(year_lag1 == YEAR - 1L, dep_lag1, NA_real_)
)]


# ---------------------------------------------------------------------
# 2. Aggregation to BANK-COUNTY Level & EXIT Identification
# ---------------------------------------------------------------------
bank_county_panel <- closure_opening_data[, .(
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  # Count surviving branches (Not closed, Not new opening)
  n_surviving = sum(closed == 0L, na.rm = TRUE), 
  n_branches = .N
), by = .(RSSDID, county, YEAR, is_large)]

bank_county_panel[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]

# --- Identify Market Exits (Charlotte's Request) ---
# If a bank closed branches (n_closed > 0) AND has 0 surviving branches, it Exited.
bank_county_panel[, is_exit := fifelse(bank_type == "CLOSER" & n_surviving == 0, 1L, 0L)]


# ---------------------------------------------------------------------
# 3. Market Denominators (TOTAL MARKET - All Banks)
# ---------------------------------------------------------------------
county_market_totals <- bank_county_panel[YEAR < 2025, .(
  branches_county_curr = sum(n_branches),
  total_deps_county_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(county, YEAR)]

setorder(county_market_totals, county, YEAR)
county_market_totals[, branches_county_lag1 := shift(branches_county_curr, 1L, type = "lag"), by = county]


# ---------------------------------------------------------------------
# 4. Aggregate INCUMBENTS
# ---------------------------------------------------------------------
# 4A. Large Incumbents
incumbent_agg_large <- bank_county_panel[, .(
  inc_deps_curr_lg  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT" & is_large == 1]),
  inc_deps_lead3_lg = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT" & is_large == 1]),
  inc_deps_lead1_lg = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT" & is_large == 1]),
  inc_deps_lag1_lg  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT" & is_large == 1])
), by = .(county, YEAR)]

# 4B. Small Incumbents
incumbent_agg_small <- bank_county_panel[, .(
  inc_deps_curr_sm  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT" & is_large == 0]),
  inc_deps_lead3_sm = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT" & is_large == 0]),
  inc_deps_lead1_sm = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT" & is_large == 0]),
  inc_deps_lag1_sm  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT" & is_large == 0])
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 5. Aggregate CLOSERS (Large & Small + Exits)
# ---------------------------------------------------------------------

# 5A. Large Closers
# Merge the 'is_exit' flag back to branch data to split the shock
closer_keys_lg <- bank_county_panel[bank_type == "CLOSER" & is_large == 1, .(RSSDID, county, YEAR, is_exit)]
relevant_branches_lg <- merge(closure_opening_data, closer_keys_lg, by = c("RSSDID", "county", "YEAR"))

closer_agg_large <- relevant_branches_lg[, .(
  # Standard Large Closure Volume
  cl_closed_lag1_lg      = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  
  # Split: Exit vs Stay
  cl_closed_lag1_lg_exit = sum_na_safe(fifelse(closed == 1L & is_exit == 1L, dep_lag1_aligned, NA_real_)),
  cl_closed_lag1_lg_stay = sum_na_safe(fifelse(closed == 1L & is_exit == 0L, dep_lag1_aligned, NA_real_)),
  
  cl_opened_curr_lg      = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  cl_rem_lag1_lg         = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR)]

# 5B. Small Closers
closer_keys_sm <- bank_county_panel[bank_type == "CLOSER" & is_large == 0, .(RSSDID, county, YEAR)]
relevant_branches_sm <- merge(closure_opening_data, closer_keys_sm, by = c("RSSDID", "county", "YEAR"))

closer_agg_small <- relevant_branches_sm[, .(
  cl_closed_lag1_sm    = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  cl_opened_curr_sm    = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  cl_rem_lag1_sm       = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 6. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- county_market_totals 
final_panel <- merge(final_panel, incumbent_agg_large, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, incumbent_agg_small, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_large, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_small, by = c("county", "YEAR"), all.x = TRUE)

# --- 6a. Fill NAs with 0 ---
cols_to_zero <- c(
  # Large Closers
  "cl_closed_lag1_lg", "cl_closed_lag1_lg_exit", "cl_closed_lag1_lg_stay", 
  "cl_opened_curr_lg", "cl_rem_lag1_lg",
  # Small Closers
  "cl_closed_lag1_sm", "cl_opened_curr_sm", "cl_rem_lag1_sm",
  # Incumbents
  "inc_deps_curr_sm", "inc_deps_lead3_sm", "inc_deps_lead1_sm", "inc_deps_lag1_sm",
  "inc_deps_curr_lg", "inc_deps_lead3_lg", "inc_deps_lead1_lg", "inc_deps_lag1_lg"
) 
for(j in cols_to_zero){
  if (j %in% names(final_panel)) {
    set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
  }
}

# --- 6b. Construct TOTAL (ALL) Aggregates ---
final_panel[, `:=`(
  # Incumbents
  inc_deps_curr_all  = inc_deps_curr_lg  + inc_deps_curr_sm,
  inc_deps_lead1_all = inc_deps_lead1_lg + inc_deps_lead1_sm,
  inc_deps_lead3_all = inc_deps_lead3_lg + inc_deps_lead3_sm,
  inc_deps_lag1_all  = inc_deps_lag1_lg  + inc_deps_lag1_sm,
  
  # Closers
  cl_closed_lag1_all = cl_closed_lag1_lg + cl_closed_lag1_sm,
  cl_opened_curr_all = cl_opened_curr_lg + cl_opened_curr_sm,
  cl_rem_lag1_all    = cl_rem_lag1_lg    + cl_rem_lag1_sm
)]


# --- 6c. COEXISTENCE FILTER ---
# Keep only markets where Large Incumbents remain.
final_panel <- final_panel[inc_deps_curr_lg >0]


final_panel[, `:=`(
  # --- Independent Variables ---
  
  # 1. Share of LARGE Closed Deposits
  share_deps_closed_lg = fifelse(total_deps_county_lag1 > 0, 
                                 cl_closed_lag1_lg / total_deps_county_lag1, 0),
  
  # 1a. Share of LARGE Closed - EXIT (Bank left county)
  share_deps_closed_lg_exit = fifelse(total_deps_county_lag1 > 0, 
                                      cl_closed_lag1_lg_exit / total_deps_county_lag1, 0),
  
  # 1b. Share of LARGE Closed - STAY (Bank consolidated)
  share_deps_closed_lg_stay = fifelse(total_deps_county_lag1 > 0, 
                                      cl_closed_lag1_lg_stay / total_deps_county_lag1, 0),
  
  # 2. Share of SMALL Closed Deposits (Requested by Phil)
  share_deps_closed_sm = fifelse(total_deps_county_lag1 > 0, 
                                 cl_closed_lag1_sm / total_deps_county_lag1, 0),
  
  # 3. Share of ALL Closed Deposits
  share_deps_closed_all = fifelse(total_deps_county_lag1 > 0, 
                                  cl_closed_lag1_all / total_deps_county_lag1, 0),
  
  
  # --- Dependent Variables: LARGE Incumbents ---
  incumbent_dep_gr_1yr_lg = fifelse(total_deps_county_lag1 > 0,
                                    (inc_deps_lead1_lg - inc_deps_curr_lg) / inc_deps_curr_lg, NA_real_),
  
  incumbent_dep_gr_1yr_cd_lg = fifelse(total_deps_county_lag1 > 0,
                                       (inc_deps_lead1_lg - inc_deps_curr_lg) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_1yr_cd_lag_lg = fifelse(total_deps_county_lag1 > 0,
                                           (inc_deps_lead1_lg - inc_deps_lag1_lg) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_lg = fifelse(total_deps_county_lag1 > 0,
                                    (inc_deps_lead3_lg - inc_deps_curr_lg) / inc_deps_curr_lg, NA_real_),
  
  incumbent_dep_gr_3yr_cd_lg = fifelse(total_deps_county_lag1 > 0,
                                       (inc_deps_lead3_lg - inc_deps_curr_lg) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_lag_lg = fifelse(total_deps_county_lag1 > 0,
                                        (inc_deps_lead3_lg - inc_deps_lag1_lg) / inc_deps_lag1_lg, NA_real_),
  
  incumbent_dep_gr_3yr_cd_lag_lg = fifelse(total_deps_county_lag1 > 0,
                                           (inc_deps_lead3_lg - inc_deps_lag1_lg) / total_deps_county_lag1, NA_real_),
  
  # --- Dependent Variables: SMALL Incumbents ---
  incumbent_dep_gr_1yr_cd_sm = fifelse(total_deps_county_lag1 > 0,
                                       (inc_deps_lead1_sm - inc_deps_curr_sm) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_cd_sm = fifelse(total_deps_county_lag1 > 0,
                                       (inc_deps_lead3_sm - inc_deps_curr_sm) / total_deps_county_lag1, NA_real_),
  
  # --- Dependent Variables: ALL Incumbents ---
  incumbent_dep_gr_1yr_cd_all = fifelse(total_deps_county_lag1 > 0,
                                        (inc_deps_lead1_all - inc_deps_curr_all) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_cd_all = fifelse(total_deps_county_lag1 > 0,
                                        (inc_deps_lead3_all - inc_deps_curr_all) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_cd_lag_all = fifelse(total_deps_county_lag1 > 0,
                                            (inc_deps_lead3_all - inc_deps_lag1_all) / total_deps_county_lag1, NA_real_),
  
  # --- Controls ---
  # 1. Incumbent Market Share (Lag) - VARIANTS
  incumbent_mkt_share_lag1_lg = fifelse(total_deps_county_lag1 > 0, inc_deps_lag1_lg / total_deps_county_lag1, 0),
  incumbent_mkt_share_lag1_sm = fifelse(total_deps_county_lag1 > 0, inc_deps_lag1_sm / total_deps_county_lag1, 0),
  incumbent_mkt_share_lag1_all= fifelse(total_deps_county_lag1 > 0, inc_deps_lag1_all/ total_deps_county_lag1, 0),
  
  # 2. Closer Remaining Market Share (Lag) - VARIANTS
  closer_remaining_mkt_share_lag1_lg = fifelse(total_deps_county_lag1 > 0, cl_rem_lag1_lg / total_deps_county_lag1, 0),
  closer_remaining_mkt_share_lag1_sm = fifelse(total_deps_county_lag1 > 0, cl_rem_lag1_sm / total_deps_county_lag1, 0),
  closer_remaining_mkt_share_lag1_all= fifelse(total_deps_county_lag1 > 0, cl_rem_lag1_all/ total_deps_county_lag1, 0),
  
  # 3. Legacy pointers (keep for backward compatibility if needed, pointing to Large as default)
  incumbent_mkt_share_lag1        = fifelse(total_deps_county_lag1 > 0, inc_deps_lag1_lg / total_deps_county_lag1, 0),
  closer_remaining_mkt_share_lag1 = fifelse(total_deps_county_lag1 > 0, cl_rem_lag1_lg / total_deps_county_lag1, 0),
  
  log_total_market_size_lag1 = log(total_deps_county_lag1 + 1),
  relocation_openings_vol_share = fifelse(total_deps_county_lag1 > 0, cl_opened_curr_lg / total_deps_county_lag1, 0)
)]


# ---------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------
output_columns <- c("county", "YEAR", 
                    
                    # Dependent (Large)
                    "incumbent_dep_gr_1yr_lg",
                    "incumbent_dep_gr_1yr_cd_lg",
                    "incumbent_dep_gr_1yr_cd_lag_lg",
                    "incumbent_dep_gr_3yr_lg",
                    "incumbent_dep_gr_3yr_lag_lg",
                    "incumbent_dep_gr_3yr_cd_lg", 
                    "incumbent_dep_gr_3yr_cd_lag_lg",
                    
                    # Dependent (Small)
                    "incumbent_dep_gr_1yr_cd_sm",
                    "incumbent_dep_gr_3yr_cd_sm",
                    
                    # Dependent (All)
                    "incumbent_dep_gr_1yr_cd_all",
                    "incumbent_dep_gr_3yr_cd_all",
                    "incumbent_dep_gr_3yr_cd_lag_all",
                    
                    # Shocks
                    "share_deps_closed_lg",      # Large Total
                    "share_deps_closed_lg_exit", # Large Exit
                    "share_deps_closed_lg_stay", # Large Consolidation
                    "share_deps_closed_sm",      # Small Total
                    "share_deps_closed_all",     # All Total
                    
                    # Controls (UPDATED)
                    "log_total_market_size_lag1",
                    "incumbent_mkt_share_lag1_lg",  "incumbent_mkt_share_lag1_sm",  "incumbent_mkt_share_lag1_all",
                    "closer_remaining_mkt_share_lag1_lg", "closer_remaining_mkt_share_lag1_sm", "closer_remaining_mkt_share_lag1_all",
                    "incumbent_mkt_share_lag1",      # Legacy
                    "closer_remaining_mkt_share_lag1", # Legacy
                    
                    "relocation_openings_vol_share",
                    "total_deps_county_lag1",
                    "branches_county_lag1")

final_panel_clean <- final_panel[, ..output_columns]

saveRDS(final_panel_clean, output_path)