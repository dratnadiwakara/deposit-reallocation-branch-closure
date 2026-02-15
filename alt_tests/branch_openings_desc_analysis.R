# ==============================================================================
# Descriptive Analysis of Branch Openings
# ==============================================================================
# This script provides comprehensive descriptive statistics on branch openings:
# 1. Time trends (2000-2025)
# 2. Bank size category analysis (using inflation-adjusted thresholds)
# 3. Geographic patterns at county level
# ==============================================================================

rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(tidyr)
library(maps)
library(viridis)

# Set data directory
data_dir <- "C:/OneDrive/data/nrs_branch_closure"

# ==============================================================================
# 1. Load Core Data
# ==============================================================================

cat("Loading data...\n")

# Load branch data
branch_year <- readRDS('C:/OneDrive/data/fdic_sod_1995_2025_simple.rds')
setDT(branch_year)
setnames(branch_year, "YEAR", "yr")
branch_year[, zip := str_pad(ZIPBR, 5, "left", "0")]
branch_year[, STCNTYBR := str_pad(STCNTYBR, 5, "left", "0")]

# Load CPI data
cpi <- readRDS(file.path(data_dir, "cpi.rds"))
setDT(cpi)

# Load deposit beta models (contains bank characteristics)
beta_cycles <- readRDS(file.path(data_dir, "deposit_beta_results.rds"))

# Load call report data for complete bank size history
call_data <- readRDS(file.path(data_dir, "call_data_01282026.rds"))
setDT(call_data)
setnames(call_data, "total_assets", "bank_assets")
call_data[, `:=`(
  yr = year(D_DT),
  month = month(D_DT)
)]
call_data <- call_data[month == 12, .(ID_RSSD, yr, bank_assets)]

# ==============================================================================
# 2. Identify New Branch Openings
# ==============================================================================

cat("Identifying new branch openings...\n")

# Flag first year each branch appears
setorder(open_data <- copy(branch_year), UNINUMBR, yr)
open_data[, new_branch := fifelse(is.na(shift(yr)), 1L, 0L), by = UNINUMBR]

# Exclude OTS-to-FDIC regulatory transfers in 2011
banks_with_openings <- open_data[new_branch == 1 & yr %between% c(2004, 2011), 
                                 .(has_opening = 1), 
                                 by = .(RSSDID, yr)]

bank_opening_pattern <- dcast(banks_with_openings, RSSDID ~ yr, 
                              value.var = "has_opening", fill = 0)

bank_opening_pattern[, no_openings_2004_2010 := 
                       `2004` == 0 & `2005` == 0 & `2006` == 0 & 
                       `2007` == 0 & `2008` == 0 & `2009` == 0 & 
                       `2010` == 0]

ots_transfer_banks <- bank_opening_pattern[
  no_openings_2004_2010 == TRUE & `2011` > 0, 
  RSSDID
]

open_data <- open_data[!RSSDID %in% ots_transfer_banks]

# Filter to 2000+ and actual openings
new_branches <- open_data[yr >= 2000 & new_branch == 1]

cat("Total new branches 2000-2025:", nrow(new_branches), "\n")

# ==============================================================================
# 3. Create Inflation-Adjusted Bank Size Categories
# ==============================================================================

cat("Creating bank size categories...\n")

# Calculate adjustment ratio for each year (base year 2019)
cpi[, adj_ratio := CPI / CPI[year == 2019]]

# Define size thresholds in 2019 dollars
size_categories <- data.table(
  category = c("Large (>$100B)", "Regional ($10B-$100B)", 
               "Community ($1B-$10B)", "Small (<$1B)"),
  threshold_2019_upper = c(Inf, 100e6, 10e6, 1e6),
  threshold_2019_lower = c(100e6, 10e6, 1e6, 0),
  order = 1:4
)

# Create year-specific thresholds
year_thresholds <- cpi[year >= 2000, .(year, adj_ratio)]
year_thresholds <- year_thresholds[rep(1:.N, each = nrow(size_categories))]
year_thresholds[, category := rep(size_categories$category, 
                                  length(unique(year)))]
year_thresholds[, order := rep(size_categories$order, 
                               length(unique(year)))]

# Merge thresholds
year_thresholds <- merge(year_thresholds, 
                         size_categories[, .(category, threshold_2019_upper, 
                                             threshold_2019_lower)],
                         by = "category")

# Adjust thresholds by inflation
year_thresholds[, `:=`(
  threshold_upper = threshold_2019_upper * adj_ratio,
  threshold_lower = threshold_2019_lower * adj_ratio
)]

# Assign size categories to banks
bank_sizes <- merge(call_data, year_thresholds, 
                    by.x = "yr", by.y = "year", 
                    allow.cartesian = TRUE)

bank_sizes[, in_category := bank_assets > threshold_lower & 
             bank_assets <= threshold_upper]
bank_sizes <- bank_sizes[in_category == TRUE]
bank_sizes <- bank_sizes[, .(ID_RSSD, yr, category, order, bank_assets)]

# Merge size categories with new branches
new_branches_sized <- merge(new_branches, bank_sizes,
                            by.x = c("RSSDID", "yr"),
                            by.y = c("ID_RSSD", "yr"),
                            all.x = TRUE)

cat("Branches with size data:", 
    nrow(new_branches_sized[!is.na(category)]), "\n")

# ==============================================================================
# 4. ANALYSIS 1: Time Trends in Branch Openings
# ==============================================================================

cat("\n=== ANALYSIS 1: Time Trends ===\n")

# Overall trend
openings_by_year <- new_branches[, .N, by = yr]
setorder(openings_by_year, yr)

cat("\nBranch Openings by Year (2000-2025):\n")
print(openings_by_year)

# Create plot
p1 <- ggplot(openings_by_year, aes(x = yr, y = N)) +
  geom_line(linewidth = 1.2, color = "#2c3e50") +
  geom_point(size = 3, color = "#2c3e50") +
  geom_vline(xintercept = 2008, linetype = "dashed", 
             color = "#e74c3c", alpha = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", 
             color = "#e74c3c", alpha = 0.5) +
  annotate("text", x = 2008, y = max(openings_by_year$N) * 0.95, 
           label = "Financial Crisis", angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 2020, y = max(openings_by_year$N) * 0.95, 
           label = "COVID-19", angle = 90, vjust = -0.5, size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "New Branch Openings Over Time",
    subtitle = "U.S. Banking Industry, 2000-2025",
    x = "Year",
    y = "Number of New Branches",
    caption = "Source: FDIC Summary of Deposits\nNote: Excludes OTS regulatory transfers in 2011"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(p1)

# ==============================================================================
# 5. ANALYSIS 2: Openings by Bank Size Category
# ==============================================================================

cat("\n=== ANALYSIS 2: Bank Size Analysis ===\n")

# Aggregate by size and year
openings_by_size <- new_branches_sized[!is.na(category), 
                                       .N, 
                                       by = .(yr, category, order)]
setorder(openings_by_size, yr, order)

# Summary statistics by size category
size_summary <- new_branches_sized[!is.na(category), 
                                   .(
                                     total_openings = .N,
                                     pct_of_total = .N / nrow(new_branches_sized[!is.na(category)]) * 100,
                                     avg_per_year = .N / length(unique(yr)),
                                     first_year = min(yr),
                                     last_year = max(yr)
                                   ), 
                                   by = .(category, order)]
setorder(size_summary, order)

cat("\nBranch Openings by Bank Size Category (2000-2025):\n")
print(size_summary)

# Plot: Stacked area chart
p2 <- ggplot(openings_by_size, aes(x = yr, y = N, fill = category)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(
    values = c("Large (>$100B)" = "#2c3e50",
               "Regional ($10B-$100B)" = "#3498db",
               "Community ($1B-$10B)" = "#2ecc71",
               "Small (<$1B)" = "#f39c12"),
    name = "Bank Size\n(2019 dollars)"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "New Branch Openings by Bank Size",
    subtitle = "Size categories adjusted for inflation (base year: 2019)",
    x = "Year",
    y = "Number of New Branches",
    caption = "Source: FDIC Summary of Deposits and Call Reports"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(p2)



# ==============================================================================
# 6. ANALYSIS 3: Geographic Patterns - County Level
# ==============================================================================

cat("\n=== ANALYSIS 3: Geographic Analysis ===\n")

# Aggregate openings by county
county_openings <- new_branches[, .N, by = STCNTYBR]
setnames(county_openings, "N", "new_branches")

# Get total branches and deposits by county (most recent year)
county_totals <- branch_year[yr == max(yr), 
                             .(
                               total_branches = .N,
                               total_deposits = sum(DEPSUMBR, na.rm = TRUE)
                             ),
                             by = STCNTYBR]

# Merge
county_stats <- merge(county_openings, county_totals, 
                      by = "STCNTYBR", all = TRUE)
county_stats[is.na(new_branches), new_branches := 0]

# Calculate relative metrics
county_stats[, `:=`(
  opening_rate = (new_branches / total_branches) * 100,
  openings_per_billion = (new_branches / (total_deposits / 1e9))
)]

# Parse state and county FIPS
county_stats[, `:=`(
  state_fips = substr(STCNTYBR, 1, 2),
  county_fips = substr(STCNTYBR, 3, 5),
  fips = as.numeric(STCNTYBR)
)]

# Top counties by absolute openings
top_counties_abs <- county_stats[order(-new_branches)][1:20]

cat("\nTop 20 Counties by Number of Branch Openings:\n")
print(top_counties_abs[, .(STCNTYBR, new_branches, total_branches, 
                           opening_rate, total_deposits)])

# Top counties by opening rate
top_counties_rate <- county_stats[total_branches >= 10][order(-opening_rate)][1:20]

cat("\nTop 20 Counties by Opening Rate (min 10 existing branches):\n")
print(top_counties_rate[, .(STCNTYBR, new_branches, total_branches, 
                            opening_rate)])

# Summary statistics
cat("\nGeographic Summary Statistics:\n")
cat("Counties with openings:", nrow(county_stats[new_branches > 0]), "\n")
cat("Total counties:", nrow(county_stats), "\n")
cat("Median openings per county:", median(county_stats$new_branches), "\n")
cat("Mean openings per county:", 
    round(mean(county_stats$new_branches), 2), "\n")

# ==============================================================================
# 7. Create Maps
# ==============================================================================

cat("\nCreating county-level maps...\n")

# Load county map data
county_map <- map_data("county")
setDT(county_map)

# Get state FIPS codes
data(fips_codes, package = "tidycensus")
fips_lookup <- data.table(fips_codes)
fips_lookup[, fips := as.numeric(paste0(state_code, county_code))]
fips_lookup[, `:=`(
  region = tolower(state_name),
  subregion = tolower(gsub(" County| Parish| Borough| Census Area", "", county))
)]
fips_lookup <- unique(fips_lookup[, .(fips, region, subregion)])

# Merge with county stats
county_map_data <- merge(county_map, fips_lookup, 
                         by = c("region", "subregion"), 
                         all.x = TRUE)
county_map_data <- merge(county_map_data, county_stats, 
                         by = "fips", all.x = TRUE)
county_map_data[is.na(new_branches), new_branches := 0]

# MAP 1: Absolute number of openings
# Create categorical breaks for better visualization
opening_breaks <- c(0, 1, 10, 25, 50, 100, 200, Inf)
opening_labels <- c("0", "1-9", "10-24", "25-49", "50-99", "100-199", "200+")

county_map_data[, openings_cat := cut(new_branches, 
                                      breaks = opening_breaks,
                                      labels = opening_labels,
                                      include.lowest = TRUE,
                                      right = FALSE)]

p4 <- ggplot(county_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = openings_cat), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "0" = "gray95",
      "1-9" = "#ffffcc",
      "10-24" = "#c7e9b4",
      "25-49" = "#7fcdbb",
      "50-99" = "#41b6c4",
      "100-199" = "#2c7fb8",
      "200+" = "#253494"
    ),
    name = "New Branches\n(2000-2025)",
    drop = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  labs(
    title = "Branch Openings by County",
    subtitle = "Total new branches, 2000-2025",
    caption = "Source: FDIC Summary of Deposits"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    legend.position = "right"
  )

print(p4)

# MAP 2: Opening rate (as % of existing branches)
# Only for counties with at least 5 branches to avoid noise
county_map_data[, rate_cat := fcase(
  total_branches < 5 | is.na(opening_rate), "Too few branches",
  opening_rate == 0, "0%",
  opening_rate <= 5, "0-5%",
  opening_rate <= 10, "5-10%",
  opening_rate <= 20, "10-20%",
  opening_rate <= 30, "20-30%",
  default = "30%+"
)]

county_map_data[, rate_cat := factor(rate_cat,
                                     levels = c("Too few branches", "0%", "0-5%", "5-10%", 
                                                "10-20%", "20-30%", "30%+"))]

p5 <- ggplot(county_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate_cat), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "Too few branches" = "gray90",
      "0%" = "gray80",
      "0-5%" = "#f0f9e8",
      "5-10%" = "#bae4bc",
      "10-20%" = "#7bccc4",
      "20-30%" = "#43a2ca",
      "30%+" = "#0868ac"
    ),
    name = "Opening Rate",
    drop = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  labs(
    title = "Branch Opening Rate by County",
    subtitle = "New branches as % of 2025 branch count (counties with 5+ branches)",
    caption = "Source: FDIC Summary of Deposits"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5, size = 10),
    legend.position = "right"
  )

print(p5)

# MAP 3: Openings per billion in deposits
# Better approach: Use percentile-based breaks and log scale for better visualization
county_map_data[, has_intensity := !is.na(openings_per_billion) & 
                  total_deposits > 0 & 
                  new_branches > 0]

# Calculate percentile breaks for non-zero values
intensity_quantiles <- quantile(county_map_data[has_intensity == TRUE]$openings_per_billion,
                                probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1),
                                na.rm = TRUE)

cat("\nOpening Intensity Distribution (per $1B deposits):\n")
print(intensity_quantiles)

# Create categorical version for better visualization
county_map_data[, intensity_cat := fcase(
  new_branches == 0 | is.na(openings_per_billion), "No openings",
  openings_per_billion <= intensity_quantiles[2], "Low (0-25th)",
  openings_per_billion <= intensity_quantiles[3], "Low-Med (25-50th)",
  openings_per_billion <= intensity_quantiles[4], "Medium (50-75th)",
  openings_per_billion <= intensity_quantiles[5], "Med-High (75-90th)",
  openings_per_billion <= intensity_quantiles[6], "High (90-95th)",
  default = "Very High (95+th)"
)]

county_map_data[, intensity_cat := factor(intensity_cat, 
                                          levels = c("No openings", "Low (0-25th)", "Low-Med (25-50th)", 
                                                     "Medium (50-75th)", "Med-High (75-90th)", "High (90-95th)", 
                                                     "Very High (95+th)"))]

p6 <- ggplot(county_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = intensity_cat), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "No openings" = "gray90",
      "Low (0-25th)" = "#fef0d9",
      "Low-Med (25-50th)" = "#fdd49e",
      "Medium (50-75th)" = "#fdbb84",
      "Med-High (75-90th)" = "#fc8d59",
      "High (90-95th)" = "#e34a33",
      "Very High (95+th)" = "#b30000"
    ),
    name = "Opening Intensity\n(percentile)",
    drop = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  labs(
    title = "Branch Opening Intensity by County",
    subtitle = "New branches per $1 billion in deposits (2000-2025 openings / 2025 deposits)",
    caption = "Source: FDIC Summary of Deposits\nNote: Counties with no openings shown in gray"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5, size = 10),
    legend.position = "right"
  )

print(p6)

